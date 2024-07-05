package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.execution.analyses.impl.cg.AbstractRTABuilder.TypeNode
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.InteractionType.{Initialization, InteractionType, MethodRequest, Internal}
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.{LookupRequestRepresentation, LookupResponseRepresentation, OracleInteractionError}
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.{ApplicationMethod, LookupApplicationMethodRequest, LookupApplicationMethodResponse}
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphResolutionMode.{CHA, NaiveRTA, OracleCallGraphResolutionMode, RTA}
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * This class manages concurrent communications between a client analysis application and the oracle CG builder for one
 * specific project context. It buffers and decouples outgoing lookup requests and incoming lookup responses. It also
 * asynchronously runs a main resolver loop that waits for all requests to be answered. Resolution will time out if the
 * client does not provide at least one new response every thirty seconds. This class also implements some caching to
 * reduce actual communication overhead.
 *
 *
 * @param dataAccessor The database accessor for this instance. Needed for resolving the third party libraries in the index.
 */
class InteractiveOracleAccessor(dataAccessor: DataAccessor) {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private final val clientResponseTimeoutMillis: Long = 30000

  private[cg] var oracleCGBuilderOpt: Option[OracleCallGraphBuilder] = None

  private[cg] var resolverLoopFuture: Option[Future[Unit]] = None

  private[cg] val isRunning: AtomicBoolean = new AtomicBoolean(false)

  private[cg] val outgoingRequestsBuffer: mutable.Queue[LookupRequestRepresentation] = mutable.Queue.empty
  private[cg] val outgoingRequestId: AtomicInteger = new AtomicInteger(0)
  private[cg] val outgoingRequestHashesSeen: mutable.Set[Int] = mutable.Set.empty
  private[cg] val outgoingRequestIdMap: mutable.Map[Int, LookupApplicationMethodRequest] = mutable.Map.empty

  private[cg] val incomingResponsesBuffer: mutable.Queue[LookupApplicationMethodResponse] = mutable.Queue.empty

  private[cg] val unansweredRequestIds: mutable.Set[Int] = mutable.Set.empty

  private[cg] val errors = mutable.ListBuffer.empty[OracleInteractionError]

  def hasFatalErrors: Boolean = errors.exists(_.isFatal)
  def firstFatalError: Option[OracleInteractionError] = errors.find(_.isFatal)

  private[cg] def logError(error: OracleInteractionError): OracleInteractionError = {
    log.error(error.toString)
    errors.addOne(error)
    error
  }


  /**
   * Initializes a resolution session for a given project context. After the invocation, we either have a valid OracleCallGraphBuilder
   * initialized and ready for interaction, or else errors have been returned by this method.
   * @param libraryGAVs GAVs of the libraries that are to be considered for this project context
   * @param libraryTypes Types present in the project context (not libraries, just project itself)
   * @param typesInstantiated The names of all types that are instantiated in the application. Only necessary for naive RTA
   * @param jreVersion Optionally a JRE version to load for resolution
   * @param mode Resolution mode to use for this oracle
   * @return An Either object that can represent a Unit value (left) or an interaction error (right) which will during initialization always be fatal
   */
  def initialize(libraryGAVs: Set[String],
                 libraryTypes: Set[TypeNode],
                 typesInstantiated: Set[String],
                 jreVersion: Option[String],
                 mode: OracleCallGraphResolutionMode): Either[Unit, OracleInteractionError] = {
    // Avoid dual initialization - if that happens, that's a non-fatal user error
    if(oracleCGBuilderOpt.isDefined){
      val error = OracleInteractionError("Got a second initialization message, ignoring.", isFatal = false, isUserError = true, Initialization)
      return Right(logError(error))
    }

    // Make sure the library GAVs are valid and stored in the index
    for(libraryGAV <- libraryGAVs){
      JavaEntities.gavToProgramIdent(libraryGAV) match {
        case Some(ident) if dataAccessor.hasEntity(ident, SoftwareEntityKind.Program) =>
          // Good case, don't do anything here
        case Some(_) =>
          val error = OracleInteractionError(s"Library not in index: $libraryGAVs", isFatal = true, isUserError = false, interactionType = Initialization)
          return Right(logError(error))
        case None =>
          val error = OracleInteractionError(s"Invalid library GAV: $libraryGAV", isFatal = true, isUserError = true, interactionType = Initialization)
          return Right(logError(error))
      }
    }

    Try {
      val libraries = libraryGAVs.map(JavaEntities.gavToProgramIdent(_).get).map(dataAccessor.awaitGetEntity(_, None).get.asInstanceOf[JavaProgram])
      val builder = new OracleCallGraphBuilder(libraries, libraryTypes, jreVersion, queueRequest)

      mode match {
        case CHA => builder.useCHA()
        case NaiveRTA => builder.useNaiveRTA(typesInstantiated)
        case RTA => builder.useRTA()
      }

      builder
    } match {
      case Success(builder) =>
        //noinspection ScalaUnnecessaryParentheses
        Left((oracleCGBuilderOpt = Some(builder)))
      case Failure(ex) =>
        oracleCGBuilderOpt = None
        val error = OracleInteractionError(s"Got an unexpected error while initializing: ${ex.getMessage}", isFatal = true, isUserError = false, Initialization)
        Right(logError(error))
    }
  }

  /**
   * Main interface for starting CG resolution at a specific invocation statement inside an application method. Will start
   * the asynchronous resolution loop in the background and start producing lookup requests. A caller is required to check
   * the outgoing request buffer (nextRequest) and to send those requests to the client. Incoming responses must be returned
   * to this instance via pushResponse. Only when all requests have been responded to, the execution will finish.
   *
   *
   * @param callingContext Application method that the entry call has been made in
   * @param ccPC PC that the entry call has been made at
   * @param typesInstantiated Types that have been instantiated so far (at the call site). Only needed for full-blown RTA.
   * @param ec Implicit execution context to run the main resolver loop on
   * @return Either a unit value (if successful) or an OracleInteractionError
   */
  def startResolution(callingContext: ApplicationMethod,
                      ccPC: Int,
                      typesInstantiated: Set[String])(implicit ec: ExecutionContext): Either[Unit, OracleInteractionError] = {
    if(isRunning.get()){
      val error = OracleInteractionError(s"Request for a new entry point while still resolving - wait for resolution to finish!",
        isFatal = false, isUserError = true, interactionType = MethodRequest)
      logError(error)
      Right(error)
    } else {
      isRunning.set(true)
      resolverLoopFuture = Some(Future(runBuilderLoop(callingContext, ccPC, typesInstantiated)))
      Left(())
    }
  }

  /**
   * This method handles the interaction with the actual OracleCallGraphBuilder. It triggers the processing of all responses
   * that are in the buffer, and makes sure that we wait for all responses to be in before we terminate. It will also
   * stop on any fatal error
   *
   * @param cc Calling context of this library invocation, i.e. the application method containing an library entry point call
   * @param ccPC: PC of the entry call inside the calling method(cc)
   * @param typesInstantiated Set of type names that are instantiated for this entry point
   */
  private[cg] def runBuilderLoop(cc: ApplicationMethod, ccPC: Int, typesInstantiated: Set[String]): Unit = {

    var lastResponseTime = System.currentTimeMillis()
    var hasNewResponses = incomingResponsesBuffer.synchronized{ incomingResponsesBuffer.nonEmpty }
    var isWaitingForResponses = unansweredRequestIds.synchronized{ unansweredRequestIds.nonEmpty }

    Try {
      oracleCGBuilderOpt.get.buildFromApplicationMethod(cc, typesInstantiated, ccPC)
    }.flatten match {
      case Success(view) =>
        log.info(s"Successfully finished initial computations starting at ${cc.definingTypeName}.${cc.methodName} PC $ccPC - got ${view.reachableMethods().size} reachable methods")
      case Failure(ex) =>
        // Will immediately skip following loop, as this is a fatal error
        val error = OracleInteractionError(s"Internal error during initial computations starting at ${cc.definingTypeName}.${cc.methodName} PC $ccPC: ${ex.getMessage}", isFatal = true, isUserError = false, interactionType = Internal)
        logError(error)
    }

    while((hasNewResponses || isWaitingForResponses) && !hasFatalErrors){

      if(hasNewResponses){
        val response = incomingResponsesBuffer.synchronized{ incomingResponsesBuffer.dequeue() }

        lastResponseTime = System.currentTimeMillis()

        oracleCGBuilderOpt.get.processResponse(response)
      } else {
        log.info(s"Waiting for responses on ${unansweredRequestIds.size} requests from client")
        Thread.sleep(1000)

        if(System.currentTimeMillis() - lastResponseTime > clientResponseTimeoutMillis){
          val error = OracleInteractionError(s"Timed out while waiting for client response (timeout $clientResponseTimeoutMillis ms)", isFatal = true, isUserError = true, MethodRequest)
          logError(error) // Will add error to List and break out of the loop on next iteration
        }
      }

      hasNewResponses = incomingResponsesBuffer.synchronized{ incomingResponsesBuffer.nonEmpty }
      isWaitingForResponses = unansweredRequestIds.synchronized{ unansweredRequestIds.nonEmpty }
    }

    if(hasFatalErrors){
      log.error(s"Stopped CG builder loop due to fatal errors: ${firstFatalError.get.toString}")
    } else {
      log.info(s"Finished main CG builder loop, all requests have been answered.")
    }

    isRunning.set(false)
  }

  /**
   * This method is a callback for the internal CG builder to request method definitions from the client. It will perform some
   * housekeeping, assign a unique request ID and avoid duplicate requests. It will store the request and push a (data-minimal)
   * representation onto the outgoing message queue (in a threadsafe manner).
   *
   * @param request The request to send to the client
   */
  private[cg] def queueRequest(request: LookupApplicationMethodRequest): Unit = {
    outgoingRequestIdMap.synchronized{
      val requestHash = request.hashCode()

      if(!outgoingRequestHashesSeen.contains(requestHash)){
        val id = outgoingRequestId.getAndIncrement()
        outgoingRequestIdMap.put(id, request)
        outgoingRequestHashesSeen.add(requestHash)

        outgoingRequestsBuffer.synchronized {
          val requestRepresentation = LookupRequestRepresentation(id, request.mInvokeType, request.mName, request.mDescriptor, request.types)
          outgoingRequestsBuffer.enqueue(requestRepresentation)
        }

        unansweredRequestIds.synchronized{ unansweredRequestIds.add(id) }
      } else {
        log.warn(s"Skipping request that has already been sent to client")
      }
    }
  }

  /**
   * This method is used to push client responses into the internal resolution loop. The data-minimal response representation
   * passed from the API is being rebuilt into a full response object, and pushed onto the internal response buffer (in a threadsafe manner).
   *
   * @param response The (data-minimal) response representation
   * @param ec Execution context for the asynchronous execution
   * @return A future with no content that finishes once the response has been pushed onto the buffer
   */
  def pushResponse(response: LookupResponseRepresentation)(implicit ec: ExecutionContext): Future[Unit] = Future {
    val originalRequest = outgoingRequestIdMap.synchronized{ outgoingRequestIdMap(response.requestId) }

    val responseObj = LookupApplicationMethodResponse(originalRequest.ccIdent, originalRequest.ccPC,
      originalRequest.mName, originalRequest.mDescriptor, originalRequest.types, response.targetMethods,
      response.typesWithoutMethodDefinition)

    incomingResponsesBuffer.synchronized {
      incomingResponsesBuffer.enqueue(responseObj)
      unansweredRequestIds.synchronized{ unansweredRequestIds.remove(response.requestId) }
    }

  }

  /**
   * This method is used to retrieve the next request from the internal request buffer. This request must then be
   * delivered to the client, so it can produce a response and pass it to this instance via 'pushResponse'.
   * @param ec Execution context for the asynchronous execution
   * @return Future of an optional LookupRequestRepresentation - might be None if there are no request at the moment
   */
  def nextRequest()(implicit ec: ExecutionContext): Future[Option[LookupRequestRepresentation]] = Future {
    outgoingRequestsBuffer.synchronized{
      if(outgoingRequestsBuffer.nonEmpty) Some(outgoingRequestsBuffer.dequeue()) else None
    }
  }

  def isResolving: Boolean = isRunning.get()

  def succeeded: Boolean = resolverLoopFuture.exists(f => f.isCompleted && f.value.get.isSuccess)
  def failed: Boolean = resolverLoopFuture.exists(f => f.isCompleted && f.value.get.isFailure)

}

object InteractiveOracleAccessor {

  case class OracleInteractionError(message: String, isFatal: Boolean, isUserError: Boolean, interactionType: InteractionType) {
    override def toString: String = s"${ if(isFatal) "Fatal " else  "Non-Fatal " }${ if(isUserError) "User-Error " else "Error " } : $message"
  }

  case class LookupRequestRepresentation(requestId: Int, invocationTypeId: Int, mName: String, mDescriptor: String, targetTypes: Set[String])

  case class LookupResponseRepresentation(requestId: Int, targetMethods: Set[ApplicationMethod], typesWithoutMethodDefinition: Set[String])


  object InteractionType extends Enumeration {

    type InteractionType = Value

    val Initialization: Value = Value(0)
    val MethodRequest: Value = Value(1)
    val Internal: Value = Value(2)

  }
}
