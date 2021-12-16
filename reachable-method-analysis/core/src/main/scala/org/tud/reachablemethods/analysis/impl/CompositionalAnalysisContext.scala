package org.tud.reachablemethods.analysis.impl

import org.opalj.br.{ClassHierarchy, Method, ObjectType}
import org.opalj.br.analyses.Project
import org.slf4j.{Logger, LoggerFactory}
import org.tud.reachablemethods.analysis.dataaccess.{ElasticMethodData, InvocationObligation, MethodDataAccessor}
import org.tud.reachablemethods.analysis.model.MavenIdentifier

import java.net.URL
import scala.collection.mutable
import scala.util.{Failure, Success}

class CompositionalAnalysisContext(classFileFqnDependencyMap: Map[String, MavenIdentifier],
                                   methodDataAccessor: MethodDataAccessor, opalProject: Project[URL]){

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val instantiatedTypeNames: mutable.Set[String] = new mutable.HashSet[String]()

  private val elasticIdMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()
  private val signatureMethodDataIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()

  private val fqnTypeIndex: mutable.Map[String, ObjectType] = new mutable.HashMap()
  private val typeFqnMethodObjectIndex: mutable.Map[String, mutable.Set[Method]] = new mutable.HashMap()

  private val uidObligationIndex: mutable.Map[String, Option[Iterable[Either[Method, ElasticMethodData]]]] = new mutable.HashMap()


  log.debug("Indexing all types by FQN...")
  buildFqnTypeIndex()
  buildSignatureMethodIndex()
  log.debug("Done indexing all types.")
  log.debug("Indexing all dependency methods...")
  loadAllDependencies()
  log.debug("Done indexing all dependencies.")


  lazy val classHierarchy: ClassHierarchy = opalProject.classHierarchy

  lazy val instantiatedTypes: Set[String] = instantiatedTypeNames.toSet

  def indexInstantiatedTypes(typeNames: Iterable[String]): Unit = {
    typeNames.foreach(instantiatedTypeNames.add)
  }

  def isTypeInstantiated(typeName: String): Boolean = instantiatedTypes.contains(typeName)

  def resolveObligationInLibrary(obligation: InvocationObligation, libraryIdent: String): Option[Iterable[Either[Method, ElasticMethodData]]]= {
    val obligationKey = obligationInLibraryUid(obligation, libraryIdent)

    //TODO: Return either of MethodData and Method, filter out seen methods
    if(uidObligationIndex.contains(obligationKey)){
      uidObligationIndex(obligationKey)
    } else {

      val declTypeOpt = fqnTypeIndex.get(obligation.declaredTypeName)

      val result = declTypeOpt.map{ declType =>

        classHierarchy.allSubtypesIterator(declType, reflexive = true)
          .filter(t => instantiatedTypes.contains(t.fqn))
          .flatMap(t => typeFqnMethodObjectIndex.getOrElse(t.fqn, Iterable.empty))
          .filter(m => (m.name + m.descriptor.valueToString).equals(obligation.methodDescription))
          .map(m => if(signatureMethodDataIndex.contains(m.fullyQualifiedSignature)) Right(signatureMethodDataIndex(m.fullyQualifiedSignature)) else Left(m))
          .toSeq
      }

      /*val result = signatureMethodObjectIndex
        .get(obligation.methodDescription)
        .map { methods =>
          methods
            .filter{ method => declTypeOpt.isEmpty || method.classFile.thisType.isSubtypeOf(declTypeOpt.get)(classHierarchy)}
            .filter{ method =>
              val cf = method.classFile

              // Only keep methods from other libraries or the current project ( We will never resolve obligations on the current project)
              //(!classFileFqnDependencyMap.contains(cf.fqn) || !classFileFqnDependencyMap(cf.fqn).libraryIdentifier.equals(libraryIdent)) &&
                instantiatedTypes.contains(cf.thisType.fqn) && opalProject.isProjectType(cf.thisType)
            }.toSeq
        }*/

      /*val result = fqnTypeIndex
        .get(obligation.declaredTypeName)
        .map{ typeObj =>

          val subtypes = classHierarchy.allSubtypesIterator(typeObj, reflexive = false)
            .filter { t =>
              val cfOpt = opalProject.classFile(t)

              (cfOpt.isEmpty || !classFileFqnDependencyMap.contains(cfOpt.get.fqn) || !classFileFqnDependencyMap(cfOpt.get.fqn).libraryIdentifier.equals(libraryIdent)) &&
                instantiatedTypes.contains(t.fqn)
            }
            //.filter(t => opalProject.isProjectType(t))//TODO: Not true, need to find subtypes in other libraries as well..all libraries except current one!

          subtypes
            .flatMap(t => opalProject.instanceMethods.getOrElse(t, Seq.empty).map(_.method))
            .filter(ctx => obligation.methodDescription.equals(ctx.name + ctx.descriptor.valueToString))
            .filter(t => !classFileFqnDependencyMap.contains(t.classFile.fqn) || !classFileFqnDependencyMap(t.classFile.fqn).libraryIdentifier.equals(libraryIdent))//TODO: Not true, need to find subtypes in other libraries as well..all libraries except current one!
            .toSeq


          /*classHierarchy.allSubtypes(typeObj, reflexive = false)
            .filter(t => isTypeInstantiated(t.fqn))
            .filter(t => opalProject.isProjectType(t)) //TODO: Not true, need to find subtypes in other libraries as well..all libraries except current one!
            .flatMap(t => opalProject.instanceMethods.getOrElse(t, Seq.empty).map(_.method))
            .filter(ctx => obligation.methodDescription.equals(ctx.name + ctx.descriptor.valueToString))*/
        }*/

      uidObligationIndex.put(obligationKey, result)

      result
    }
  }


  def getMethodBySignatureAndClass(signature: String, classFqn: String): Option[ElasticMethodData] = {
    if(signatureMethodDataIndex.contains(signature)){
      Some(signatureMethodDataIndex(signature))
    } else {
      classFileFqnDependencyMap
        .get(classFqn)
        .flatMap(ident => methodDataAccessor.getArtifactMethodBySignatures(List(signature), ident.libraryIdentifier, ident.version).toOption)
        .flatMap(hits => hits.map(h => addToMethodIndices(h)).headOption)
    }
  }

  def signatureLookup(signature: String): Option[ElasticMethodData] = {
    signatureMethodDataIndex.get(signature)
  }

  private def addToMethodIndices(methodData: ElasticMethodData, preloadCallees: Boolean = true): ElasticMethodData = {

    elasticIdMethodIndex.put(methodData.elasticId, methodData)
    signatureMethodDataIndex.put(methodData.signature, methodData)

    if(preloadCallees){
      indexCallees(methodData)
    }

    methodData
  }

  private def indexCallees(methodData: ElasticMethodData): Unit = {

    val missingSignatures = methodData.calleeSignatures.filter(!signatureMethodDataIndex.contains(_))

    if(missingSignatures.nonEmpty){
      methodDataAccessor
        .getArtifactMethodBySignatures(missingSignatures, methodData.analyzedLibrary, methodData.libraryVersion) match {
        case Success(callees) =>
          callees.foreach(c => addToMethodIndices(c, preloadCallees = false)) // For now: No transitive preload
        case Failure(ex) =>
          log.error(s"Failed to index callees of ${methodData.signature}", ex)
      }
    }
  }

  private def obligationInLibraryUid(obligation: InvocationObligation, libIdent: String): String =
    obligation.declaredTypeName + obligation.methodDescription + libIdent

  private def buildFqnTypeIndex(): Unit = {
    opalProject.allClassFiles.foreach(cf => fqnTypeIndex.put(cf.thisType.fqn, cf.thisType))
  }

  private def buildSignatureMethodIndex(): Unit = {
    opalProject.allMethods.foreach { m =>
      val methodKey = m.name + m.descriptor.valueToString

      if(!typeFqnMethodObjectIndex.contains(m.classFile.thisType.fqn)){
        typeFqnMethodObjectIndex.put(m.classFile.thisType.fqn, new mutable.HashSet[Method]())
      }

      typeFqnMethodObjectIndex(m.classFile.thisType.fqn).add(m)
    }
  }

  private def loadJreData(): Unit = {
    val jreIdent = classFileFqnDependencyMap.values.find(i => i.artifactId.equals("<jre>")).get
    methodDataAccessor.getArtifactMethods(jreIdent.libraryIdentifier, jreIdent.version) match {
      case Success(hits) =>
        hits.foreach(addToMethodIndices(_, preloadCallees = false))
      case Failure(ex) =>
        log.error("Failed to load JRE methods", ex)
    }
  }

  private def loadAllDependencies(): Unit = {
    classFileFqnDependencyMap.values.toList.distinct.foreach{ ident =>
      methodDataAccessor.getArtifactMethods(ident.libraryIdentifier, ident.version) match {
        case Success(hits) =>
          hits.foreach(addToMethodIndices(_, preloadCallees = false))
        case Failure(ex) =>
          log.error(s"Failed to load Dependency methods for ${ident.libraryIdentifier}", ex)
      }
    }
  }

}
