package de.tudo.sse.spareuse.core.storage

import de.tudo.sse.spareuse.core.model.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities._
import de.tudo.sse.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}

import scala.util.{Failure, Success, Try}

trait EntityAccessor {

  def getLibrary(ident: String, resolutionScope: SoftwareEntityKind): Try[JavaLibrary] =
    getEntityAs[JavaLibrary](ident, SoftwareEntityKind.Library, resolutionScope)

  def getProgram(ident: String, resolutionScope: SoftwareEntityKind): Try[JavaProgram] =
    getEntityAs[JavaProgram](ident, SoftwareEntityKind.Program, resolutionScope)

  def getPackage(ident: String, resolutionScope: SoftwareEntityKind): Try[JavaPackage] =
    getEntityAs[JavaPackage](ident, SoftwareEntityKind.Package, resolutionScope)

  def getClass(ident: String, resolutionScope: SoftwareEntityKind): Try[JavaClass] =
    getEntityAs[JavaClass](ident, SoftwareEntityKind.Class, resolutionScope)

  def getMethod(ident: String, resolutionScope: SoftwareEntityKind): Try[JavaMethod] =
    getEntityAs[JavaMethod](ident, SoftwareEntityKind.Method, resolutionScope)

  def getInvokeStatement(ident: String): Try[JavaInvokeStatement] =
    getEntityAs[JavaInvokeStatement](ident, SoftwareEntityKind.InvocationStatement, SoftwareEntityKind.InvocationStatement)

  def getFieldStatement(ident: String): Try[JavaFieldAccessStatement] =
    getEntityAs[JavaFieldAccessStatement](ident, SoftwareEntityKind.FieldAccessStatement, SoftwareEntityKind.FieldAccessStatement)


  def initializeEntityTables(): Unit

  def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[String]): Try[Seq[GenericEntityData]]

  def getEntityChildren(uid: String, skip: Int, limit:Int): Try[Seq[SoftwareEntityData]]

  def getEntityKind(entityIdent: String): Try[SoftwareEntityKind]

  def getEntity(ident: String): Try[SoftwareEntityData] = {
    if(hasEntity(ident)){
      getEntityKind(ident).flatMap(kind => getEntity(ident, kind, SoftwareEntityKind.InvocationStatement))
    } else Failure(new IllegalStateException(s"Entity not present: $ident"))
  }

  def getEntity(ident: String, kind: SoftwareEntityKind, resolutionScope: SoftwareEntityKind): Try[SoftwareEntityData]

  def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean

  def hasEntity(ident: String): Boolean


  private def getEntityAs[T <: SoftwareEntityData](ident: String,
                                                   kind: SoftwareEntityKind,
                                                   resolutionScope: SoftwareEntityKind): Try[T] =
    getEntity(ident, kind, resolutionScope).flatMap{
      case it: T => Success(it)
      case _ => Failure(new IllegalStateException(s"Not of kind ${kind.toString}: $ident"))
    }
}
