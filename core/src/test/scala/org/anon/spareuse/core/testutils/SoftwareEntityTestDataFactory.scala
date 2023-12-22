package org.anon.spareuse.core.testutils

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod, JavaProgram}
import org.anon.spareuse.core.model.entities.{GenericEntityData, JavaEntities, SoftwareEntityData}

object SoftwareEntityTestDataFactory {

  /**
   * Build a generic entity data object with no hash and no parent. If no arguments are supplied, a default dummy entity
   * of type 'Program' is returned.
   *
   * @param name Name for the generic entity
   * @param uid UID for the generic entity - ensure consistency to name if needed
   * @param kind Kind for the generic entity - defaults to 'Program'
   * @param language Language for the generic entity - defaults to 'Java'
   * @param repository Repository for the generic entity - default to 'Maven'
   * @return GenericEntityData object with no parent
   */
  def genericEntity(name: String = "dummy.group:artifact:1.0.0",
                    uid: String = "dummy.group!dummy.group:artifact:1.0.0",
                    kind: SoftwareEntityKind = SoftwareEntityKind.Program,
                    language: String = "Java",
                    repository: String = "Maven"): SoftwareEntityData =
    new GenericEntityData(name, language, kind, repository, None, uid, None)


  def fullProgram(gav: String): JavaProgram = {
    JavaEntities.buildProgram(gav)
  }

  def fullMethodEntity(gav: String, packageName: String, className: String, methodName: String, returnType: String = "String", paramTypeNames: Seq[String] = Seq.empty): JavaMethod = {
    assert(gav.count(_ == ':') == 2)

    val lib = JavaEntities.buildLibrary(gav.substring(0, gav.lastIndexOf(':')))
    val prog = JavaEntities.buildProgramFor(lib, gav)
    val pack = JavaEntities.buildPackageFor(prog, packageName)
    val classObj = JavaEntities.buildClassFor(pack,
      className,
      className,
      isInterface = false,
      isFinal = false,
      isAbstract = false)

    methodFor(classObj, methodName, returnType, paramTypeNames)
  }

  def methodFor(jc: JavaClass, methodName: String = "toString", returnType: String = "String", paramTypeNames: Seq[String] = Seq.empty, isFinal: Boolean = false, isStatic: Boolean = false, isAbstract: Boolean = false, visibility: String = "public", hash: Int = 0): JavaMethod = {
    JavaEntities.buildMethodFor(jc, methodName, returnType, paramTypeNames, isFinal, isStatic, isAbstract, visibility, hash)
  }
}
