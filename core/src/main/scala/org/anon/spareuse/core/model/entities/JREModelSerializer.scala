package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaNewInstanceStatement, JavaPackage, JavaProgram, JavaStatement, PathIdentifiableJavaEntity}
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.core.utils.toHex
import org.slf4j.{Logger, LoggerFactory}
import spray.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString}

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.{Failure, Success, Try}

object JREModelSerializer {

  private final val log: Logger = LoggerFactory.getLogger(getClass)
  private final val opalHelper: OPALProjectHelper = new OPALProjectHelper()

  def serializeModelTo(theModel: JavaProgram, outFile: File): Try[File] = Try {

    def stmtToJson(stmt: JavaStatement): JsObject = stmt match {
      case jis: JavaInvokeStatement =>
        new JsObject(Map(
          "methodName" -> JsString(jis.targetMethodName),
          "declaredTypeFqn" -> JsString(jis.targetTypeName),
          "paramTypeNames" ->  JsArray(jis.targetMethodParameterTypeNames.map(n => JsString(n)).toVector),
          "returnTypeFqn" -> JsString(jis.returnTypeName),
          "invocationType" -> JsNumber(jis.invokeStatementType.id),
          "pc" -> JsNumber(jis.instructionPc),
          "uid" -> JsString(jis.uid)
        ))
      case jfas: JavaFieldAccessStatement =>
        new JsObject(Map(
          "fieldName" -> JsString(jfas.targetFieldName),
          "fieldTypeFqn" -> JsString(jfas.targetFieldTypeName),
          "declaredTypeFqn" -> JsString(jfas.targetTypeName),
          "accessType" -> JsNumber(jfas.fieldAccessType.id),
          "pc" -> JsNumber(jfas.instructionPc),
          "uid" -> JsString(jfas.uid)
        ))
      case jnis: JavaNewInstanceStatement =>
        new JsObject(Map(
          "typeName" -> JsString(jnis.instantiatedTypeName),
          "pc" -> JsNumber(jnis.instructionPc),
          "uid" -> JsString(jnis.uid)
        ))
    }

    def methodToJson(method: JavaMethod): JsObject = new JsObject(Map(
      "methodName" -> JsString(method.name),
      "returnTypeFqn" -> JsString(method.returnType),
      "paramTypeNames" -> JsArray(method.paramTypes.map(n => JsString(n)).toVector),
      "methodUid" -> JsString(method.uid),
      "finalMethod" -> JsBoolean(method.isFinal),
      "staticMethod" -> JsBoolean(method.isStatic),
      "abstractMethod" -> JsBoolean(method.isAbstract),
      "methodVisibility" -> JsString(method.visibility),
      "hash" -> JsNumber(method.methodHash),
      "statements" -> JsArray(method.getStatements.map(stmtToJson).toVector)
    ))

    def classToJson(klass: JavaClass): JsObject = new JsObject(Map(
      "className" -> JsString(klass.name),
      "thisTypeFqn" -> JsString(klass.thisType),
      "classUid" -> JsString(klass.uid),
      "superTypeFqn" -> JsString(klass.superType.getOrElse("NONE")),
      "interfaceFqns" -> JsArray(klass.interfaceTypes.map(n => JsString(n)).toVector),
      "interfaceType" -> JsBoolean(klass.isInterface),
      "finalType" -> JsBoolean(klass.isFinal),
      "abstractType" -> JsBoolean(klass.isAbstract),
      "hashedBytes" -> JsString(klass.binaryHash.map(toHex).getOrElse("NONE")),
      "methods" -> JsArray(klass.getMethods.map(methodToJson).toVector)
    ))

    def packageToJson(p: JavaPackage): JsObject = new JsObject(Map(
      "packageName" -> JsString(p.name),
      "packageUid" -> JsString(p.uid),
      "classes" -> JsArray(p.getClasses.map(classToJson).toVector)
    ))

    def programToJson(cp: JavaProgram): JsObject = new JsObject(Map(
      "programName" -> JsString(cp.name),
      "programIdent" -> JsString(cp.identifier),
      "programUid" -> JsString(cp.uid),
      "repositoryIdent" -> JsString(cp.repository),
      "hashedBytes" -> JsString(cp.binaryHash.map(toHex).getOrElse("NONE")),
      "packages" -> JsArray(cp.getPackages.map(packageToJson).toVector)
    ))

    val json = programToJson(theModel)
    val jsonString = json.prettyPrint

    Files.writeString(outFile.toPath, jsonString, StandardOpenOption.CREATE_NEW)

    outFile
  }

  def serializeJreModel(jreRoot: File, jreVersion: String, outDir: File): Try[File] = Try {

    if(!jreRoot.exists() || !jreRoot.isDirectory)
      throw new IllegalArgumentException(s"Not a valid directory: ${jreRoot.getAbsolutePath}")

    if(!outDir.exists() || !outDir.isDirectory)
      throw new IllegalArgumentException(s"Not a valid output directory: ${outDir.getAbsolutePath}")

    val outFile = Paths.get(outDir.getPath, s"jre-$jreVersion.json").toFile

    if (outFile.exists())
      throw new IllegalStateException(s"There already is a serialized object model for JRE $jreVersion")

    val relevantFiles = getJarFilesRecursive(jreRoot)

    if(relevantFiles.isEmpty)
      throw new IllegalArgumentException(s"No JARs / JMODs contained in JRE root: ${jreRoot.getAbsolutePath}")

    log.info(s"Parsing ${relevantFiles.size} JAR files for JRE $jreVersion.")

    val allJreClasses = relevantFiles
      .map{ containerFile =>
        if(containerFile.getName.endsWith(".jar"))
          opalHelper.readClassesFromJarStream(new FileInputStream(containerFile), containerFile.toURI.toURL, loadImplementation = true)
        else
          opalHelper.readClassesFromJmodFile(containerFile, loadImplementation = true)
      }
      .filter{
        case Success(_) => true
        case Failure(ex) =>
          log.error("Failed to load a JRE file", ex)
          false
      }
      .flatMap(_.get)

    val jreModel = OPALJavaConverter.convertProgram(s"<JRE>:$jreVersion", "<none>", allJreClasses.map(_._1))
    opalHelper.freeOpalResources()

    log.info("Done creating object model for JRE. Writing model to file ... ")

    serializeModelTo(jreModel, outFile) match {
      case Success(outFile) =>
        log.info(s"Successfully wrote JRE model to file: ${outFile.getName}")
        outFile
      case Failure(ex) =>
        log.error("Failed to write JRE model to file", ex)
        throw ex
    }
  }

  def getJarFilesRecursive(directory: File): List[File] = {
    val directChildJars = directory
      .listFiles
      .filter(f => f.isFile && f.getName.toLowerCase().endsWith("java.base.jmod") || f.getName.toLowerCase().endsWith("rt.jar"))
      .toList
    directChildJars ++ directory.listFiles.filter(_.isDirectory).flatMap(getJarFilesRecursive).toList
  }

}
