package org.anon.spareuse.execution

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.execution.analyses.impl.ifds.MethodTACProvider
import org.opalj.ai.domain
import org.opalj.ai.fpcf.properties.AIDomainFactoryKey
import org.opalj.br.analyses.Project
import org.opalj.tac.ComputeTACAIKey

import java.io.File
import java.net.URL

package object analyses {

  def loadFixture(resourceName: String): File = {
    val theFile = new File(getClass.getClassLoader.getResource(resourceName).toURI)
    if (!theFile.exists()) throw new IllegalStateException(s"Missing resource: $resourceName")
    theFile
  }

  def buildProject(classFiles: File*): Project[URL] = Project(classFiles.toArray, Array.empty[File])

  def getTACProvider(project: Project[URL]): MethodTACProvider = {
    // Use simplest AI domain for TAC
    project.updateProjectInformationKeyInitializationData(AIDomainFactoryKey) {
      case None => Set(classOf[domain.RecordDefUse])
      case Some(requirements) => requirements + classOf[domain.RecordDefUse]
    }

    // Mapping of methods to their TAC
    project.get(ComputeTACAIKey)
  }

  def getCallGraphProject: Project[URL] = {
    Project(Array(callsFixtureName, interfaceFixtureName, interfaceImplFixtureName).map(loadFixture), Array.empty[File])
  }

  def toObjectModel(project: Project[URL], ident: String = "org.anon.test:1.0.0", loadClassContents: Boolean = true): JavaProgram = {
    OPALJavaConverter.convertProgram(ident, "<default>", project.allClassFiles.toList, loadClassContents)
  }

  // Use the following code to refer to java fixtures that are located in the fixtures-java directory and compiled to the
  // resources directory using `sbt compileRunnerFixtures`

  val complexCfgFixtureName = "BranchingTaint.class"
  val simpleSelfContainedFixtureName = "StringConcatHelper.class"
  val simpleCfgExternalCallFixtureName = "SimpleStringTaint.class"
  val callsFixtureName = "Calls.class"
  val interfaceFixtureName = "CallTarget.class"
  val interfaceImplFixtureName = "CallTargetImpl.class"

  val allFixtureNames: Seq[String] = Seq(complexCfgFixtureName, simpleCfgExternalCallFixtureName, simpleSelfContainedFixtureName,
    callsFixtureName, interfaceFixtureName, interfaceImplFixtureName)

  def foreachFixture(implicit executor: File => Unit): Unit = allFixtureNames.map(loadFixture).foreach(executor)



}
