package org.anon.spareuse.execution

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

}
