package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.execution.AnalysisRunnerConfig
import org.anon.spareuse.execution.analyses.{getCallGraphProject, toObjectModel}
import org.opalj.br.analyses.Project

import java.net.URL

trait CallGraphTestSupport {

  protected val objFqn: String = "java/lang/Object"
  protected val theOpalHelper: OPALProjectHelper = new OPALProjectHelper(loadJreClassImplementation = false)

  protected lazy val jreObjectModel: JavaProgram = {
    println("Loading JRE domain model, this might take some time ... ")
    val jreProg = OPALJavaConverter.convertProgram("<NONE>:<THE_JRE>", "<default>", theOpalHelper.jreClasses.map(_._1), "<NONE>", loadClassContents = false)
    assert(jreProg.allClasses.exists(_.thisType == objFqn))
    assert(jreProg.allClasses.nonEmpty)
    println(s"Done loading ${jreProg.allClasses.size} JRE classes")
    theOpalHelper.freeOpalResources()
    jreProg
  }

  protected val cgOpalProject: Project[URL] = getCallGraphProject

  protected def getCgFixtureModel: JavaProgram = toObjectModel(cgOpalProject)

  protected def resetModelLoader(): Unit = {
    JreModelLoader.clear()
    JreModelLoader.indexJreData("jre-data")
  }

}
