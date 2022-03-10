package org.tud.cgcrawling.callgraphs

import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.model.LibraryCallGraphEvolution
import org.tud.cgcrawling.opal
import org.tud.cgcrawling.opal.OPALProjectHelper

import scala.util.Try

/**
 * Provides means to analyze the JRE as an independent "library". This is far less complicated than regular library analysis,
 * as we do not have to download or cache any dependencies, and there is only one release. However, since the data model expects
 * artifacts to be of the Maven GAV format, we have to fake a library identifier for the JRE, which is G:"<none>", A:"<jre>". We use
 * the actual JRE version (via system property "java.version") as the version identifier.
 */
object JreCallgraphBuilder {

  def buildCallgraphEvolution(): Try[LibraryCallGraphEvolution] = Try {
    val jreVersionString = System.getProperty("java.version")
    val fakeJreIdentifier = MavenIdentifier("", "<none>", "<jre>", jreVersionString)
    val theCallGraphEvolution = new LibraryCallGraphEvolution(fakeJreIdentifier.groupId, fakeJreIdentifier.artifactId)

    val opalProject = new opal.OPALProjectHelper().buildJreOPALProject()

    // We can use this internal method of the CallGraphBuilder, since it is package private and there is no internal
    // state to worry about (CallGraphBuilder has no state)
    val callgraph = CallGraphBuilder.buildCgObjectModel(opalProject, fakeJreIdentifier,
      Map.empty, CallGraphBuilder.getInstantiatedTypeNames(opalProject))

    theCallGraphEvolution.applyNewRelease(callgraph, Set.empty, fakeJreIdentifier.version)

    theCallGraphEvolution
  }

}
