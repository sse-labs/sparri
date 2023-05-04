package org.anon.spareuse.eval.performance.dependencies

import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.maven.dependencies.JekaDependencyExtractor

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

class SimpleTransitiveDependencyAnalysis extends TransitiveDependencyAnalysis {

  override def getAllDependencies(rootArtifactGAV: String): Try[Set[String]] = Try {
    val resolver = new JekaDependencyExtractor

    val rootIdent = MavenIdentifier.fromGAV(rootArtifactGAV).get
    val dependencies = new mutable.HashSet[String]()

    val toProcess = new ListBuffer[String]

    resolver.resolveDependencies(rootIdent).get.map(i => i.identifier.toString).foreach(gav => toProcess.append(gav))

    while(toProcess.nonEmpty){
      val current = toProcess.remove(0)
      val currentDeps = resolver.resolveDependencies(MavenIdentifier.fromGAV(current).get).get.map(d => d.identifier.toString)
      dependencies.add(current)

      val newDeps = currentDeps.diff(toProcess).diff(dependencies.toSeq).filterNot(dep => dep.equals(current))
      toProcess.appendAll(newDeps)
    }

    dependencies.toSet
  }

}
