package org.tud.cgcrawling.callgraphs

import akka.actor.ActorSystem
import org.opalj.br.Method
import org.opalj.br.analyses.cg.InitialEntryPointsKey
import org.opalj.br.instructions.{INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, VirtualMethodInvocationInstruction}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.opal.OPALProjectHelper

import scala.collection.mutable
import scala.util.{Failure, Try}

class CustomCgAlgorithmTest extends AnyFlatSpec with must.Matchers {

  private val config = new Configuration


  private val identifier1: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "love.forte.simple-robot", "api", "2.3.0")
  private val identifier2: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "com.librato.metrics", "metrics-librato", "5.1.4")

  "A custom resolution algorithm" must "detect entry points" in {
    val system = ActorSystem("test")

    Try{
      val builder = new LibraryCallgraphBuilder(identifier1.groupId, identifier1.artifactId, config)(system)
      val thirdPartyClasses = builder.getAllThirdPartyClassesWithCache(identifier1, false)
      val downloadResponse = builder.downloader.downloadJar(identifier1)

      assert(downloadResponse.jarFile.isDefined)

      val projectClasses = OPALProjectHelper.readClassesFromJarStream(downloadResponse.jarFile.get.is, downloadResponse.jarFile.get.url, true)

      assert(projectClasses.isSuccess)

      val project = OPALProjectHelper.buildOPALProject(projectClasses.get, thirdPartyClasses)

      val entries = project.get(InitialEntryPointsKey).filter(m => project.allProjectClassFiles.contains(m.classFile) && m.body.isDefined)
      val reachableMethods: mutable.Set[Method] = new mutable.HashSet[Method]()
      val methodSignaturesSeen: mutable.HashSet[String] = new mutable.HashSet[String]()

      def getAllCallees(method: Method) = {
        method.body.get.instructions
          .filter(instr => instr != null && instr.isMethodInvocationInstruction)
          .flatMap {
            case iv: INVOKEVIRTUAL =>
              project.virtualCall(method.classFile.thisType, iv)

            case is: INVOKESTATIC =>
              val resolvedCall = project.staticCall(method.classFile.thisType, is)

              if(resolvedCall.hasValue){
                Traversable(resolvedCall.value)
              } else {
                println("Failed to resolve static call " + is.toString())
                Traversable.empty
              }

            case special: INVOKESPECIAL =>
              val resolvedCall = project.specialCall(method.classFile.thisType, special)

              if(resolvedCall.hasValue){
                Traversable(resolvedCall.value)
              } else {
                println("Failed to resolve special call " + special.toString())
                Traversable.empty
              }

            case interface: INVOKEINTERFACE =>
              project.interfaceCall(method.classFile.thisType, interface)
          }
      }
      var cnt = 0
      def analyzeCallees(method: Method): Unit = {
        reachableMethods.add(method)
        methodSignaturesSeen.add(method.fullyQualifiedSignature)
        cnt += 1

        if(cnt % 200 == 0) println("Method: " + cnt)
        if(method.body.isDefined){
          getAllCallees(method)
            .foreach{ callee =>
              if(!methodSignaturesSeen.contains(callee.fullyQualifiedSignature)){

                if(project.isProjectType(callee.classFile.thisType)){
                  analyzeCallees(callee)
                } else {
                  reachableMethods.add(callee)
                  methodSignaturesSeen.add(callee.fullyQualifiedSignature)
                }
              }


            }
        }
      }

      entries.foreach(analyzeCallees)
      val totalProjectMethodsCount = project.allMethods.count(m => m.body.isDefined && project.allProjectClassFiles.contains(m.classFile))
      val nonVisitedProjectMethodsCount = project.allMethods.filter(m => m.body.isDefined && project.allProjectClassFiles.contains(m.classFile))
        .count(m => !methodSignaturesSeen.contains(m.fullyQualifiedSignature))

      val allMethods = reachableMethods.toList.distinct

      println("Got " + allMethods.size + " reachable methods with " + allMethods.count(m => !project.allProjectClassFiles.contains(m.classFile)) + " external methods:")
      println(s"Got $totalProjectMethodsCount total library methods, $nonVisitedProjectMethodsCount not visited.")




    } match {
      case Failure(ex) =>
        system.terminate()
        throw ex
      case _ =>
    }


  }

  private def timedOp[T](op: () => T): (Long, T) = {
    val start = System.currentTimeMillis()
    val result = op.apply()
    (System.currentTimeMillis() - start, result)
  }
}
