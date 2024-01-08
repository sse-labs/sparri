package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.formats.json.CustomObjectWriter
import org.anon.spareuse.execution.analyses.{buildProject, foreachFixture, getTACProvider}
import org.scalatest.funspec.AnyFunSpec
import spray.json.JsObject

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.util.Try

class IFDSMethodGraphTest extends AnyFunSpec{

  private val theBuilder = new IFDSTaintFlowSummaryBuilderImpl (None)

  describe("An IFDS method graph"){

    it("should be correctly converted to generic results representation"){

      foreachFixture { fixture =>
        val graphs = buildMethodSummariesForFile(fixture)

        graphs.foreach { graph =>

          val graphResult = graph.toResultRepresentation(squashIdentityStmts = false)

          // Check that all real statements are contained in the result representation, and are linked to the correct predecessors
          graph.statementNodes.foreach { stmtNode =>
            assert(graphResult.statements.exists(stmtRep => stmtRep.pc == stmtNode.stmt.pc && stmtRep.predecessors.equals(stmtNode.getPredecessors.map(_.stmt.pc).toSeq)))
          }

          // Assert that the actual graph and the result have the same number of statements
          assert(graphResult.statements.size == graph.statementNodes.size)

          // Assert that all real facts are contained in the result representation
          graph.allFacts.foreach { factObj =>
            assert(graphResult.facts.exists(factRep => factRep.identifier.equals(factObj.uniqueIdent)))
          }

          // Assert that the actual graph and the result have the same number of facts
          assert(graph.allFacts.size == graphResult.facts.size)
          // Assert that the ids given to the facts in the result are unique
          assert(graphResult.facts.map(_.uid).toSet.size == graphResult.facts.size)

          // Assert that the fact-to-id mapping works
          val factIdMap = graphResult.facts.map(f => (f.identifier, f.uid)).toMap

          graph.statementNodes.foreach{ stmtNode =>
            val activations = stmtNode.allActivations
            val resultActivations = graphResult.statements.find(stmtRep => stmtRep.pc == stmtNode.stmt.pc).get.activations

            activations.foreach { a =>
              val realTargetId = factIdMap(a._1.uniqueIdent)
              val realEnablingFactIds = a._2.map(f => factIdMap(f.uniqueIdent))
              assert(resultActivations.exists(activationRep => activationRep.sourceFactId == realTargetId && activationRep.enablingFactIds.toSet.equals(realEnablingFactIds)))
            }
          }
        }
      }

    }

    it("should be serializable using the defined result format"){

      foreachFixture{ fixture =>

        val graphs = buildMethodSummariesForFile(fixture)
        val graphResults = graphs.map(_.toResultRepresentation(squashIdentityStmts = false))
        val writer = new CustomObjectWriter(theBuilder.descriptor.analysisData.resultFormat)

        val jsonTries = graphResults.map(res => Try(writer.write(res)))

        assert(jsonTries.forall(_.isSuccess))

        val jsonStrings = jsonTries.map { jsTry =>
          val content = jsTry.get
          assert(content.isInstanceOf[JsObject])
          content.toString()
        }

        jsonStrings.foreach(println)
      }
    }

  }



  private def buildMethodSummariesForFile(fixtureFile: File): ArraySeq[IFDSMethodGraph] = {
    val project = buildProject(fixtureFile)
    val tacProvider = getTACProvider (project)
    val allMethods = project.allMethodsWithBody.filterNot (m => m.toJava.contains ("init") )

    allMethods.map (m => theBuilder.analyzeMethod (m) (tacProvider) )
  }

}
