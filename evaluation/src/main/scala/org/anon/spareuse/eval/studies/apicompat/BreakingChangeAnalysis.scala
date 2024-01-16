package org.anon.spareuse.eval.studies.apicompat

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.eval
import org.anon.spareuse.eval.{getAllMethodsForClass, getApiBaseUrl}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}

import scala.util.Try

class BreakingChangeAnalysis {

  private val apiBaseUrl = getApiBaseUrl

  def calculateChanges(oldReleaseGav: String, newReleaseGav: String): Try[BreakingChangeReport] = Try {
    implicit val httpClient: CloseableHttpClient = HttpClients.createDefault()

    val allOldTypes = getAllTypeData(oldReleaseGav).get
    val oldTypesLookup = allOldTypes.map(jc => (jc.thisType, jc)).toMap
    val allNewTypes = getAllTypeData(newReleaseGav).get
    val newTypesLookup = allNewTypes.map(jc => (jc.thisType, jc)).toMap

    val oldMethodLookup = allOldTypes
      .flatMap{ jc =>
        jc
          .getChildren
          .map(_.asInstanceOf[JavaMethod])
          .map(jm => (MethodIdentifier(jc.thisType, jm), jm))
      }
      .toMap

    val newMethodLookup = allNewTypes
      .flatMap { jc =>
        jc
          .getChildren
          .map(_.asInstanceOf[JavaMethod])
          .map(jm => (MethodIdentifier(jc.thisType, jm), jm))
      }
      .toMap

    val classesRemoved = allOldTypes.filterNot(_.isInterface).toSet.diff(allNewTypes.filterNot(_.isInterface).toSet).size
    val interfacesRemoved = allOldTypes.filter(_.isInterface).toSet.diff(allNewTypes.filter(_.isInterface).toSet).size
    val methodsRemoved = oldMethodLookup.keys.toSet.diff(newMethodLookup.keys.toSet).size

    val returnTypesChanged = oldMethodLookup.count{ t =>
      val oldIdent = t._1
      val oldMethod = t._2

      newMethodLookup.contains(oldIdent) && !newMethodLookup(oldIdent).returnType.equals(oldMethod.returnType)
    }

    // Count all interface types, for which new methods exist in the second release that have not been present in the first release
    val methodsAddedToInterface = allOldTypes
      .filter(_.isInterface)
      .count{ i =>
        val allOldMethodIdents = i.getChildren.map(e => MethodIdentifier(i.thisType, e.asInstanceOf[JavaMethod]))

        if(!newTypesLookup.contains(i.thisType) || !newTypesLookup(i.thisType).isInterface)  false
        else {
          val allNewMethodIdents = newTypesLookup(i.thisType).getChildren.map(e => MethodIdentifier(i.thisType, e.asInstanceOf[JavaMethod]))
          allNewMethodIdents.diff(allOldMethodIdents).nonEmpty
        }
      }

    val removedFromSuperclasses = allOldTypes.count{ oldType =>
      val oldSupertypes = oldType.superType.map(Set(_)).getOrElse(Set.empty) ++ oldType.interfaceTypes

      if(newTypesLookup.contains(oldType.thisType)) {
        val newSupertypes = newTypesLookup(oldType.thisType).superType.map(Set(_)).getOrElse(Set.empty) ++ newTypesLookup(oldType.thisType).interfaceTypes

        oldSupertypes.diff(newSupertypes).nonEmpty
      } else false
    }

    val methodNowFinal = oldMethodLookup.filterNot(_._2.isFinal).count{ t =>
      newMethodLookup.contains(t._1) && newMethodLookup(t._1).isFinal
    }

    // Count those new, abstract methods for which a) the type existed in the old release and b) it did not contain that abstract method (or it was not abstract)
    val abstractMethodAdded = newMethodLookup.filter(_._2.isAbstract).count{ t =>
      oldTypesLookup.contains(t._1.declaredTypeFqn) && (!oldMethodLookup.contains(t._1) || !oldMethodLookup(t._1).isAbstract)
    }

    val methodAccessibilityDecreased = oldMethodLookup.count { t =>
      if(newMethodLookup.contains(t._1)){
        val oV = t._2.visibility
        val nV = newMethodLookup(t._1).visibility

        oV match {
          case "public" => !nV.equals("public")
          case "protected" => nV.equals("default") || nV.equals("private")
          case "default" => nV.equals("private")
          case "private" => false
        }

      } else false
    }

    val addedFinalModifier = allOldTypes.filterNot(_.isFinal).filter(t => newTypesLookup.contains(t.thisType)).count{ t =>
      newTypesLookup(t.thisType).isFinal
    }

    val addedAbstractModifier = allOldTypes.filterNot(_.isAbstract).filter(t => newTypesLookup.contains(t.thisType)).count { t =>
      newTypesLookup(t.thisType).isAbstract
    }

    val paramTypesChanged = allOldTypes.filter(t => newTypesLookup.contains(t.thisType)).map { oT =>
      val oldMethods = oT.getChildren.map(_.asInstanceOf[JavaMethod])
      val newMethods = newTypesLookup(oT.thisType).getChildren.map(_.asInstanceOf[JavaMethod])

      val oldLookup = oldMethods.map(m => (MethodIdentifier(oT.thisType, m), m)).toMap
      val newLookup = newMethods.map(m => (MethodIdentifier(oT.thisType, m), m)).toMap

      val oldIdent = oldLookup.keys.toSet
      val newIdent = newLookup.keys.toSet

      val uniqueOldMethods = oldIdent.diff(newIdent)
      val uniqueNewMethods = newIdent.diff(oldIdent)

      uniqueOldMethods.count{ uom =>
        val oldRetType = oldLookup(uom).returnType
        uniqueNewMethods.exists( unm => unm.methodName.equals(uom.methodName) && unm.parameterTypes.size == uom.parameterTypes.size && oldRetType.equals(newLookup(unm).returnType))
      }
    }.sum

    val numberOfArgsChanged = allOldTypes.filter(t => newTypesLookup.contains(t.thisType)).map { oT =>
      val oldMethods = oT.getChildren.map(_.asInstanceOf[JavaMethod])
      val newMethods = newTypesLookup(oT.thisType).getChildren.map(_.asInstanceOf[JavaMethod])

      val oldLookup = oldMethods.map(m => (MethodIdentifier(oT.thisType, m), m)).toMap
      val newLookup = newMethods.map(m => (MethodIdentifier(oT.thisType, m), m)).toMap

      val oldIdent = oldLookup.keys.toSet
      val newIdent = newLookup.keys.toSet

      val uniqueOldMethods = oldIdent.diff(newIdent)
      val uniqueNewMethods = newIdent.diff(oldIdent)

      uniqueOldMethods.count { uom =>
        val oldRetType = oldLookup(uom).returnType
        uniqueNewMethods.exists(unm => unm.methodName.equals(uom.methodName) && unm.parameterTypes.size != uom.parameterTypes.size && oldRetType.equals(newLookup(unm).returnType))
      }
    }.sum

    BreakingChangeReport(
      methodsRemoved,
      classesRemoved,
      paramTypesChanged,
      returnTypesChanged,
      interfacesRemoved,
      numberOfArgsChanged,
      methodsAddedToInterface,
      removedFromSuperclasses,
      methodAccessibilityDecreased,
      methodNowFinal,
      abstractMethodAdded,
      addedFinalModifier,
      addedAbstractModifier
    )
  }

  private def getAllTypeData(gav: String)(implicit httpClient: CloseableHttpClient): Try[Seq[JavaClass]] = Try {
    eval
      .getAllTypesForProgram(gav, apiBaseUrl, httpClient)
      .get
      .map { jc =>
        val methods: Set[SoftwareEntityData] = getAllMethodsForClass(jc.uid, apiBaseUrl, httpClient).get.toSet
        jc.setChildren(methods)
        jc
      }
  }
}