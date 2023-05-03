package org.anon.spareuse.eval.studies.apicompat

case class BreakingChangeReport(methodsRemoved: Int, classesRemoved: Int, paramTypesChanged: Int, returnTypesChanged: Int, interfacesRemoved: Int, numberOfArgsChanged: Int,
                                methodsAddedToInterface: Int, removedFromSuperclasses: Int, methodAccessibilityDecreased: Int, methodNowFinal: Int, abstractMethodAdded: Int,
                                addedFinalModifier: Int, addedAbstractModifier: Int)
