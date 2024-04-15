package org.anon.spareuse.core.storage.postgresql

import slick.lifted.TableQuery
import slick.jdbc.PostgresProfile.api._

trait PostgresAnalysisTables {

  protected val analysesTable = TableQuery[SoftwareAnalyses]
  protected val analysisRunsTable = TableQuery[SoftwareAnalysisRuns]
  protected val analysisRunInputsTable = TableQuery[AnalysisRunInputs]
  protected val analysisResultsTable = TableQuery[AnalysisResults]
  protected val resultFormatsTable = TableQuery[ResultFormats]
  protected val nestedResultFormatsTable = TableQuery[NestedResultFormatReferences]
  protected val resultValiditiesTable = TableQuery[AnalysisResultValidities]
  protected val runResultsTable = TableQuery[AnalysisRunResultRelations]

  protected lazy val idReturningAnalysisRunTable = analysisRunsTable returning analysisRunsTable.map(_.id)
  protected lazy val idReturningFormatTable = resultFormatsTable returning resultFormatsTable.map(_.id)
  protected lazy val idReturningResultTable = analysisResultsTable returning analysisResultsTable.map(_.id)

  protected lazy val allAnalysisTables = Seq(resultFormatsTable, nestedResultFormatsTable, analysesTable, analysisRunsTable,
    analysisRunInputsTable, analysisResultsTable, resultValiditiesTable, runResultsTable)

}
