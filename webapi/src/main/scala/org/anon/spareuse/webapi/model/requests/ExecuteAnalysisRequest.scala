package org.anon.spareuse.webapi.model.requests

final case class ExecuteAnalysisRequest(Inputs: Array[Long], Configuration: String, User: Option[String], BaselineRun: Option[String])
