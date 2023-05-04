package org.anon.spareuse.webapi.model.requests

final case class ExecuteAnalysisRequest(Inputs: Array[String], Configuration: String, User: Option[String])
