package org.anon.spareuse.core.storage.postgresql

import scala.concurrent.ExecutionContext

class PostgresDataAccessor(implicit context: ExecutionContext)
  extends PostgresSparriSupport with PostgresEntityAccessor with PostgresAnalysisAccessor {

  override val executor: ExecutionContext = context

}
