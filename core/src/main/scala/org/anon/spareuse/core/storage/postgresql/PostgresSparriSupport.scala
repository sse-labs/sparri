package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.storage.postgresql.ResultFormatPredef.allPredefFormats
import org.anon.spareuse.core.utils.ObjectCache
import org.slf4j.{Logger, LoggerFactory}
import slick.dbio.DBIO
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait PostgresSparriSupport extends PostgresAnalysisTables with PostgresEntityTables with DataAccessor {

  this: PostgresDataAccessor =>

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  protected val idToIdentifierCache = new ObjectCache[Long, String](maxEntries = 10000)

  protected val simpleQueryTimeout: FiniteDuration = 5.seconds
  protected val longActionTimeout: FiniteDuration = 60.seconds

  protected lazy val db = Database.forConfig("spa-reuse.postgres")

  override def initializeEntityTables(): Unit = {

    val existing = db.run(MTable.getTables)
    val f = existing.flatMap(v => {
      val names = v.map(mt => mt.name.name)
      val createIfNotExist = allEntityTables.filter(table => !names.contains(table.baseTableRow.tableName)).map(_.schema.create)
      db.run(DBIO.sequence(createIfNotExist))
    })(executor)

    Await.ready(f, longActionTimeout)
  }

  override def shutdown(): Unit = {
    db.close()
  }

  override def initializeAnalysisTables(): Unit = {
    val existing = db.run(MTable.getTables)
    val f = existing.flatMap(v => {
      val names = v.map(mt => mt.name.name)
      val createIfNotExist = allAnalysisTables.filter(table => !names.contains(table.baseTableRow.tableName)).map(_.schema.create)
      db.run(DBIO.sequence(createIfNotExist))
    })(executor)

    Await.ready(f, longActionTimeout)

    storePredefinedFormats()
  }

  private def storePredefinedFormats(): Unit = {

    val allPredefIds = allPredefFormats.map(_.id).toSet

    val resultingFormats = Await.result(db.run(resultFormatsTable.filter(f => f.id inSet allPredefIds).result), simpleQueryTimeout)

    resultingFormats.size match {
      case 0 =>
        val query = DBIO.seq(resultFormatsTable += ResultFormatPredef.StringFormat, resultFormatsTable += ResultFormatPredef.NumberFormat,
          resultFormatsTable += ResultFormatPredef.EntityRefFormat, resultFormatsTable += ResultFormatPredef.EmptyFormat)

        Await.ready(db.run(query), simpleQueryTimeout)
      case x if x == allPredefIds.size =>
        allPredefFormats.foreach { predefF =>
          if (resultingFormats.count(_.equals(predefF)) != 1) throw new IllegalStateException("Illegal database state: Predefined formats are invalid")
        }
      case _ => throw new IllegalStateException("Illegal database state: Predefined Formats are not stored correctly")
    }
  }
}
