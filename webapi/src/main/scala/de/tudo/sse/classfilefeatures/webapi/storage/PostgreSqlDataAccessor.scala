package de.tudo.sse.classfilefeatures.webapi.storage

import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.{PostgreSqlConnectionConfiguration, PostgreSqlConnectivity}

class PostgreSqlDataAccessor(override protected val connectionConfiguration: PostgreSqlConnectionConfiguration) extends PostgreSqlConnectivity{

}
