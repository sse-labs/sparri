package org.tud.cgcrawling.dependencies

import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.{DefaultRepositorySystemSession, RepositorySystem}
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory

import java.nio.file.Paths

object AetherRepositoryProvider {



  lazy val centralRepository: RemoteRepository =
    new RemoteRepository.Builder("central", "default", "https://repo1.maven.org/maven2/").build()

  lazy val repoSystem: RepositorySystem = {
    val locator = MavenRepositorySystemUtils.newServiceLocator()
    locator.addService(classOf[RepositoryConnectorFactory], classOf[BasicRepositoryConnectorFactory])
    locator.addService(classOf[TransporterFactory], classOf[FileTransporterFactory])
    locator.addService(classOf[TransporterFactory], classOf[HttpTransporterFactory])

    locator.getService(classOf[RepositorySystem])
  }

  lazy val repoSession: DefaultRepositorySystemSession = {
    val session = MavenRepositorySystemUtils.newSession()
    val localRepo = new LocalRepository(Paths.get("./repo/", "local-repo").toString)
    session.setLocalRepositoryManager(repoSystem.newLocalRepositoryManager(session, localRepo))
  }
}
