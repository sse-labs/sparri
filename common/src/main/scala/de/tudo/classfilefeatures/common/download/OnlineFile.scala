package de.tudo.classfilefeatures.common.download

import java.io.InputStream
import java.net.URL

case class OnlineFile(is: InputStream, url: URL)
