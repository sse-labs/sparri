// Copyright (C) 2018 The Delphi Team.
// See the LICENCE file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package de.tudo.sse.classfilefeatures.extractor

import de.tudo.sse.classfilefeatures.common.rabbitmq.MqConnectionConfiguration
import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlConnectionConfiguration
import org.opalj.tac.cg.{AbstractCallGraphKey, XTACallGraphKey}

import java.net.URI

class Configuration extends MqConnectionConfiguration with PostgreSqlConnectionConfiguration{

  val CallGraphAlgorithm: AbstractCallGraphKey = XTACallGraphKey

  val limit : Int = 100

  val mqUsername: String = "<CHANGEME>"
  val mqPassword: String = "<CHANGEME>"
  val mqHost: String = "<CHANGEME>"
  val mqPort: Int = 8080
  val mqQueueName: String = "library-identifiers"

  val codeSizeCgCutoffBytes: Int = 1500000

  val numberOfTransformerThreads: Int = 2
  val numberOfStorageThreads: Int = 2

}