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
package org.tud.cgcrawling

import java.net.URI

class Configuration {


  val mavenRepoBase: URI = new URI("https://repo1.maven.org/maven2/") // TODO: Create a local demo server "http://localhost:8881/maven2/"

  val mqUsername: String = "<changeme>"
  val mqPassword: String = "<changeme>"
  val mqHost: String = "ls5vs029.cs.tu-dortmund.de"
  val mqPort: Int = 8080
  val mqQueueName: String = "library-identifiers"

}