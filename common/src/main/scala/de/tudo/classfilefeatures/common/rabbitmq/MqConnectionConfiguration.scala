package de.tudo.classfilefeatures.common.rabbitmq

trait MqConnectionConfiguration {

  val mqUsername: String
  val mqPassword: String
  val mqHost: String
  val mqPort: Int
  val mqQueueName: String

}
