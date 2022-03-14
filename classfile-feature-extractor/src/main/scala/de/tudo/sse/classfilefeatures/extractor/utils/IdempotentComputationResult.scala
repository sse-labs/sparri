package de.tudo.sse.classfilefeatures.extractor.utils

class IdempotentComputationResult[T] private(private val wasExecuted: Boolean,
                                             private val wasSuccessful: Boolean,
                                             private val result: Option[T],
                                             private val error: Option[Throwable]){

  def successfullyExecuted: Boolean = wasExecuted && wasSuccessful

  def computationResult: Option[T] = result

  def errorCause: Option[Throwable] = error
}

object IdempotentComputationResult {

  def notExecuted[T](): IdempotentComputationResult[T] =
    new IdempotentComputationResult[T](false, false, None, None)

  def notSuccessful[T](): IdempotentComputationResult[T] =
    new IdempotentComputationResult[T](true, false, None, None)

  def notSuccessful[T](cause: Throwable): IdempotentComputationResult[T] =
    new IdempotentComputationResult[T](true, false, None, Some(cause))

  def success[T](result: T): IdempotentComputationResult[T] =
    new IdempotentComputationResult[T](true, true, Some(result), None)

}