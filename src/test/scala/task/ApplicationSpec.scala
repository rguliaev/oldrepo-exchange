package task

import org.scalatest._

class ApplicationSpec extends FlatSpec with Matchers with ResourceReader {

  "ResourceReader" should "fail with invalid clients format" in {
    clientsEither("clients1.txt") should be ('isLeft)
    clientsEither("clients2.txt") should be ('isLeft)
  }

  "ResourceReader" should "pass with valid clients format" in {
    clientsEither() should be ('isRight)
  }

  "ResourceReader" should "fail with invalid orders format" in {
    ordersEither("orders1.txt") should be ('isLeft)
    ordersEither("orders2.txt") should be ('isLeft)
    ordersEither("orders3.txt") should be ('isLeft)
  }

  "ResourceReader" should "pass with valid orders format" in {
    ordersEither() should be ('isRight)
  }
}
