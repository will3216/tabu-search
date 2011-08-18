package edu.bryant.tabu

import org.specs2.mutable.Specification

class TabuSpec extends Specification {
  "Config" should {
    val tabu = new Tabu("src/test/scala/edu/bryant/tabu/fixtures/tabu.config");

    "know the number of steps" in {
      tabu.config.getInt("number_of_steps").getOrElse(0) mustEqual 5
    }

    "set the interest rate" in {
      Tabu.interest_rate mustEqual 0.1
    }
  }
}
