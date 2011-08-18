package edu.bryant.tabu

import org.specs2.mutable.Specification

class TabuSpec extends Specification {
  "Config" should {
    Tabu.set_config_file("src/test/scala/edu/bryant/tabu/fixtures/tabu.config");

    "know the number of steps" in {
      Tabu.config.getInt("number_of_steps").getOrElse(0) mustEqual 5
    }

    "set the interest rate" in {
      Tabu.config.getDouble("interest_rate").get mustEqual 0.0068
    }
  }
}
