package edu.bryant.tabu

import net.lag.configgy.Configgy
import net.lag.logging.Logger

object Tabu {
  var interest_rate = 0.0068
}

class Tabu(configFile: String = "tabu.config") {
  Configgy.configure(configFile)
  val config = Configgy.config

  Tabu.interest_rate = config.getDouble("interest_rate").getOrElse(0.0068)
}
