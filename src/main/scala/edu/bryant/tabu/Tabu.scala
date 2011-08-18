package edu.bryant.tabu

import net.lag.configgy.Configgy
import net.lag.logging.Logger

class Tabu(configFile: String = "tabu.config") {
  Configgy.configure(configFile)
  val config = Configgy.config
}
