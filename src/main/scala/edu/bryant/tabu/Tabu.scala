package edu.bryant.tabu

import net.lag.configgy.Configgy
import net.lag.logging.Logger

object Tabu {
  var config = Configgy.config

  def set_config_file(configFile: String) = {
    Configgy.configure(configFile)
    config = Configgy.config
  }
}
