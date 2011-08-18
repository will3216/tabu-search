package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  var best_solution = new Solution(Array[Task]())
  var current_solution = new Solution(Array[Task]())
  var tabu_list = Array[Solution]()

  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}
  def fill_list = do {
    best_solution = new Solution(task_list.map(t => t.random_start_time(task_list.foldLeft(0) {(sum,t) => sum + t.duration})))
    current_solution = new Solution(best_solution.task_list_clone)
  } while(!best_solution.valid)

  def search: Solution = {
    fill_list
    val total_cycles = Tabu.config.getInt("cycles").get
    var cycles = 0
    println("Best: " + best_solution.value.toString)

    do {
      var aspiration_solution = aspiration

      if(aspiration_solution.value <= current_solution.value) {
        current_solution = new Solution(aspiration_solution.task_list_clone)
        println("Aspiration: " + aspiration_solution.value.toString)
        println("Best: " + best_solution.value.toString)
        if(aspiration_solution.value < best_solution.value) {
          best_solution = new Solution(aspiration_solution.task_list_clone)
          cycles = 0
          println("reset")
          println(best_solution.value.toString)
        }
      }

      var step_solution = step

      if(step_solution.value <= current_solution.value) {
        current_solution = new Solution(step_solution.task_list_clone)
        println("Step: " + step_solution.value.toString)
        println("Best: " + best_solution.value.toString)
        if(step_solution.value < best_solution.value) {
          best_solution = new Solution(step_solution.task_list_clone)
          cycles = 0
          println("reset")
          println(best_solution.value.toString)
        }
      }
      cycles += 1
    } while(cycles < total_cycles)

    best_solution
  }

  def step: Solution = {
    var new_solution = new Solution(best_solution.task_list_clone)
    val steps = Tabu.config.getConfigMap("steps").get

    steps.keys.filter(_ != "aspiration").foreach {step =>
      println(step)
      var cycles = 1
      val current_step = steps.getConfigMap(step).get
      do {
        new_solution = randomly_change_start_times(current_solution, current_step.getString("removes").get.toInt)
        cycles += 1
      } while((tabu(new_solution) || !(!tabu(new_solution) && new_solution.value <= current_solution.value)) && cycles < current_step.getString("cycles").get.toInt)
    }

    new_solution
  }

  def aspiration: Solution = {
    println("aspiration")
    val steps = Tabu.config.getConfigMap("steps").get
    val aspiration = steps.getConfigMap("aspiration").get
    randomly_change_start_times(current_solution, aspiration.getString("removes").get.toInt)
  }

  def tabu(solution: Solution): Boolean = {
    val is_tabu = tabu_list.exists(i => i.task_list == solution.task_list)
    if(!is_tabu) {
      tabu_list = Array(solution) ++ tabu_list.take(Tabu.config.getInt("tabu_size").get - 1)
    }

    is_tabu
  }

  def randomly_change_start_times(solution: Solution, quantity: Int):Solution = {
    var new_solution = new Solution(current_solution.task_list_clone)

    do {
      (0 to quantity).foreach {i =>
        new_solution.task_list(scala.util.Random.nextInt(task_list.size - 1))
          .random_start_time(new_solution.task_list.foldLeft(0) {(sum,t) => sum + t.duration})
      }
    } while(!new_solution.valid)

    new_solution
  }
}
