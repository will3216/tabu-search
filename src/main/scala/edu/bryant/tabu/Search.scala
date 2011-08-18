package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  var best_solution = new Solution(Array[Task]())
  fill_list

  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}
  def fill_list = do {
    best_solution = new Solution(task_list.map(t => t.random_start_time(task_list.foldLeft(0) {(sum,t) => sum + t.duration})))
  } while(!best_solution.valid)

  def step: Solution = {
    var current_solution = new Solution(best_solution.task_list)
    val steps = Tabu.config.getConfigMap("steps").get

    steps.keys.filter(_ != "aspiration").foreach {step =>
      var cycles = 1
      val current_step = steps.getConfigMap(step).get
      do {
        randomly_change_start_times(current_solution, current_step.getString("removes").get.toInt)
        cycles += 1
      } while(cycles < current_step.getString("cycles").get.toInt)
    }

    current_solution
  }

  def randomly_change_start_times(solution: Solution, quantity: Int):Solution = {
    var new_solution = new Solution(best_solution.task_list)

    do {
      (0 to quantity).foreach {i =>
        new_solution.task_list(scala.util.Random.nextInt(task_list.size - 1))
          .random_start_time(solution.task_list.foldLeft(0) {(sum,t) => sum + t.duration})
      }
    } while(!new_solution.valid)

    new_solution
  }
}
