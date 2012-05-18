package edu.bryant.tabu

object SolutionFactory {
  def available_resources: Array[Int] = {
    Tabu.config.getList("resources_available").map(_.toInt).toArray
  }

  val available = available_resources
  val interest_rate = Tabu.config.getDouble("interest_rate").get
  def build_new_solution(task_list: Array[Task]): Solution = {
    val solution = new Solution(available, interest_rate, task_list)
    solution
  }
  def clone(solution: Solution): Solution = {

    val new_solution = new Solution(available, interest_rate,
      solution.task_list_clone)
    new_solution
  }
}
