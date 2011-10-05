package edu.bryant.tabu


class Input {
  def create_task_list(): Array[Task] = {
    

    val num_of_tasks = Tabu.config.getInt("number_of_tasks").get
    var task_list: Array[Task] = Array()
    for (i:Int <- 1 to num_of_tasks) {
      val resource_reqs = Array.fill(Tabu.config.getInt("number_of_resources").get)(0)
      Tabu.config.getList("task_resource_requirements." ++ i.toString).map{j => def conv(string: String): Int = {val int_val = string.toInt; int_val}; conv(j)}.copyToArray(resource_reqs)
      val task:Array[Task] = Array.fill(1)(TaskFactory.build_empty(task_id = Tabu.config.getInt("task_ids." ++ i.toString).get, product_id = Tabu.config.getInt("product_id." ++ i.toString).get, duration = Tabu.config.getInt("task_duration." ++ i.toString).get, cost = Tabu.config.getInt("task_costs." ++ i.toString).get, probability = Tabu.config.getDouble("task_probability_of_success." ++ i.toString).get, resource_requirements = resource_reqs, outsourcing_cost = Tabu.config.getInt("task_costs_of_outsourcing." ++ i.toString).get, Tabu.config.getList("task_requirements." ++ i.toString).map{j => def conv(string: String): Int = {val int_val = string.toInt; int_val}; conv(j)}.toArray))

      task_list = task_list ++ task;
    }

    task_list
  }


}