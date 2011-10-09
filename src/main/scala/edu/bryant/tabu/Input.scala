package edu.bryant.tabu


class Input {
  def create_task_list(example_number: Int): Array[Task] = {
    def get_first_task: Int = example_number match {
      case 1 | 4 | 5 | 7 => 1
      case 2 | 6 => 11
      case 3 => 21
    }
    def get_last_task: Int = example_number match {
      case 1 => 10
      case 2 | 4 => 20
      case 3 | 5 | 6 | 7 => 30
    }
    val first_task = get_first_task
    val last_task = get_last_task

    var task_list: Array[Task] = Array()
    for (i:Int <- first_task to last_task) {
      if(example_number == 5){
        if(i < 11 && i > 20){
          val resource_reqs = Array.fill(Tabu.config.getInt("number_of_resources").get)(0)
          Tabu.config.getList("task_resource_requirements." ++ i.toString).map{j => def conv(string: String): Int = {val int_val = string.toInt; int_val}; conv(j)}.copyToArray(resource_reqs)
            val task:Array[Task] = Array.fill(1)(TaskFactory.build_empty(task_id = Tabu.config.getInt("task_ids." ++ i.toString).get,
            product_id = Tabu.config.getInt("product_id." ++ i.toString).get, duration = Tabu.config.getInt("task_duration." ++ i.toString).get,
            cost = Tabu.config.getInt("task_costs." ++ i.toString).get, probability = Tabu.config.getDouble("task_probability_of_success." ++ i.toString).get,
            resource_requirements = resource_reqs, outsourcing_cost = Tabu.config.getInt("task_costs_of_outsourcing." ++ i.toString).get,
            Tabu.config.getList("task_requirements." ++ i.toString).map{j => def conv(string: String): Int = {val int_val = string.toInt; int_val}; conv(j)}.toArray))

            task_list = task_list ++ task
        }
      }else {
        val resource_reqs = Array.fill(Tabu.config.getInt("number_of_resources").get)(0)
        Tabu.config.getList("task_resource_requirements." ++ i.toString).map{j => def conv(string: String): Int = {val int_val = string.toInt; int_val}; conv(j)}.copyToArray(resource_reqs)
        val task:Array[Task] = Array.fill(1)(TaskFactory.build_empty(task_id = Tabu.config.getInt("task_ids." ++ i.toString).get,
          product_id = Tabu.config.getInt("product_id." ++ i.toString).get, duration = Tabu.config.getInt("task_duration." ++ i.toString).get,
          cost = Tabu.config.getInt("task_costs." ++ i.toString).get, probability = Tabu.config.getDouble("task_probability_of_success." ++ i.toString).get,
          resource_requirements = resource_reqs, outsourcing_cost = Tabu.config.getInt("task_costs_of_outsourcing." ++ i.toString).get,
          Tabu.config.getList("task_requirements." ++ i.toString).map{j => def conv(string: String): Int = {val int_val = string.toInt; int_val}; conv(j)}.toArray))

        task_list = task_list ++ task;
      }
    }
    task_list
  }


}
