package edu.bryant.tabu

object TaskFactory {
  def build(task_id: Int = 1, product_id: Int = 1, duration: Int = 1, cost: Int = 1, probability: Double = 1.0,
    resource_requirements: Array[Int] = Array(1), outsourcing_cost: Int = 1, precedence_constraint: Array[Int] = Array(1), start_time: Int = 1, outsourced: Boolean = false) = {

    val task = build_empty(task_id, product_id, duration, cost, probability, resource_requirements, outsourcing_cost, precedence_constraint)

    task.set_start_time(start_time)
    task.outsourced = outsourced

    task
  }

  def build_empty(task_id: Int = 1, product_id: Int = 1, duration: Int = 1, cost: Int = 1, probability: Double = 1.0,
    resource_requirements: Array[Int] = Array(1), outsourcing_cost: Int = 1, precedence_constraint: Array[Int] = Array(1)) = {

    new Task(task_id, product_id, duration, cost, probability, resource_requirements, outsourcing_cost, precedence_constraint)
  }
}
