package edu.bryant.tabu

class Task(val task_id: Int, val product_id: Int, val duration: Int, val cost: Int,
  val resource_requirements: Array[Int], val outsourcing_cost: Int, val precedence_constraint: Array[Int]) {
}
