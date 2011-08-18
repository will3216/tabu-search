package edu.bryant.tabu

import org.specs2.mutable.Specification

class TaskSpec extends Specification {
  "Task" should {
    val task = new Task(task_id = 1,
                        product_id = 1,
                        duration = 1,
                        cost = 1,
                        resource_requirements = Array(1),
                        outsourcing_cost = 1,
                        precedence_constraint = Array(1));

    "have parameters" in {
      task.task_id mustEqual 1
      task.product_id mustEqual 1
      task.duration mustEqual 1
      task.cost mustEqual 1
      task.resource_requirements mustEqual Array(1)
      task.outsourcing_cost mustEqual 1
      task.precedence_constraint mustEqual Array(1)
    }
  }

}
