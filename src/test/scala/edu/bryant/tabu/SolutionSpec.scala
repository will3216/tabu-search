package edu.bryant.tabu

import org.specs2.mutable.Specification

class SolutionSpec extends Specification {
  "Solution" should {
    Tabu.interest_rate = 0.0068

    "have a task list" in {
      val solution = new Solution(Array(TaskFactory.build()))
      solution.task_list must not be empty
    }

    "compute the cost due to delay" in {
      val task1 = TaskFactory.build(cost=80000, duration=150, probability=1.0, outsourced=false, start_time=0)
      val task2 = TaskFactory.build(outsourcing_cost=160000, duration=100, probability=0.98, outsourced=true, start_time=150)
      val solution = new Solution(Array(task1, task2))

      solution.cost_due_to_delay must beCloseTo(250000.0, 2500)
    }

    "compute the present value cost" in {
      val task1 = TaskFactory.build(cost=80000, duration=150, probability=1.0, outsourced=false, start_time=0)
      val task2 = TaskFactory.build(outsourcing_cost=160000, duration=100, probability=0.98, outsourced=true, start_time=150)
      val solution = new Solution(Array(task1, task2))

      solution.present_value_cost must beCloseTo(136541.0, 1365)
    }

    "compute the value" in {
      val task1 = TaskFactory.build(cost=80000, duration=150, probability=1.0, outsourced=false, start_time=0)
      val task2 = TaskFactory.build(outsourcing_cost=160000, duration=100, probability=0.98, outsourced=true, start_time=150)
      val solution = new Solution(Array(task1, task2))

      solution.value must beCloseTo(386541.0, 6365)
    }
  }
}
