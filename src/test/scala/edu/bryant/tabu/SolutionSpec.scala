package edu.bryant.tabu

import org.specs2.mutable.Specification

class SolutionSpec extends Specification {
  "Solution" should {
    val tabu = Tabu.set_config_file("tabu.config")

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

  "constraints" should {
    val tabu = Tabu.set_config_file("src/test/scala/edu/bryant/tabu/fixtures/tabu.config")

    "require tasks to be in the correct order" in {
      val solution = new Solution(Array(TaskFactory.build(task_id=4, start_time=3), TaskFactory.build(task_id=7, start_time=0, duration=2)))

      solution.task_order_constraint mustEqual(true)
    }

    "fail when tasks aren't in the correct order" in {
      val solution = new Solution(Array(TaskFactory.build(task_id=4, start_time=0), TaskFactory.build(task_id=7, start_time=3, duration=2)))

      solution.task_order_constraint mustEqual(false)
    }

    "not fail if properly utilizing resources" in {
      val solution = new Solution(Array(TaskFactory.build(start_time=0, resource_requirements = Array(1,1,0,0)), TaskFactory.build(start_time=0, resource_requirements = Array(0,0,1,1))))

      solution.resource_constraint mustEqual(true)
    }

    "fail if utilizing too many resources" in {
      val solution = new Solution(Array(TaskFactory.build(start_time=0, resource_requirements = Array(0,1,1,0)), TaskFactory.build(start_time=0, resource_requirements = Array(0,0,1,1))))

      solution.resource_constraint mustEqual(false)
    }
  }
}
