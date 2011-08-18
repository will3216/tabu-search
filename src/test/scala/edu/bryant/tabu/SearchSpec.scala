package edu.bryant.tabu

import org.specs2.mutable.Specification

class SearchSpec extends Specification {
  "Search" should {
    "accept a list of tasks" in {
      val task_list = Array[Task](TaskFactory.build())
      val search = new Search(task_list)
      search.task_list must not be empty
    }

    "sum the duration of the tasks" in {
      val task_list = Array[Task](TaskFactory.build(duration = 1), TaskFactory.build(duration = 2))
      val search = new Search(task_list)

      search.duration mustEqual 3
    }

    "generate an initial solution" in {
      val task_list = Array[Task](TaskFactory.build_empty(duration = 1), TaskFactory.build_empty(duration = 2))
      val search = new Search(task_list)

      search.fill_list
      search.best_solution.task_list(0).start_time must beGreaterThan(-1)
    }

    "generate a new solution in step" in {
      val task_list = Array[Task](TaskFactory.build_empty(duration = 1), TaskFactory.build_empty(duration = 2))
      val search = new Search(task_list)

      search.fill_list
      search.step.task_list(0).start_time must beGreaterThan(-1)
    }

    "generate a new solution in search" in {
      val task_list = Array[Task](TaskFactory.build_empty(duration = 150, probability = 1, resource_requirements = Array(1,0,0,0), outsourcing_cost = 16, cost = 8, task_id = 1),
                                  TaskFactory.build_empty(duration = 100, task_id = 2, cost = 8, outsourcing_cost = 16, probability = 1, resource_requirements = Array(0,1,0,0)),
                                  TaskFactory.build_empty(duration = 120, task_id = 3, cost = 5, outsourcing_cost = 10, probability = 1, resource_requirements = Array(0,0,1,0)),
                                  TaskFactory.build_empty(duration = 10, task_id = 4, cost = 1, outsourcing_cost = 2, probability = 0.84, resource_requirements = Array(0,0,0,1)),
                                  TaskFactory.build_empty(duration = 90, task_id = 5, cost = 49, outsourcing_cost = 98, probability = 0.98, resource_requirements = Array(1,0,0,0)),
                                  TaskFactory.build_empty(duration = 180, task_id = 6, cost = 111, outsourcing_cost = 222, probability = 1, resource_requirements = Array(0,1,0,0)))
      val search = new Search(task_list)

      search.search.task_list(0).start_time must beGreaterThan(-1)
    }

    "know if a solution is tabu" in {
      val task_list = Array[Task](TaskFactory.build(), TaskFactory.build_empty())
      val search = new Search(task_list)

      search.tabu_list = Array(new Solution(task_list))
      search.tabu(new Solution(task_list)) mustEqual true
    }
  }
}
