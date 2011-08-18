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

      search.best_solution.task_list(0).start_time must beGreaterThan(-1)
    }
  }
}
