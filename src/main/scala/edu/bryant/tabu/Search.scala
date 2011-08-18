package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  var best_solution = new Solution(Array[Task]())
  fill_list

  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}
  def fill_list = do {
    best_solution = new Solution(task_list.map(t => t.random_start_time(task_list.foldLeft(0) {(sum,t) => sum + t.duration})))
  } while(!best_solution.valid)
}
