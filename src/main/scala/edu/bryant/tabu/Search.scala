package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  task_list.foreach(t => t.random_start_time(task_list.foldLeft(0) {(sum,t) => sum + t.duration}))

  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}
  var best_solution = new Solution(task_list)
}
