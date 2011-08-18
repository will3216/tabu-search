package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}
}
