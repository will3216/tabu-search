package edu.bryant.tabu

class Solution(val task_list: Array[Task]) {
  def value = cost_due_to_delay + present_value_cost

  def cost_due_to_delay: Double = {
    val discrete_cost = scala.collection.immutable.HashMap[Int, Array[Double]](
      1 -> Array(1000, 5000, 10000),
      2 -> Array(1000, 5000, 12000),
      3 -> Array(1000, 5000, 12000)
    )
    val discrete_time = scala.collection.immutable.HashMap[Int, Array[Double]](
      1 -> Array(0, 300, 600),
      2 -> Array(0, 300, 600),
      3 -> Array(0, 300, 600)
    )
    val end_times_per_product = task_list.groupBy(_.product_id).mapValues(_.map(t => t.start_time + t.duration).max)

    end_times_per_product.foldLeft(0.0) {
      case (sum, (p,t)) => sum + (0 to 2).foldLeft(0.0) {
        (sum1,m) => sum1 + (discrete_cost(p)(m) * (Array(t - discrete_time(p)(m), 0).max))
      }
    }
  }

  def present_value_cost: Double = {
    def probability(task: Task): Double = {
      task_list.filter(t => t.product_id == task.product_id && t.start_time + t.duration <= task.start_time).foldLeft(0.0) {(sum, t) =>
        sum + scala.math.log(t.probability)}
    }

    task_list.foldLeft(0.0) { (sum, t) => sum + (t.actual_cost * scala.math.exp(-Tabu.interest_rate * t.start_time + probability(t))) }
  }
}
