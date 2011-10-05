package edu.bryant.tabu

class Solution(val task_list: Array[Task]) {
  def value = cost_due_to_delay + present_value_cost
  def valid = !task_list.exists(_.empty_start_time) && task_order_constraint && resource_constraint

  def resource_constraint: Boolean = {
    def combine(a: Array[Int], b: Array[Int]): Array[Int] = {
      a.zip(b).map(c => c._1 + c._2)
    }
    def enough(required: Array[Int], available: Array[Int]): Boolean = {
      required.zip(available).foldLeft(true) {(valid, i) => valid && i._1 <= i._2}
    }
    def available: Array[Int] = {
      Tabu.config.getList("resources_available").map(_.toInt).toArray
    }

    val resource_constrained = !task_list.map {t =>
      enough(task_list.filter(i =>
        i.task_id != t.task_id && i.start_time <= t.start_time && (i.start_time + i.duration) >= t.start_time
      ).map(_.resource_requirements).foldLeft(t.resource_requirements) {(a,b) => combine(a,b)}, available)
    }.exists(_ == false)

    resource_constrained
  }

  def task_order_constraint: Boolean = {
    val precedence_constrained = task_list.foldLeft(true) {(valid, t) => valid && Tabu.config.getList("task_requirements." + t.task_id.toString).foldLeft(true) {(valid2, i) =>
      val previous = task_list.filter(_.task_id == i.toInt)
      valid2 && (previous.size == 0 || t.start_time >= (previous.first.start_time + previous.first.duration))}
    }
    precedence_constrained
  }

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

    task_list.foldLeft(0.0) { (sum, t) => sum + (t.actual_cost * scala.math.exp(-Tabu.config.getDouble("interest_rate").get * t.start_time + probability(t))) }*10000
  }

  def task_list_clone: Array[Task] = task_list.map{t =>
    val task = new Task(t.task_id, t.product_id, t.duration, t.cost, t.probability, t.resource_requirements, t.outsourcing_cost, t.precedence_constraint)
    task.outsourced = t.outsourced

    if (t.empty_start_time == true) {
      task.no_start_time
    } else{
      task.set_start_time(t.start_time)
    }

    if(t.empty_latest_start_time == true) {
      task.no_latest_start_time
    }else{
      task.set_latest_start_time(t.latest_start_time)
    }

    if(t.empty_earliest_start_time == true) {
      task.no_earliest_start_time
    }else{
      task.set_earliest_start_time(t.earliest_start_time)
    }

    task
  }
}
