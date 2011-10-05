package edu.bryant.tabu

class Task(val task_id: Int, val product_id: Int, val duration: Int, val cost: Int, val probability: Double,
  val resource_requirements: Array[Int], val outsourcing_cost: Int, val precedence_constraint: Array[Int]) {
  var outsourced: Boolean = false
  private var _start_time: Option[Int] = None;
  private var _earliest_start_time: Option[Int] = None;
  private var _latest_start_time: Option[Int] = None;

  def random_start_time(sum_all_durations: Int) = {
    set_start_time(scala.util.Random.nextInt(sum_all_durations - duration + 1))
    this
  }

  def set_start_time(value: Int): Option[Int]= { 
    _start_time = Some(value)
    Some(value)
  }
  def start_time = { _start_time.get }
  def empty_start_time = {_start_time.isEmpty}
  def no_start_time: Option[Int] = {
    _start_time = None
    None
  }

  def set_earliest_start_time(value: Int): Option[Int] = { 
    _earliest_start_time = Some(value)
    Some(value)
  }
  def earliest_start_time = _earliest_start_time.get
  def empty_earliest_start_time = _earliest_start_time.isEmpty
  def no_earliest_start_time: Option[Int] = {
    _earliest_start_time = None
    None
  }

  def set_latest_start_time(value: Int): Option[Int] = { 
    _latest_start_time = Some(value)
    Some(value)
  }
  def latest_start_time = _latest_start_time.get
  def empty_latest_start_time = _latest_start_time.isEmpty
  def no_latest_start_time: Option[Int] = { 
    _latest_start_time = None
    None
  }

  def actual_cost: Int = outsourced match {
    case true => outsourcing_cost
    case false => cost
  }
}
