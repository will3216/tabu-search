package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  val random = new scala.util.Random
  random.setSeed(System.nanoTime)
  var best_solution = new Solution(available, interest_rate, Array[Task]())
  var current_solution = new Solution(available, interest_rate, Array[Task]())
  var tabu_list = Array[Solution]()
  var non_improving_cycles = 0
  val total_cycles = Tabu.config.getInt("cycles").get
  val steps = Tabu.config.getConfigMap("steps").get
  val number_of_non_aspiration_steps = steps.keys.filter(_ != "aspiration").size
  val step_values = Array.fill(number_of_non_aspiration_steps, 3)(0)
  fill_step_values_array
  val aspiration_removes = steps.getConfigMap("aspiration").get.getString("removes").get.toInt
  val tabu_size = Tabu.config.getInt("tabu_size").get
  def available_resources: Array[Int] = {
      Tabu.config.getList("resources_available").map(_.toInt).toArray
  }
  val interest_rate = Tabu.config.getDouble("interest_rate").get
  val available = available_resources
  var total_fails = 0
  var total_task_fails = 0
  val solution_packing = Tabu.config.getString("solution_packing").get.toBoolean

  def fill_step_values_array = {
    //time to sleep! remember to finish this or do not save!
    var i = 0
    steps.keys.filter(_ != "aspiration").foreach {step =>
    val current_step = steps.getConfigMap(step).get
    step_values(i)(0) = current_step.getString("removes").get.toInt
    step_values(i)(1) = current_step.getString("cycle_max").get.toInt
    step_values(i)(2) = current_step.getString("moves_max").get.toInt
    i+=1
    }
  }

  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}

  def fill_list = do {
    best_solution = new Solution(available, interest_rate, task_list.map(t => t.random_start_time(task_list.foldLeft(0) {(sum,t) => sum + t.duration})))
    current_solution = new Solution(available, interest_rate, best_solution.task_list_clone)
  } while(!best_solution.valid)

  def search: Solution = {
    fill_list

    do {
      var aspiration_solution = aspiration
        current_solution = new Solution(available, interest_rate, aspiration_solution.task_list_clone)
        if(aspiration_solution.value < best_solution.value) {
          best_solution = new Solution(available, interest_rate, aspiration_solution.task_list_clone)
          non_improving_cycles = 0
        }
      step
      non_improving_cycles += 1
    } while(non_improving_cycles < total_cycles)
    println("Fail Count Total: " + total_fails.toInt)
    println("Task Fail Count Total: " + total_task_fails.toInt)

    best_solution
  }

  def step: Solution = {
    var new_solution = new Solution(available, interest_rate, current_solution.task_list_clone)

    for(i <- 0 until number_of_non_aspiration_steps){

      var step_best = new Solution(available, interest_rate, current_solution.task_list_clone)
      var move_best = new Solution(available, interest_rate, current_solution.task_list_clone)
      var moves = 1
      do{
        var cycles = 0
        do {

          new_solution = new_neighbor(current_solution, step_values(i)(0))

          if (tabu(new_solution) == false) {
            if (cycles == 0){
              move_best = new Solution(available, interest_rate, new_solution.task_list_clone)
            }else{
              if (move_best.value >= new_solution.value) {
                move_best = new Solution(available, interest_rate, new_solution.task_list_clone)
                if (move_best.value < step_best.value) {
                  step_best = new Solution(available, interest_rate, move_best.task_list_clone)
                }
              }
            }
            cycles += 1
          }

        } while(cycles < step_values(i)(1))

        current_solution = new Solution(available, interest_rate, move_best.task_list_clone)

        moves += 1
      }while (moves < step_values(i)(2))

      tabu_list = tabu_list.take(0)
      current_solution = new Solution(available, interest_rate, step_best.task_list_clone)
      var new_value = step_best.value

      if(solution_packing == true){
        val transpose_solution = new Solution(available, interest_rate, step_best.task_list_clone)
        transpose_start_times(transpose_solution)
        val transpose_value = transpose_solution.value

        if(new_value > transpose_value){
          step_best = new Solution(available, interest_rate, transpose_solution.task_list_clone)
          new_value = transpose_value
        }
      }

      if (step_best.value < best_solution.value) {
        best_solution = new Solution(available, interest_rate, step_best.task_list_clone)
        non_improving_cycles = 0
      }
    }

    new_solution
  }

  def aspiration: Solution = {
    val aspiration_solution = new_neighbor(current_solution, aspiration_removes)
    aspiration_solution
  }

  def tabu(solution: Solution): Boolean = {
    val is_tabu = tabu_list.exists(i => i.task_list == solution.task_list)
    if(!is_tabu) {
      tabu_list = Array(solution) ++ tabu_list.take(tabu_size - 1)
    }

    is_tabu
  }

  def randomly_change_start_times(solution: Solution, quantity: Int):Solution = {
    var new_solution = new Solution(available, interest_rate, solution.task_list_clone)

    do {
      (0 to quantity).foreach {i =>
        new_solution.task_list(random.nextInt(task_list.size - 1))
          .random_start_time(new_solution.task_list.foldLeft(0) {(sum,t) => sum + t.duration})
      }
    } while(!new_solution.valid)

    new_solution
  }

  def new_neighbor(solution:Solution, removes: Int): Solution = {
    var new_valid_neighbor = new Solution(available, interest_rate, solution.task_list_clone)
    var fail_count = 0
    val max_fails = 10
    var good_neighbor = false
    do{
      var neighbor = new Solution(available, interest_rate, remove_start_times(solution, removes).task_list_clone)
      new_start_time_boundaries(neighbor)
      fail_count = 0
      do{
        val possible_neighbor = new Solution(available, interest_rate, neighbor.task_list_clone)
        possible_neighbor.task_list.filter(t => t.empty_start_time == true).foreach{y =>
          val earliest = y.earliest_start_time
          val latest = y.latest_start_time
          if (latest - earliest > 0) {
            var task_fail_count = 0
            var good_start_time = false
            do{
                y.set_start_time(random.nextInt(latest - earliest) + earliest)
                good_start_time = resource_constrained_task(y, possible_neighbor.task_list)
                if(good_start_time == false){task_fail_count +=1; total_task_fails += 1}
            }while(good_start_time == false && task_fail_count <= max_fails)
          }
        }

        if (possible_neighbor.valid == true){
          neighbor = new Solution(available, interest_rate, possible_neighbor.task_list_clone)
          good_neighbor = true
        }else{
          fail_count += 1
          total_fails += 1
        }
      }while(good_neighbor == false && fail_count <= max_fails)
      new_valid_neighbor = new Solution(available, interest_rate, neighbor.task_list_clone)
    }while(good_neighbor == false)
    new_valid_neighbor
  }

  def remove_start_times(solution: Solution, removes: Int): Solution = {
    val remove_solution = new Solution(available, interest_rate, shuffle(solution.task_list_clone, random))

    for (i <- 0 until removes){
      remove_solution.task_list(i).no_start_time
    }
    remove_solution
  }

  def shuffle[Task](modified: Array[Task], rng: scala.util.Random): Array[Task] = {
    def swap(a: Int, b: Int) = {
      val temp = modified(a)
      modified(a) = modified(b)
      modified(b) = temp
    }
    for (i <- 0 until (modified.size)) {
      swap(i, rng.nextInt(modified.size))
    }
    modified
  }

  def new_start_time_boundaries(solution: Solution){
    def all_tasks_have_start_time_bounds: Boolean={
      if (solution.task_list.exists(p => p.empty_earliest_start_time == true) || solution.task_list.exists(p => p.empty_latest_start_time == true)){
        false
      }else{true}
    }

    def remove_preceding_latest_start_times(task_id: Option[Int]): Option[Int] = {
      if(task_id.isEmpty != true){
        solution.task_list.find(p => p.task_id == task_id.get).get.no_latest_start_time
        solution.task_list.find(p => p.task_id == task_id.get).get.precedence_constraint.foreach{i =>
          remove_preceding_latest_start_times(Some(i))
        }
      }
      None
    }

    def remove_subsequent_earliest_start_times(task_id: Option[Int]): Option[Int] = {
      if(task_id.isEmpty != true){
        solution.task_list.find(p => p.task_id == task_id.get).get.no_earliest_start_time
        solution.task_list.filter(i => i.precedence_constraint.exists(j => j == task_id.get)).foreach{j =>
          remove_subsequent_earliest_start_times(Some(j.task_id))
        }
      }
      None
    }

    def find_new_earliest_start_time(task: Task): Option[Int] = {
      var biggest: Option[Int] = None

      def bigger(value: Int){
        if(biggest.isEmpty != true){
          if(value > biggest.get){
            biggest = Some(value)
          }
        }else{
          biggest = Some(value)
        }
      }

      task.precedence_constraint.foreach{i =>
        var p = solution.task_list.find(j => j.task_id == i).get
        if(p.empty_start_time != true){
          bigger(p.start_time + p.duration)
        }else{
          if(p.empty_earliest_start_time != true){
            bigger(p.earliest_start_time + p.duration)
          }
        }
      }

      if(task.precedence_constraint.isEmpty == true){
        biggest = Some(0)
      }

      if(biggest.isEmpty != true){
        task.set_earliest_start_time(biggest.get)
      }
      biggest
    }

    def find_new_latest_start_time(task: Task): Option[Int] = {
      var smallest: Option[Int] = None

      def smaller(value: Int){
        if(smallest.isEmpty != true){
          if(value < smallest.get){
            smallest = Some(value)
          }
        }else{
          smallest = Some(value)
        }
      }

      solution.task_list.filter(i=> i.precedence_constraint.exists(j=> j == task.task_id)).foreach{p =>
        if(p.empty_start_time != true){
          smaller(p.start_time)
        }else{
          if(p.empty_latest_start_time != true){
            smaller(p.latest_start_time)
          }
        }
      }

      if(solution.task_list.exists(i=> i.precedence_constraint.exists(j=> j == task.task_id) == false) == true){
        smallest = Some(duration)
      }

      if(smallest.isEmpty != true){
        task.set_latest_start_time(smallest.get + task.duration)
      }
      smallest
    }

    solution.task_list.filter(i => i.empty_start_time == true).foreach{j =>
      remove_preceding_latest_start_times(Some(j.task_id))
      remove_subsequent_earliest_start_times(Some(j.task_id))
    }

    while(all_tasks_have_start_time_bounds == false){
      solution.task_list.foreach{i =>
        find_new_earliest_start_time(i)
        find_new_latest_start_time(i)
      }
      val sum = solution.task_list.count(i => i.empty_latest_start_time == true) + solution.task_list.count(i => i.empty_earliest_start_time == true)
    }
  }

  def resource_constrained_task(t: Task, task_list: Array[Task]): Boolean = {
    def combine(a: Array[Int], b: Array[Int]): Array[Int] = {
      a.zip(b).map(c => c._1 + c._2)
    }
    def enough(required: Array[Int], available: Array[Int]): Boolean = {
      required.zip(available).foldLeft(true) {(valid, i) => valid && i._1 <= i._2}
    }

    val resource_constrained = {
      enough(task_list.filter(i => i.task_id != t.task_id && i.empty_start_time == false).filter(i =>
        i.start_time <= t.start_time && (i.start_time + i.duration) >= t.start_time
      ).map(_.resource_requirements).foldLeft(t.resource_requirements) {(a,b) => combine(a,b)}, available)
    }
    resource_constrained
  }

  def transpose_start_times(solution: Solution): Solution = {
    val negative_transpose = solution.task_list.map(t => t.start_time).min
    solution.task_list.foreach(t => t.set_start_time(t.start_time - negative_transpose))
    solution
  }
}
