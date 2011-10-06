package edu.bryant.tabu

class Search(val task_list: Array[Task]) {
  val random = new scala.util.Random
  random.setSeed(System.nanoTime)
  var best_solution = new Solution(Array[Task]())
  var current_solution = new Solution(Array[Task]())
  var tabu_list = Array[Solution]()
  var non_improving_cycles = 0

  def duration = task_list.foldLeft(0) {(sum,t) => sum + t.duration}

  def fill_list = do {
    best_solution = new Solution(task_list.map(t => t.random_start_time(task_list.foldLeft(0) {(sum,t) => sum + t.duration})))
    current_solution = new Solution(best_solution.task_list_clone)
  } while(!best_solution.valid)

  def search: Solution = {
    fill_list
    val total_cycles = Tabu.config.getInt("cycles").get

    do {
      var aspiration_solution = aspiration
        current_solution = new Solution(aspiration_solution.task_list_clone)
        if(aspiration_solution.value < best_solution.value) {
          best_solution = new Solution(aspiration_solution.task_list_clone)
          non_improving_cycles = 0
        }
      step
      non_improving_cycles += 1
    } while(non_improving_cycles < total_cycles)
    best_solution
  }

  def step: Solution = {
    var new_solution = new Solution(current_solution.task_list_clone)
    val steps = Tabu.config.getConfigMap("steps").get


    steps.keys.filter(_ != "aspiration").foreach {step =>

      val current_step = steps.getConfigMap(step).get
      var step_best = new Solution(current_solution.task_list_clone)
      var move_best = new Solution(current_solution.task_list_clone)
      var moves = 1
      do{
        var cycles = 0
        do {

          new_solution = new_neighbor(current_solution, current_step.getString("removes").get.toInt)

          if (tabu(new_solution) == false) {
            if (cycles == 0){
              move_best = new Solution(new_solution.task_list_clone)
            }else{
              if (move_best.value >= new_solution.value) {
                move_best = new Solution(new_solution.task_list_clone)
                if (move_best.value < step_best.value) {
                  step_best = new Solution(move_best.task_list_clone)
                }
              }
            }
            cycles += 1
          }

        } while(cycles < current_step.getString("cycle_max").get.toInt)

        current_solution = new Solution(move_best.task_list_clone)

        moves += 1
      }while (moves < current_step.getString("moves_max").get.toInt)

      tabu_list = tabu_list.take(0)
      current_solution = new Solution(step_best.task_list_clone)

      if (step_best.value < best_solution.value) {
        best_solution = new Solution(step_best.task_list_clone)
        non_improving_cycles = 0
      }
    }

    new_solution
  }

  def aspiration: Solution = {
    val steps = Tabu.config.getConfigMap("steps").get
    val aspiration = steps.getConfigMap("aspiration").get
    new_neighbor(current_solution, aspiration.getString("removes").get.toInt)
  }

  def tabu(solution: Solution): Boolean = {
    val is_tabu = tabu_list.exists(i => i.task_list == solution.task_list)
    if(!is_tabu) {
      tabu_list = Array(solution) ++ tabu_list.take(Tabu.config.getInt("tabu_size").get - 1)
    }

    is_tabu
  }

  def randomly_change_start_times(solution: Solution, quantity: Int):Solution = {
    var new_solution = new Solution(solution.task_list_clone)

    do {
      (0 to quantity).foreach {i =>
        new_solution.task_list(random.nextInt(task_list.size - 1))
          .random_start_time(new_solution.task_list.foldLeft(0) {(sum,t) => sum + t.duration})
      }
    } while(!new_solution.valid)

    new_solution
  }

  def new_neighbor(solution:Solution, removes: Int): Solution = {
    var new_valid_neighbor = new Solution(solution.task_list_clone)
    var fail_count = 0
    val max_fails = 50
    var good_neighbor = false
    do{
      var neighbor = new Solution(remove_start_times(solution, removes).task_list_clone)
      new_start_time_boundaries(neighbor)
      fail_count = 0
      do{
        val possible_neighbor = new Solution(neighbor.task_list_clone)
        possible_neighbor.task_list.filter(t => t.empty_start_time == true).foreach{y =>
          val earliest = y.earliest_start_time
          val latest = y.latest_start_time
          if (latest - earliest > 0) {
            y.set_start_time(random.nextInt(latest - earliest) + earliest)
          }
        }

        if (possible_neighbor.valid == true){
          neighbor = new Solution(possible_neighbor.task_list_clone)
          good_neighbor = true
          fail_count = 50
        }else{fail_count += 1}
      }while(good_neighbor == false || fail_count != max_fails)
      new_valid_neighbor = new Solution(neighbor.task_list_clone)
    }while(good_neighbor == false)
    new_valid_neighbor
  }

  def remove_start_times(solution: Solution, removes: Int): Solution = {
    val remove_solution = new Solution(shuffle(solution.task_list_clone, random))

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
}
