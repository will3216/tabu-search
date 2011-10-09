import edu.bryant.tabu
import net.lag.configgy.{Configgy,RuntimeEnvironment}
import tabu._

object Main extends App {
  //private val runtime = new RuntimeEnvironment(getClass)
  //runtime.load(args)
  Configgy.configure("tabu.config")

  val input = new Input
  val example_number = Tabu.config.getList("examples_to_run").map(_.toInt).toArray
  for(ex <- 0 until example_number.size){
    val loaded_task_list = input.create_task_list(example_number(ex));
    for(j <- 1 to 5){
      for(i <- 1 to 100){
        var exp_id = i.toString + "nic" + j.toString
        var p = 0
        if(i <= 50) {p = i }else {
          if(i <= 75){p = (i - 50)*2 + 50 }else {
            p = (i - 75)*4 + 100
          }
        }
        run_and_print_and_output(p, exp_id, loaded_task_list, example_number(ex))
      }
    }
    def run_and_print_and_output(variable_value: Int, exp_id: String, loaded_task_list: Array[Task], example: Int){
      val new_search = new Search(loaded_task_list, variable_value);
      var time = System.nanoTime
      var best_solution: Solution = new_search.search;
      time = System.nanoTime - time
      print_results(exp_id, best_solution, time/1e9, variable_value, example)
    }
  }
  //println("Optimal Value: " + best_solution.value.toString)
  //best_solution.task_list.foreach{ i =>
    //println(i.task_id + ", " + i.start_time + "\n")
  //}
  def optimal_start_time(task: Task): Int = task.task_id match {
    case 1 => 10
    case 2 => 220
    case 3 => 0
    case 4 => 150
    case 5 => 160
    case 6 => 320
    case 7 => 120
    case 8 => 320
    case 9 => 250
    case 10 => 500
  }

  def optimal_solution_maker(solution: Solution): Solution = {
    solution.task_list.foreach(i => i.set_start_time(optimal_start_time(i)))
    solution
  }

  def print_results(exp_id: String, solution: Solution, run_time: Double, variable_value: Int, example: Int){
    val output = new java.io.FileWriter("results_file_7.txt", true)
    output.write(exp_id.toString + ", non-improving cycles, " + example.toString + ", " + variable_value.toString + ", " +
      solution.value.toString + ", " + run_time.toString)
    sort_tasks_by_start_time.foreach{ i =>
      output.write(", " + i.start_time)
    }
    output.append('\n')
    output.close

    def sort_tasks_by_start_time: Array[Task] = {
      //scala.util.Sorting.stableSort(solution.task_list.map(i => i.task_id),
        //| (e1: Int, e2: Int), e1 < e2).map(i => solution.task_list.find(j => j.task_id == i).get)
        solution.task_list.sortBy(_.task_id)
    }
  }
}
