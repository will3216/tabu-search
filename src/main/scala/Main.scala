import edu.bryant.tabu
import net.lag.configgy.{Configgy,RuntimeEnvironment}
import tabu._

object Main extends App {
  private val runtime = new RuntimeEnvironment(getClass)
  runtime.load(args)

  val input = new Input
  val loaded_task_list = input.create_task_list();
  val new_search = new Search(loaded_task_list);
  var best_solution: Solution = new_search.search;
  println("Optimal Value: " + best_solution.value.toString)
  best_solution.task_list.foreach{ i =>
    println(i.task_id + ", " + i.start_time + "\n")
  }
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
}
