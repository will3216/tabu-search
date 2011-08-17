import org.specs2.mutable.Specification
import org.specs2.matcher.DataTables

import scala.collection.immutable.HashMap

class MainSpec extends Specification with DataTables {
  val specs = HashMap[Int,Int](
    1 -> 2,
    2 -> 3
  )

  specs map { data =>
    "succ(%s)".format(data._1) in {
      data._1 + 1 === data._2
    }
  }
}
