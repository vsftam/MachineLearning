package vsftam

import scala.annotation.implicitNotFound

/**
  * Created by vincenttam on 2/18/17.
  *
  * Simple DSL with the below grammar:
  *
  * lets delegate (Task1) [and (Task2)]  to (WorkerX) supervisedBy (SupervisorY)
  * lets delegate (Task1) supervisedBy (SupervisorY) to (WorkerX)
  *
  */
object SimpleDSL {

  case class Task(name: String)
  case class Worker(name: String)
  case class Supervisor(name: String)

  sealed trait TaskDefined
  sealed trait WorkerDefined
  sealed trait SupervisorDefined
  trait Yes extends TaskDefined with WorkerDefined with SupervisorDefined
  trait No extends TaskDefined with WorkerDefined with SupervisorDefined

  @implicitNotFound("Grammar is not valid: " +
    "Task Defined (exp: Yes): ${T}, " +
    "Worker Defined (exp: Yes): ${W}, " +
    "SupervisorDefined (exp: Yes or No): ${S}")
  class FSA[T <: TaskDefined, W <: WorkerDefined, S <: SupervisorDefined](
    tasks: List[Task],
    worker: Option[Worker],
    supervisor: Option[Supervisor]
  ) {
    def submit(implicit proof: FSA[T,W,S]) = {
      println(s"$tasks taken by ${worker.get}, by ${supervisor.get}")
    }
  }

  object FSA {
    implicit val endStateS3 = new S3()
    implicit val endStateS5 = new S5()
  }

  def lets = new S1(List.empty[Task], None, None)

  class S1 (t : List[Task], w: Option[Worker], s: Option[Supervisor])
    extends FSA[No,No,No] (t, w, s) {
    def delegate(task: Task) : S2 = {
      val t2 = task :: t
      new S2(t2, w, s)
    }
  }

  class S2 (t : List[Task], w: Option[Worker], s: Option[Supervisor])
    extends FSA[Yes,No,No] (t, w, s) {
    def and(task: Task) : S2 = {
      val t2 = task :: t
      new S2(t2, w, s)
    }
    def to(worker: Worker): S3 = new S3(t, Some(worker), s)
    def supervisedBy(supervisor: Supervisor): S4 = new S4(t, w, Some(supervisor))
  }

  class S3 (t : List[Task], w: Option[Worker], s: Option[Supervisor])
    extends FSA[Yes,Yes,No] (t, w, s) {
    def this() = this(List.empty[Task], None, None)
    def supervisedBy(supervisor: Supervisor): S5 = new S5(t, w, Some(supervisor))
  }
  class S4 (t : List[Task], w: Option[Worker], s: Option[Supervisor])
    extends FSA[Yes,No,Yes] (t, w, s) {
    def to(worker: Worker): S5 = new S5(t, Some(worker), s)
  }

  class S5 (t : List[Task], w: Option[Worker], s: Option[Supervisor])
    extends FSA[Yes,Yes,Yes] (t, w, s) {
    def this() = this(List.empty[Task], None, None)
  }


  def main(args: Array[String]): Unit = {
    val task1 = new Task("task 1")
    val task2 = new Task("task 2")
    val workerA = new Worker("worker A")
    val supervisorY = new Supervisor("supervisor Y")

    (lets delegate task1 and task2 to workerA supervisedBy supervisorY).submit
    // below won't compile as grammar not correct
    // (lets delegate task1 and task2 supervisedBy supervisorY).submit
  }
}
