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

  /**
    * Types
    */
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
    def submit(implicit proof: FSA[T,W,S]) = println(s"$tasks taken by ${worker.get}, by ${supervisor.get}")
  }

  object FSA {
    implicit val endStateS3 = new S3()
    implicit val endStateS5 = new S5()
  }

  /**
    * States
    */
  case class S1 (t : List[Task], w: Option[Worker], s: Option[Supervisor]) extends FSA[No,No,No] (t, w, s)
  case class S2 (t : List[Task], w: Option[Worker], s: Option[Supervisor]) extends FSA[Yes,No,No] (t, w, s)
  case class S3 (t : List[Task], w: Option[Worker], s: Option[Supervisor]) extends FSA[Yes,Yes,No] (t, w, s) {
    def this() = this(List.empty[Task], None, None)
  }
  case class S4 (t : List[Task], w: Option[Worker], s: Option[Supervisor]) extends FSA[Yes,No,Yes] (t, w, s)
  class S5 (t : List[Task], w: Option[Worker], s: Option[Supervisor]) extends FSA[Yes,Yes,Yes] (t, w, s) {
    def this() = this(List.empty[Task], None, None)
  }

  /**
    * Transitions
    */
  def lets = new S1(List.empty[Task], None, None)

  implicit class AfterLets(s1: S1) {
    def delegate(task: Task) : S2 = new S2(task :: s1.t, s1.w, s1.s)
  }

  implicit class AfterDelegate(s2: S2) {
    def and(task: Task) : S2 = new S2(task :: s2.t, s2.w, s2.s)
    def to(worker: Worker): S3 = new S3(s2.t, Some(worker), s2.s)
    def supervisedBy(supervisor: Supervisor): S4 = new S4(s2.t, s2.w, Some(supervisor))
  }

  implicit class AfterDelegateTo(s3: S3) {
    def supervisedBy(supervisor: Supervisor): S5 = new S5(s3.t, s3.w, Some(supervisor))
  }

  implicit class AfterSupervisedBy(s4: S4) {
    def to(worker: Worker): S5 = new S5(s4.t, Some(worker), s4.s)
  }

  /**
    * Test
    */
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
