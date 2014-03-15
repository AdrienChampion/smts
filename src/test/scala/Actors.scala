/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *  This file is part of Smts.                                               *
 *                                                                           *
 *  Smts is free software: you can redistribute it and/or modify             *
 *  it under the terms of the GNU Lesser General Public License as           *
 *  published by the Free Software Foundation, either version 3 of the       *
 *  License, or (at your option) any later version.                          *
 *                                                                           *
 *  Smts is distributed in the hope that it will be useful,                  *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU Lesser General Public License for more details.                      *
 *                                                                           *
 *  You should have received a copy of the GNU Lesser General Public         *
 *  License along with Smts.                                                 *
 *  If not, see <http://www.gnu.org/licenses/>.                              *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package smts.test

import java.io.Writer

import akka.actor._
import com.typesafe.config.ConfigFactory

/** Tests basic functionalities of the Smts SMT solver wrapper. */
object Actors extends smts.SmtsFactory[
  ExprStructure.Expr, ExprStructure.Ident, ExprStructure.Sort
] with SmtParsers with smts.utils.OptionHandler with App {

  import ExprStructure._


  // |===| Option related things.

  object Options {
    // If true, then success parsing is activated.
    private var _success = false
    def success = _success
    def noSuccess = _success = !_success

    // An optional log location.
    private var _log: Option[String] = None
    def log = _log
    def log_= (newLog: String) = _log = Some(newLog)

    // The solver info that will be used.
    private var _solver: SolverInfo = createSolver("z3")
    def solver = _solver
    private def createSolver(s: String) =s match {
      case "z3" => Z3(success, models = true, unsatCores = true)
      case "cvc4" => CVC4(success, models = true)
      case "mathsat5" => MathSat5(success, models = true, unsatCores = true)
      case _ => throw new Exception("Should not be reachable.")
    }
    def createSolverInfo(s: String) = _solver = createSolver(s)
  }

  val optionPrint = { s: String => println(s) }
  val myArguments = Nil
  val myOptions = {
    (
      "--solver=", { s: String => optionValue(s) match {
        case "z3" => Options.createSolverInfo("z3")
        case "cvc4" | "CVC4" => Options.createSolverInfo("cvc4")
        case "mathsat" | "MathSat" | "mathsat5" | "MathSat5" =>
          Options.createSolverInfo("mathsat5")
        case "dreal" | "dReal" => Options.createSolverInfo("dreal")
        case _ => optionError("unexpected solver value \"" + optionValue(s) + "\".")
      }},
      "<string>: the solver to use. z3, mathsat5 or cvc4 (default z3)." :: Nil
    ) :: (
      "--log=", { s: String => {
        Options.log = optionValue(s)
        Options.createSolverInfo(Options.solver.name)
      }},
      "<file>: a file to log the smt lib 2 queries to." :: Nil
    ) :: (
      "--noSuccess", { s: String => Options.noSuccess },
      ": deactivates success parsing." :: Nil
    ) :: Nil
  }

  setOptions()


  def title(s: String) = println("|=====| \033[31;1m" + s + "\033[0m |=====|")

  /** Preventing Akka from loading dead letters. */
  val customConf = ConfigFactory.parseString(
"""
akka {
  log-dead-letters=0
  log-dead-letters-during-shutdown=off
}
"""
  )

  val toKill = new scala.collection.mutable.HashSet[ActorRef]

  val actorSystem = ActorSystem("Smts",ConfigFactory.load(customConf))

  println
  title("Initialization.")
  print("Creating client for testing smts... ")
  val client = actorSystem.actorOf(Props(new SolverBenchs {
    def act = {
      case solver: ActorRef => {
        checksatTest(
          () => { restart() ; println ; modelTest(
            () => { restart() ; println ; unsatCoreTest(
              () => { println ; killSystem() }
            )}
          )}
        )
      }
    }
    def receive = act
    def stop = { println("Stopping.") ; context stop self }
  }), name = "client")

  toKill += client
  println("Done")

  print(
    "Creating a smts instance with " + Options.solver.name + " as the solver... "
  )
  val solver = actorSystem.actorOf(
    Smts(client, Options.solver, log = Options.log),
    name = "solver" + Options.solver.name
  )

  toKill += solver
  println("Done")


  println("Starting tests.")
  println
  client ! solver


  trait SolverBenchs extends Actor {
    import ExprStructure._
    import Messages._

    def unexpected(msg: Any) = {
      msg match {
        case SolverError(msgs) => {
          println("> Solver error:")
          msgs.foreach(m => println("  > " + m))
        }
        case m: SmtsMsg => println("Unexpected Smts message: " + m + ".")
        case m => println("Unexpected non-Smts message: " + m + ".")
      }
      println
      killSystem()
    }

    def act: Receive

    def killSystem() = { title("Exiting.") ; println ; actorSystem shutdown () }

    def restart(): Unit = { println("Restarting.") ; solver ! Restart }

    def checksatTest(continuation: () => Unit): Unit = {
      title("Starting CHECKSAT test.")
      val a = Ident("a")
      val notA = Not(a)
      val aNotA = AndN(a :: notA :: Nil)
      println("Setting logic to QF_LIA.")
      solver ! SetLogic(smts.logics.QF_LIA)
      println("Declaring function symbol " + a + ".")
      solver ! DeclareFun((a,Nil,BoolSort) :: Nil)
      println("Asserting " + aNotA + ".")
      solver ! Assert(aNotA)
      println("CheckSat.")
      solver ! CheckSat
      context become {
        case Sat => { println("> Sat.") ; context become act ; continuation() }
        case Unsat => { println("> Unsat.") ; context become act ; continuation() }
        case Unknown => {
          println("> Solver can't decide satisfiability.")
          context become act ; continuation()
        }
        case msg => unexpected(msg)
      }
    }

    def modelTest(continuation: () => Unit): Unit = {
      title("Starting MODEL test.")
      val a = Ident("a")
      val b = Ident("b")
      val notA = Not(a)
      val bNotA = AndN(b :: notA :: Nil)
      println("Setting logic to QF_LIA.")
      solver ! SetLogic(smts.logics.QF_LIA)
      println("Declaring function symbols " + a + " and " + b + ".")
      solver ! DeclareFun((a,Nil,BoolSort) :: (b,Nil,BoolSort) :: Nil)
      println("Asserting " + bNotA + ".")
      solver ! Assert(bNotA)
      println("CheckSat.")
      solver ! CheckSat
      context become {
        case Sat => {
          println("> Sat.")
          println("Getting model.")
          solver ! GetModel
          context become {
            case Model(model) => {
              println("> Model:") ; println("  > " + model)
              context become act ; continuation()
            }
            case msg => unexpected(msg)
          }
        }
        case Unsat => { println("> Unsat.") ; context become act ; continuation() }
        case Unknown => {
          println("> Solver can't decide satisfiability.")
          context become act ; continuation()
        }
        case msg => unexpected(msg)
      }
    }

    def unsatCoreTest(continuation: () => Unit): Unit = Options.solver match {
      case _: CVC4 => continuation()
      case _ => {
        title("Starting UNSAT CORE test.")
        val a = Ident("a")
        val b = Ident("b")
        val label1 = "label1"
        val label2 = "label2"
        val label3 = "label3"
        val notA = Not(a)
        val bExpr = b
        val notAExpr = Not(a)
        val bAndA = AndN(b :: a :: Nil)
        val map = new scala.collection.immutable.HashMap[String,Expr] +
        ((label1, bExpr)) + ((label2, notAExpr)) + ((label3,bAndA))
        def getExpr(label: String) = map.get(label).get
        println("Setting logic to QF_LIA.")
        solver ! SetLogic(smts.logics.QF_LIA)
        println("Declaring function symbols " + a + " and " + b + ".")
        solver ! DeclareFun((a,Nil,BoolSort) :: (b,Nil,BoolSort) :: Nil)
        for ((label,expr) <- map) {
          println("Asserting " + expr + " with label \"" + label + "\".")
          solver ! Assert(expr,Some(Ident(label)))
        }
        println("CheckSat.")
        solver ! CheckSat
        context become {
          case Sat => {
            println("> Sat.")
            context become act ; continuation()
          }
          case Unsat => {
            println("> Unsat.")
            println("GetUnsatCore.")
            solver ! GetUnsatCore
            context become {
              case UnsatCore(core) => {
                println("> Core:")
                core.foreach(
                  label => println("  > [" + label + "] -> " + getExpr(label))
                )
                context become act ; continuation()
              }
              case msg => unexpected(msg)
            }
          }
          case Unknown => {
            println("> Solver can't decide satisfiability.")
            context become act ; continuation()
          }
          case msg => unexpected(msg)
        }
      }
    }

  }

}
