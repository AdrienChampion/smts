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

import scala.collection.Traversable
import scala.collection.immutable.HashMap

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
    private var _solver: SolverInfo = Z3(models = true, unsatCores = true)
    def solver = _solver
    def createSolverInfo(s: String) =
      _solver = SolverInfo(s, models = Some(true), unsatCores = Some(true))
  }

  val optionPrint = { s: String => println(s) }
  val myArguments = Nil
  val myOptions = {
    (
      "--solver=", { s: String => Options.createSolverInfo(optionValue(s)) },
      "<string>: the solver to use. z3, mathsat5 or cvc4 (default z3)." :: Nil
    ) :: (
      "--log=", { s: String => Options.log = optionValue(s) },
      "<file>: a file to log the smt lib 2 queries to." :: Nil
    ) :: (
      "--conf=", { s: String => SolverInfo load optionValue(s) },
      "<file>: an smts configuration file to load." :: Nil
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
        checksatBool(
          () => { restart() ; println ; modelBool(
            () => { restart() ; println ; unsatCoreBool(
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
    import Logics.{Logic,QF_LIA}

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

    def checksatBool(continuation: () => Unit): Unit = {
      val a = Ident("a")
      val notA = Not(a)
      val aNotA = AndN(a :: notA :: Nil)
      title("Starting CHECKSAT test on Booleans.")
      checksatTest(
        QF_LIA, (a,Nil,BoolSort) :: Nil,
        (aNotA,None) :: Nil, msg => continuation()
      )
    }

    def modelBool(continuation: () => Unit): Unit = {
      val a = Ident("a")
      val b = Ident("b")
      val notA = Not(a)
      val bNotA = AndN(b :: notA :: Nil)
      title("Starting MODEL test on Booleans.")
      modelTest(
        QF_LIA,
        (a,Nil,BoolSort) :: (b,Nil,BoolSort) :: Nil,
        (notA,None) :: (bNotA,None) :: Nil,
        msg => continuation()
      )
    }

    def unsatCoreBool(continuation: () => Unit): Unit = Options.solver match {
      case _: CVC4 => continuation()
      case _ => {
        title("Starting UNSAT CORE test on Booleans.")
        val a = Ident("a")
        val b = Ident("b")
        val label1 = "label1"
        val label2 = "label2"
        val label3 = "label3"
        val notA = Not(a)
        val bExpr = b
        val notAExpr = Not(a)
        val bAndA = AndN(b :: a :: Nil)
        val map = new HashMap[String,Expr] +
          ((label1, bExpr)) + ((label2, notAExpr)) + ((label3,bAndA))
        val list: List[(Expr,Option[String])] =
          map.toList map (pair => (pair._2,Some(pair._1)))
        unsatCoreTest(
          QF_LIA,
          (a,Nil,BoolSort) :: (b,Nil,BoolSort) :: Nil,
          list, map, msg => continuation()
        )
      }
    }

    def checksatTest(
      logic: Logic,
      funs: Traversable[(Ident,Traversable[Sort],Sort)],
      exprs: Traversable[(Expr,Option[String])],
      continuation: FromSmtsMsg => Unit
    ): Unit = {
      println("Setting logic to " + logic + ".")
      solver ! SetLogic(logic)

      println("Declaring function symbol(s)")
      funs foreach (fun => {
        println("> " + fun)
        solver ! DeclareFun(fun :: Nil)
      })

      println("Asserting")
      exprs foreach (expr => {
        println(
          "> " + expr._1 + (if (expr._2.isDefined) " \033[1m(" + expr._2.get + ")\033[0m")
        )
        solver ! Assert(expr._1,expr._2)
      })

      println("Checksat")
      solver ! CheckSat

      context become {
        case Sat => { println("> Sat.") ; context become act ; continuation(Sat) }
        case Unsat => { println("> Unsat.") ; context become act ; continuation(Unsat) }
        case Unknown => {
          println("> Solver can't decide satisfiability.")
          context become act ; continuation(Unknown)
        }
        case msg => unexpected(msg)
      }
    }


    def modelTest(
      logic: Logic,
      funs: Traversable[(Ident,Traversable[Sort],Sort)],
      exprs: Traversable[(Expr,Option[String])],
      continuation: FromSmtsMsg => Unit
    ): Unit = {
      checksatTest(logic,funs,exprs, msg => msg match {
        case Sat => {

          println("Getting model.")
          solver ! GetModel
          context become {
            case message@Model(model) => {
              println("> Model:")
              model foreach (binding => println(" > " + binding))
              continuation(message)
            }
            case msg => unexpected(msg)
          }

        }
        case Unsat => {
          println("> Unexpected unsat result, moving on.")
          continuation(Unsat)
        }
        case msg => {
          println("> Moving on.")
          continuation(msg)
        }
      })
    }


    def unsatCoreTest(
      logic: Logic,
      funs: Traversable[(Ident,Traversable[Sort],Sort)],
      exprs: Traversable[(Expr,Option[String])],
      map: HashMap[String,Expr],
      continuation: FromSmtsMsg => Unit
    ): Unit = checksatTest(logic,funs,exprs, msg => msg match {
      case Sat => {
        println("> Unexpected sat result, moving on.")
        continuation(Sat)
      }
      case Unsat => {

        println("Getting unsat core.")
        solver ! GetUnsatCore
        context become {
          case message@UnsatCore(core) => {
            println("> Unsat core:")
            core foreach (label => println(
              " > " + label + " -> " + (map get label match {
                case Some(expr) => expr.toString
                case None => "\033[1m<undefined>\033[0m"
              })
            ))
            continuation(message)
          }
          case msg => unexpected(msg)
        }

      }
      case msg => {
        println("> Moving on.")
        continuation(msg)
      }
    })

  }

}
