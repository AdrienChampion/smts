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
] with SmtParsers with App {

  import ExprStructure._

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

  println("Creating client for testing smts.")
  val client = actorSystem.actorOf(Props(new SolverBenchs {
    def act = {
      case "start" => println("Starting.")
      case solver: ActorRef => {
        println ; println("Starting the first simple test.")
        simpleTest1(solver)
        // killSystem
      }
    }
    def receive = act
    def stop = { println("Stopping.") ; context stop self }
  }), name = "client")
  println("Done.")

  println("Creating a smts instance with z3 as the solver.")
  val solver = actorSystem.actorOf(
    Smts(client, Z3(), log = Some("toto.log")), name = "solver"
  )
  println("Done.")

  println("Sending start message to client.")
  client ! "start"
  println("Done.")

  println("Sending solver to client.")
  client ! solver
  println("Done.")

  // println("Sleeping for two seconds.")
  // Thread.sleep(2000)
  // println("Done.")

  // println("Killing everything.")
  // toKill foreach (actor => actorSystem stop actor)
  // actorSystem shutdown ()
  // println("Done.")

  // println("Exiting.")
  // println


  trait SolverBenchs extends Actor {
    import ExprStructure._
    import Messages._

    def act: Receive

    def killSystem = actorSystem shutdown ()

    def simpleTest1(solver: ActorRef) = {
      val a = Ident("a")
      val notA = Not(a)
      val aNotA = AndN(a :: notA :: Nil)
      println("Declaring function symbol " + a + ".")
      solver ! DeclareFun((a,Nil,BoolSort) :: Nil)
      println("Asserting " + aNotA + ".")
      solver ! Assert(aNotA)
      println("CheckSat.")
      solver ! CheckSat
      context become {
        case Sat => { println("Sat.") ; context become act }
        case Unsat => { println("Unsat.") ; context become act }
        case Unknown => {
          println("Solver can't decide satisfiability.") ; context become act
        }
        case SolverError(msgList) => {
          println("SolverError:")
          msgList foreach (msg => println("  " + msg))
          context become act
        }
      }
    }

  }

}
