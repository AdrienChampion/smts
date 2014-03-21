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

import akka.actor._

import smts._
import ExprStructure._
import com.typesafe.config.ConfigFactory

object DRealTest extends SmtsFactory[Expr,Ident,Sort]
with SmtParsers with App with smts.utils.OptionHandler {

  import Messages._

  // An optional log location.
  private var _log: Option[String] = None
  def log = _log
  def log_= (newLog: String) = _log = Some(newLog)
  val optionPrint = { s: String => println(s) }
  val myArguments = Nil
  val myOptions = {
    (
      "--log=", { s: String => log = optionValue(s) },
      "<file>: a file to log the smt lib 2 queries to." :: Nil
    ) :: Nil
  }

  setOptions()


  def space = println
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
  val actorSystem = ActorSystem("Smts",ConfigFactory.load(customConf))

  space
  title("Initialization.")
  space
  println("Creating client for testing smts... ")
  space
  title("Testing.")
  space
  val client = actorSystem.actorOf(Props(new Actor {
    import Messages._
    val dreal = context.actorOf(Smts(self,DReal(
      initWith = SetLogic(Logics.QF_NRA) :: Nil
    ),log=log), name = "dreal")
    def send(msg: ToSmtsMsg) = dreal ! msg

    val x = Ident("x")
    val y = Ident("y")
    val pow = Ident("pow")
    val cos = Ident("cos")
    val sin = Ident("sin")
    val ddt = Ident("d/dt")

    def test1(continuation: () => Unit) = {
      val expr = Gt(x,RatConst(1))
      println("Sending a script to")
      println("> Declare x.") 
      println("> Assert " + expr + ".")
      println("> Checksat.")
      println("> Exit.")
      send(Script(
        DeclareFun((x,Nil,RealSort) :: Nil) ::
        Assert(expr) ::
        CheckSat ::
        ExitSolver :: Nil
      ))
      context become {
        case msg => handleMsg(msg, m => { space ; continuation() })
      }
    }

    def test2(continuation: () => Unit) = {
      println("Declaring x and y.")
      send(DeclareFun((x,Nil,RealSort) :: (y,Nil,RealSort) :: Nil))
      val expr1 = Eq(
        Plus(FunApp(pow,x :: IntConst(2) :: Nil), FunApp(pow,y :: IntConst(2) :: Nil)) :: RatConst(1) :: Nil
      )
      println("Asserting")
      println("  " + expr1)
      send(Assert(expr1))
      val expr2 = Eq(
        Plus(FunApp(cos, x :: Nil), FunApp(sin, y :: Nil)) :: RatConst(1) :: Nil
      )
      println("Asserting")
      println("  " + expr2)
      send(Assert(expr2))
      println("Checksat.")
      send(CheckSat)
      println("Exit.")
      send(ExitSolver)
      context become {
        case msg => handleMsg(msg, m => { space ; continuation() })
      }
    }

    def test3(continuation: () => Unit) = {
      println("Declaring x and y.")
      send(DeclareFun((x,Nil,RealSort) :: (y,Nil,RealSort) :: Nil))
      val ode = Eq(
        Ddt(x) :: FunApp(pow,y :: IntConst(2) :: Nil) :: Nil
      )
      val odeId = Ident("myOde")
      println("Defining ode " + odeId + ": " + ode)
      send(DefineOde(odeId,ode))
      val expr1 = Eq(
        Plus(FunApp(pow,x :: IntConst(2) :: Nil), FunApp(pow,y :: IntConst(2) :: Nil)) :: RatConst(1) :: Nil
      )
      println("Asserting")
      println("  " + expr1)
      send(Assert(expr1))
      val expr2 = Eq(
        Plus(FunApp(cos, x :: Nil), FunApp(sin, y :: Nil)) :: RatConst(1) :: Nil
      )
      println("Asserting")
      println("  " + expr2)
      send(Assert(expr2))
      println("Checksat.")
      send(CheckSat)
      println("Exit.")
      send(ExitSolver)
      context become {
        case msg => handleMsg(msg, m => { space ; continuation() })
      }
    }

    override def preStart = test1(() => test2(() => test3(() => stop)))

    def handleMsg(msg: Any, continuation: FromSmtsMsg => Unit) = msg match {
      case Sat => { println("Sat.") ; continuation(Sat) }
      case Unsat => { println("Unsat.") ; continuation(Unsat) }
      case Unknown => { println("Unknown.") ; continuation(Unknown) }
      case _ => { println("Unexpected message " + msg) ; stop }
    }

    def receive = {
      case msg => handleMsg(msg,m => ())
    }

    def stop = { println("Stopping.") ; actorSystem.shutdown }
  }), name = "client")


}
