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

package smts

import java.io.{Writer,BufferedReader}

import akka.actor._

/** Provides basic actor traits. */
trait SmtsActors[Expr,Ident,Sort]
extends SmtsIO[Expr,Ident,Sort]
with SmtsLog[Expr,Ident,Sort] {

  /** Actor the user interacts with, also writes on the solver process input. */
  trait MasterActor extends Actor with SmtsWriter {
    import Messages.{ToSmtsMsg,FromSmtsMsg,Restart,KillSolver}

    /** '''ActorRef''' to the client of this actor. */
    protected val client: ActorRef
    /** The '''ReaderActor''' this actor interacts with. */
    protected val reader: ActorRef

    protected def notifyReader(msg: ToSmtsMsg) = reader ! msg
    protected def notifyReader(br: BufferedReader) = reader ! br

    /** Function handling the messages from the user and the reader. */
    protected def handleMessage(msg: Any) = msg match {
      case Restart => restart
      case KillSolver => { killSolver ; context stop self }
      case msg: ToSmtsMsg => writeMsg(msg)
      case msg: FromSmtsMsg => client ! msg
      case msg => throw new UnexpectedMessageException(msg)
    }

    override def postStop = { context stop reader ; killSolver }

  }

  /** Actor hidden from the user reading and parsing the solver output. */
  trait ReaderActor extends Actor with SmtsReaderSuccess {
    import Messages.{SmtsMsg,ToSmtsMsg}

    /** The '''MasterActor''' this actor interacts with. */
    protected val master: ActorRef

    def notifyMaster(msg: SmtsMsg) = master ! msg

    def handleMessage(msg: Any) = msg match {
      case br: BufferedReader => solverReader = br
      case msg: ToSmtsMsg => readMsg(msg)
      case msg => throw new UnexpectedMessageException(msg)
    }

  }

  /** Exception thrown when an illegal message is received by the master or reader. */
  class UnexpectedMessageException(val msg: Any) extends Exception(
    "Unexpected message received by Smts: \"" + msg + "\"."
  )

}
