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

import java.io.{BufferedReader,BufferedWriter}

import akka.actor._

/** Trait to extends to make an Smts Factor. */
trait SmtsFactory[Expr,Ident,Sort]
extends SmtsActors[Expr,Ident,Sort] {

  /** Factory object for Smts.
    * */
  // object SmtsActor {
  //   import akka.actor._

  //   private var instanceCount = 0

  //   def apply(
  //     master: ActorRef, solverInfo: SolverInfo, freeRestarts: Boolean = false
  //   ) = 
  // }

  /** Master with only one solver process. */
  class Master(
    protected[this] val client: ActorRef,
    protected[this] val solverInfo: SolverInfo
  ) extends MasterActor with SmtsWriterSimple {

    protected[this] val reader =
      if (solverInfo.success) context.actorOf(Props(
        new ReaderSuccess(self,getSolverReader)
      ), name = "reader") else context.actorOf(Props(
        new ReaderNoSuccess(self,getSolverReader)
      ), name = "reader")

    def receive = {
      case msg => handleMessage(msg)
    }
  }

  /** Reader with success parsing. */
  class ReaderSuccess(
    protected[this] val master: ActorRef,
    protected[this] var solverReader: BufferedReader
  ) extends ReaderActor with SmtsReaderSuccess {
    def receive = {
      case msg => handleMessage(msg)
    }
  }

  /** Reader without success parsing. */
  class ReaderNoSuccess(
    /** The '''MasterActor''' this actor interacts with. */
    _master: ActorRef,
    /** Actual reader on the solver process output. */
    _solverReader: BufferedReader
  ) extends ReaderSuccess(_master,_solverReader) with SmtsReaderNoSuccess

}
