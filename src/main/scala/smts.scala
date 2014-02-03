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

  /** Factory object for Smts. */
  object Smts {
    import akka.actor._

    /** The number of instances created so far. */
    private var _instanceCount = 0
    /** The number of instances created so far. */
    def instanceCount = _instanceCount

    /** Creates a new Smts instance.
      * @param client The actor this solver will interact with.
      * @param solverInfo The solver information and configuration.
      * @param freeRestars If true two solver processes will be used to provide
      * seemingly free restarts by swapping solvers (default '''false''').
      * @param log Allows to specify a file path for logging (default '''None'''). */
    def apply(
      client: ActorRef, solverInfo: SolverInfo,
      freeRestarts: Boolean = false, log: Option[String] = None
    ) = Props(
      new Master(client,solverInfo,log)
    )
  }

  /** Master with only one solver process.
    * @param log Allows to specify a file path for logging. */
  class Master private[smts](
    protected[this] val client: ActorRef,
    protected[this] val solverInfo: SolverInfo,
    val log: Option[String]
  ) extends MasterActor with SmtsWriterSimple {

    protected[this] val reader = context.actorOf(Props(
      (solverInfo.success,log) match {
        case (true,None) => new ReaderSuccess(self,getSolverReader)
        case (false,None) => new ReaderNoSuccess(self,getSolverReader)
        case (true,Some(path)) =>
          new ReaderSuccess(self,getSolverReader) with ReaderLog {
            def logFilePath = path
          }
        case (false,Some(path)) =>
          new ReaderNoSuccess(self,getSolverReader) with ReaderLog {
            def logFilePath = path
          }
      }
    ), name = "reader")

    override def preStart = initSolver

    def receive = {
      case msg => handleMessage(msg)
    }
  }

  /** Reader with success parsing. */
  class ReaderSuccess private[smts](
    protected[this] val master: ActorRef,
    protected[this] var solverReader: BufferedReader
  ) extends ReaderActor with SmtsReaderSuccess {
    def receive = {
      case msg => handleMessage(msg)
    }
  }

  /** Reader without success parsing. */
  class ReaderNoSuccess private[smts](
    /** The '''MasterActor''' this actor interacts with. */
    _master: ActorRef,
    /** Actual reader on the solver process output. */
    _solverReader: BufferedReader
  ) extends ReaderSuccess(_master,_solverReader) with SmtsReaderNoSuccess

}
