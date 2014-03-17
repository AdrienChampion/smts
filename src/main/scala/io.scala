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

import java.io._

/** Provides writing and reading traits. */
trait SmtsIO[Expr,Ident,Sort]
extends SmtsSolvers[Expr,Ident,Sort] {

  /** Writes on the solver process input. Forwards all messages to the
    * reader to parse for '''success'''. */
  trait SmtsWriter extends SmtLibPrinters {
    import Messages.ToSmtsMsg

    /** The solver information. */
    protected def solverInfo: SolverInfo

    /** Initializes the solver by sending the start messages. */
    protected def initSolver =
      solverInfo.startMsgs foreach (msg => writeMsg(msg))

    /** Restarts the solver. */
    def restart: Unit
    /** Kills the solver. */
    def killSolver: Unit

    /** Creates a new solver process. */
    protected def createSolverProcess = (new ProcessBuilder(
      solverInfo.command.split(" "): _*
    )).redirectErrorStream(true).start

    /** Yields a '''Writer''' on the solver process. */
    protected def solverWriter: Writer

    /** Notifies the reader it will have to parse something. */
    protected def notifyReader(msg: ToSmtsMsg): Unit
    /** Notifies the reader that the reader on the solver process changed. */
    protected def notifyReader(br: BufferedReader): Unit

    /** Writes the command the message corresponds to. */
    protected def writeMsg(msg: ToSmtsMsg): Unit = try {
      writeMsg(msg,solverWriter) ; notifyReader(msg)
    } catch {
      case e: IOException => {
        // notifyMaster(SolverClosed)
        // println("Triggered by message " + msg + ".")
        throw e
      }
    }
  }

  /** Writer with a single solver process. */
  trait SmtsWriterSimple extends SmtsWriter {
    import Messages.Restart

    /** The solver process. */
    protected var solverProcess = createSolverProcess
    /** Returns a '''BufferedWriter''' on the solver process. */
    protected def getSolverWriter = new BufferedWriter(
      new OutputStreamWriter(solverProcess.getOutputStream)
    )
    /** Returns a '''BufferedReader''' on the solver process. */
    protected def getSolverReader = new BufferedReader(
      new InputStreamReader(solverProcess.getInputStream)
    )
    /** Actual writer on the solver process input. */
    protected var _solverWriter = getSolverWriter
    protected def solverWriter = _solverWriter

    def restart = {
      killSolver
      solverProcess = createSolverProcess
      _solverWriter = getSolverWriter
      notifyReader(getSolverReader)
      initSolver
    }
    def killSolver = solverProcess.destroy
  }

  // /** Writes on the solver process input. Only forwards the query
  //   * messages (those producing a result). */
  // trait WriterNoSuccess[Expr,Ident,Sort] extends SmtsWriter[Expr,Ident,Sort] {
  //   override protected def writeAndLog(msg: Messages.ToSmtsMsg) = {
  //     super.writeAndLog(msg)
  //     msg match {
  //       case _: Messages.QueryMsg => notifyReader(msg)
  //       case _ => ()
  //     }
  //   }
  // }

  // /** Writes on the solver process input. Forwards all messages to the
  //   * reader to parse for '''success'''. */
  // trait WriterSuccess[Expr,Ident,Sort] extends SmtsWriter[Expr,Ident,Sort] {
  //   import Messages.ToSmtsMsg
  //   override protected def writeAndLog(msg: Messages.ToSmtsMsg): Unit = {
  //     super.writeAndLog(msg) ; notifyReader(msg)
  //   }
  // }


  /** Reads on the solver process output based on the message it
    * receives. Parses success. */
  trait SmtsReaderSuccess extends SmtLibParsers {
    import Messages.{SmtsMsg,ToSmtsMsg,FromSmtsMsg}
    import scala.annotation.tailrec

    /** The solver information. */
    protected def solverInfo: SolverInfo

    /** Yields a '''Reader''' on the solver process. */
    protected var solverReader: BufferedReader

    /** Logs a command corresponding to a message if logging is activated. */
    protected def logMsg(msg: ToSmtsMsg) = ()
    /** Logs the result of a message if logging is activated. */
    protected def logResultLine(line: String) = ()
    /** Logs the result of a message if logging is activated. */
    protected def logSpace = ()

    /** Notifies the reader's master on its reading. */
    protected def notifyMaster(msg: FromSmtsMsg)

    /** Returns the number of open parentheses minus the number of
      * closed parentheses of a string. */
    protected def getParenthesisCount(line: String) = {
      val (open,closed) = line.foldLeft((0,0))(
        (pair,c) =>
        if (c == '(') (pair._1 + 1,pair._2)
        else if (c == ')') (pair._1,pair._2 + 1)
        else pair
      )
      open - closed
    }

    /** Updates the solver reader on restarts. */
    protected def restart(newSolverReader: BufferedReader) = {
      logSpace ; logSpace
      logResultLine("|=======| RESTART |=======|")
      logSpace
      solverReader = newSolverReader
    }

    /** Reads on the solver reader and parses it. */
    protected def readMsg(msg: ToSmtsMsg) = {

      val line = {
        var temp = solverReader.readLine
        while (temp == "") temp = solverReader.readLine
        temp
      }
      logResultLine(line)
      // printReader("Line: " + line)

      // Internal loop to get all the lines.
      @tailrec
      def loop(
        text: String = line, parentheses: Int = getParenthesisCount(line)
      ): String = parentheses match {
        case _ if (text startsWith "(error") => text
        case 0 => text
        case n if n >= 0 => {
          val newLine = solverReader.readLine
          // printReader("Line: " + newLine)
          logResultLine(newLine)
          loop(text + " " + newLine, parentheses + getParenthesisCount(newLine))
        }
        case _ => throw new NegParCountException(text)
      }

      try {
        val text = msg match {
          case Messages.KillSolver if line == null =>
            { logResultLine("success") ; "success" }
          case _ => loop()
        }
        // printReader("Logging result line.")
        logSpace
        phrase(solverInfo.getParser(msg))(
          new PackratReader(new scala.util.parsing.input.CharSequenceReader(text))
        ) match {
          case Success(smtsMsg,_) => notifyMaster(smtsMsg)
          case NoSuccess(msg,next) => notifyMaster(Messages.SolverError(
            "Error while parsing output of message:" :: msg.toString ::
              next.pos.longString.split("\n").toList
          ))
        }
      } catch {
        case NegParCountException(line) => notifyMaster(Messages.SolverError(
          "Illegal negative count of parentheses triggered by message" ::
            msg.toString :: "Output: " :: line.split("\n").toList
        ))
      }
    }
  }

  /** Reads on the solver process output based on the message it
    * receives. Does not parse success. */
  trait SmtsReaderNoSuccess extends SmtsReaderSuccess {
    import Messages.{ToSmtsMsg,QueryMsg}
    override protected def readMsg(msg: ToSmtsMsg) = {
      msg match {
        case _: QueryMsg => {
          // printReader("Query, reading answer.")
          super.readMsg(msg)
        }
        case _ => {
          // printReader("Not a query, doing nothing.")
          ()
        }
      }
    }
  }

  /** Exception thrown when the difference between open and closed
    * parentheses is negative. */
  protected case class NegParCountException(lines: String) extends Exception(
    "Illegal negative count of parentheses in lines " + lines + "."
  )

}
