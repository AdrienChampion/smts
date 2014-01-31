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

/** Writes on the solver process input. Forwards all messages to the
  * reader to parse for '''success'''. */
trait SmtsWriter[Expr,Ident,Sort] extends SmtLibPrinters[Expr,Ident,Sort] {
  import Messages.ToSmtsMsg

  /** Yields a '''Writer''' on the solver process. */
  protected def solverWriter: Writer

  /** Logs the command of the message if logging is activated. */
  protected def logMsg(msg: ToSmtsMsg): Unit

  /** Notifies the reader it will have to parse something. */
  protected def notifyReader(msg: ToSmtsMsg): Unit

  /** Writes the command the message corresponds to. */
  protected def handleMsg(msg: ToSmtsMsg): Unit = {
    writeMsg(msg,solverWriter) ; logMsg(msg)
  }
}

/** Writes on the solver process input. Only forwards the query
  * messages (those producing a result). */
trait WriterNoSuccess[Expr,Ident,Sort] extends SmtsWriter[Expr,Ident,Sort] {
  override protected def handleMsg(msg: Messages.ToSmtsMsg) = {
    super.handleMsg(msg)
    msg match {
      case _: Messages.QueryMsg => notifyReader(msg)
      case _ => ()
    }
  }
}

/** Writes on the solver process input. Forwards all messages to the
  * reader to parse for '''success'''. */
trait WriterSuccess[Expr,Ident,Sort] extends SmtsWriter[Expr,Ident,Sort] {
  import Messages.ToSmtsMsg
  override protected def handleMsg(msg: Messages.ToSmtsMsg): Unit = {
    super.handleMsg(msg) ; notifyReader(msg)
  }
}


/** Reads on the solver process output based on the message it
  * receives. */
trait SmtsReader[Expr,Ident,Sort] extends SmtLibParsers[Expr,Ident,Sort] {
  import Messages.{SmtsMsg,ToSmtsMsg,FromSmtsMsg}
  import scala.annotation.tailrec

  /** Yields a '''Reader''' on the solver process. */
  protected def solverReader: BufferedReader

  /** Logs the result of a message if logging is activated. */
  protected def logResultLine(line: String)

  /** Notifies the reader's master on its reading. */
  protected def notifyMaster(msg: SmtsMsg)

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

  /** Reads on the solver reader and parses it. */
  protected def readMsg(msg: ToSmtsMsg) = {
    val line = {
      var temp = solverReader.readLine
      while (temp == "") temp = solverReader.readLine
      temp
    }

    logResultLine(line)

    // Internal loop to get all the lines.
    @tailrec
    def loop(
      text: String = line, parentheses: Int = getParenthesisCount(line)
    ): String = parentheses match {
      case _ if (text startsWith "(error") => text
      case 0 => text
      case n if n >= 0 => {
        val newLine = solverReader.readLine
        logResultLine(newLine)
        loop(text + " " + newLine, parentheses + getParenthesisCount(newLine))
      }
      case _ => throw new NegParCountException(text)
    }

    try {
      val (text,parser) = (loop(),getParser(msg))
      phrase(parser)(new PackratReader(new scala.util.parsing.input.CharSequenceReader(text))) match {
        case Success(smtsMsg,_) => notifyMaster(smtsMsg)
        case NoSuccess(msg,next) => notifyMaster(Messages.SolverError(
          "Error while parsing output of message" :: msg.toString ::
          msg :: next.pos.longString.split("\n").toList
        ))
      }
    } catch {
      case NegParCountException(line) => notifyMaster(Messages.SolverError(
        "Illegal negative count of parentheses triggered by message" ::
        msg.toString :: "Output: " :: line.split("\n").toList
      ))
    }
  }


  /** Exception thrown when the difference between open and closed
    * parentheses is negative. */
  protected case class NegParCountException(lines: String) extends Exception(
    "Illegal negative count of parentheses in lines " + lines + "."
  )
}
