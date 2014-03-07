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

import java.io.{Writer,BufferedWriter,FileWriter}

/** Provides the '''ReaderLog''' trait. */
trait SmtsLog[Expr,Ident,Sort] extends SmtsIO[Expr,Ident,Sort] {

  /** Reader should extend this trait to activate logging. */
  trait ReaderLog extends SmtsReaderSuccess with SmtLibPrinters {
    import Messages.ToSmtsMsg

    /** Path to the logging file. */
    def logFilePath: String
    /** '''BufferedWriter''' on the logging file. */
    val logBW = new BufferedWriter(new FileWriter(logFilePath))

    override protected def logMsg(msg: ToSmtsMsg) = writeMsg(msg,logBW)
    override protected def logResultLine(line: String) = {
      logBW write "; " ; logBW write line ; logBW write "\n" ; logBW.flush
    }
    override protected def logSpace = logBW write "\n"
  }
}
