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

import java.io._

import scala.annotation.tailrec

import smts._

import smts.test.ExprStructure._

/** Tests Smts' writer and reader. */
object IOTest extends Verboser with OptionHandler {

  object Options {
    private var _log = false
    def log = _log
    def activateLog = _log = true
    private var _dirs: List[String] = Nil
    def dirs = _dirs
    def dirs_= (newDirs: Traversable[String]) = _dirs = newDirs.toList
  }

  val optionPrint = { s: String => verbln(s) }

  val myOptions = {
    ("-h", { s: String => { printHelp() ; sys exit -1 } }, "     prints this message." :: Nil) ::
    ("--help", { s: String => { printHelp() ; sys exit -1 } }, " also prints this message." :: Nil) ::
    ("--log", { s: String => Options.activateLog }, " activates logging." :: Nil) ::
    Nil
  }

  val myArguments = {
    ("--dir=", { s: String => { Options.dirs = optionValue(s).split(",") } }, "<dir1>,<dir2>,... The directories where the benchmarks are." :: Nil) ::
    Nil
  }

  object Timer {
    private var _timer: Long = 0
    def time = _timer
    def now = System.currentTimeMillis - _timer
    def start = _timer = System.currentTimeMillis
    def stop = _timer = System.currentTimeMillis - _timer
  }

  object Logger {
    private var _writer: BufferedWriter = null
    def writer = _writer
    def flush = _writer.flush
    def newFile(filePath: String) = _writer = new BufferedWriter(new FileWriter(filePath))
  }
  def log(s: String) = Logger.writer write s
  def logln(s: String) = { log(s) ; log("\n") }
  def logln() = log("\n")

  object BenchStats {
    object Current {
      private var _cSuccess = 0
      def successInc = {
        _cSuccess += 1
        _success += 1
      }
      private var _cDifferent = 0
      def differentInc = {
        _cDifferent += 1
        _different += 1
      }
      private var _cFailed = 0
      def failedInc = {
        _cFailed += 1
        _failed += 1
      }
      private var _cError = 0
      def errorInc = {
        _cError += 1
        _error += 1
      }
      def reset = { _cSuccess = 0 ; _cDifferent = 0 ; _cFailed = 0 }
    }

    private var _currentFile = ""
    def currentFile = _currentFile
    def currentFile_= (newFile: String) = _currentFile = newFile

    private var _time: Long = 0
    def start = _time = System.currentTimeMillis
    def stop = _time -= System.currentTimeMillis
    def time = _time

    // def updateGlobalStatus = {
    //   printGlobalProgress({ max => max * fileCount / totalFileCount })
    //   printGeneralStatus(success,different,failed,total)
    //   printBenchNumber(fileCount, totalFileCount)
    // }
    private var _fileCount: Int = 0
    def fileCount = _fileCount
    def fileCountIncrement = {
      _fileCount += 1
      // updateGlobalStatus
    }
    private var _totalFileCount: Int = 0
    def totalFileCount = _totalFileCount
    def totalFileCount_= (n: Int) = {
      _totalFileCount = n
    }
    // def update(s: Int, d: Int, f: Int) = {
    //   _success += s ; _different += d ; _failed += f
      // updateGlobalStatus
    // }
    private var _success: Int = 0
    def success = _success
    private var _different: Int = 0
    def different = _different
    private var _failed: Int = 0
    def failed = _failed
    private var _error: Int = 0
    def error = _error
    def total = success + different + failed + error

    def printStatus = println ("Success: " + success + ", different: " + different + ", failed: " + failed + ". Total: " + total + ".")
  }

  // Creating solver process.
  val command = "z3 -smt2 -in"

  object Solver {
    private var solver = (new ProcessBuilder(command.split(" "): _*)).redirectErrorStream(true).start
    private var _writer = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream))
    def writer = _writer
    def write = _writer
    private var _reader = new BufferedReader(new InputStreamReader(solver.getInputStream),10000000)
    def reader = _reader
    def kill = solver.destroy
    def restart = {
      solver.destroy
      solver = (new ProcessBuilder(command.split(" "): _*)).redirectErrorStream(true).start
      _writer = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream))
      _reader = new BufferedReader(new InputStreamReader(solver.getInputStream),10000000)
    }
  }

  trait SmtsTrait extends SmtsTest with WriterNoSuccess[Expr,Ident,Sort] with SmtsReader[Expr,Ident,Sort] {
    import Messages.{ToSmtsMsg,SmtsMsg}

    // Writer side.
    protected def solverWriter = Solver.writer
    protected def notifyReader(msg: ToSmtsMsg) = readMsg(msg)

    // Reader side.
    protected def solverReader = Solver.reader
  }

  // Instantiating reader and writer on the structured Smts.
  val smts = if (Options.log) new SmtsTrait {
    import Messages._
    def apply(msg: ToSmtsMsg) = writeMsg(msg)
    def checkMsg(msg: SmtsMsg): Unit = {
      println("emy elle est trop jolie")
      msg match {
        case Messages.SolverError(expl: List[String]) => {
          BenchStats.Current.errorInc
          Logger.writer write ("; Solver error:\n")
          expl foreach (e => Logger.writer write (";   " + e + "\n"))
        }
        case _ => {
          BenchStats.Current.successInc
          Logger.writer write ("; Success:\n")
          Logger.writer write ("; ") ; Logger.writer write msg.toString
        }
      }
    }
    protected def notifyMaster(msg: SmtsMsg) = { println("notify master") ; checkMsg(msg) }
    def bw = Logger.writer
    protected def logMsg(msg: Messages.ToSmtsMsg) = if (Options.log) writeMsg(msg,bw)
    protected def logResultLine(line: String) = if (Options.log) { bw write ";" ; bw write line }
  } else new SmtsTrait {
    import Messages._
    def apply(msg: ToSmtsMsg) = writeMsg(msg)
    def checkMsg(msg: SmtsMsg): Unit = {
      println("emy elle est trop jolie")
      msg match {
      case Messages.SolverError(expl: List[String]) => {
        BenchStats.Current.errorInc
        Logger.writer write ("; Solver error:\n")
        expl foreach (e => Logger.writer write (";   " + e + "\n"))
        Logger.flush
      }
      case _ => {
        BenchStats.Current.successInc
        Logger.writer write ("; Success:\n")
        Logger.writer write ("; ") ; Logger.writer write msg.toString
        Logger.flush
      }
    }
    }
    protected def notifyMaster(msg: SmtsMsg) = checkMsg(msg)
    protected def logMsg(msg: Messages.ToSmtsMsg) = ()
    protected def logResultLine(line: String) = ()
  }
  if (smts == null) verbln("Null smts on init.")

  setOptions()

  space
  title0("Testing Smts' writer and reader.")
  Logger.newFile("ioTest.log")
  verbln("Logging in file ioTest.log.")
  space

  logln("Smts' io.")
  logln("  arguments: " + args.toList)
  logln
  logln
  Logger.flush

  Timer.start
  verbln("Directory cardinality: " + Options.dirs.size)
  Options.dirs foreach (arg => {
    exploreAndDo(arg,workOnBench)
  })
  Timer.stop
  verbln("Killing solver.")
  Solver.kill

  def workOnBench(filePath: String): Unit = {
    verbln("Working on file " + filePath)

    // printBenchName(filePath)
    // BenchStats.updateGlobalStatus
    logln("Working on file " + filePath)
    val br = new BufferedReader(new FileReader(filePath))

    val lnr = new LineNumberReader(new FileReader(new File(filePath)))
    lnr.skip(Long.MaxValue)
    val lineCount = lnr.getLineNumber()

    @tailrec
    def loop(line: String = "", success: Int = 0, different: Int = 0, failed: Int = 0, lines: Int = 0): (Int,Int,Int) = {
      // printFileStatus(success, different, failed, success + different + failed)
      // printFileProgress(lines,lineCount)
      val nuLine = br.readLine
      if (nuLine != null) {
        val cleanLine = if (line == "") nuLine.trim else line + " " + nuLine.trim
        val op = cleanLine count (c => c == '(')
        val cp = cleanLine count (c => c == ')')
        if (op == cp) {
          smts parseCommand cleanLine match {
            case smts.Succ(msg) => {
              val sw = new StringWriter()
              smts.writeMsg(msg,sw)
              if ((cleanLine + "\n").replaceAll("\\s+","") == sw.toString.replaceAll("\\s+","")) {
                logln("Parsed command")
                log("  " + sw.toString)
                logln("from line:")
                logln("  " + cleanLine)
                Logger.flush
                smts(msg)
                loop("", lines + 1)
              } else {
                logln("Parse successful but strings are different. Original:")
                logln("  " + cleanLine)
                logln("Printed version:")
                log("  " + sw.toString)
                Logger.flush
                BenchStats.Current.differentInc
                loop("", lines + 1)
              }
            }
            case smts.Fail(msg) => {
              logln("Parse failed on line:")
              logln("  " + cleanLine)
              logln("Error message:")
              logln(msg)
              BenchStats.Current.failedInc
              loop("", lines + 1)
            }
          }
        } else loop(cleanLine, lines + 1)
      } else (success,different,failed)
    }

    if (smts == null) verbln("Null smts")
    smts.clearConsigned
    val result = loop()
    verbln("Restarting solver.")
    Solver.restart
    BenchStats.Current.reset
    verbln()
    val sum = result._1 + result._2 + result._3
    logln("Success:   " + result._1)
    logln("Different: " + result._2)
    logln("Failed:    " + result._3)
    logln("Total:     " + sum)
    logln
    Logger.flush
    BenchStats.fileCountIncrement
    BenchStats.printStatus
  }

  def exploreAndDo(filePath: String, work: String => Unit): Unit = {
    verbln("Directory (or file): " + filePath + ".")
    val file = new File(filePath)
    if (file.isDirectory) {
      val dir = if (filePath endsWith "/") filePath else filePath + "/"
      file.list.foreach(subFile => exploreAndDo(dir + subFile,work))
    }
    else if (filePath endsWith ".smt2") work(filePath)
    else ()
  }

}
