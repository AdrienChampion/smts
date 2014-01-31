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
import sys.process.stringSeqToProcess

import smts._

import smts.test.ExprStructure._

/** Tests Smts' writer and reader. */
object IOTest extends Verboser with OptionHandler with Animator {

  object Options {
    private var _log: Option[String] = None
    def log = _log
    def log_= (newLog: String) = _log = Some(newLog)
    private var _timeout: Option[Int] = None
    def timeout = _timeout
    def timeout_= (newTo: Int) = _timeout = Some(newTo)
    private var _dirs: List[String] = Nil
    def dirs = _dirs
    def dirs_= (newDirs: Traversable[String]) = _dirs = newDirs.toList
  }

  val optionPrint = { s: String => verbln(s) }

  val myOptions = {
    ("-h", { s: String => { printHelp() ; sys exit -1 } }, "     prints this message." :: Nil) ::
    ("--help", { s: String => { printHelp() ; sys exit -1 } }, " also prints this message." :: Nil) ::
    ("--log=", { s: String => Options.log = optionValue(s) }, " activates logging." :: Nil) ::
    ("--timeout=", { s: String => Options.timeout = optionValue(s).toInt }, " sets a timeout for each query to the solver, in seconds." :: Nil) ::
    Nil
  }

  val myArguments = {
    ("--dir=", { s: String => Options.dirs = optionValue(s).split(",") }, "<dir1>,<dir2>,... The directories where the benchmarks are." :: Nil) ::
    Nil
  }

  val animatorLineCount = 6
  val animatorPrint = { s: String => verb(s) }

  // Line 1.
  def printBenchNumber(benchs: Int, total: Int) = printLine(1, {
    benchs.toString :: "/" :: total.toString :: " files benchmarked so far (" ::
    Timer.now.toString :: "ms)." :: Nil
  })

  // Line 2.
  def printGeneralStatus() = printLine(2, {
    " Success: " :: BenchStats.success.toString ::
    ", failed: " :: BenchStats.failed.toString ::
    ", error: " :: BenchStats.error.toString ::
    ", total: " :: BenchStats.total.toString ::
    " || Timeouts: " :: BenchStats.timeout.toString ::
    ", unknown: " :: BenchStats.unknown.toString ::
    ", different: " :: BenchStats.different.toString ::
    "." :: Nil
  })

  // Line 3.
  val globalProgressAnim = new KawaiiAnimation()
  def printGlobalProgress(current: Int, max: Int) = animLine(3,current,max,globalProgressAnim)

  // Line 4.
  def printBenchName(file: String) = printLine(4, {
    "Currently running on file \"" :: file :: "\"." :: Nil
  })

  // Line 5.
  def printFileStatus() = printLine(5, {
    " Success: " :: BenchStats.Current.success.toString ::
    ", failed: " :: BenchStats.Current.failed.toString ::
    ", error: " :: BenchStats.Current.error.toString ::
    ", total: " :: BenchStats.Current.total.toString ::
    " || Timeouts: " :: BenchStats.Current.timeout.toString ::
    ", unknown: " :: BenchStats.Current.unknown.toString ::
    ", different: " :: BenchStats.Current.different.toString ::
    "." :: Nil
  })

  // Line 6.
  val fileProgressAnim = new KawaiiAnimation()
  def printFileProgress(current: Int, max: Int) = animLine(6,current,max,fileProgressAnim)




  setOptions()





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
    def newFile(filePath: String) = _writer = { logging = true ; new BufferedWriter(new FileWriter(filePath)) }
    private var logging = false
    def log(s: String) = if (logging) Logger.writer write s
    def logln(s: String) = if (logging) { log(s) ; log("\n") }
    def logln() = log("\n")
  }
  def log(s: String) = Logger.log(s)
  def logln(s: String) = Logger.logln(s)
  def logln() = Logger.logln()

  object BenchStats {
    def updateFileStatus(lines: Int, lineCount: Int) = {
      printFileStatus()
      printFileProgress(lines,lineCount)
    }

    object Current {
      private var _cSuccess = 0
      def success = _cSuccess
      def successInc = {
        _cSuccess += 1
        _success += 1
      }

      private var _cFailed = 0
      def failed = _cFailed
      def failedInc = {
        _cFailed += 1
        _failed += 1
      }

      private var _cError = 0
      def error = _cError
      def errorInc = {
        _cError += 1
        _error += 1
      }

      private var _cTimeout = 0
      def timeout = _cTimeout
      def timeoutInc = {
        _cTimeout += 1
        _timeout += 1
      }

      private var _cDifferent = 0
      def different = _cDifferent
      def differentInc = {
        _cDifferent += 1
        _different += 1
      }

      private var _cUnknown = 0
      def unknown = _cUnknown
      def unknownInc = {
        _cUnknown += 1
        _unknown += 1
      }
      def reset = { _cSuccess = 0 ; _cDifferent = 0 ; _cFailed = 0 ; _cError = 0 ; _cTimeout = 0 ; _cUnknown = 0}
      def total = success + failed + error
    }

    private var _currentFile = ""
    def currentFile = _currentFile
    def currentFile_= (newFile: String) = _currentFile = newFile

    private var _time: Long = 0
    def start = _time = System.currentTimeMillis
    def stop = _time -= System.currentTimeMillis
    def time = _time

    def updateGlobalStatus = {
      printGlobalProgress(fileCount,totalFileCount)
      printGeneralStatus()
      printBenchNumber(fileCount, totalFileCount)
    }
    private var _fileCount: Int = 0
    def fileCount = _fileCount
    def fileCountIncrement = {
      _fileCount += 1
      updateGlobalStatus
    }
    private var _totalFileCount: Int = 0
    def totalFileCount = _totalFileCount
    def totalFileCount_= (n: Int) = {
      _totalFileCount = n
    }

    private var _success: Int = 0
    def success = _success
    private var _failed: Int = 0
    def failed = _failed
    private var _error: Int = 0
    def error = _error
    private var _different: Int = 0
    def different = _different
    private var _timeout: Int = 0
    def timeout = _timeout
    private var _unknown: Int = 0
    def unknown = _unknown
    def total = success + failed + error
  }

  // Creating solver process.
  val command = "z3 -smt2 -in" + (Options.timeout match {
    case Some(to) => " -t:" + to
    case None => ""
  })

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

  trait SmtsTrait extends SmtsTest with WriterSuccess[Expr,Ident,Sort] with SmtsReader[Expr,Ident,Sort] {
    import Messages.{ToSmtsMsg,SmtsMsg}

    // Writer side.
    protected def solverWriter = Solver.writer
    protected def notifyReader(msg: ToSmtsMsg) = readMsg(msg)

    // Reader side.
    protected def solverReader = Solver.reader
  }

  // Instantiating reader and writer on the structured Smts.
  val smts = if (Options.log.isDefined) new SmtsTrait {
    import Messages._
    def apply(msg: ToSmtsMsg) = handleMsg(msg)
    def checkMsg(msg: SmtsMsg): Unit = msg match {
      case Messages.SolverError(expl) => {
        BenchStats.Current.errorInc
        Logger.writer write ("; Solver error:\n")
        expl foreach (e => Logger.writer write (";   " + e + "\n"))
        Logger.flush
      }
      case Unknown => { BenchStats.Current.unknownInc ; BenchStats.Current.successInc }
      case _ => BenchStats.Current.successInc
    }
    protected def notifyMaster(msg: SmtsMsg) = checkMsg(msg)
    def bw = Logger.writer
    protected def logMsg(msg: Messages.ToSmtsMsg) = writeMsg(msg,bw)
    protected def logResultLine(line: String) = { bw write "; " ; bw write line ; bw write "\n" }

  } else new SmtsTrait {
    import Messages._
    def apply(msg: ToSmtsMsg) = handleMsg(msg)
    def checkMsg(msg: SmtsMsg): Unit = msg match {
      case Messages.SolverError(expl: List[String]) => {
        BenchStats.Current.errorInc
        Logger.writer write ("; Solver error:\n")
        expl foreach (e => Logger.writer write (";   " + e + "\n"))
        Logger.flush
      }
      case _ => BenchStats.Current.successInc
    }
    protected def notifyMaster(msg: SmtsMsg) = checkMsg(msg)
    protected def logMsg(msg: Messages.ToSmtsMsg) = ()
    protected def logResultLine(line: String) = ()
  }

  BenchStats.totalFileCount = (Options.dirs.foldLeft(0)(
    (n,dir) => n + (Seq("bash","-c", "find " + dir + " -iname \"*.smt2\" | wc -l") !!).replaceAll("\\s+","").toInt
  ))

  if (BenchStats.totalFileCount == 0) {
    space
    verbln("No benchmarks found, exiting.")
    space
    sys exit 0
  }

  space
  title0("Testing Smts' writer and reader.")
  Options.log match {
    case Some(file) => {
      Logger.newFile(file)
      verbln("Logging in file " + file + ".")
    }
    case None => ()
  }
  space
  initAnim()

  logln("; Smts' io.")
  logln(";   arguments: " + args.toList)
  logln("; Solver command: " + command)
  logln
  logln
  Logger.flush

  Timer.start
  Options.dirs foreach (arg => {
    exploreAndDo(arg,workOnBench)
  })
  Timer.stop
  Solver.kill

  def workOnBench(filePath: String): Unit = {
    // verbln("Working on file " + filePath)

    printBenchName(filePath)
    BenchStats.updateGlobalStatus
    logln("; Working on file " + filePath)
    smts(smts.Messages.SetOption(":print-success true"))
    val br = new BufferedReader(new FileReader(filePath))

    val lnr = new LineNumberReader(new FileReader(new File(filePath)))
    lnr.skip(Long.MaxValue)
    val lineCount = lnr.getLineNumber()

    @tailrec
    def loop(line: String = "", lines: Int = 0): Unit = {
      BenchStats.updateFileStatus(lines,lineCount)
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
                if (
                  try { smts(msg) ; true } catch {
                    case e: java.io.IOException => {
                      BenchStats.Current.timeoutInc
                      BenchStats.Current.successInc
                      false
                    }
                  }
                ) loop("", lines + 1) else ()
              } else {
                logln("; Parse successful but strings are different. Line:")
                logln(";   " + cleanLine)
                log(sw.toString)
                logln("")
                Logger.flush
                BenchStats.Current.differentInc
                if (
                  try { smts(msg) ; true } catch {
                    case e: java.io.IOException => {
                      BenchStats.Current.timeoutInc
                      BenchStats.Current.successInc
                      false
                    }
                  }
                ) loop("", lines + 1) else ()
              }
            }
            case smts.Fail(msg) => {
              logln("; Parse failed on line:")
              logln(";   " + cleanLine)
              logln("; Error message:")
              logln("; " + msg)
              logln("")
              BenchStats.Current.failedInc
              loop("", lines + 1)
            }
          }
        } else loop(cleanLine, lines + 1)
      } else ()
    }

    smts.clearConsigned
    loop()
    Solver.restart
    BenchStats.Current.reset
    logln
    logln("; Success:   " + BenchStats.Current.success)
    logln("; Failed:    " + BenchStats.Current.failed)
    logln("; Error:     " + BenchStats.Current.error)
    logln("; Total:     " + BenchStats.Current.total)
    logln("; Different: " + BenchStats.Current.different)
    logln("; Unknown:   " + BenchStats.Current.unknown)
    logln("; Timeout: " + BenchStats.Current.timeout)
    logln
    logln
    Logger.flush
    BenchStats.fileCountIncrement
  }

  def exploreAndDo(filePath: String, work: String => Unit): Unit = {
    val file = new File(filePath)
    if (file.isDirectory) {
      val dir = if (filePath endsWith "/") filePath else filePath + "/"
      file.list.foreach(subFile => exploreAndDo(dir + subFile,work))
    }
    else if (filePath endsWith ".smt2") work(filePath)
    else ()
  }

}
