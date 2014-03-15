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

import scala.annotation.tailrec
import sys.process.stringSeqToProcess

import smts.utils._

/** Provides a trait for benchmarking structures and solvers. */
package object bench {

  /** Used for the users to benchmark their expression structure, and the solvers. */
  trait SmtsBenchTrait[Expr,Ident,Sort]
  extends smts.SmtsIO[Expr,Ident,Sort] with SmtsLog[Expr,Ident,Sort] {

    /** This function will be called at the end of each benchmark
      * and is intended to clean hash consigns. */
    def clearConsigned = ()

    /** Launches the benchmarks with function '''run'''. */
    trait SmtsBench extends App with NiceBench {

      /** Launches the benchmarks. */
      def run() = {

        // Setting the options of the run.
        setOptions()

        // Getting the total file count.
        BenchStats.totalFileCount = (Options.dirs.foldLeft(0)(
          (n,dir) => n + (Seq(
            "bash","-c", "find " + dir + " -iname \"*.smt2\" | wc -l"
          ) !!).replaceAll("\\s+","").toInt
        ))

        if (BenchStats.totalFileCount == 0) {
          space
          verbln("No benchmarks found, exiting.")
          space
          sys exit 0
        }

        space
        title0("Benchmarking.")
        Options.log match {
          case Some(file) => {
            Logger.newFile(file)
            verbln("Logging in file " + file + ".")
          }
          case None => ()
        }
        space
        verbln("Creating Smts object.")

        trait BenchSmts extends SmtsWriterSimple
        with SmtsReaderSuccess with SmtLibCommandParsers {
          import Messages.{
            SolverError,ToSmtsMsg,FromSmtsMsg,Unknown,Timeout,SolverClosed
          }

          /** Potential results of parsing a command (see '''parseCommand'''). */
          sealed trait ParseResult
          /** Parsing was successful. */
          case class Succ(val msg: ToSmtsMsg)
          /** Parsing failed. */
          case class Fail(val msg: String)

          protected def solverInfo = Options.solver match {
            case "z3" =>
              Z3(success = true, timeoutQuery = Options.timeout)
            case "cvc4" =>
              CVC4(success = true, timeoutQuery = Options.timeout)
            case "mathsat" =>
              MathSat5(success = true, timeoutQuery = Options.timeout)
            case s => {
              verbln("Unexpected solver " + s + ".") ; space ; sys exit -1
            }
          }
          def command = solverInfo.command

          protected def notifyReader(msg: ToSmtsMsg) = readMsg(msg)
          protected def notifyReader(br: BufferedReader) = solverReader = br

          protected var solverReader: BufferedReader = getSolverReader

          protected def notifyMaster(msg: FromSmtsMsg) = msg match {
            case SolverError(expl) => {
              BenchStats.Current.errorInc
              logln("; Solver error:")
              expl foreach (e => logln(";     " + e))
              logFlush()
            }
            case Unknown => {
              BenchStats.Current.unknownInc ; BenchStats.Current.successInc
            }
            case Timeout => {
              BenchStats.Current.timeoutInc ; BenchStats.Current.successInc
            }
            case _ => BenchStats.Current.successInc
          }

          /** Parses a command from the input string. */
          def parseCommand(s: String) = phrase(commandParser)(
            new PackratReader(new scala.util.parsing.input.CharSequenceReader(s))
          ) match {
            case Success(result,next) => Succ(result)
            case Failure(msg,next) => Fail(msg + "\n" + next.pos.longString)
            case Error(msg,next) => Fail(msg + "\n" + next.pos.longString)
          }

          def apply(msg: ToSmtsMsg) = writeMsg(msg)
        }

        val benchSmts = Options.log match {
          case None => new BenchSmts {}
          case Some(file) => new BenchSmts {
            override protected def logMsg(msg: Messages.ToSmtsMsg) =
              writeMsg(msg,Logger.writer)
            override protected def logResultLine(line: String) = {
              log("; ") ; logln(line)
            }
            override protected def logSpace = logln("")

            logln("; Smts bench edition.")
            logln(";   arguments: " + args.toList)
            logln("; Solver command: " + command)
            logn()
            logn()
            logFlush()
            initSolver
          }
        }

        initAnim()

        Timer.start
        Options.dirs foreach (arg => {
          exploreAndDo(arg,workOnBench)
        })
        Timer.stop
        benchSmts.killSolver

        outitAnim()
        logn()
        logln("Done in    " + Timer.time + " ms.")
        logln("Success:   " + BenchStats.success)
        logln("Failed:    " + BenchStats.failed)
        logln("Total:     " + BenchStats.total)
        logln("Timeout:   " + BenchStats.timeout)
        logln("Unknown:   " + BenchStats.unknown)
        logln("Different: " + BenchStats.different)
        logFlush()
        space
        sys exit 0

        def workOnBench(filePath: String): Unit = {

          printBenchName(filePath)
          BenchStats.updateGlobalStatus
          logln("; Working on file " + filePath)
          val br = new BufferedReader(new FileReader(filePath))

          val lnr = new LineNumberReader(new FileReader(new File(filePath)))
          lnr.skip(Long.MaxValue)
          val lineCount = lnr.getLineNumber()

          @tailrec
          def loop(line: String = "", lines: Int = 0): Unit = {
            BenchStats.updateFileStatus(lines,lineCount)
            BenchStats.updateGlobalStatus
            val nuLine = br.readLine
            if (nuLine != null) {
              val cleanLine = if (line == "") nuLine.trim else line + " " + nuLine.trim
              val op = cleanLine count (c => c == '(')
              val cp = cleanLine count (c => c == ')')
              if (op == cp) benchSmts parseCommand cleanLine match {
                case benchSmts.Succ(msg) => {
                  val sw = new StringWriter()
                  benchSmts.writeMsg(msg,sw)
                  if ((cleanLine + "\n").replaceAll("\\s+","") == sw.toString.replaceAll("\\s+","")) {
                    try { benchSmts(msg) ; true } catch {
                      case e: java.io.IOException => {
                        BenchStats.Current.timeoutInc
                        BenchStats.Current.successInc
                        false
                      }
                    }
                    loop("", lines + 1)
                  } else {
                    logln("; Parse successful but strings are different. Line:")
                    logln(";   " + cleanLine)
                    log(sw.toString)
                    logln("")
                    logFlush()
                    BenchStats.Current.differentInc
                    try { benchSmts(msg) ; true } catch {
                      case e: java.io.IOException => {
                        BenchStats.Current.timeoutInc
                        BenchStats.Current.successInc
                        false
                      }
                    }
                    loop("", lines + 1)
                  }
                }
                case benchSmts.Fail(msg) => {
                  logln("; Parse failed on line:")
                  logln(";   " + cleanLine)
                  logln("; Error message:")
                  logln("; " + msg)
                  logn()
                  BenchStats.Current.failedInc
                  loop("", lines + 1)
                }
              } else loop(cleanLine, lines + 1)
            } else ()
          }

          loop()
          benchSmts.restart
          BenchStats.Current.reset
          logn()
          logln("; Success:   " + BenchStats.Current.success)
          logln("; Failed:    " + BenchStats.Current.failed)
          logln("; Error:     " + BenchStats.Current.error)
          logln("; Total:     " + BenchStats.Current.total)
          logln("; Timeout:   " + BenchStats.Current.timeout)
          logln("; Unknown:   " + BenchStats.Current.unknown)
          logln("; Different: " + BenchStats.Current.different)
          logn()
          logn()
          logFlush()
          BenchStats.fileCountIncrement
          clearConsigned
        }
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

  }


  /** Uses the traits from '''smts.utils''' to pretty print and handle options. */
  trait NiceBench extends Verboser with OptionHandler with Animator {

    // |=====| Log things.

    object Logger {
      private var _writer: BufferedWriter = null
      def writer = _writer
      def newFile(filePath: String) = {
        _logging = true
        _writer = new BufferedWriter(new FileWriter(filePath))
      }
      private var _logging = false
      def logging = _logging
    }

    lazy val log: String => Unit =
      if (Logger.logging) { s => Logger.writer write s }
      else { s => () }
    lazy val logln: String => Unit =
      if (Logger.logging) { s => Logger.writer write (s + "\n") }
      else { s => () }
    lazy val logn: () => Unit =
      if (Logger.logging) { ()=> Logger.writer write "\n" }
      else { () => () }
    lazy val logFlush: () => Unit =
      if (Logger.logging) { () => Logger.writer.flush }
      else { () => () }


    // |=====| Benchmark statistics (performs the printing).

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
        def reset = {
          _cSuccess = 0 ; _cDifferent = 0 ; _cFailed = 0 ;
          _cError = 0 ; _cTimeout = 0 ; _cUnknown = 0
        }
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

    // |=====| Timer to time things.

    object Timer {
      private var _timer: Long = 0
      def time = _timer
      def now = System.currentTimeMillis - _timer
      def start = _timer = System.currentTimeMillis
      def stop = _timer = System.currentTimeMillis - _timer
    }


    // |=====| Option handling.

    /** Options of the benchmarking session. */
    object Options {
      private var _solver: String = "z3"
      def solver = _solver
      def solver_= (newSolver: String) = _solver = newSolver
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

    override val helpHeader = {
      "Benchmark your structure on SMT lib 2 files. Files are parsed to" ::
      "construct the Smts messages (trait ToSmtsMsg) corresponding to each" ::
      "query. All the commands are executed on the underlying solver using" ::
      "the messages, and the results are parsed to produce messages (trait" ::
      "FromSmtsMsg).  Activate logging to produce a file containing all the" ::
      "details of the run." ::
      "" ::
      "Keys used in printing and logging:" ::
      "  success: a command was parsed and the solver did not return an" ::
      "      error." ::
      "  failed: parsing of a command failed." ::
      "  error: a command was parsed but the solver returned an error." ::
      "  different: a command was parsed, but the message produced did not" ::
      "      match --modulo whitespaces-- the command." ::
      "  timeout: the solver explicitely returned \"timeout\"." ::
      "  unknown: the solver answered \"unknown\". Warning: this might be" ::
      "      because of per query timeouts, in Z3 for example." ::
      "" ::
      Nil
    }

    val optionPrint = { s: String => verbln(s) }

    val myOptions = {
      ("-h", { s: String => {
        printHelpHeader() ; sys exit 0
      } }, "     prints this message." :: Nil) ::
      ("--help", { s: String => {
        printHelpHeader() ; sys exit 0 }
      }, " also prints this message." :: Nil) ::
      ("--log=", { s: String =>
        Options.log = optionValue(s)
      }, "<file> activates logging to the specified file." :: Nil) ::
      ("--solver=", { s: String => optionValue(s) match {
        case "z3" | "mathsat" | "cvc4" => Options.solver = optionValue(s)
        case string => {
          optionError("unexpected solver value \"" + string + "\".")
          printHelp() ; sys exit -1
        }
      }}, "<string> the solver to use: z3, mathsat or cvc4 (default z3)." :: Nil) ::
      ("--timeout=", { s: String =>
        Options.timeout = optionValue(s).toInt
      }, "<int> sets a timeout for each query to the solver, in milliseconds." :: Nil) ::
      Nil
    }

    val myArguments = {
      ("--dir=", { s: String =>
        Options.dirs = optionValue(s).split(",")
      }, {
        "<dir1>,<dir2>,... The directories where the benchmarks are." ::
        "Directories are explored recursively." ::
        "All files matching \"*.smt2\" will be ran." :: Nil
      }) ::
      Nil
    }


    // |=====| Animation.

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
    def printGlobalProgress(current: Int, max: Int) =
      animLine(3,current,max,globalProgressAnim)

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
    def printFileProgress(current: Int, max: Int) =
      animLine(6,current,max,fileProgressAnim)
  }
}
