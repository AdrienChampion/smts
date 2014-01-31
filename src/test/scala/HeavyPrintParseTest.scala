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

import sys.process.stringSeqToProcess
import scala.annotation.tailrec

/** Tests basic functionalities of the Smts SMT solver wrapper. */
object HeavyPrintParseTest extends Verboser with OptionHandler with Animator {

  val optionPrint = { s: String => verbln(s) }
  val animatorPrint = { s: String => verb(s) }

  object Smts extends SmtsTest

  object Options {
    val logDefault = "./benchParsePrintTest.log"
    private var _logFile: String = logDefault
    def logFile = _logFile
    def logFile_= (newLogFile: String) = _logFile = newLogFile
    private var _dirs: List[String] = Nil
    def dirs = _dirs
    def dirs_= (newDirs: Traversable[String]) = _dirs = newDirs.toList
  }

  val myOptions = {
    ("-h", { s: String => { printHelp() ; sys exit -1 } }, "     prints this message." :: Nil) ::
    ("--help", { s: String => { printHelp() ; sys exit -1 } }, " also prints this message." :: Nil) ::
    ("--log=", { s: String => Options.logFile = optionValue(s) },
      "<file> This file will be used for logging, (default \"" + Options.logDefault + "\")." :: Nil) ::
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

  val animatorLineCount = 6

  // Line 1.
  def printBenchNumber(benchs: Int, total: Int) = printLine(1, {
    benchs.toString :: "/" :: total.toString :: " files benchmarked so far (" ::
    Timer.now.toString :: "ms)." :: Nil
  })

  // Line 2.
  def printGeneralStatus(success: Int, different: Int, failed: Int, total: Int) = printLine(2, {
    "Success: " :: success.toString :: ", different: " :: different.toString ::
    ", failed: " :: failed.toString :: ". Total: " :: failed.toString :: "." :: Nil
  })

  // Line 3.
  val globalProgressAnim = new KawaiiAnimation()
  def printGlobalProgress(current: Int, max: Int) = animLine(3,current,max,globalProgressAnim)

  // Line 4.
  def printBenchName(file: String) = printLine(4, {
    "Currently running on file \"" :: file :: "\"." :: Nil
  })

  // Line 5.
  def printFileStatus(success: Int, different: Int, failed: Int, total: Int) = printLine(5, {
    " Success: " :: success.toString :: ", different: " :: different.toString ::
    ", failed: " :: failed.toString :: ". Total: " :: total.toString :: ". " :: Nil
  })

  // Line 6.
  val fileProgressAnim = new KawaiiAnimation()
  def printFileProgress(current: Int, max: Int) = animLine(6,current,max,fileProgressAnim)

  setOptions()

  val br = new BufferedWriter(new FileWriter(Options.logFile))
  def log(s: String) = br write s
  def logln(s: String) = { br write s ; br write "\n" }
  def logln() = br write "\n"
  def flush = br.flush

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
  title0("Launching parsing and printing benchmarks.")
  verbln("Logging in file " + Options.logFile + ".")
  space
  initAnim()

  logln("Parsing and printing benchmarks.")
  logln("  arguments: " + args.toList)
  logln
  logln

  Timer.start
  Options.dirs foreach (arg => {
    exploreAndDo(arg,workOnBench)
  })
  Timer.stop

  outitAnim()
  logln
  logln("Done in " + Timer.time + "ms.")
  logln("Success:   " + BenchStats.success)
  logln("Different: " + BenchStats.different)
  logln("Failed:    " + BenchStats.failed)
  logln("Total:     " + BenchStats.total)
  flush
  space
  sys exit 0

  def workOnBench(filePath: String): Unit = {
    // verbln("Working on file " + filePath)

    printBenchName(filePath)
    BenchStats.updateGlobalStatus
    logln("Working on file " + filePath)
    val br = new BufferedReader(new FileReader(filePath))

    val lnr = new LineNumberReader(new FileReader(new File(filePath)))
    lnr.skip(Long.MaxValue)
    val lineCount = lnr.getLineNumber()

    @tailrec
    def loop(line: String = "", success: Int = 0, different: Int = 0, failed: Int = 0, lines: Int = 0): (Int,Int,Int) = {
      printFileStatus(success, different, failed, success + different + failed)
      printFileProgress(lines,lineCount)
      val nuLine = br.readLine
      if (nuLine != null) {
        val cleanLine = if (line == "") nuLine.trim else line + " " + nuLine.trim
        val op = cleanLine count (c => c == '(')
        val cp = cleanLine count (c => c == ')')
        if (op == cp) {
          Smts parseCommand cleanLine match {
            case Smts.Succ(msg) => {
              val sw = new StringWriter()
              Smts.writeMsg(msg,sw)
              if ((cleanLine + "\n").replaceAll("\\s+","") == sw.toString.replaceAll("\\s+",""))
                loop("", success + 1, different, failed, lines + 1)
              else {
                logln("Parse successful but strings are different. Original:")
                logln("  " + cleanLine)
                logln("Printed version:")
                log("  " + sw.toString)
                loop("", success, different + 1, failed, lines + 1)
              }
            }
            case Smts.Fail(msg) => {
              logln("Parse failed on line:")
              logln("  " + cleanLine)
              logln("Error message:")
              logln(msg)
              loop("", success, different, failed + 1, lines + 1)
            }
          }
        } else loop(cleanLine, success, different, failed, lines + 1)
      } else (success,different,failed)
    }

    Smts.clearConsigned
    val result = loop()
    flush
    BenchStats.update(result._1, result._2, result._3)
    val sum = result._1 + result._2 + result._3
    logln("Success:   " + result._1)
    logln("Different: " + result._2)
    logln("Failed:    " + result._3)
    logln("Total:     " + sum)
    logln
    BenchStats.fileCountIncrement
  }

  object BenchStats {
    def updateGlobalStatus = {
      printGlobalProgress(fileCount,totalFileCount)
      printGeneralStatus(success,different,failed,total)
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
    def update(s: Int, d: Int, f: Int) = {
      _success += s ; _different += d ; _failed += f
      updateGlobalStatus
    }
    private var _success: Int = 0
    def success = _success
    private var _different: Int = 0
    def different = _different
    private var _failed: Int = 0
    def failed = _failed
    def total = success + different + failed
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
