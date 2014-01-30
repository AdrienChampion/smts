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
object BenchParsePrintTest extends Tester {

  object Timer {
    private var _timer: Long = 0
    def time = _timer
    def now = System.currentTimeMillis - _timer
    def start = _timer = System.currentTimeMillis
    def stop = _timer = System.currentTimeMillis - _timer
  }

  /** Six lines printer. */
  def savePos() = print("\033[s")
  def loadPos() = print("\033[u")
  def clearLine() = print("\033[K")
  def goUp(n: Int) = print("\033[" + n + "A")
  def goDown(n: Int) = print("\033[" + n + "B")
  def initDisplay() = {
    print("\033[?25l")
    verbln("") ; verbln("") ; verbln("") ; verbln("") ; verbln("") ; verbln("") ; verb("")
    // Save position.
    savePos()
  }
  def closeDisplay() = { loadPos() ; println("\033[?25h") }

  def printBenchNumber(benchs: Int, total: Int) = {
    loadPos() ; goUp(6) ; clearLine()
    print(benchs.toString + "/" + total + " files benchmarked so far (" + Timer.now + "ms).")
  }
  def printGeneralStatus(success: Int, different: Int, failed: Int, total: Int) = {
    loadPos() ; goUp(5)
    print("Success: " + success + ", different: " + different + ", failed: " + failed + ". Total: " + total + ".")
  }
  val printGlobalProgress = new Animation(4)
  def printBenchName(file: String) = {
    loadPos() ; goUp(3) ; clearLine()
    print("Currently running on file \"" + file + "\".")
  }
  def printFileStatus(success: Int, different: Int, failed: Int, total: Int) = {
    loadPos() ; goUp(2) ; clearLine()
    print(" Success: " + success + ", different: " + different + ", failed: " + failed + ". Total: " + total + ". ")
  }
  val printFileProgress = new Animation(1) // new Animation(0,"|" :: "/" :: "-" :: "\\" :: Nil, "X",1)

  class Animation(
    val pos: Int, protected var sprites: List[String] =
      "\033[34m\\(\033[0m\033[31m^_^\033[0m\033[34m)/\033[0m" ::
      "\033[34m-(\033[0m\033[31m^_^\033[0m\033[34m)-\033[0m" ::
      "\033[34m/(\033[0m\033[31m^_^\033[0m\033[34m)\\\033[0m" ::
      "\033[34m|(\033[0m\033[31m^_^\033[0m\033[34m)|\033[0m" :: Nil,
    val lastSprite: String = "\033[1;34m\\(\033[1;31m*o*\33[1;34m)/\033[0m",
    val spriteSize: Int = 7
  ){
    val length = 100
    def maxPos = length - spriteSize
    private var previous = -10
    def apply(spritePos: Int => Int) = {
      val nuPos = spritePos(maxPos)
      if (nuPos != previous) {
        previous = nuPos
        loadPos() ; goUp(pos) ; clearLine ; print("[") ; print("=" * nuPos)
        if (nuPos == maxPos) print(lastSprite) else sprites match {
          case head :: tail => { print(head) ; sprites = tail :+ head }
          case Nil => throw new Exception("Display: error, no sprite to display.")
        }
        print(" " * (length - (nuPos + spriteSize))) ; print("]")
      }
    }
    def apply() = {
      sprites match {
        case head :: tail => { print(head) ; sprites = tail :+ head }
        case Nil => throw new Exception("Display: error, no sprite to display.")
      }
    } 
  }

  val logFile = "./benchParsePrintTest.log"
  val br = new BufferedWriter(new FileWriter(logFile))
  def log(s: String) = br write s
  def logln(s: String) = { br write s ; br write "\n" }
  def logln() = br write "\n"
  def flush = br.flush

  BenchStats.totalFileCount = (args.foldLeft(0)(
    (n,arg) => n + (Seq("bash","-c", "find " + arg + " -iname \"*.smt2\" | wc -l") !!).replaceAll("\\s+","").toInt
  ))

  if (BenchStats.totalFileCount == 0) {
    space
    verbln("No benchmarks found, exiting.")
    space
    sys exit 0
  }

  space
  title0("Launching parsing and printing benchmarks.")
  verbln("Logging in file " + logFile + ".")
  space
  initDisplay

  logln("Benchmarking, arguments: " + args.toList)
  logln
  logln

  Timer.start
  args foreach (arg => {
    exploreAndDo(arg,workOnBench)
  })
  Timer.stop

  closeDisplay
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
      printFileProgress({ max => max * lines / lineCount })
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
    // if (result._1 == sum) verbln("Done, all successful (" + result._1 + ").")
    // else verbln("Done, total: " + sum + ". Success: " + result._1 + ", different: " + result._2 + ", failed: " + result._3 + ".")
    // verbln("")
  }

  object BenchStats {
    def updateGlobalStatus = {
      printBenchNumber(fileCount, totalFileCount)
      printGlobalProgress({ max => max * fileCount / totalFileCount })
      printGeneralStatus(success,different,failed,total)
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
      updateGlobalStatus
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
