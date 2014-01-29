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

/** Tests basic functionalities of the Smts SMT solver wrapper. */
object BenchParsePrintTest extends Tester {

  val logFile = "./benchParsePrintTest.log"
  val br = new BufferedWriter(new FileWriter(logFile))
  def log(s: String) = br write s
  def logln(s: String) = { br write s ; br write "\n" }
  def logln() = br write "\n"
  def flush = br.flush

  space
  title0("Launching parsing and printing benhchmarks.")
  verbln("Logging in file " + logFile + ".")
  space
  space

  logln("Benchmarking, arguments: " + args.toList)
  logln
  logln

  args foreach (arg => {
    title1("Working on " + arg + ".")
    space
    exploreAndDo(arg,workOnBench)
    title1("Done working on " + arg + ".")
  })

  title0("Done.")
  verbln("Success:   " + BenchResult.success + " / " + BenchResult.total + ".")
  verbln("Different: " + BenchResult.different + " / " + BenchResult.total + ".")
  verbln("Failed:    " + BenchResult.failed + " / " + BenchResult.total + ".")
  logln
  logln
  logln("Success:   " + BenchResult.success + " / " + BenchResult.total + ".")
  logln("Different: " + BenchResult.different + " / " + BenchResult.total + ".")
  logln("Failed:    " + BenchResult.failed + " / " + BenchResult.total + ".")
  flush
  space
  sys exit 0

  def workOnBench(filePath: String): Unit = {
    verbln("Working on file " + filePath)
    logln("Working on file " + filePath)
    val br = new BufferedReader(new FileReader(filePath))

    @tailrec
    def loop(line: String = "", success: Int = 0, different: Int = 0, failed: Int = 0): (Int,Int,Int) = {
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
                loop("", success + 1, different, failed)
              else {
                logln("Parse successful but strings are different. Original:")
                logln("  " + cleanLine)
                logln("Printed version:")
                log("  " + sw.toString)
                loop("", success, different + 1, failed)
              }
            }
            case Smts.Fail(msg) => {
              logln("Parse failed on line:")
              logln("  " + cleanLine)
              logln("Error message:")
              logln(msg)
              loop("", success, different, failed + 1)
            }
          }
        } else loop(cleanLine, success, different, failed)
      } else (success,different,failed)
    }

    val result = loop()
    flush
    BenchResult.update(result._1, result._2, result._3)
    val sum = result._1 + result._2 + result._3
    logln("Success:   " + result._1 + " / " + sum + ".")
    logln("Different: " + result._2 + " / " + sum + ".")
    logln("Failed:    " + result._3 + " / " + sum + ".")
    logln
  }

  object BenchResult {
    def update(s: Int, d: Int, f: Int) = { success += s ; different += d ; failed += f }
    var success: Int = 0
    var different: Int = 0
    var failed: Int = 0
    def total = success + different + failed
  }

  def exploreAndDo(filePath: String, work: String => Unit): Unit = {
    val file = new File(filePath)
    if (file.isDirectory) file.list.foreach(subFile => exploreAndDo(subFile,work))
    else if (filePath endsWith ".smt2") work(filePath)
    else ()
  }

}
