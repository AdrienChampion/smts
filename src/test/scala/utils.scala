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

/** Verbose with prefix. */
trait Verboser extends App {

  /** Prefix used in the verbose mode. */
  val prefix: String = ""
  /** Verbose printer. */
  def verb(s: String) = { print(prefix) ; print(s) }
  /** Verbose printer. */
  def verbln(s: String) = { verb(s) ; println }
  /** Verbose printer. */
  def verbln() = { print(prefix) ; println }

  /** Prints a line containing only the prefix. */
  def space = verbln("")
  /** Displays a nice title (level 0). */
  def title0(s: String) = verbln("|=======| " + s + " |=======|")
  /** Displays a nice title (level 1). */
  def title1(s: String) = verbln("|=====| " + s + " |=====|")
  /** Displays a nice title (level 2). */
  def title2(s: String) = verbln("|===| " + s + " |===|")

}

/** Handles the options of the '''App'''. */
trait OptionHandler extends App {
  val optionPrint: String => Unit
  def optionError(s: String) = { optionPrint("\033[1;31mError\033[0m " + s) }
  def optionValue(s: String) = s.split("=")(1)
  /** List of triplets: the option prefix, what to do with it, and an
    * explanation of the option (used for '''printHelp'''). */
  val myOptions: List[(String, String => Unit, List[String])]
  /** List of triplets: the option prefix, what to do with it, and an
    * explanation of the option (used for '''printHelp'''). */
  val myArguments: List[(String, String => Unit, List[String])]
  val unexpectedOptionThrowsError = true
  val helpHeader: String = ""

  def printHelp(text: String = "") = {
    if (text != "") optionPrint(text)
    optionPrint(helpHeader)
    optionPrint("\033[1mMandatory arguments:\033[0m")
    myArguments foreach (arg => arg._3 match {
      case h :: t => {
        optionPrint("  \033[1m" + arg._1 + "\033[0m" + h)
        t foreach (line => optionPrint("    " + line))
      }
      case Nil => optionPrint("  \033[1m" + arg._1 + "\033[0m")
    })
    optionPrint("\033[1mOptions:\033[0m")
    myOptions foreach (opt => opt._3 match {
      case h :: t => {
        optionPrint("  \033[1m" + opt._1 + "\033[0m" + h)
        t foreach (line => optionPrint("    " + line))
      }
      case Nil => optionPrint("  \033[1m" + opt._1 + "\033[0m")
    })
  }

  def setOptions() = {
    val arguments = new scala.collection.mutable.HashSet[(String, String => Unit, List[String])] ++ myArguments
    args foreach (arg => {
      myArguments find (myArg => arg startsWith myArg._1) match {
        case Some(triplet) => { triplet._2(arg) ; arguments -= triplet }
        case None => myOptions find (opt => arg startsWith opt._1) match {
          case Some((_,optDo,_)) => optDo(arg)
          case None if unexpectedOptionThrowsError => {
            optionError("unexpected argument \"" + arg + "\".")
            sys exit -1
          }
          case None => ()
        }
      }
    })
    if (!arguments.isEmpty) {
      optionError("following mandatory arguments are missing:")
      arguments foreach (arg => arg._3 match {
        case h :: t => {
          optionPrint("  \033[1m" + arg._1 + "\033[0m" + h)
          t foreach (line => optionPrint("    " + line))
        }
        case Nil => optionPrint("  \033[1m" + arg._1 + "\033[0m")
      })
      sys exit -1
    }
  }
}

/** Basic trait for the animations. */
trait Animator {
  import java.io.{BufferedWriter,FileWriter}

  trait Animation {
    /** All the sprites to use in the animation. Should all be the same length. */
    protected var sprites: List[String]
    /** The sprite to display at 100%. Should the same length as the other sprites. */
    def lastSprite: String
    /** The beginning of the progress bar. */
    val barStartString: String
    /** The end of the progress bar. */
    val barStopString: String
    /** Characters filling the bar before the sprite. */
    val barToEmptyChar: Char
    /** Characters filling the bar after the sprite. */
    val barFilledChar: Char
    /** Size of each of the sprites. */
    val spriteSize: Int
    /** Length of the progress bar in characters. */
    val barLength: Int

    private def maxPos = barLength - spriteSize
    private var previousPosition = -42
    private[Animator] def apply(index: Int, current: Int, max: Int) = {
      val nuPos = current * maxPos / max
      if (nuPos != previousPosition) {
        previousPosition = nuPos
        print("\033[?25l")
        loadPos() ; goUp(animatorLineCount - index + 1) ; clearLine ; print(barStartString)
        print(barFilledChar.toString * nuPos)
        if (nuPos == maxPos) print(lastSprite) else sprites match {
          case head :: tail => { print(head) ; sprites = tail :+ head }
          case Nil => throw new Exception("Display: error, no sprite to display.")
        }
        print(barToEmptyChar.toString * (barLength - (nuPos + spriteSize)))
        print(barStopString) ; loadPos
        print("\033[?25h")
      }
    }
  }

  class KawaiiAnimation(val barLength: Int = 100) extends Animation {
    protected var sprites: List[String] =
      "\033[34m\\(\033[0m\033[31m^_^\033[0m\033[34m)/\033[0m" ::            // \(^_^)/
      "\033[34m-(\033[0m\033[31m^_^\033[0m\033[34m)-\033[0m" ::             // -(^_^)-
      "\033[34m/(\033[0m\033[31m^_^\033[0m\033[34m)\\\033[0m" ::            // /(^_^)\
      "\033[34m|(\033[0m\033[31m^_^\033[0m\033[34m)|\033[0m" :: Nil         // |(^_^)|
    def lastSprite: String = "\033[1;34m\\(\033[1;31m*o*\33[1;34m)/\033[0m" // \(*o*)/
    val spriteSize: Int = 7
    val barStartString = "["
    val barStopString = "]"
    val barToEmptyChar = '-'
    val barFilledChar = '='
  }

  val animatorPrint: String => Unit
  val animatorLineCount: Int
  private def savePos() = print("\033[s")
  private def loadPos() = print("\033[u")
  private def clearLine() = print("\033[K")
  private def goUp(n: Int) = print("\033[" + n + "A")
  private def goDown(n: Int) = print("\033[" + n + "B")

  /** @param lineCount Number of lines in the animator. */
  def initAnim() = {
    (1 to animatorLineCount) foreach { i => animatorPrint("\n") }
    animatorPrint("")
    // Save position.
    savePos()
  }
  def outitAnim() = { loadPos() ; println("") }
  def printLine(index: Int, ss: List[String]) = {
    print("\033[?25l")
    loadPos() ; goUp(animatorLineCount - index + 1) ; clearLine()
    ss foreach { print(_) } ; loadPos() ; print("\033[?25h")
  }

  /** Progression bar
    * @param index The index of the line.
    * @param current The current value of whatever the bar represents.
    * @param max The maximal value of whatever the bar represents. */
  def animLine(index: Int, current: Int, max: Int, animation: Animation) = animation(index,current,max)
}
