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

/** Simple trait to factorize features used in all test files. */
trait Tester extends App {

  /** Prefix used in the verbose mode. */
  val prefix = "[Tester] "
  /** Verbose printer. */
  def verb(s: String) = { print(prefix) ; print(s) }
  /** Verbose printer. */
  def verbln(s: String) = verb(s + "\n")

  /** Prints a line containing only the prefix. */
  def space = verbln("")
  /** Displays a nice title (level 0). */
  def title0(s: String) = verbln("|=======| " + s + " |=======|")
  /** Displays a nice title (level 1). */
  def title1(s: String) = verbln("|=====| " + s + " |=====|")
  /** Displays a nice title (level 2). */
  def title2(s: String) = verbln("|===| " + s + " |===|")

}
