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

import scala.language.reflectiveCalls

import smts._

/** Tests basic functionalities of the Smts SMT solver wrapper. */
object BasicTests extends Verboser {

  space
  title0("Launching BasicTests.")
  space
  space

  testParsers

  title0("Done.")
  space
  sys exit 0

  def testParsers = {
    import java.io.{Writer, BufferedWriter}

    def test = {
      verbln("Creating a SmtLib parser with String as Expr.")
      val smtLibParser = new SmtLibParsers[String, String, String] {
        def expr2Smt(s: String, writer: Writer) = ()
        def ident2Smt(s: String, writer: Writer) = ()
        def sort2Smt(s: String, writer: Writer) = ()
        lazy val smt2Expr: PackratParser[String] = {
          "(" ~ smt2Expr ~ ")" ^^ { case op~str~cp => op + str + cp } |
          """[^()]*""".r ~ smt2Expr^^ { case anyth ~ more => anyth + " " + more } | 
          """[^()]*""".r ^^ { case anyth => anyth }
        }
        lazy val smt2Ident: PackratParser[String] = """[a-zA-Z_][a-zA-Z0-9_$]*""".r ^^ { case ident => ident }
        lazy val smt2Num: PackratParser[String] = {
          """[1-9][0-9]*""".r ^^ { case ident => ident } |
          "0"
        }
        lazy val smt2Sort: PackratParser[String] = { "Bool" | "Int" | "Real" }
        def parse(s: String) =
          phrase(resultParser)(new PackratReader(new scala.util.parsing.input.CharSequenceReader(s))) match {
            case Success(result,next) => result
            case Failure(msg,next) => { verbln("  Failure:" + msg) ;  println(next.pos.longString) }
            case Error(msg,next) => { verbln("  Error:" + msg) ;  println(next.pos.longString) }
          }
        def identParse(s: String) =
          phrase(identParser)(new PackratReader(new scala.util.parsing.input.CharSequenceReader(s))) match {
            case Success(result,next) => result
            case Failure(msg,next) => { verbln("  Failure:" + msg) ;  println(next.pos.longString) }
            case Error(msg,next) => { verbln("  Error:" + msg) ;  println(next.pos.longString) }
          }
      }
      verbln("Creation successful.")
      def testParsingIdent(toParse: String) = {
        verbln("Attempting to parse identifier [" + toParse + "]")
        val result = smtLibParser.identParse(toParse)
        verbln("  Result: " + result)
      }
      def testParsing(toParse: String) = {
        verbln("Attempting to parse [" + toParse + "]")
        val result = smtLibParser.parse(toParse)
        verbln("  Result: " + result)
      }
      title2("Ident parsing.")
      testParsingIdent("^ont50342")
      testParsingIdent("@toto")
      testParsingIdent("?y2")
      testParsingIdent("_%$^taoe/*@d&.ppo<i-bt+kjob_>mm=m.")
      testParsingIdent("|snta,h.snthilrc[]{+)(*][=&!]+)}{)(*#!@-snzvw;@/eo#q^?57621629%642|")
      title2("Error parser.")
      testParsing("(error \"stah,.prc[{}]+)(!*&{}(!{##&{+}(*{+)}*=u1642064720%@/-szwm;'\")")
      title2("Check sat parser.")
      testParsing("sat")
      testParsing("unsat")
      testParsing("unknown")
      title2("Model parser.")
      testParsing("(model)")
      testParsing("(model (define-fun toto () Bool (toto)))")
      testParsing("(model (define-fun toto ((nattp Bool) (na205 Int)) Bool (I can write whatever I want)))")
      title2("Unsat core parser.")
      testParsing("(|satohe0642!)[&{}#yoeui@;.,@/}{-tioeui| %31Ntot$&% toto tata17_)")
      title2("Values parser.")
      testParsing("(((this is an expr) (value)) ((another expr) (another value)) ((a last expr) (a last value)))")
      title2("Done testing smt lib parsers.")
    }

    title1("Testing parsers.")
    space
    test
    space
    title1("Done testing parsers.")
    space
    space
  }
}
