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

import java.io.Writer

import scala.collection.mutable.OpenHashMap
import scala.util.parsing.combinator.{PackratParsers,RegexParsers}

import smts._

/** Contains a simple data structures with printers and parsers for testing Smts. */
object ExprStructure extends RegexParsers with PackratParsers {


  // |=====| Expression structure.

  /** Extended by all components of the expression structure. */
  sealed trait Expr {
    /** Writes this expression to a '''Writer''' in the smt lib 2 standard. */
    def writeTo(w: Writer): Unit
  }

  /** Hash consign trait for expressions. */
  trait ConsignedExpr[In,Out] {
    /** A hash consign to control memory consumption. */
    protected val consign = new OpenHashMap[In,Out]
  }

  /** Identifier class. */
  class Ident private(val id: String) extends BoolExpr with ArithExpr {
    def writeTo(w: Writer) = w write id
  }
  object Ident extends ConsignedExpr[String, Ident] {
    def apply(id: String) = consign.getOrElseUpdate(id, new Ident(id))
    def unapply(arg: Ident) = Some(arg.id)
  }

  /** Extended by all the Boolean expressions. */
  sealed trait BoolExpr extends Expr

  /** True object. */
  object True extends BoolExpr { def writeTo(w: Writer) = w write "true" }
  /** False object. */
  object False extends BoolExpr { def writeTo(w: Writer) = w write "false" }

  /** Not class. */
  class Not private(val kid: BoolExpr) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(not " ; kid writeTo w ; w write ")"
    }
  }
  object Not extends ConsignedExpr[BoolExpr,Not] {
    def apply(kid: BoolExpr): BoolExpr = kid match {
      case Not(e) => e
      case _ => consign.getOrElseUpdate(kid, new Not(kid))
    }
    def unapply(arg: Not) = Some(arg.kid)
  }

  /** N-ary and class. */
  class AndN private(val kids: Traversable[BoolExpr]) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(and"
      kids foreach (kid => { w write " " ; kid writeTo w })
      w write ")"
    }
  }
  object AndN extends ConsignedExpr[Traversable[BoolExpr], AndN] {
    def apply(kids: Traversable[BoolExpr]): BoolExpr = kids.size match {
      case 0 => False
      case 1 => kids.head
      case _ => consign.getOrElseUpdate(kids, new AndN(kids))
    }
    def unapply(arg: AndN) = Some(arg.kids)
  }

  /** N-ary or class. */
  class OrN private(val kids: Traversable[BoolExpr]) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(or"
      kids foreach (kid => { w write " " ; kid writeTo w })
      w write ")"
    }
  }
  object OrN extends ConsignedExpr[Traversable[BoolExpr], OrN] {
    def apply(kids: Traversable[BoolExpr]): BoolExpr = kids.size match {
      case 0 => True
      case 1 => kids.head
      case _ => consign.getOrElseUpdate(kids, new OrN(kids))
    }
    def unapply(arg: OrN) = Some(arg.kids)
  }

  /** N-ary equal class. */
  class Eq private(val kids: Traversable[Expr]) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(=" ; kids foreach (kid => { w write " " ; kid writeTo w }) ; w write ")"
    }
  }
  object Eq extends ConsignedExpr[Traversable[Expr], Eq] {
    def apply(kids: Traversable[Expr]) = kids.size match {
      case n if n > 1 => consign.getOrElseUpdate(kids, new Eq(kids))
      case n => throw new Exception("Illegal attempt to create an Eq node with " + n + " kids: " + kids)
    }
    def unapply(arg: Eq) = Some(arg.kids)
  }

  /** Less than class. */
  class Lt private(val lhs: Expr, val rhs: Expr) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(< " ; lhs writeTo w ; w write " " ; rhs writeTo w ; w write ")"
    }
  }
  object Lt extends ConsignedExpr[(Expr,Expr), Lt] {
    def apply(lhs: Expr, rhs: Expr) = consign.getOrElseUpdate((lhs,rhs), new Lt(lhs,rhs))
    def unapply(arg: Lt) = Some((arg.lhs, arg.rhs))
  }

  /** Less or equal class. */
  class Le private(val lhs: Expr, val rhs: Expr) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(<= " ; lhs writeTo w ; w write " " ; rhs writeTo w ; w write ")"
    }
  }
  object Le extends ConsignedExpr[(Expr,Expr), Le] {
    def apply(lhs: Expr, rhs: Expr) = consign.getOrElseUpdate((lhs,rhs), new Le(lhs,rhs))
    def unapply(arg: Le) = Some((arg.lhs, arg.rhs))
  }

  /** Greater or equal class. */
  class Ge private(val lhs: Expr, val rhs: Expr) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(>= " ; lhs writeTo w ; w write " " ; rhs writeTo w ; w write ")"
    }
  }
  object Ge extends ConsignedExpr[(Expr,Expr), Ge] {
    def apply(lhs: Expr, rhs: Expr) = consign.getOrElseUpdate((lhs,rhs), new Ge(lhs,rhs))
    def unapply(arg: Le) = Some((arg.lhs, arg.rhs))
  }

  /** Greater than class. */
  class Gt private(val lhs: Expr, val rhs: Expr) extends BoolExpr {
    def writeTo(w: Writer) = {
      w write "(> " ; lhs writeTo w ; w write " " ; rhs writeTo w ; w write ")"
    }
  }
  object Gt extends ConsignedExpr[(Expr,Expr), Gt] {
    def apply(lhs: Expr, rhs: Expr) = consign.getOrElseUpdate((lhs,rhs), new Gt(lhs,rhs))
    def unapply(arg: Le) = Some((arg.lhs, arg.rhs))
  }


  /** Extended by all the integer nodes. */
  sealed trait ArithExpr extends Expr

  /** Integer constant. */
  class IntConst(val value: BigInt) extends ArithExpr {
    def writeTo(w: Writer) = w write value.toString
  }
  object IntConst extends ConsignedExpr[BigInt, IntConst] {
    def apply(value: BigInt) = consign.getOrElseUpdate(value, new IntConst(value))
    def unapply(arg: IntConst) = Some(arg.value)
  }

  /** Rational constant. */
  class RatConst(val num: BigInt, val den: BigInt) extends ArithExpr {
    def writeTo(w: Writer) = {
      w write "(/ " ; w write num.toString ; w write " " ; w write den.toString ; w write ")"
    }
  }
  object RatConst extends ConsignedExpr[(BigInt,BigInt), RatConst] {
    def apply(num: BigInt, den: BigInt) = consign.getOrElseUpdate((num,den), new RatConst(num,den))
    def unapply(arg: RatConst) = Some((arg.num, arg.den))
  }

  /** Plus class. */
  class Plus(val lhs: ArithExpr, val rhs: ArithExpr) extends ArithExpr {
    def writeTo(w: Writer) = {
      w write "(+ " ; w write lhs.toString ; w write " " ; w write rhs.toString ; w write ")"
    }
  }
  object Plus extends ConsignedExpr[(ArithExpr,ArithExpr), Plus] {
    def apply(lhs: ArithExpr, rhs: ArithExpr) = consign.getOrElseUpdate((lhs,rhs), new Plus(lhs,rhs))
    def unapply(arg: Plus) = Some((arg.lhs,arg.rhs))
  }

  /** Minus class. */
  class Minus(val lhs: ArithExpr, val rhs: ArithExpr) extends ArithExpr {
    def writeTo(w: Writer) = {
      w write "(- " ; w write lhs.toString ; w write " " ; w write rhs.toString ; w write ")"
    }
  }
  object Minus extends ConsignedExpr[(ArithExpr,ArithExpr), Minus] {
    def apply(lhs: ArithExpr, rhs: ArithExpr) = consign.getOrElseUpdate((lhs,rhs), new Minus(lhs,rhs))
    def unapply(arg: Minus) = Some((arg.lhs, arg.rhs))
  }

  /** Unary minus class. */
  class UMinus(val kid: ArithExpr) extends ArithExpr {
    def writeTo(w: Writer) = {
      w write "(- " ; w write kid.toString ; w write ")"
    }
  }
  object UMinus extends ConsignedExpr[ArithExpr, UMinus] {
    def apply(kid: ArithExpr) = consign.getOrElseUpdate(kid, new UMinus(kid))
    def unapply(arg: UMinus) = Some(arg.kid)
  }

  /** Times class. */
  class Mult(val lhs: ArithExpr, val rhs: ArithExpr) extends ArithExpr {
    def writeTo(w: Writer) = {
      w write "(* " ; lhs writeTo w ; w write " " ; rhs writeTo w ; w write ")"
    }
  }
  object Mult extends ConsignedExpr[(ArithExpr,ArithExpr), Mult] {
    def apply(lhs: ArithExpr, rhs: ArithExpr) = consign.getOrElseUpdate((lhs,rhs), new Mult(lhs,rhs))
    def unapply(arg: Mult) = Some((arg.lhs, arg.rhs))
  }

  // |=====| Sort structure.

  sealed trait Sort { def writeTo(w: Writer): Unit }
  class IdentSort(val id: String) extends Sort {
    def writeTo(w: Writer) = w write id
  }
  object IdentSort extends ConsignedExpr[String,IdentSort] {
    def apply(id: String) = consign.getOrElseUpdate(id, new IdentSort(id))
    def unapply(arg: IdentSort) = Some(arg.id)
  }
  class NestedSort(val id: String, val sorts: Seq[Sort]) extends Sort {
    def writeTo(w: Writer) = {
      w write "(" ; w write id ; sorts foreach (sort => {
        w write " " ; sort writeTo w
      }) ; w write ")"
    }
  }
  object NestedSort extends ConsignedExpr[(String, Seq[Sort]), NestedSort] {
    def apply(id: String, sorts: Seq[Sort]) = consign.getOrElseUpdate((id,sorts), new NestedSort(id,sorts))
  }

}

object Smts extends SmtLibCommandParsers[ExprStructure.Expr, ExprStructure.Ident, ExprStructure.Sort] with SmtLibPrinters[ExprStructure.Expr, ExprStructure.Ident, ExprStructure.Sort] {

  import ExprStructure._

  // |=====| Parsers.

  lazy val testExprParser: PackratParser[Expr] = exprParser
  lazy val identExprParser: PackratParser[Ident] = identParser ^^ { case id => Ident(id) }
  lazy val exprParser: PackratParser[BoolExpr] = {
    identExprParser |
    "true" ^^ { case _ => True } |
    "false" ^^ { case _ => False } |
    "(" ~ "not" ~> exprParser <~ ")" ^^ { case kid => Not(kid) } |
    "(" ~ "and" ~> rep(exprParser) <~ ")" ^^ { case kids => AndN(kids) } |
    "(" ~ "or" ~> rep(exprParser) <~ ")" ^^ { case kids => OrN(kids) } |
    "(" ~ "=" ~> rep(exprParser) <~ ")" ^^ { case kids => Eq(kids) } |
    "(" ~ "<"  ~> exprParser ~ exprParser <~ ")" ^^ { case lhs~rhs => Lt(lhs,rhs) } |
    "(" ~ "<=" ~> exprParser ~ exprParser <~ ")" ^^ { case lhs~rhs => Le(lhs,rhs) } |
    "(" ~ ">=" ~> exprParser ~ exprParser <~ ")" ^^ { case lhs~rhs => Ge(lhs,rhs) } |
    "(" ~ ">"  ~> exprParser ~ exprParser <~ ")" ^^ { case lhs~rhs => Gt(lhs,rhs) }
  }

  lazy val arithParser: PackratParser[ArithExpr] = {
    identParser ^^ { case id => Ident(id) } |
    bigIntParser ^^ { case value => IntConst(value) } |
    "(" ~ "/" ~> bigIntParser ~ bigIntParser <~ ")" ^^ { case num~den => RatConst(num,den) } |
    "(" ~ "+" ~> arithParser ~ arithParser <~ ")" ^^ { case lhs~rhs => Plus(lhs,rhs) } |
    "(" ~ "-" ~> arithParser ~ arithParser <~ ")" ^^ { case lhs~rhs => Minus(lhs,rhs) } |
    "(" ~ "-" ~> arithParser <~ ")" ^^ { case kid => UMinus(kid) } |
    "(" ~ "*" ~> arithParser ~ arithParser <~ ")" ^^ { case lhs~rhs => Mult(lhs,rhs) }
  }

  lazy val bigIntParser: PackratParser[BigInt] = {
    """[1-9][0-9]*""".r ^^ { case num => BigInt(num) } |
    "0" ^^ { case _ => BigInt("0") }
  }

  lazy val sortParser: PackratParser[Sort] = {
    identParser ^^ { case id => IdentSort(id) } |
    "(" ~> identParser ~ rep1(sortParser) <~ ")" ^^ {
      case id~sorts => NestedSort(id,sorts)
    }
  }


  def expr2Smt(expr: Expr, w: Writer) = expr writeTo w
  def ident2Smt(ident: Ident, w: Writer) = ident writeTo w
  def sort2Smt(sort: Sort, w: Writer) = sort writeTo w
  lazy val smt2Expr: PackratParser[Expr] = testExprParser
  lazy val smt2Ident: PackratParser[Ident] = identExprParser
  lazy val smt2Sort: PackratParser[Sort] = sortParser

  sealed trait ParseResult
  case class Succ(val msg: Messages.ToSmtsMsg)
  case class Fail(val msg: String)

  def parseCommand(s: String) =
    phrase(commandParser)(new PackratReader(new scala.util.parsing.input.CharSequenceReader(s))) match {
      case Success(result,next) => Succ(result)
      case Failure(msg,next) => Fail(msg + "\n" + next.pos.longString)
      case Error(msg,next) => Fail(msg + "\n" + next.pos.longString)
    }

}
