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

/** This trait contains all the standard SMT lib 2 parsers used in Smts. */
trait SmtLibParsers[Expr, Ident, Sort] extends SmtsCore[Expr, Ident, Sort] {

  import Messages._

  /** Parses an smt lib 2 identifiers (used mostly for unsat cores). */
  lazy val identParser: PackratParser[String] = {
    """[a-zA-Z+\-\/\*=%?!\.$_~&\^<>@][a-zA-Z0-9+\-\/\*=%?!\.$_~&\^<>@]*""".r ^^ {
      case id => id
    } |
    """[\|][^\\\|]*[\|]""".r ^^ { case id => id }
  }

  /** Parses an error. */
  lazy val errorParser: PackratParser[SolverError] =
    "(" ~ "error" ~ "\"" ~> """[^\"]*""".r <~ "\"" ~ ")" ^^ {
      case msg => SolverError(msg)
    }

  /** Function parameter parser. */
  lazy val paramParser: PackratParser[(Ident,Sort)] =
    "(" ~> smt2Ident ~ smt2Sort <~ ")" ^^ { case ident ~ sort => (ident,sort) }

  /** Definition parser. */
  lazy val defineParser: PackratParser[Binding] =
    "(" ~ "define-fun" ~> smt2Ident ~ "(" ~ rep(paramParser) ~ ")" ~ smt2Sort ~ smt2Expr <~ ")" ^^ {
      case id ~ _ ~ params ~ _ ~ sort ~ expr => Binding(id,sort,params,expr)
    }

  /** Model parser. */
  lazy val modelParser: PackratParser[Model] =
    "(" ~ "model" ~> rep(defineParser) <~ ")" ^^ {
      case funs => Model(funs)
    }

  /** Values parser. */
  lazy val valuesParser: PackratParser[Values] =
    "(" ~> rep(valueParser) <~ ")" ^^ { case values => {
      val map = values.foldLeft(
        new scala.collection.immutable.HashMap[Expr,Expr]
      )((map,pair) => map + pair)
      Values(map)
    }}

  /** Value parser. */
  lazy val valueParser: PackratParser[(Expr,Expr)] =
    "(" ~> smt2Expr ~ smt2Expr <~ ")" ^^ { case expr ~ value => (expr,value) }

  /** Unsat core parser. */
  lazy val unsatCoreParser: PackratParser[UnsatCore] =
    "(" ~> rep(identParser) <~ ")" ^^ { case ids => UnsatCore(ids) }

  /** Parses a check-sat result. */
  lazy val satParser: PackratParser[FromSmtsMsg] = {
    "sat" ^^ { case _ => Sat } |
    "unsat" ^^ { case _ => Unsat } |
    "unknown" ^^ { case _ => Unknown }
  }

  /** Parser used for check-sat results. */
  lazy val checkSatParser: PackratParser[FromSmtsMsg] = { satParser | errorParser }
  /** Parser used for get-model results. */
  lazy val getModelParser: PackratParser[FromSmtsMsg] = { modelParser | errorParser }
  /** Parser used for get-value results. */
  lazy val getValueParser: PackratParser[FromSmtsMsg] = { valuesParser | errorParser }
  /** Parser used for get-unsat-core results. */
  lazy val getUnsatCoreParser: PackratParser[FromSmtsMsg] = { unsatCoreParser | errorParser }


  /** Parser for any result, used for testing. */
  lazy val resultParser: PackratParser[FromSmtsMsg] = { errorParser | satParser | modelParser | unsatCoreParser | valuesParser }

}
