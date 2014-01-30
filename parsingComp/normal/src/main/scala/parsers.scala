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
  lazy val identParser: Parser[String] = {
    """[a-zA-Z+\-\/\*=%?!\.$_~&\^<>@][a-zA-Z0-9+\-\/\*=%?!\.$_~&\^<>@]*""".r ^^ {
      case id => id
    } |
    """[\|][^\\\|]*[\|]""".r ^^ { case id => id }
  }

  /** Parses an error. */
  lazy val errorParser: Parser[SolverError] =
    "(" ~ "error" ~ "\"" ~> """[^\"]*""".r <~ "\"" ~ ")" ^^ {
      case msg => SolverError(msg)
    }

  /** Function parameter parser. */
  lazy val paramParser: Parser[(Ident,Sort)] =
    "(" ~> smt2Ident ~ smt2Sort <~ ")" ^^ { case ident ~ sort => (ident,sort) }

  /** Definition parser. */
  lazy val defineParser: Parser[Binding] =
    "(" ~ "define-fun" ~> smt2Ident ~ "(" ~ rep(paramParser) ~ ")" ~ smt2Sort ~ smt2Expr <~ ")" ^^ {
      case id ~ _ ~ params ~ _ ~ sort ~ expr => Binding(id,sort,params,expr)
    }

  /** Model parser. */
  lazy val modelParser: Parser[Model] =
    "(" ~ "model" ~> rep(defineParser) <~ ")" ^^ {
      case funs => Model(funs)
    }

  /** Values parser. */
  lazy val valuesParser: Parser[Values] =
    "(" ~> rep(valueParser) <~ ")" ^^ { case values => {
      val map = values.foldLeft(
        new scala.collection.immutable.HashMap[Expr,Expr]
      )((map,pair) => map + pair)
      Values(map)
    }}

  /** Value parser. */
  lazy val valueParser: Parser[(Expr,Expr)] =
    "(" ~> smt2Expr ~ smt2Expr <~ ")" ^^ { case expr ~ value => (expr,value) }

  /** Unsat core parser. */
  lazy val unsatCoreParser: Parser[UnsatCore] =
    "(" ~> rep(identParser) <~ ")" ^^ { case ids => UnsatCore(ids) }

  /** Parses a check-sat result. */
  lazy val satParser: Parser[FromSmtsMsg] = {
    "sat" ^^ { case _ => Sat } |
    "unsat" ^^ { case _ => Unsat } |
    "unknown" ^^ { case _ => Unknown }
  }

  /** Parser used for check-sat results. */
  lazy val checkSatParser: Parser[FromSmtsMsg] = { satParser | errorParser }
  /** Parser used for get-model results. */
  lazy val getModelParser: Parser[FromSmtsMsg] = { modelParser | errorParser }
  /** Parser used for get-value results. */
  lazy val getValueParser: Parser[FromSmtsMsg] = { valuesParser | errorParser }
  /** Parser used for get-unsat-core results. */
  lazy val getUnsatCoreParser: Parser[FromSmtsMsg] = { unsatCoreParser | errorParser }


  /** Parser for any result, used for testing. */
  lazy val resultParser: Parser[FromSmtsMsg] = { errorParser | satParser | modelParser | unsatCoreParser | valuesParser }

}

/** This trait contains parsers for the smt lib 2 commands. Used for testing. */
trait SmtLibCommandParsers[Expr, Ident, Sort] extends SmtLibParsers[Expr, Ident, Sort] {
  import Messages._

  lazy val intParser: Parser[String] = {
    """[1-9][0-9]*""".r ^^ { case n => n } |
    "0" ^^ { case n => n }
  }

  lazy val realParser: Parser[String] = {
    intParser ~ "." ~ intParser ^^ { case int~_~dec => int + "." + dec } |
    intParser <~ "." ^^ { case int => int + "." } |
    "." ~> intParser ^^ { case dec => "."  + dec} |
    intParser ^^ { case res => res }
  }

  lazy val commandParser: Parser[ToSmtsMsg] = {
    "(" ~ "set-option" ~> """[:][a-zA-Z][a-zA-Z\-]""".r <~ ")" ^^ { case option => SetOption(option) } |
    "(" ~ "set-option" ~> """[:][a-zA-Z][a-zA-Z\-]""".r ~ ("true" | "false" | intParser) <~ ")" ^^ {
      case option~value => SetOption(option + " " + value)
    } |
    "(" ~ "set-info" ~> ":" ~ """[a-zA-Z][a-zA-Z0-9\-]*""".r ~ realParser <~ ")" ^^ {
      case c~info~value => SetInfo(c + info,Some(value))
    } |
    "(" ~ "set-info" ~> ":" ~ """[a-zA-Z][a-zA-Z0-9\-]*""".r ~ identParser <~ ")" ^^ {
      case c~info~value => SetInfo(c + info,Some(value))
    } |
    "(" ~ "set-info" ~> ":" ~ """[a-zA-Z][a-zA-Z0-9\-]*""".r ~ "\"" ~ identParser <~ "\"" ~ ")" ^^ {
      case c~info~_~value => SetInfo(c + info,Some("\"" + value + "\""))
    } |
    "(" ~ "set-info" ~> identParser <~ ")" ^^ {
      case info => SetInfo(info)
    } |
    "(" ~ "set-logic" ~> identParser <~ ")" ^^ { case logic => SetLogic(logic) } |
    "(" ~ "declare-sort" ~> smt2Ident ~ intParser <~ ")" ^^ {
      case id~arity => DeclareSort(id,arity.toInt)
    } |
    "(" ~ "define-sort" ~> smt2Sort ~ "(" ~ rep(smt2Ident) ~ ")" ~ smt2Sort <~ ")" ^^ {
      case id~_~sorts~_~sort => DefineSort(id,sorts,sort)
    } |
    "(" ~ "declare-fun" ~> smt2Ident ~ "(" ~ rep(smt2Sort) ~ ")" ~ smt2Sort <~ ")" ^^ {
      case id~_~sorts~_~sort => DeclareFun(id,sorts,sort)
    } |
    "(" ~ "define-fun" ~> smt2Ident ~ "(" ~ rep(paramParser) ~ ")" ~ smt2Sort ~ smt2Expr <~ ")" ^^ {
      case id~_~ids~_~sort~expr => DefineFun(id,ids,sort,expr)
    } |
    "(" ~ "push" ~> intParser <~ ")" ^^ { case n => Push(n.toInt) } |
    "(" ~ "pop" ~> intParser <~ ")" ^^ { case n => Pop(n.toInt) } |
    "(" ~ "check-sat" ~ ")" ^^ { case _ => CheckSat } |
    "(" ~ "get-model" ~ ")" ^^ { case _ => GetModel } |
    "(" ~ "get-value" ~ "(" ~> rep1(smt2Expr) <~ ")" ^^ { case values => GetValue(values) } |
    "(" ~ "get-unsat-core" ~ ")" ^^ { case _ => GetUnsatCore } |
    "(" ~ "assert" ~> smt2Expr <~ ")" ^^ { case expr => Assert(expr) } |
    "(" ~ "assert" ~ "(" ~ "!" ~> smt2Expr ~ ":named" ~ smt2Ident <~ ")" ^^ { case expr~_~id => Assert(expr,Some(id)) } |
    "(" ~ "exit" ~ ")" ^^ { case _ => KillSolver } |
    // Unsupported commands.
    "(" ~ "get-assertions" ~ ")" ^^ { case _ => DummyMsg("Unsupported command get-assertions.") } |
    "(" ~ "get-proof" ~ ")" ^^ { case _ => DummyMsg("Unsupported command get-proof.") } |
    "(" ~ "get-assignment" ~ ")" ^^ { case _ => DummyMsg("Unsupported command get-assgnment.") } |
    "(" ~ "get-option" ~> identParser <~ ")" ^^ { case option => DummyMsg("Unsupported command get-option (" + option + ").") } |
    "(" ~ "get-info" ~> identParser <~ ")" ^^ { case info => DummyMsg("Unsupported command get-info (" + info + ").") }
  }
}
