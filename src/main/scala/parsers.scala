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

/** Provides the '''SmtLibParsers''' and '''SmtLibCommandParsers''' traits. */
trait SmtsParsers[Expr,Ident,Sort] extends SmtsCore[Expr,Ident,Sort] {

  /** This trait contains all the standard SMT lib 2 parsers used in Smts. */
  trait SmtLibParsers {
    import Messages._

    /** Parses an smt lib 2 identifier (used mostly for unsat cores). */
    lazy val identParser: PackratParser[String] = {
      """[a-zA-Z+\-\/\*=%?!\.$_~&\^<>@][a-zA-Z0-9+\-\/\*=%?!\.$_~&\^<>@]*""".r ^^ {
        case id => id
      } |
      """[\|][^\\\|]*[\|]""".r ^^ { case id => id }
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
      "unsat" ^^ { case _ => Unsat }
    }

    /** Parses unknown. */
    lazy val unknownParser: PackratParser[FromSmtsMsg] =
      "unknown" ^^ { case _ => Unknown }
    /** Parses unsupported. */
    lazy val unsupportedParser: PackratParser[FromSmtsMsg] =
      "unsupported" ^^ { case _ => Unsupported }

    /** Parses a timeout. */
    lazy val timeoutParser: PackratParser[FromSmtsMsg] = "timeout" ^^{ _ => Timeout }

    /** Parses an error. */
    lazy val errorParser: PackratParser[SolverError] =
      "(" ~ "error" ~ "\"" ~> """[^\"]*""".r <~ "\"" ~ ")" ^^ {
        case msg => SolverError("The solver output an error:" :: msg :: Nil)
      }

    /** Parses error, unknown, unsupported, and timeout. */
    lazy val failureParser: PackratParser[FromSmtsMsg] = {
      errorParser | unknownParser | unsupportedParser | timeoutParser
    }

    /** Parses the string "success". */
    lazy val successParser: PackratParser[FromSmtsMsg] = "success" ^^ {
      case _ => Messages.SuccessMsg
    }
    /** Parser used for check-sat results. */
    lazy val checkSatParser: PackratParser[FromSmtsMsg] = {
      satParser | failureParser | timeoutParser
    }
    /** Parser used for get-model results. */
    lazy val getModelParser: PackratParser[FromSmtsMsg] = {
      modelParser | failureParser | timeoutParser
    }
    /** Parser used for get-value results. */
    lazy val getValueParser: PackratParser[FromSmtsMsg] = {
      valuesParser | failureParser | timeoutParser
    }
    /** Parser used for get-unsat-core results. */
    lazy val getUnsatCoreParser: PackratParser[FromSmtsMsg] = {
      unsatCoreParser | failureParser | timeoutParser
    }

    protected def getParser(msg: ToSmtsMsg): PackratParser[FromSmtsMsg] = msg match {
      case CheckSat => checkSatParser
      case GetModel => getModelParser
      case GetValue(exprs) => getValueParser
      case GetUnsatCore => getUnsatCoreParser
      case _ => successParser
    }


    /** Parser for any result, used for testing. */
    lazy val resultParser: PackratParser[FromSmtsMsg] = {
      failureParser | satParser | modelParser | unsatCoreParser | valuesParser
    }

  }

  /** This trait contains parsers for the smt lib 2 commands. Used for testing. */
  trait SmtLibCommandParsers extends SmtLibParsers {
    import Messages._

    lazy val intParser: PackratParser[String] = {
      """[1-9][0-9]*""".r ^^ { case n => n } |
      "0" ^^ { case n => n }
    }

    lazy val realParser: PackratParser[String] = {
      intParser ~ "." ~ intParser ^^ { case int~_~dec => int + "." + dec } |
      intParser <~ "." ^^ { case int => int + "." } |
      "." ~> intParser ^^ { case dec => "."  + dec} |
      intParser ^^ { case res => res }
    }

    lazy val commandParser: PackratParser[ToSmtsMsg] = {
      "(" ~ "set-option" ~> """[:][a-zA-Z][a-zA-Z\-]""".r <~ ")" ^^ {
        case option => SetOption(option)
      } |
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
}
