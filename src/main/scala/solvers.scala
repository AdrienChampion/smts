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

/** Trait gathering the solver information for the underlying solvers. */
trait SmtsSolvers[Expr,Ident,Sort]
extends SmtsCore[Expr,Ident,Sort]
with SmtsPrinters[Expr,Ident,Sort]
with SmtsParsers[Expr,Ident,Sort] {

  // |=====| Solver info.

  /** Extended by all the solver classes. */
  sealed trait SolverInfo extends SmtLibParsers {
    import Messages.{ToSmtsMsg,SetOption}

    /** The command launching the solver. */
    val command: String
    /** If true Smts will parse for success (default '''false'''). */
    val success: Boolean
    /** If true model generation will be activated in the solver
      * (default '''false'''). */
    val models: Boolean
    /** If true unsat core generation will be activated in the solver
      * (default '''false'''). */
    val unsatCores: Boolean
    /** Allows to specify a timeout in milliseconds (default '''None'''). */
    val timeout: Option[Int]
    /** Allows to specify a timeout in milliseconds for each query to the solver
      * (default '''None'''. */
    val timeoutQuery: Option[Int]
    /** Commands the solver will always be launched with (default '''Nil'''). */
    val initWith: List[ToSmtsMsg]

    /** Standard smts name of the solver. */
    val name: String

    /** Commands to write when launching the solver. */
    val startMsgs: List[ToSmtsMsg] = {
      val withCores =
        if (unsatCores) SetOption(":produce-unsat-cores true") :: initWith
        else initWith
      val withModels =
        if (models) SetOption(":produce-models true") :: withCores
        else withCores
      if (success) SetOption(":print-success true") :: withModels else withModels
    }

    /** Converts an integer in milliseconds to its string representation in
      * seconds. */
    def millis2secs(millis: Int) = {
      val milliString = millis.toString
      milliString.size match {
        case n if n > 3 => milliString.take(n-3)
        case n => "0." + ("0" * (n-3)) + milliString
      }
    }
  }

  /** Solver information class for Z3. */
  case class Z3(
    val success: Boolean = false,
    val models: Boolean = false,
    val unsatCores: Boolean = false,
    val baseCommand: String = "z3 -in -smt2",
    val initWith: List[Messages.ToSmtsMsg] = Nil,
    val timeout: Option[Int] = None,
    val timeoutQuery: Option[Int] = None
  ) extends SolverInfo {
    val command = baseCommand + ( timeout match {
      case None => ""
      case Some(to) => " -T:" + millis2secs(to)
    }) + (timeoutQuery match {
      case None => ""
      case Some(to) => " -t:" + millis2secs(to)
    })
    val name = "z3"
    override def toString = name
  }

  /** Solver information class for mathsat. */
  case class MathSat5(
    val success: Boolean = false,
    val models: Boolean = false,
    val unsatCores: Boolean = false,
    val baseCommand: String = "mathsat",
    val initWith: List[Messages.ToSmtsMsg] = Nil,
    val timeout: Option[Int] = None,
    val timeoutQuery: Option[Int] = None
  ) extends SolverInfo {
    val command = baseCommand
    val name = "mathsat5"
    override def toString = name

    /** MathSat has a different convention for get-model answers. */
    override lazy val modelParser: PackratParser[Messages.Model] =
      "(" ~> rep(defineParser) <~ ")" ^^ {
        case funs => Messages.Model(funs)
      }

    /** MathSat has a different convention for model definition. */
    override lazy val defineParser: PackratParser[Binding] =
      "(" ~> smt2Ident ~ smt2Expr <~ ")" ^^ {
        case id ~ expr => UntypedBinding(id,expr)
      }
  }

  /** Solver information class for CVC4. Unsat cores deactivated. */
  case class CVC4(
    val success: Boolean = false,
    val models: Boolean = false,
    val baseCommand: String = "cvc4 -q --lang=smt",
    val initWith: List[Messages.ToSmtsMsg] = Nil,
    val timeout: Option[Int] = None,
    val timeoutQuery: Option[Int] = None
  ) extends SolverInfo {
    val command = baseCommand + ( timeout match {
      case None => ""
      case Some(to) => " --tlimit=" + to
    }) + (timeoutQuery match {
      case None => ""
      case Some(to) => " --tlimit-per=" + to
    })
    val unsatCores: Boolean = false
    val name = "cvc4"
    override def toString = name
  }

}
