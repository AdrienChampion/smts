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

import java.io.Writer

import scala.util.parsing.combinator.{PackratParsers,RegexParsers}

/** Basic trait for Smts, contains only the '''smt2Expr''' and '''expr2Smt'''
  * functions.
  * @tparam Expr The user's type for the data structure.
  * @tparam Ident The user's type for identifiers.
  * @tparam Sort The user's type for sorts. */
trait SmtsCore[Expr, Ident, Sort] extends RegexParsers with PackratParsers {

  def printMaster(s: String) = println("[\033[31;1mMaster\033[0m] " + s)
  def printReader(s: String) = println("[\033[33;1mReader\033[0m] " + s)

  // |=====| User specified printer and parsers for the expression structure.

  /** Prints an '''Expr''' in the SMT lib 2 standard in the specified '''Writer'''. */
  def expr2Smt(expr: Expr, writer: Writer): Unit
  /** Prints an '''Ident''' in the SMT lib 2 standard in the specified '''Writer'''. */
  def ident2Smt(ident: Ident, writer: Writer): Unit
  /** Prints a '''Sort''' in the SMT lib 2 standard in the specified '''Writer'''. */
  def sort2Smt(sort: Sort, writer: Writer): Unit
  /** Parser for the smt lib 2 standard generating expressions of type '''Expr'''. */
  val smt2Expr: PackratParser[Expr]
  /** Parser for identifier. */
  val smt2Ident: PackratParser[Ident]
  /** Parser for sorts. */
  val smt2Sort: PackratParser[Sort]


  // |=====| Data structures.

  /** Trait gathering typed bindings and untyped bindings (mathsat does not
    * precise the sorts on models). */
  trait Binding {
    val ident: Ident ; val expr: Expr
  }

  /** Class for bindings (models).
    * @param ident The identifier of the function.
    * @param sort The sort of the function.
    * @param params The parameters of the function as pairs of identifiers and sorts.
    * @param expr The expression of the function. */
  case class TypedBinding(
    val ident: Ident, val sort: Sort,
    val params: Traversable[(Ident,Sort)], val expr: Expr
  ) extends Binding {
    override def toString() =
      "def " + ident.toString + "(" + params.foldLeft("")((s,p) =>
        if (p == params.head) s + p._1 + ": " + p._2
        else s + ", " + p._1 + ": " + p._2
      ) + "): " + sort + " = " + expr
  }

  /** Class for bindings (models).
    * @param ident The identifier of the function.
    * @param sort The sort of the function.
    * @param params The parameters of the function as pairs of identifiers and sorts.
    * @param expr The expression of the function. */
  case class UntypedBinding(
    val ident: Ident, val expr: Expr
  ) extends Binding {
    override def toString() = "def " + ident.toString + " = " + expr
  }


  // |=====| Messages.

  /** Contains all the messages used in Smts. */
  object Messages {

    /** Extended by all the messages used in Smts. */
    sealed trait SmtsMsg


    /** Extended by all the messages Smts can receive. */
    sealed trait ToSmtsMsg extends SmtsMsg

    /** Extended by all the messages producing a result. */
    sealed trait QueryMsg extends SmtsMsg

    /** Dummy message for testing purposes. */
    case class DummyMsg(msg: String) extends ToSmtsMsg

    /** Kills the underlying solver process and the Smts actors handling it. */
    object KillSolver extends ToSmtsMsg

    /** Restarts the underlying solver process. */
    object Restart extends ToSmtsMsg {
      import java.io.BufferedReader
      protected[smts] def br: Option[BufferedReader] = None
    }

    /** Set-option command. */
    case class SetOption(option: String) extends ToSmtsMsg

    /** Set-info command. */
    case class SetInfo(
      option: String, value: Option[String] = None
    ) extends ToSmtsMsg

    /** Set-logic command. */
    class SetLogic private(val logic: logics.Logic) extends ToSmtsMsg
    /** Companion object for '''SetLogic'''. */
    object SetLogic {
      /** Creates a '''SetLogic" message.
        * @param logic A string representing the logic, make sure it is legal. */
      def apply(logic: String) = new SetLogic(new logics.Logic { val asString = logic })
      /** Creates a '''SetLogic" message.
        * @param logic One of the logics provided by Smts. */
      def apply(logic: logics.Logic) = new SetLogic(logic)
      def unapply(arg: SetLogic) = Some(arg.logic)
    }

    /** Declare-sort command. */
    class DeclareSort private(
      val decs: Traversable[(Ident,Int)]
    ) extends ToSmtsMsg
    /** Companion object for '''DeclareSort'''. */
    object DeclareSort {
      def apply(decs: Traversable[(Ident, Int)]) = new DeclareSort(decs)
      def apply(dec: (Ident, Int)) = new DeclareSort(dec :: Nil)
      def apply(id: Ident, arity: Int) = new DeclareSort((id,arity) :: Nil)
      def unapply(arg: DeclareSort) = Some(arg.decs)
    }

    /** Define-sort command. */
    class DefineSort private(
      val defs: Traversable[(Sort, Traversable[Ident], Sort)]
    ) extends ToSmtsMsg
    /** Companion object for '''DefineSort'''. */
    object DefineSort {
      def apply(defs: Traversable[(Sort, Traversable[Ident], Sort)]) = new DefineSort(defs)
      def apply(defi: (Sort, Traversable[Ident], Sort)) = new DefineSort(defi :: Nil)
      def apply(name: Sort, ids: Traversable[Ident], sort: Sort) = new DefineSort((name,ids,sort) :: Nil)
      def unapply(arg: DefineSort) = Some(arg.defs)
    }

    /** Declare-fun command. */
    class DeclareFun private(
      val decs: Traversable[(Ident, Traversable[Sort], Sort)]
    ) extends ToSmtsMsg
    /** Companion object for '''DeclareFun'''. */
    object DeclareFun {
      def apply(decs: Traversable[(Ident, Traversable[Sort], Sort)]) = new DeclareFun(decs)
      def apply(dec: (Ident, Traversable[Sort], Sort)) = new DeclareFun(dec :: Nil)
      def apply(ident: Ident, paramSort: Traversable[Sort], sort: Sort) =
        new DeclareFun((ident,paramSort,sort) :: Nil)
      def unapply(arg: DeclareFun) = Some(arg.decs)
    }

    /** Define-fun command. */
    class DefineFun private(
      val defs: Traversable[(Ident, Traversable[(Ident,Sort)], Sort, Expr)]
    ) extends ToSmtsMsg
    /** Companion object for '''DefineFun'''. */
    object DefineFun {
      def apply(defs: Traversable[(Ident, Traversable[(Ident,Sort)], Sort, Expr)]) =
        new DefineFun(defs)
      def apply(ident: Ident, params: Traversable[(Ident,Sort)], sort: Sort, expr: Expr) =
        new DefineFun((ident,params,sort,expr) :: Nil)
      def unapply(arg: DefineFun) = Some(arg.defs)
    }

    /** Push command. */
    case class Push(n: Int) extends ToSmtsMsg
    /** Pop command. */
    case class Pop(n: Int) extends ToSmtsMsg


    // |=====| Queries.

    /** Check sat command. */
    object CheckSat extends ToSmtsMsg with QueryMsg
    /** Get model command. */
    object GetModel extends ToSmtsMsg with QueryMsg
    /** Get value command. */
    case class GetValue(val exprs: Traversable[Expr]) extends ToSmtsMsg with QueryMsg
    /** Get unsat core command. */
    object GetUnsatCore extends ToSmtsMsg with QueryMsg

    /** Assert command. */
    case class Assert(
      val expr: Expr, val label: Option[Ident] = None
    ) extends ToSmtsMsg


    /** Extended by all the messages Smts can send. */
    sealed trait FromSmtsMsg extends SmtsMsg

    /** Success message. Internal, never actually sent to anyone. */
    case object SuccessMsg extends FromSmtsMsg { override def toString() = "Success" }
    /** Unknown result. */
    case object Unknown extends FromSmtsMsg { override def toString() = "Unknown" }
    /** Unsupported result. */
    case object Unsupported extends FromSmtsMsg { override def toString() = "Unsupported" }
    /** Unsupported result. */
    case object SolverClosed extends FromSmtsMsg { override def toString() = "Solver closed" }
    /** Time out. */
    case object Timeout extends FromSmtsMsg { override def toString() = "Timeout" }

    /** Sat result. */
    case object Sat extends FromSmtsMsg { override def toString() = "Sat" }
    /** Unsat result. */
    case object Unsat extends FromSmtsMsg { override def toString() = "Unsat" }

    /** Get-model result.
      * @param model The model asked by a previous get-model message. */
    case class Model(val model: Traversable[Binding]) extends FromSmtsMsg {
      override def toString() = "Model " + model
    }

    /** Get-unsat-core result.
      * @param core The labels of the elements of the unsat core. */
    case class UnsatCore(val core: Traversable[String]) extends FromSmtsMsg {
      override def toString() = "Unsat core: " + core
    }

    /** Get-values result.
      * @param values A map from expressions to values. */
    case class Values(val values: Map[Expr,Expr]) extends FromSmtsMsg {
      override def toString() = "Values: " + values
    }

    /** Error result.
      * @param msg The text of the error. */
    case class SolverError(val msgs: Traversable[String]) extends FromSmtsMsg {
      override def toString() = "Solver error: {" + msgs + "}"
    }
  }

}
