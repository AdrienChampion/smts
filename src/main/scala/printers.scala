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

/** Provides the '''SmtLibPrinters''' trait. */
trait SmtsPrinters[Expr,Ident,Sort] extends SmtsCore[Expr, Ident, Sort] {

  /** This trait contains all the standard SMT lib 2 printers use in Smts. */
  trait SmtLibPrinters {

    // |=====| Settings.

    def printSetLogic(w: Writer, logic: logics.Logic) = {
      w write "(set-logic " ; logic writeTo w ; w write ")\n"
    }
    def printSetOption(w: Writer, option: String) = {
      w write "(set-option " ; w write option ; w write ")\n"
    }
    def printSetInfo(w: Writer, info: String, value: Option[String]) = {
      w write "(set-info " ; w write info
      value match {
        case Some(s) => { w write " " ; w write s }
        case None => ()
      }
      w write ")\n"
    }
    def printExit(w: Writer) = w write "(exit)\n"


    // |=====| Declarations and definitions.

    def printDeclareSort(w: Writer, dec: (Ident, Int)) = {
      w write "(declare-sort " ; ident2Smt(dec._1,w) ; w write " "
      w write dec._2.toString ; w write ")\n"
    }
    def printDefineSort(w: Writer, defi: (Sort, Traversable[Ident], Sort)) = {
      w write "(define-sort " ; sort2Smt(defi._1,w) ; w write " ("
      defi._2 foreach (id => { w write " " ; ident2Smt(id,w) })
      w write " ) " ; sort2Smt(defi._3,w) ; w write ")\n"
    }
    def printDeclareFun(w: Writer, dec: (Ident, Traversable[Sort], Sort)) = {
      w write "(declare-fun " ; ident2Smt(dec._1,w) ; w write " ("
      dec._2 foreach (sort => { w write " " ; sort2Smt(sort,w) })
      w write " ) " ; sort2Smt(dec._3,w) ; w write ")\n"
    }
    def printDefineFun(w: Writer, defi: (Ident, Traversable[(Ident, Sort)], Sort, Expr)) = {
      w write "(define-fun " ; ident2Smt(defi._1,w) ; w write " ("
      defi._2 foreach (pair => {
        w write " (" ; ident2Smt(pair._1,w) ; w write " " ; sort2Smt(pair._2,w) ; w write ")"
      })
      w write " ) " ; sort2Smt(defi._3,w) ; w write " " ; expr2Smt(defi._4,w) ; w write ")\n"
    }


    // |=====| Queries.

    def printCheckSat(w: Writer) = w write "(check-sat)\n"
    def printGetModel(w: Writer) = w write "(get-model)\n"
    def printGetValue(w: Writer, exprs: Traversable[Expr]) = {
      w write "(get-value ("
      exprs foreach (expr => { w write " (" ; expr2Smt(expr,w) ; w write ")" })
      w write " ))\n"
    }
    def printGetUnsatCore(w: Writer) = w write "(get-unsat-core)\n"


    // |=====| Other.

    def printAssert(w: Writer, expr: Expr, label: Option[Ident]) = {
      w write "(assert "
      label match {
        case Some(label) => {
          w write "(! " ; expr2Smt(expr,w) ; w write " :named "
          ident2Smt(label,w) ; w write ")"
        }
        case None => expr2Smt(expr,w)
      }
      w write ")\n"
    }
    def printPush(w: Writer, n: Int) = { w write "(push " ; w write n.toInt ; w write ")\n" }
    def printPop(w: Writer, n: Int) = { w write "(pop " ; w write n.toInt ; w write ")\n" }


    // |=====| Function handling the messages.

    /** Calls the proper print function base on the input message. */
    def writeMsg(msg: Messages.ToSmtsMsg, w: Writer) = {
      import Messages._
      msg match {
        case DummyMsg(msg) => { w write "Dummy[" ; w write msg ; w write "]\n" }
        case KillSolver => printExit(w)
        case SetOption(option) => printSetOption(w, option)
        case SetInfo(info,value) => printSetInfo(w, info, value)
        case SetLogic(logic) => printSetLogic(w, logic)
        case DeclareSort(decs) => decs foreach (dec => printDeclareSort(w, dec))
        case DefineSort(defs) => defs foreach (defi => printDefineSort(w, defi))
        case DeclareFun(decs) => decs foreach (dec => printDeclareFun(w, dec))
        case DefineFun(defs) => defs foreach (defi => printDefineFun(w, defi))
        case Push(n) => printPush(w, n)
        case Pop(n) => printPop(w, n)
        case CheckSat => printCheckSat(w)
        case GetModel => printGetModel(w)
        case GetValue(exprs) => printGetValue(w, exprs)
        case GetUnsatCore => printGetUnsatCore(w)
        case Assert(expr,label) => printAssert(w,expr,label)
        case msg => throw new Exception("Unexpected message: " + msg)
      }
      w.flush
    }

  }

}
