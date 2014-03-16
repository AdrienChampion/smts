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

/** Contains the logics and sorts provided by smts. */
trait LogicsAndSorts {

  /** Contains the logics used in Smts. */
  object Logics {

    /** Extended by all the logics. */
    trait Logic {
      import java.io.Writer
      /** The string representation of the logic. */
      def asString: String
      /** Prints the logic in the '''Writer'''. */
      def writeTo(w: Writer) = w write asString
    }

    /** Quantifier Free Uninterpreted Functions. */
    case object QF_UF extends Logic { val asString = "QF_UF" }

    /** Quantifier Free Linear Integer Arithmetic. */
    case object QF_LIA extends Logic { val asString = "QF_LIA" }

    /** Quantifier Free Non linear Integer Arithmetic. */
    case object QF_NIA extends Logic { val asString = "QF_NIA" }

    /** Quantifier Free Linear Real Arithmetic. */
    case object QF_LRA extends Logic { val asString = "QF_LRA" }

    /** Quantifier Free Arrays, arbitrary Uninterpreted sorts and Functions,
      * Linear Integer Arithmetic. */
    case object QF_AUFLIA extends Logic { val asString = "QF_AUFLIA" }

    /** Arrays, arbitrary Uninterpreted sorts and Functions, Linear Integer
      * Arithmetic. */
    case object AUFLIA extends Logic { val asString = "AUFLIA" }

    /** Arrays, arbitrary Uninterpreted sorts and Functions, Linear Integer and
      * Real Arithmetic. */
    case object AUFLIRA extends Logic { val asString = "AUFLIRA" }

    /** Arrays, arbitrary Uninterpreted sorts and Functions, Non linear Integer
      * and Real Arithmetic. */
    case object AUFNIRA extends Logic { val asString = "AUFNIRA" }

    /** Linear Real Arithmetic. */
    case object LRA extends Logic { val asString = "LRA" }

    /** Quantifier Free Integer Difference Logic. */
    case object QF_IDL extends Logic { val asString = "QF_IDL" }

    /** Quantifier Free Real Difference Logic. */
    case object QF_RDL extends Logic { val asString = "QF_RDL" }

    /** Quantifier Free Uninterpreted Functions Integer Difference Logic. */
    case object QF_UFIDL extends Logic { val asString = "QF_UFIDL" }

    /** Quantifier Free Bit_Vectors. */
    case object QF_BV extends Logic { val asString = "QF_BV" }

    /** Quantifier Free booleans, Arrays, arbitrary additional sorts and constants
      * (but no functions). */
    case object QF_AX extends Logic { val asString = "QF_AX" }

    /** Quantifier Free booleans, Arrays, Bit_Vectors, with all array index and
      * value sorts being bit_vector sorts. */
    case object QF_ABV extends Logic { val asString = "QF_ABV" }

    /** QF_BV with Arrays, arbitrary sorts and function symbols. */
    case object QF_AUFBV extends Logic { val asString = "QF_AUFBV" }

  }


  /** Objects for all the sorts provided by Smts. */
  object Sorts {

    /** Inherited by all the sorts. */
    trait Sort {
      import java.io.Writer
      /** The string representation of the sort */
      val asString: String
      /** Prints the sort in the '''Writer'''. */
      def writeTo(w: Writer) = w write asString
      override def toString() = asString
    }

    /** Bool sort. */
    case object BoolSort extends Sort { val asString = "Bool" }

    /** Int sort. */
    case object IntSort extends Sort { val asString = "Int" }

    /** Real sort. */
    case object RealSort extends Sort { val asString = "Real" }

    /** Fixed Size Bit Vector sort.
      * @param n The size of the bit vector. */
    case class FSBitVecSort(n: Int) extends Sort { val asString = "(_ BitVec " + n + ")" }

    /** Nested sort.
      * @param ident The ident of the custom sort.
      * @param sorts The sorts nested in the sort. */
    case class NestedSort(ident: String, sorts: Traversable[Sort]) extends Sort {
      val asString = "(" + ident + " (" + sorts.foldLeft("")((s,sort) => s + " " + sort.asString) + "))"
    }
  }

}
