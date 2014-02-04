package smts.test

import java.io._

object Test extends App {
  val command = "z3 -smt2 -in"
  val solver = (new ProcessBuilder(command.split(" "): _*)).redirectErrorStream(true).start
  println("Successfully created solver.")
  solver.destroy
}
