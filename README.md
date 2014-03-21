Smts
====

An actor-based scala library for interactions with [SMT-LIB
2](www.smtlib.org) compliant SMT solvers.

Smts (S-M-tees) allows to develop algorithms using SMT solvers without
worrying about which solvers is/are actually used.  Interactions are
asynchronous and follow the actor paradigm thanks to the Akka library.
Messages are consistent with the SMT-LIB 2 standard and most of them
correspond directly to SMT-LIB 2 commands.  The actual underlying
solver is ran in a separate process and smts communicates with it
using system pipes. Smts supports Z3, CVC4 and MathSat5. Which solvers
are used can be specified after compilation of the software thanks to
a simple configuration file.

Smts does not impose a data structure for expressions to the users.
They must provide their own as well as a printer for expressions, and
a parser for whatever kind of expressions will be used. For instance,
no parser is required to perform satisfiability checks as their is no
expression to parse. Smts handles the printing and parsing of
everything but expressions and sorts, consistently with the underlying
solver.

Features:
* Smts is parallel thanks to the Akka library.
* Users send messages corresponding to SMT-LIB 2 commands.
* Smts supports Z3, CVC4 and MathSat5. Examples are provided in
  src/test/scala/Actors.scala.
* dReal: support for the dReal solver. DReal HAS NO INTERACTIVE MODE
  and only supports checksats. After your CheckSat message, you should
  send an ExitSolver message to get the answer back. The solver then
  restarts and a new job can be sent. See src/test/scala/DReal.scala
  to see how to use it.
* No data structure imposed: users instantiate smts on their
  expression structure, to avoid useless translations.
* Free restarts: an smts instance can maintain two underlying solver,
  one active and one passive. When asked to restart, smts will restart
  the active solver and swap two solvers. As a result restarting an
  smts instance is seemingly free time-wise. Not that activating free
  restarts will use more memory.

* Solver hub (**not implemented yet**): a hub gathers several solvers
  to work concurrently on many jobs at once.


License
=======

Smts is released under the LGPL v3 license. Note that since the actual
solvers are ran in a separate process, neither smts or your software
is contaminated by the license of the solvers.
