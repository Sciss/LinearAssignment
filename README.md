# LinearAssignment

[![Build Status](https://travis-ci.org/Sciss/LinearAssignment.svg?branch=main)](https://travis-ci.org/Sciss/LinearAssignment)

## statement

LinearAssignment is a collection of linear assignment problem algorithms for the Scala programming language.
For now, these are mostly translations from the C code by Harold Cooper:

- https://github.com/hrldcpr/hungarian
- https://github.com/hrldcpr/pyLAPJV

which were published under MIT license. Scala translation and adaption
(C)opyright 2018–2020 by Hanns Holger Rutz. All rights reserved. The project is released under 
the [GNU Lesser General Public License](https://raw.github.com/Sciss/LinearAssignment/master/LICENSE) v2.1+ and 
comes with absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`

__Note:__ Jonker-Volgenant seems to be broken; it hangs or produced non-optimum results.

## requirements / installation

The project builds against Scala 2.13, 2.12 using sbt. Run the tests using `sbt test`.
