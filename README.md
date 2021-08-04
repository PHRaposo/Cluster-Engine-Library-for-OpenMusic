# Cluster-Engine-Library-for-OpenMusic-and-OM#
Cluster Engine is a constraint solver for solving polyphonic constraint satisfaction problems where both the pitch and the rhythmic structure can be restricted by an arbitrary number of constraints (rules), and the solver then searches for a solution that is consistent with all constraints. This library supports user-defined rules, and highly flexible ways to control which aspects of the resulting score are controlled by certain rules. For example, you can independently control with compositional rules the melody and harmony of the music you generate. 

Cluster Engine is the successor of PWMC (Sandred, 2010). It was originally developed as a library for the free composition environment [[http://www2.siba.fi/pwgl][PWGL]]. 

The present version runs in OM and OM# and was ported by Paulo Raposo. Special thanks to Karim Haddad for the compiled version of the library.  

For others versions that runs in PWGL, but also on plain Common Lisp in order to make it useable within [[http://opusmodus.com][Opusmodus]] and -- via a [[http://www.sbcl.org][SBCL]] interface -- in the music and media programming environment [[https://cycling74.com/products/max][Max]] (Vincenot, 2017), see: 

https://github.com/tanders/cluster-engine


Cluster Engine has been successfully tested on PWGL (based on Lispworks), SBCL and Opusmodus ([[http://ccl.clozure.com][Clozure CL]]).

* Installation

Move the Cluster-Engine folder to the OM/Libraries/ folder (it's important to use this location to see all the tutorials pictures in OM).   


** References 

Sandred, Ö. (2010) PWMC, a Constraint-Solving System for Generating Music Scores. /Computer Music Journal/. 34(2), 8–24.

Vincenot, J. (2017) LISP in Max: Exploratory Computer-Aided Composition in Real-Time. /ICMC 2017/. 





