README File for Interpreter Project
=================================== 

Authors: Abir Varma
         Imoleayo Abel


The goal of this project was to design "bottom-up", an interprter in Racket that has the capability of 
ruunning in itself.


General Overview:
=================
* The interpreter:

	The interpreter simply represents a global envionment that acts as a mutable stack 
	of immutable frames - each containing mutable variable-value bindings. To represent
	this, different functions were written to mimic in-built Racket functions with the 
	excption of a few primitive functions that were used as-is

* To run the interpreter, the user enters (repl) on the Racket command line

* Implementation of Feautres:
  Most features were structured to include the following sub-functions:

	Tester: 	Procedure that checks if an input command is of the form of the 
			currently implemented feature
	Selector:	Procedure that selects arguments to the feature being implemented
			E.g: For (<feature> <arg1,arg2,..>), this methods returns the list 
			of arguments <arg1,arg2,...> of single argument for the case of one argment
	Evaluators:	Procedure that eventually implements the desired feature on an input command
			after it has passed the above 2 pre-processing stages. This procedure sometimes 
			calls other helper functions.


Success:
==========
* Our interpreter works as required with all required features and features in Extension-A working
  as required. Extensions-B and C were however not implemented.
* The interpreter was able to run inside of itself successfully. This meta-circularity feature was tested 
  up until a depth of 3 after which patience became a big issue.

