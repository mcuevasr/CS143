# CS143
Semantic Analyzer and Code Generator for CS 143

This code implements that latter stages of a compiler for an object-oriented programming language called COOL which was written for the purpose of this class.

semant.cc includes the code for traversing a program which in an earlier phase been assembled into a syntax tree. This tree can be thought of as having an object of type 'Program' as the root which then has several child nodes of type 'Class'. 'Class' nodes then have child nodes themselves representing the variables and functions associated with each class.
Expressions are the building blocks of any function, variable declaration, or class. Each expression has a declared type which is simply stated in the declaration, as well as an inferred type. It is one of the main tasks of the semantic analyzer to examine the structure of the Expression object and infer what the type of the Expression should be as written in the COOL program. If the inferred type, does not agree with the declared type, a semantic error is thrown.

The entry point into this program is semant(), which performs the semantic analysis in several passes throught the syntax tree. 
  1) First collects all class names and gathers them into a class table, simultaneously ensuring there are no illegal inheritance cycles or any other illegal class definition      errors.
  2) Gather all declarations of class variables (referred to as Attributes) and methods into tables. Simultaneously check for any illegal declarations as well as any illegal            overriding of attributes or methods inherited from parent classes.
  3) Finally, check initialization of all variables and the return values of all methods to make sure that they match the declared type. The inferred type is obtained using the        TypeCheckExpression function on each different type of Expression object. The result of obtaining this inferred type is then compared against the declared type for agreement.


Code Generator for CS 143

This code generated assembly code for the COOL program that has been gathered into a syntax tree as explained in the semantic analyzer. The generated code can then be run on a MIPS simulator called SPIM.

The organization of this file can be most easily seen my looking at the code() function, where we will see that the code generation is separated into several stages:
  code_global_data - emit names and definitions that will be should be globally visible, such as class tables and method dispatch tables.
  code_select_gc - emit code that will specify settings for the garbage collection feature
  
  code_class_name_tab - emit list of all class name references
  code_obj_table - emit a list of references to prototype objects for each class
  code_prot_objs - emit code for the creation of default prototype objects that are used for undeclared variables
  code_disp_tabs - for each class, emit code for a table of pointers to code for each every method or variable declaration
  
  code_inits - emit code representing variable declarations
  code_methods - emit code for methods
  
  Once all of this code is emitted, the MIPS simulator can use it to run the program.
