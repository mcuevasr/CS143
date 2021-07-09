#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <list>

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;

static ClassTable *classtable;
static SymbolTable<Symbol, Symbol> attr_table;
static SymbolTable<Symbol, Feature_class> method_table;
static std::map<Symbol, SymbolTable<Symbol, Feature_class>*> method_tables;
static std::map<Symbol, SymbolTable<Symbol, Symbol>*> attr_tables;
static Class_ current_class = NULL;
static Class_ current_ancestor = NULL;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

Symbol class__class::get_parent() {return parent;}
Symbol class__class::get_name() {return name;}
Features class__class::get_features() { return features;}

bool method_class::isMethod() {return true;}
bool method_class::isAttribute() {return false;}
bool attr_class::isMethod() {return false;}
bool attr_class::isAttribute() {return true;}

Symbol attr_class::get_type_decl() {return type_decl;}
Symbol attr_class::get_name() {return name;}
Symbol method_class::get_name() {return name;}
Symbol method_class::get_type_decl() {return return_type;}
Formals method_class::get_formals() {return formals;}
Formals attr_class::get_formals() {return NULL;}

Expression attr_class::get_init() { return init; }
Expression method_class::get_init() { return expr;}

/* Constructor for a ClassTable object, which will represent the collection of 
all classes specified in the program.
The 'classes' parameter represents a list of class of objects that have been
previously gathered. 
This function will loop through the list and insert into a class map
after checking that there are no duplicate/illegal class names and that
no class inherits from an illegal type.
Finally, this constructor will loop through the gathered classes and 
detect any illegal cyclical inheritance.
*/

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) 
{
    install_basic_classes();
    /* add all classes to map while checking for non-inheritance errors */
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      current_class = classes->nth(i); 
      Symbol current_name = current_class->get_name();
   
      if(current_name == SELF_TYPE) {
        semant_error(current_class) 
	  << "Redefinition of basic class SELF_TYPE." << endl;
      }     
      if(classes->nth(i)->get_parent() == SELF_TYPE) {
        semant_error(current_class) << "Class " << current_name 
	  << " cannot inherit class SELF_TYPE." << endl;
      }
      if(classes->nth(i)->get_parent() == Int 
	 || classes->nth(i)->get_parent() == Str
	 || classes->nth(i)->get_parent() == Bool) 
      {
        semant_error(current_class) << "Class "
	  << classes->nth(i)->get_name() << " cannot inherit class "
	  << classes->nth(i)->get_parent() << "." << endl;
      }

      if(classes->nth(i)->get_parent() == current_name) {
        semant_error(current_class) << "Class " << current_name 
          << " or an ancestor of " << current_name 
	  << " is involved in an inheritance cycle." << endl;
      }

      // insert class only if it hasn't already been defined
      if(class_map.find(current_name) == class_map.end()) {
        class_map.insert(std::make_pair(current_name, current_class));
	method_tables.insert(std::make_pair(current_name, new SymbolTable<Symbol, Feature_class>()));
	attr_tables.insert(std::make_pair(current_name, new SymbolTable<Symbol, Symbol>()));
      } else {
        semant_error(current_class) << "Class " << current_name << " was previously defined."  <<endl;
      }
    }// end for-loop for inserting all classes
 
    if(class_map.find(Main) == class_map.end()) {
      semant_error() << "Class Main is not defined." << endl;
    }

    //now check all classes to see if any ancestors are equal to themselves
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      Class_ current_class = classes->nth(i);
      Symbol original_name = current_class->get_name();
      Symbol current_parent = current_class->get_parent();

      while(current_parent != Object) {
      
	if(class_map.find(current_parent) == class_map.end()) {
          semant_error(current_class) << "Class " << current_class->get_name() 
	    << " inherits from an undefined class " << current_parent << "." << endl; 
	  break;
	}

	if(current_parent == original_name) {
          semant_error(current_class) << "Class " << original_name 
	    << " is involved in an inheritance cycle." << endl;
	  break;
	}

	current_class = class_map[current_parent];
	current_parent = current_class->get_parent();
      } 

    }
}

/*
Returns a hierarchical representation of all of a class's ancestors,
with the class 'Object' being the default class from which all class types
inherit from.
*/

std::list<Symbol> ClassTable::InheritancePath(Symbol name) {
  std::list<Symbol> path;

  if(name == SELF_TYPE) name = current_class->get_name();
  
  path.push_front(name);
  while(name != Object) { 
    name = class_map[name]->get_parent(); 
    path.push_front(name);
  }
  return path;
}

/*
Ensure that child inherits from parent as specified during the initial
class table construction.
*/

bool ClassTable::CheckConformance(Symbol child, Symbol parent) {
  while(child != No_class) {
    if(child == parent) return true;
    child = class_map.find(child)->second->get_parent();
  }
  return false;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    class_map.insert(std::make_pair(Object, Object_class));
    class_map.insert(std::make_pair(IO, IO_class));
    class_map.insert(std::make_pair(Int, Int_class));
    class_map.insert(std::make_pair(Bool, Bool_class));
    class_map.insert(std::make_pair(Str, Str_class));
method_tables.insert(std::make_pair(Object, new SymbolTable<Symbol, Feature_class>()));
method_tables.insert(std::make_pair(IO, new SymbolTable<Symbol, Feature_class>()));
method_tables.insert(std::make_pair(Int, new SymbolTable<Symbol, Feature_class>()));
method_tables.insert(std::make_pair(Bool, new SymbolTable<Symbol, Feature_class>()));
method_tables.insert(std::make_pair(Str, new SymbolTable<Symbol, Feature_class>()));
attr_tables.insert(std::make_pair(Object, new SymbolTable<Symbol, Symbol>()));
attr_tables.insert(std::make_pair(IO, new SymbolTable<Symbol, Symbol>()));
attr_tables.insert(std::make_pair(Int, new SymbolTable<Symbol, Symbol>()));
attr_tables.insert(std::make_pair(Bool, new SymbolTable<Symbol, Symbol>()));
attr_tables.insert(std::make_pair(Str, new SymbolTable<Symbol, Symbol>()));


}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

bool list_contains(std::list<Symbol> list, Symbol symbol) {
  for(std::list<Symbol>::iterator it = list.begin(); it != list.end(); it++) {
    if((*it) == symbol) return true;
  }
  return false;
}

Symbol get_LCA(std::map<Symbol, Expression>* branch_map) {

  //select a random branch and use its inheritance for finding LCA
  Symbol lca_candidate = branch_map->begin()->first; 

  bool found_lca = false;
  while(!found_lca) {
    //check every branch to see if lca_candidate is in their inheritance path
    for(std::map<Symbol, Expression>::iterator map_iter = branch_map->begin();
        map_iter != branch_map->end(); map_iter++) 
    {
      std::list<Symbol> path = classtable->InheritancePath(map_iter->first);
      if(!list_contains(path, lca_candidate)) {

        lca_candidate = classtable->class_map.find(lca_candidate)->second->get_parent();
	found_lca = false;
	break;
      } else {
        found_lca = true;
      }
    }
  }
  return lca_candidate;
}

Symbol assign_class::TypeCheckExpression() {
  Symbol lvalue_type = NULL;
  if(name == self) {
    classtable->semant_error(current_class->get_filename(), this) 
      <<"Cannot assign to 'self'." << endl;
    lvalue_type = SELF_TYPE;
  }
  if(attr_tables[current_class->get_name()]->lookup(name)== NULL && name != self) {
    classtable->semant_error(current_class->get_filename(), this) << 
      "Assignment to undeclared variable " << name << "." << endl;
  } else {
    if(attr_tables[current_class->get_name()]->lookup(name) != NULL) 
      lvalue_type = *(attr_tables[current_class->get_name()]->lookup(name));
  }
  Symbol rvalue_type = expr->TypeCheckExpression();
  if (rvalue_type == SELF_TYPE) rvalue_type = current_class->get_name();

  if(name == self || !classtable->CheckConformance(rvalue_type,lvalue_type)) {
    if(attr_tables[current_class->get_name()]->lookup(name) != NULL) { 
      classtable->semant_error(current_class->get_filename(), this) 
        <<"Type " << expr->get_type() 
        << " of assigned expression does not conform to declared type " 
        << lvalue_type << " of identifier " << name << "." << endl;
    }
    type = Object;   
    return type;
  }

  type = expr->get_type();
  return type;
}

Symbol static_dispatch_class::TypeCheckExpression() {
  if (type_name == SELF_TYPE) {
    classtable->semant_error(current_class->get_filename(), actual->nth(actual->first())) 
      << "Static dispatch to SELF_TYPE."
      << endl;
    type = Object;
    return type;
  }
  Symbol expr_type = expr->TypeCheckExpression();

  if(classtable->class_map.find(type_name) == classtable->class_map.end()) {
    classtable->semant_error(current_class->get_filename(), this)
      << "Static dispatch to undefined class " << type_name << "." << endl;
    type = Object;
    return type; 
  }

  if(expr_type == SELF_TYPE) expr_type = current_class->get_name(); 

  if(classtable->class_map.find(expr_type) != classtable->class_map.end()
     && !classtable->CheckConformance(expr_type, type_name)) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Expression type " << expr->get_type() 
      << " does not conform to declared static dispatch type " << type_name
      << "." << endl;
    type = Object;
    return type;
  } 
 
  if(method_tables[type_name]->lookup(name) == NULL) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Static dispatch to undefined method " << name << "." << endl;
      type = Object;
      return type;
  }
  Formals formals = (method_tables[type_name]->lookup(name))->get_formals();

  if(formals->len() != actual->len()) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Method " << name
      << " invoked with wrong number of arguments." << endl;
  
    type = (method_tables[type_name]->lookup(name))->get_type_decl(); 
    return type; 
  }

  int i1 = formals->first();
  int i2 = actual->first();

  for(; formals->more(i1) && actual->more(i2);
	i1 = formals->next(i1), i2 = actual->next(i2)) 
  {
    Symbol actual_type = actual->nth(i2)->TypeCheckExpression();
    if(actual_type == SELF_TYPE) actual_type = current_class->get_name();
    if(!classtable->CheckConformance(actual_type, formals->nth(i1)->get_type())) 
    {
      classtable->semant_error(current_class->get_filename(), this) 
	<< "In call of method "
        << name << ", type " << actual->nth(i2)->get_type() 
        << " of parameter " << formals->nth(i1)->get_name() 
	<< " does not conform to declared type " << formals->nth(i1)->get_type()
	<< "." << endl;
    }     
  }

  type = (method_tables[type_name]->lookup(name))->get_type_decl(); 
  if(type==SELF_TYPE) { 
    if(expr->get_type() != SELF_TYPE) type = expr->get_type(); 
  }
  return type; 
}

Symbol dispatch_class::TypeCheckExpression() { 
  Symbol expr_type = expr->TypeCheckExpression();
 
  if(expr_type == SELF_TYPE) expr_type = current_class->get_name(); 

  if(classtable->class_map.find(expr_type) == classtable->class_map.end()) {
    classtable->semant_error(current_class->get_filename(), this)
      << "Dispatch on undefined class " << expr_type << "." << endl;
    type = Object;
    return type;
  }

  if(method_tables[expr_type]->lookup(name) == NULL) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Dispatch to undefined method " 
      << name << "." << endl;
    type = Object;
    return type;
  }

  Formals formals = (method_tables[expr_type]->lookup(name))->get_formals();
  if(formals->len() != actual->len()) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Method " << name
      << " called with wrong number of arguments." << endl;
    type = (method_tables[expr_type]->lookup(name))->get_type_decl(); 
    return type;
  }

  int i1 = formals->first();
  int i2 = actual->first();

  for(; formals->more(i1) && actual->more(i2);
	i1 = formals->next(i1), i2 = actual->next(i2)) 
  {
    Symbol actual_type = actual->nth(i2)->TypeCheckExpression();
    if(actual_type == SELF_TYPE) actual_type = current_class->get_name();
    if(!classtable->CheckConformance(actual_type, formals->nth(i1)->get_type())) 
    {
      classtable->semant_error(current_class->get_filename(), this) 
	<< "In call of method "
        << name << ", type " << actual->nth(i2)->get_type() 
        << " of parameter " << formals->nth(i1)->get_name() 
	<< " does not conform to declared type " << formals->nth(i1)->get_type()
	<< "." << endl;
    }     
  }
  type = (method_tables[expr_type]->lookup(name))->get_type_decl(); 
  if(type==SELF_TYPE) { 
    if(expr->get_type() != SELF_TYPE) type = expr->get_type(); 
  }
  return type; 
}
Symbol cond_class::TypeCheckExpression() { 
  if(pred->TypeCheckExpression() != Bool) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Predicate of 'if' does not have type Bool." << endl; 
  }
  std::map<Symbol, Expression>* branch_map = new std::map<Symbol, Expression>();

  Symbol then_type = then_exp->TypeCheckExpression();
  if(then_type == SELF_TYPE) then_type = current_class->get_name();
  Symbol else_type = else_exp->TypeCheckExpression();
  if(else_type == SELF_TYPE) else_type = current_class->get_name();

  if(classtable->class_map.find(then_type) != classtable->class_map.end())
    branch_map->insert(std::make_pair(then_type, then_exp)); 
  if(classtable->class_map.find(else_type) != classtable->class_map.end())
    branch_map->insert(std::make_pair(else_type, else_exp)); 

  if(branch_map->size()==0) type = then_type;// if all inferred types undefined 
  else type = get_LCA(branch_map);
  if(then_exp->get_type() == SELF_TYPE && else_exp->get_type() == SELF_TYPE)         type = SELF_TYPE;
  return type; 
}

Symbol loop_class::TypeCheckExpression() { 
  if(pred->TypeCheckExpression() != Bool) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Loop condition does not have type Bool." << endl; 
  }
  body->TypeCheckExpression();
  type = Object;
  return type; 
}
Symbol typcase_class::TypeCheckExpression() {
  bool all_self_type = true;	
  Symbol expr0_type = expr->TypeCheckExpression();
  if(expr0_type == SELF_TYPE) expr0_type = current_class->get_name();
  
  std::map<Symbol,Expression>*dec_branch_map = new std::map<Symbol,Expression>();
  std::map<Symbol,Expression>*inf_branch_map = new std::map<Symbol,Expression>();
  for(int i = cases->first(); cases->more(i); i = cases->next(i)){
    Case cur_case = cases->nth(i);
    if(cur_case->get_type() == SELF_TYPE) {
      classtable->semant_error(current_class->get_filename(), cur_case) 
	<< "Identifier " 
        << cur_case->get_name() <<" declared with type SELF_TYPE in case branch."
	<< endl;
    }
    Symbol dec_branch_type = cur_case->get_type(); 
    if(dec_branch_map->find(dec_branch_type) != dec_branch_map->end()) {
      classtable->semant_error(current_class->get_filename(), cur_case)
        << "Druplicate branch " <<dec_branch_type<< " in case statement."<<endl; 
    }
    dec_branch_map->insert(std::make_pair(dec_branch_type,cur_case->get_expr()));

    Symbol inf_branch_type = cur_case->TypeCheckExpression();
    if(inf_branch_type==SELF_TYPE) inf_branch_type = current_class->get_name();
    else all_self_type = false;
    if(classtable->class_map.find(inf_branch_type) 
       != classtable->class_map.end()) 
    {
    inf_branch_map->insert(std::make_pair(inf_branch_type,cur_case->get_expr()));
    }
  }
  if(inf_branch_map->size()==0) 
     type = cases->nth(cases->first())->get_expr()->get_type(); 
  else type = get_LCA(inf_branch_map);
  if(all_self_type) type = SELF_TYPE;
  return type; 
}

Symbol branch_class::TypeCheckExpression() { 
  if(name == self) {
    classtable->semant_error(current_class->get_filename(), this)
      << "'self' bound in 'case'." << endl;
  }

  if(classtable->class_map.find(type_decl) == classtable->class_map.end()) {
    classtable->semant_error(current_class->get_filename(), this)
      << "Class " << type_decl << " of case branch is undefined." << endl;
    return Object;
  }

  attr_tables[current_class->get_name()]->enterscope();
  attr_tables[current_class->get_name()]->addid(name, new Symbol(type_decl)); 
  Symbol type = expr->TypeCheckExpression();
  attr_tables[current_class->get_name()]->exitscope();
  return type; 
}

Symbol block_class::TypeCheckExpression() {
  Symbol expr_type;
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    expr_type = expr->TypeCheckExpression();
  }
  type = expr_type; 
  return expr_type; 
}

Symbol let_class::TypeCheckExpression() { 
  if(identifier == self) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "'self' cannot be bound in a 'let' expression." << endl;
  }
  Symbol init_type = init->TypeCheckExpression();
 
  if(init_type != No_type) { 


  if(type_decl == SELF_TYPE) { 
    if (init_type != SELF_TYPE) {
     classtable->semant_error(current_class->get_filename(), init) 
      << "Inferred type " << init_type
      << " of initialization of " << identifier
      << " does not conform to identifier's declared type " << type_decl << "."
      << endl; 
    }
  } else if(classtable->class_map.find(type_decl) != classtable->class_map.end()) {	
    if(init_type == SELF_TYPE) init_type = current_class->get_name();
    if(classtable->CheckConformance(init_type, type_decl) == false) {
      classtable->semant_error(current_class->get_filename(), init) 
	<< "Inferred type " << init_type
        << " of initialization of " << identifier
        << " does not conform to identifier's declared type " << type_decl << "."
        << endl; 
    }
  } else if(classtable->class_map.find(type_decl) == classtable->class_map.end()) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "Class " << type_decl << " of let-bound identifier " << identifier
      << " is undefined." << endl;
  }	  
  }
  attr_tables[current_class->get_name()]->enterscope();
  if(identifier != self) 
    attr_tables[current_class->get_name()]->addid(identifier, new Symbol (type_decl));
  type = body->TypeCheckExpression();
  attr_tables[current_class->get_name()]->exitscope();
  return type; 
}

Symbol plus_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if(e1_type != Int || e2_type != Int) {
    classtable->semant_error(current_class->get_filename(), this) 
      << "non-Int arguments: "
      << e1_type << " + " << e2_type << "." << endl;
  }
  type = Int;
  return type;  
}
Symbol sub_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if(e1_type != Int || e2_type != Int) {
    classtable->semant_error(current_class) << "non-Int arguments: "
      << e1_type << " - " << e2_type << "." << endl;
  }
  type = Int;
  return type; 
}
Symbol mul_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if(e1_type != Int || e2_type != Int) {
    classtable->semant_error(current_class) << "non-Int arguments: "
      << e1_type << " * " << e2_type << "." << endl;
  }
  type = Int;
  return type; 
}
Symbol divide_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if(e1_type != Int || e2_type != Int) {
    classtable->semant_error(current_class) << "non-Int arguments: "
      << e1_type << " / " << e2_type << "." << endl;
  }
  type = Int;
  return type; 
}
Symbol neg_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  if(e1_type != Int) {
    classtable->semant_error(current_class) << "Argument of '~' has type "
      << e1_type << " instead of Int." << endl;
  } 
  type = Int;
  return type;
}
Symbol lt_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if(e1_type != Int || e2_type != Int) {
    classtable->semant_error(current_class) << "non-Int arguments: "
      << e1_type << " < " << e2_type << "." << endl;
  }
  type = Bool;
  return type; 
}
Symbol eq_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if((e1_type == Str || e2_type == Str) || (e1_type == Int || e2_type == Int)
     || (e1_type == Bool || e2_type == Bool)) {
    
    if(e1_type != e2_type) {
      classtable->semant_error(current_class)
        << "Illegal comparison with a basic type." << endl;
    }
  }
  type = Bool;
  return type; 
}
Symbol leq_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  Symbol e2_type = e2->TypeCheckExpression();
  if(e1_type != Int || e2_type != Int) {
    classtable->semant_error(current_class) << "non-Int arguments: "
      << e1_type << " <= " << e2_type << "." << endl;
  }
  type = Bool;
  return type; 
}

Symbol comp_class::TypeCheckExpression() { 
  Symbol e1_type = e1->TypeCheckExpression();
  if(e1_type != Bool) {
    classtable->semant_error(current_class) << "Argument of 'not' has type "
      << e1_type << " instead of Bool." << endl;
  } 
  type = Bool;
  return type; 
}

Symbol bool_const_class::TypeCheckExpression() { 
  type = Bool;
  return type; 
}

Symbol new__class::TypeCheckExpression() { 
  if(type_name != SELF_TYPE 
     && classtable->class_map.find(type_name) == classtable->class_map.end()
    ) 
    { 
      classtable->semant_error(current_class->get_filename(), this) 
        << "'new' used with undefined class " << type_name << "." << endl;  
      type = Object;
      return type;
    }
  type = type_name;
  return type; 
}
Symbol isvoid_class::TypeCheckExpression() { 
	e1->TypeCheckExpression();
	type = Bool;
	return type; 
}

Symbol no_expr_class::TypeCheckExpression() { return No_type; }

Symbol object_class::TypeCheckExpression() {  
  if(name == self && attr_tables[current_class->get_name()]->lookup(name)==NULL) {
    type = SELF_TYPE;
    return type;
  }

  if (attr_tables[current_class->get_name()]->lookup(name) == NULL) {
    type = Object;
    classtable->semant_error(current_class->get_filename(), this) 
      << "Undeclared identifier " << name << "." << endl;
    return type;
  }
  
  type = *(attr_tables[current_class->get_name()]->lookup(name));
  return type;
}


Symbol int_const_class::TypeCheckExpression() {
  type = Int;
  return type;
}

Symbol string_const_class::TypeCheckExpression() {
  type = Str;
  return type;
}

void AddAttribute(Feature attribute) {
  if(attribute->get_name() == self && 
     current_class->get_name() == current_ancestor->get_name()) {
    classtable->semant_error(current_class->get_filename(),attribute) 
      << "'self' cannot be the name of an attribute." << endl;	
  }
  if(attr_tables[current_class->get_name()]->probe(attribute->get_name()) != NULL && 
		  current_class->get_name() == current_ancestor->get_name()) {
    classtable->semant_error(current_class->get_filename(), attribute) 
      << "Attribute " << attribute->get_name() << " is multiply defined in class."
      << endl;
  }


  if(attr_tables[current_class->get_name()]->lookup(attribute->get_name()) != NULL && 
		  current_class->get_name() == current_ancestor->get_name()) {
    classtable->semant_error(current_class->get_filename(), attribute) << "Attribute " 
      << attribute->get_name() << " is an attribute of an inherited class."
      << endl;
  } 
  Symbol declared_type = attribute->get_type_decl();
  if(declared_type != SELF_TYPE && declared_type != prim_slot) {
   if(classtable->class_map.find(declared_type) == classtable->class_map.end()
     && current_class->get_name() == current_ancestor->get_name()) 
   {
     classtable->semant_error(current_class->get_filename(), attribute)
     << "Class " << declared_type << " of attribute " 
     << attribute->get_name() << " is undefined." << endl;
   }
  }
  
  if(attribute->get_name() != self
      && attr_tables[current_class->get_name()]->lookup(attribute->get_name()) == NULL) 
    attr_tables[current_class->get_name()]->addid(attribute->get_name(),new Symbol(attribute->get_type_decl())); 
}

void AddMethod(Feature method) {
  Symbol name = method->get_name();

  Formals formals = method->get_formals();
  bool error = false; 

  Symbol return_type = method->get_type_decl();
  if(return_type != SELF_TYPE 
     && classtable->class_map.find(return_type) == classtable->class_map.end()
     && current_class->get_name() == current_ancestor->get_name())
  {
    classtable->semant_error(current_class->get_filename(), method)
      << "Undefined return type " << return_type << " in method " 
      << method->get_name() << "." << endl;
  }

  //CHECK FOR INHERITANCE CONFLICTS HERE
  if(method_tables[current_class->get_name()]->lookup(name) != NULL) {
    Feature inherited_method = 
	    (Feature)(method_tables[current_class->get_name()]->lookup(name));

    // check return type matches
    Formals inherited_formals = inherited_method->get_formals();

    if((inherited_method->get_type_decl() != method->get_type_decl())
       && (current_class->get_name() == current_ancestor->get_name())) {
      classtable->semant_error(current_class->get_filename(), method) 
	<< " In redefined method " << method->get_name() << "," 
	<< " return type " << method->get_type_decl() 
	<< " is different from original return type "
	<< inherited_method->get_type_decl() << "." <<  endl;	   
       error = true;   
    }

    if((formals->len() != inherited_formals->len())
       && (current_class->get_name() == current_ancestor->get_name())) {

      classtable->semant_error(current_class->get_filename(), method) 
	<< " Incompatible number of formal arguments in  redefined method " 
	<< method->get_name() <<"." <<  endl;	   
      error = true;

    } else {

    //check that formal types match

      formals = method->get_formals();
    
      int i1 = formals->first();
      int i2 = inherited_formals->first();

      for(; formals->more(i1) && inherited_formals->more(i2);
	i1 = formals->next(i1), i2 = inherited_formals->next(i2)) 
      {
        Formal formal = formals->nth(i1);
        Formal inherited_formal = inherited_formals->nth(i2);
        if((formal->get_type() != inherited_formal->get_type())
          && (current_class->get_name() == current_ancestor->get_name())) {

          classtable->semant_error(current_class->get_filename(), method) 
	    << " In redefined method " << method->get_name() << ", formal "
	    <<i1+1 << " does not match return type in inherited method." 
	    <<  endl;	 
         
	  error = true;
        }

      } 

    }     

  }// end section for checking redefined method against inherited method   

  if(!error) method_tables[current_class->get_name()]->addid(name, method);
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor does some semantic analysis for Class types */
    classtable = new ClassTable(classes);

    if(classtable->errors()) {
      cerr << "Compilation halted due to static semantic errors." << endl;
      //classtable->semant_error();
      exit(1);
    }

    //Begin second pass, in which Features are gathered into tables
    for(std::map<Symbol, Class_>::iterator it = classtable->class_map.begin(); 
		    it != classtable->class_map.end(); it++) {
      current_class = it->second;
      std::list<Symbol> path = classtable->InheritancePath(current_class->get_name());
      for(std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); iter++) {
        current_ancestor = classtable->class_map[*iter];
	Features features = current_ancestor->get_features();
        attr_tables[current_class->get_name()]->enterscope();
	method_tables[current_class->get_name()]->enterscope();
	for(int j = features->first(); features->more(j); j = features->next(j)) {
           Feature feature = features->nth(j);
	   if(feature->isAttribute()==false) AddMethod(feature);
	   else AddAttribute(feature);
	}
      }	      
    } // end of second pass for collecting all Feature declarations
    
    //begin third and final pass for typechecking all Features
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      current_class = classes->nth(i);
      std::list<Symbol> path = classtable->InheritancePath(current_class->get_name());
      //Don't typecheck methods for Object or IO
      if(current_class->get_name() == Object || current_class->get_name()==IO) { 
	      continue;
      }

      //FIRST: typecheck Attributes and all of their initializations
      Features features = current_class->get_features();
      for(int j = features->first(); features->more(j); j = features->next(j)) {
        Feature feature = features->nth(j);
	if(feature->isAttribute()==false) continue;
        if(feature->get_init()->is_empty()) continue;	

        Symbol inferred_type = feature->get_init()->TypeCheckExpression(); 
	Symbol declared_type = feature->get_type_decl();

	if(inferred_type == SELF_TYPE) inferred_type = current_class->get_name();

	if(classtable->class_map.find(declared_type)==classtable->class_map.end()
	  || classtable->class_map.find(inferred_type)==classtable->class_map.end()        ) 
	{
          continue; // don't typecheck if types are undefined
	}

	inferred_type = feature->get_init()->get_type();
        if(declared_type == SELF_TYPE) { 
		if (inferred_type != SELF_TYPE) {
                  classtable->semant_error(current_class->get_filename(),feature) 
                    << "Inferred type " <<inferred_type
		    << " of initialization of attribute "
                    <<feature->get_name()<< " does not conform to declared type "
                    << declared_type << "." << endl; 
                }     
        } else {
          if(inferred_type==SELF_TYPE) inferred_type=current_class->get_name();	
          if(classtable->CheckConformance(inferred_type, declared_type)==false) {
          classtable->semant_error(current_class->get_filename(), feature) 
		  << "Inferred type " << feature->get_init()->get_type() << 
		  " of initialization of attribute " << feature->get_name() 
		  << " does not conform to declared type " << declared_type 
		  << "." << endl;
          }  

        }
      }

      //Now typecheck methods using attribute tables
      attr_tables[current_class->get_name()]->enterscope();
      for(int j = features->first(); features->more(j); j = features->next(j)) {
        Feature feature = features->nth(j);
	if(feature->isAttribute()==true) continue;

        Formals formals = feature->get_formals(); 
        bool undefined_formal = false;
	//formal names should hide Attribute names!!
	for(int k = formals->first(); formals->more(k); k = formals->next(k)) {

	  Formal formal = formals->nth(k);
          if(formal->get_type() == SELF_TYPE) 
          {
            classtable->semant_error(current_class->get_filename(), formal) 
	     << "Formal parameter "
	     << formal->get_name() << " cannot have type SELF_TYPE." << endl;
          }
          if(formal->get_name() == self) 
          {
            classtable->semant_error(current_class->get_filename(), formal) 	   
	      << "'self' cannot be the name of a formal parameter." << endl;
          }
	  Symbol formal_type = formal->get_type();
	  if (formal_type == SELF_TYPE) formal_type = current_class->get_name();
	  if(classtable->class_map.find(formal_type)==classtable->class_map.end())
	  {
	    classtable->semant_error(current_class->get_filename(), formal)
	      << "Class " << formal_type << " of formal parameter " 
	      << formal->get_name() << " is undefined." << endl;
	      undefined_formal = true;
	  }	  
	  if(formal->get_name() != self)
	    attr_tables[current_class->get_name()]->addid(formal->get_name(), new Symbol(formal->get_type()));

        }
        if (undefined_formal) continue;
	Symbol declared_type = feature->get_type_decl();

	Symbol inferred_type = feature->get_init()->TypeCheckExpression();
        if(inferred_type == SELF_TYPE) inferred_type = current_class->get_name();

	if(classtable->class_map.find(declared_type)==classtable->class_map.end()
	|| classtable->class_map.find(inferred_type)==classtable->class_map.end()      
	) 
	{
          continue; // don't typecheck if types are undefined
	}
	inferred_type = feature->get_init()->get_type();

        if(declared_type == SELF_TYPE) { 
	  if(inferred_type != SELF_TYPE) {
            classtable->semant_error(current_class->get_filename(), feature) 
              << "Inferred return type " << inferred_type << " of method "
              <<feature->get_name()<< " does not conform to declared type "
              << declared_type << "." << endl; 
          }     
        } else {
           if(inferred_type==SELF_TYPE)inferred_type=current_class->get_name();	
           if(classtable->CheckConformance(inferred_type, declared_type)==false) {
             classtable->semant_error(current_class->get_filename(), feature) 
		  << "Inferred return type " << feature->get_init()->get_type() 
		  << " of method " << feature->get_name() << 
		  " does not conform to declared returned type " << 
		  declared_type << "." << endl;
            }  

        }
      } //end of loop going through all features for method type-checking

      attr_tables[current_class->get_name()]->exitscope();
      
      for(size_t j = 0; j < path.size(); j++) {
        attr_tables[current_class->get_name()]->exitscope();
      }
      
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


