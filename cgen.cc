
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

#include <map>
#include <symtab.h>
#include <sstream>
#include <algorithm>
#include <queue>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

int stack_top; // instance var to save current top of stack's offset from $fp
std::map<Symbol, CgenNodeP> *class_nodes_instance;
std::map<Symbol, int> *class_tags_instance;
std::map<int, Symbol>* class_tags_new_instance;
Symbol current_self;
int num_labels = 0;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{ 
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << Str << DISPTAB_SUFFIX << endl;                 // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << Int << DISPTAB_SUFFIX << endl;                 // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << Bool << DISPTAB_SUFFIX << endl;                  // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //ADDITIONAL GLOBAL NAMES
  str << GLOBAL << CLASSOBJTAB << endl; //class object table
  //str << GLOBAL << CLASSATTRTABTAB << endl; //class object table

  //proto, init, attr suffixes for each class
  
  //for(std::map<Symbol, int>::iterator it = class_tags->begin(); it != class_tags->end(); it++) {
  for(int i = 0; i < (int)(class_tags_new->size()); i++) {
    Symbol name = (*class_tags_new)[i];
    str << GLOBAL << name << PROTOBJ_SUFFIX << endl;
    str << GLOBAL << name << CLASSINIT_SUFFIX << endl;
    //str << GLOBAL << name << ATTRTAB_SUFFIX << endl;

  }  

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    

  
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_class_name_tab() {
  str << CLASSNAMETAB << LABEL;
  //for(std::map<Symbol, int>::iterator it = class_tags->begin(); it != class_tags->end(); it++) {
  for(int i = 0; i < (int)(class_tags_new->size()); i++) {
     str << WORD;
     StringEntry *str_entry = stringtable.lookup_string((*class_tags_new)[i]->get_string());
     str_entry->code_ref(str);
     str << endl;
  }

}

void CgenClassTable::code_obj_tab() {
  str << CLASSOBJTAB << LABEL;
  //for(std::map<Symbol, int>::iterator it = class_tags->begin(); it != class_tags->end(); it++) {
  for(size_t i = 0; i < class_tags_new->size(); i++) {
     str << WORD;
     //str << it->first->get_string();
     str << (*class_tags_new)[i]->get_string();
     str << PROTOBJ_SUFFIX << endl;
     str << WORD;
     //str << it->first->get_string();
     str << (*class_tags_new)[i]->get_string();
     str << CLASSINIT_SUFFIX << endl;
  }

}

void CgenClassTable::code_attr_tab_tab() {
  str << CLASSATTRTABTAB << LABEL;
  //for(std::map<Symbol, int>::iterator it = class_tags->begin(); it != class_tags->end(); it++) {
  for(size_t i = 0; i < class_tags_new->size(); i++) {
     str << WORD;
     //str << it->first->get_string();
     str << (*class_tags_new)[i]->get_string();
     str << ATTRTAB_SUFFIX << endl;
  }
}

bool method_class::is_method() { return true; }
bool attr_class::is_method() { return false;}

std::vector<CgenNodeP>* CgenNode::get_inheritance_path() {
  std::vector<CgenNodeP> *path = new std::vector<CgenNodeP>();
  CgenNodeP node = this;
  while(node->name != Object) {
    node = node->get_parentnd();
    path->push_back(node);
  }
  return path;
}

std::vector<CgenNodeP>* CgenNode::get_full_inheritance_path() {
  std::vector<CgenNodeP> *path = new std::vector<CgenNodeP>();
  CgenNodeP node = this;
  path->push_back(node);
  while(node->name != Object) {
    node = node->get_parentnd();
    path->push_back(node);
  }
  return path;
}

/*void CgenClassTable::code_disp_tabs() {
  for(List<CgenNode> *l = nds; l != NULL; l = l->tl()) {
    CgenNode *node = l->hd();
    Features features = node->get_features(); 
    Symbol name = node->name;
    emit_disptable_ref(name, str);
    str << LABEL;
    for(int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feature = features->nth(i);
      if(feature->is_method()) {
        str << WORD;
	emit_method_ref(name, feature->get_name(), str);
	str << endl;
      }
    }
    std::vector<CgenNodeP> *path = node->get_inheritance_path();
    //cout << "SIZE: " << path->size() << endl;
    //cout << path->at(0)->name << endl;
    for(size_t j = 0; j < path->size(); j++) {
      node = path->at(j);
      name = path->at(j)->name;
      features = node->get_features();
      for(int k = features->first(); features->more(k); k = features->next(k)) {
         Feature feature = features->nth(k);
	 if(feature->is_method()) {
           str << WORD;
	   emit_method_ref(name, feature->get_name(), str);
	   str << endl;
	 }
      }
    }
  }
}*/

//check if method exists in list features
bool contains_method(Features features, Symbol method) {
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature = features->nth(i);
    if(feature->is_method() == false) continue;
    //cout << "TO FIND: " << method << " FROM LIST: " << feature->get_name() << endl;
    if (method == feature->get_name()) return true;
  }
  return false;
}

void CgenClassTable::code_disp_tabs() {
  for(List<CgenNode> *l = nds; l != NULL; l = l->tl()) {
    CgenNode *node = l->hd();
    Features class_features = node->get_features();
    Symbol name = node->name;
    emit_disptable_ref(name, str);
    str << LABEL;
    std::vector<CgenNodeP> *path = node->get_full_inheritance_path();

    int index = 0;
    for(int j = path->size() - 1; j >= 0; j--) {
      CgenNodeP cur_node = path->at(j);
      Features features = cur_node->get_features();
      for(int k = features->first(); features->more(k); k = features->next(k)) {
         name = path->at(j)->name;
         Feature feature = features->nth(k);
	 if(feature->is_method()) {
	   std::string name_str = feature->get_name()->get_string();
	   if(node->dispatch_indeces.find(name_str) != node->dispatch_indeces.end()) continue;
	   //cout << "CLASS: " << node->name << endl;
           
	   for(int i = j-1; i >= 1; i--) {
	     Features descendant_features = path->at(i)->get_features();
     	     if(contains_method(descendant_features, feature->get_name())) {
	       name = path->at(i)->name;
	     }
	   }   

	   if(contains_method(class_features, feature->get_name())) {
             name = node->name; 
	   }
           str << WORD;
	   emit_method_ref(name, feature->get_name(), str);
	   str << endl;
	   //cout << "NAME: " << name << endl;
           //std::string name_str = name->get_string();
	   //name_str += std::string(".");
	   //name_str += feature->get_name()->get_string();
	   //cout << "NAME_STR: " << name_str << endl;
	   node->dispatch_indeces.insert(std::make_pair(name_str, index++));
	 }
      }
    }
  }
}

int countAttrs(std::vector<CgenNodeP> *path) {
  int count = 0;
  for(size_t i = 0; i < path->size(); i++) {
    Features features = path->at(i)->get_features();
    for(int j = features->first(); features->more(j); j = features->next(j)) {
      Feature feature = features->nth(j);
      if(!feature->is_method()) count++;
    } 
  }
  return count;
}

void add_to_env(SymbolTable<Symbol, std::string>*env, Symbol name, 
	std::string reg, int index)
{
  std::stringstream ss;
  ss << index;
  reg += ss.str();
  std::string* entry = new std::string(reg);  
  env->addid(name, entry);
}	


void CgenClassTable::code_prot_objs() {
  //for(std::map<Symbol, CgenNodeP>::iterator it = class_nodes->begin(); it != class_nodes->end(); it++) {
  for(size_t i = 0; i < class_tags_new->size(); i++) {
     Symbol name = (*class_tags_new)[i];
     //str << it->first << PROTOBJ_SUFFIX << LABEL;
     str << name << PROTOBJ_SUFFIX << LABEL;

     //str << WORD << (*class_tags)[it->first] << endl; 
     str << WORD << i << endl;
     
     str << WORD;
       //std::vector<CgenNodeP> *path = (it->second)->get_full_inheritance_path(); 
       std::vector<CgenNodeP> *path = (*class_nodes)[name]->get_full_inheritance_path(); 
       int size = 3 + countAttrs(path);
     str << size << endl;

     //str << WORD << it->first << DISPTAB_SUFFIX << endl;
     str << WORD << name << DISPTAB_SUFFIX << endl;

     //default values for basic classes
     if(name == Str) {
       str << WORD;
       inttable.lookup_string("0")->code_ref(str);
       str << endl;
     }
     if(name == Str || name == Int || name == Bool) {
       str << WORD << "0" << endl;;
     }

     CgenNodeP class_ptr = (*class_nodes)[name];
     //if(!((it->second)->basic())) {
    
     (*class_nodes_instance)[name]->env = new SymbolTable<Symbol, std::string>();
     SymbolTable<Symbol, std::string> *env =(*class_nodes_instance)[name]->env;     
     env->enterscope();

     int attr_offset = 3;

     for(int j = path->size() - 1; j >= 0; j--) {

     class_ptr = path->at(j);

     if(!(class_ptr->basic())) {
	//Features features = it->second->get_features();
	Features features = class_ptr->get_features();
	for(int i =features->first(); features->more(i); i = features->next(i)) {
          Feature feature = features->nth(i);
          if(feature->is_method()) continue;
	  str << WORD;
          if(feature->get_type() == Int) {
            inttable.lookup_string("0")->code_ref(str); 
	  }
	  else if(feature->get_type() == Str) {
            stringtable.lookup_string("")->code_ref(str);
	  }
          else if(feature->get_type() == Bool) {
            BoolConst(false).code_ref(str);
	  }
	  else {
            str << "0";
	  } 
	  str << endl;

	  add_to_env(env, feature->get_name(), SELF, attr_offset++);
        }
     }
     }
     str << WORD << "-1" << endl; 
  } 
}
//assume symbol table contains name key
std::pair<std::string, int> get_pair_from_env(SymbolTable<Symbol, std::string>* env, Symbol name) 
{
  std::pair<std::string, int> pair;
  std::string value = *(env->lookup(name));
  
  pair.first = value.substr(0, 3);
  int i;
  std::istringstream(value.substr(3)) >> i;
  pair.second = i;
  
  return pair;
}

void CgenClassTable::code_inits() {
  for(std::map<Symbol, CgenNodeP>::iterator it = class_nodes->begin(); it != class_nodes->end(); it++) {
    current_self = it->first; 

    str << it->first << CLASSINIT_SUFFIX << LABEL;
    emit_addiu(SP, SP, -12, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 16, str);
    emit_move(SELF, ACC, str);

    /* Call Parent init */
    if(it->second->get_parentnd()->name != No_class) {
      str << JAL;
      emit_init_ref(it->second->get_parentnd()->name, str);  
      str << endl;
    }
 
    Features features = it->second->get_features();
    //it->second->env = new SymbolTable<Symbol, std::string>();
    SymbolTable<Symbol, std::string> *env = it->second->env;
    //env->enterscope();

    //std::vector<CgenNodeP> *path = (*class_nodes)[name]->get_full_inheritance_path(); 
    
    //need to add to env first so we can refer to default values 

    /*

    int attr_offset = 3;
    for(int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feature = features->nth(i);
      if(feature->is_method()) continue;
      //env->addid(((attr_class*)(feature))->name, new int(1+attr_offset++));
      add_to_env(env, ((attr_class*)(feature))->name, SELF, attr_offset++); 
    }
   */
   // attr_offset = 3; 
    stack_top = -4; //initial offset from FP
    for(int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feature = features->nth(i);
      //env->enterscope();
      if(!feature->is_method()) {
	Expression init =  (Expression)(((attr_class*)(feature))->init);
	if(!(init->is_empty())) {//generate code only if init is not empty
//	  cout << "TYPE " << init->get_type() << endl;
          init->code(str, env); 
          int offset = get_pair_from_env(env, feature->get_name()).second;
          emit_store(ACC, offset, SELF, str);
          //env->addid(((attr_class*)(feature))->name, new int(1+attr_offset));
	  //add_to_env(env, ((attr_class*)(feature))->name, SELF, offset);
          //attr_offset++;
	}
      }
    }
    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12, str);
    emit_return(str);
  }
}

void CgenClassTable::code_methods() {
  for(std::map<Symbol, CgenNodeP>::iterator it = class_nodes->begin(); it != class_nodes->end(); it++) {
      
    current_self = it->first;
    if(current_self == IO || current_self == Object || current_self == Str)
	    continue;
    Features features = it->second->get_features();
    SymbolTable<Symbol, std::string> *env = it->second->env;

    for(int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feature = features->nth(i);
      if(!feature->is_method()) continue;
      Symbol name = (Symbol)(((method_class*)(feature))->name);
      Expression expr =  (Expression)(((method_class*)(feature))->expr);

      //if(name != main_meth) continue; //TODO: delete this

      str << it->first << "." << name << LABEL;

      emit_addiu(SP, SP, -12, str);
      emit_store(FP, 3, SP, str);
      emit_store(SELF, 2, SP, str);
      emit_store(RA, 1, SP, str);
      emit_addiu(FP, SP, 16, str);
      emit_move(SELF, ACC, str);

      //add formals into environment
      env->enterscope();
      Formals formals = ((method_class*)(feature))->formals;
      int num_formals = 0;      
      for(int j = formals->first(); formals->more(j); j = formals->next(j)) {
	      num_formals++;
      }
      int top_index = num_formals - 1;
      for(int j = formals->first(); formals->more(j); j = formals->next(j)) {
        Formal formal = formals->nth(j);
	//env->addid(formal->get_name(), new int(formal_index--));
	add_to_env(env, formal->get_name(), FP, top_index--);  
      }

      if(expr->is_empty()) continue; //generate code only if expr is not empty
      expr->code(str, env); 

      env->exitscope();
     /*  

      Expressions actuals = ((dispatch_class*)(expr))->actual;
      int f = actuals->first();
      actuals->nth(f)->code(str, env);
      //emit_load_string(ACC,stringtable.lookup_string("1\n"), str);
      emit_store(ACC, 0, SP, str);
      emit_addiu(SP, SP, -4, str); 
      emit_move(ACC, SELF, str); //SELF needs to go in ACC
      
      char *address = "IO.out_int";
      emit_jal(address, str);
      
      */

      //emit_bne(ACC, ZERO, 0, str);
      //str << "label0:" << LABEL;

      //init->code(str, env); 
      //emit_store(ACC, attr_offset, SELF, str);
      //env->addid(((attr_class*)(feature))->name, new int(attr_offset));
      //emit_move(ACC, SELF, str);
      //emit_addiu(SP, SP, 4, str);

      //stack_top -= formal_index;
      //emit_addiu(SP, SP, -4*(formal_index), str);


      emit_load(FP, 3, SP, str);
      emit_load(SELF, 2, SP, str);
      emit_load(RA, 1, SP, str);
      emit_addiu(SP, SP, 12 + 4*(num_formals), str);
      emit_return(str);

    }
  }
}



CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   class_tags = new std::map<Symbol, int>;
   class_tags_new = new std::map<int, Symbol>;
   class_tags_instance = class_tags;
   class_tags_new_instance = class_tags_new;
   class_nodes = new std::map<Symbol, CgenNodeP>;
   class_nodes_instance = class_nodes;
   int i = 0;
   for (List<CgenNode> *l = nds; l != NULL; l = l->tl()) {
     CgenNode* node = l->hd();
     class_tags_new->insert(std::make_pair(i, node->get_name()));	
     class_tags->insert(std::make_pair(node->get_name(), i));	
     if(node->get_name() == Str) stringclasstag = i;
     if(node->get_name() == Int) intclasstag = i;
     if(node->get_name() == Bool) boolclasstag = i;
     class_nodes->insert(std::make_pair(node->get_name(), node));
     i++;
   }
   
   //stringclasstag = (*class_tags)[Str]/* Change to your String class tag here*/;
   //intclasstag =  (*class_tags)[Int] /* Change to your Int class tag here */;
   //boolclasstag = (*class_tags)[Bool]/* Change to your Bool class tag here */;
 
   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
 
  code_class_name_tab();
  code_obj_tab();
  //code_attr_tab_tab();
  code_disp_tabs();
  code_prot_objs(); 

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  code_inits();
  code_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  //env->enterscope();
  //int stack_init_offset = stack_top;
  //code expr first?
  expr->code(s, env);
  //emit_move(T1, ACC, s); //T1 holds expr value

  //try replacing with emit_push afterwards
  
  //int index = *(env->lookup(name));
  int index = get_pair_from_env(env, name).second;
  if(get_pair_from_env(env, name).first==SELF) { 
    emit_store(ACC, index, SELF, s); 
    if(cgen_Memmgr == 1) {
      emit_addiu(A1, SELF, 4*index, s);
      emit_jal("_GenGC_Assign", s);
    }
  }
  else emit_store(ACC, index, FP, s);

  //emit_store(T1, 0, ACC, s);
  //emit_move(ACC, T1, s);
  
  //env->exitscope();
}

void static_dispatch_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;
  int num_actuals = 0;

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    emit_store(ACC, 0, SP, s);
    emit_addiu(SP, SP, -4, s); 
    stack_top--;
    num_actuals++;
  }
  
  //emit_move(ACC, SELF, s); //SELF needs to go in ACC
  expr->code(s, env);

   //code for when expression is void
  int not_void_label = num_labels++;
  emit_bne(ACC, ZERO, not_void_label, s);
  emit_load_string(ACC,stringtable.lookup_string((*class_nodes_instance)[current_self]->get_filename()->get_string()),s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_dispatch_abort", s);
  emit_label_ref(not_void_label, s);
  s << LABEL;


  //std::string dispatch_class_str = type_name->get_string();
  Symbol expr_type = expr->get_type();
  //TODO: will the expression ever be empty?
  //if(!(expr->is_empty())) expr->code(s, env);
  //if(!(expr->is_empty()) && type != SELF_TYPE) expr->code(s, env);
  CgenNodeP node; 
  if(expr_type == SELF_TYPE) expr_type = current_self;
  node = class_nodes_instance->at(expr_type);
  
  //dispatch_class_str += ".";
  std::string dispatch_class_str = name->get_string();

  //cout << "DISP STR: " << dispatch_class_str << endl;

  for(std::map<std::string, int>::iterator it = node->dispatch_indeces.begin();
		  it != node->dispatch_indeces.end(); it++) {
    //cout << "NAME: " << it->first << " INDEX: " << it->second << endl;
  }

  int dispatch_index = (node->dispatch_indeces).find(dispatch_class_str)->second;
//cout << "DISP INDEX: " << dispatch_index << endl;
  
  //emit_load(T1, 2, ACC, s); //2 is where the dispatch table pointer is
  std::string disp_tab_str =  type_name->get_string();
  disp_tab_str += DISPTAB_SUFFIX;
  emit_load_address(T1, (char*)(disp_tab_str.c_str()), s); 
  emit_load(T1, dispatch_index, T1, s); 
  emit_jalr(T1, s);

  // - num_actuals because the called method already decremented the 
  // stack to account for the use of formal parameters
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top -num_actuals), s);
  stack_top += (stack_init_offset-stack_top); 

  env->exitscope();
}

void dispatch_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;

    int num_actuals = 0;
  //Expressions actuals = ((dispatch_class*)(expr))->actual;
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    emit_store(ACC, 0, SP, s);
    emit_addiu(SP, SP, -4, s); 
    stack_top--;
    num_actuals++;
  }
  
  emit_move(ACC, SELF, s); //SELF needs to go in ACC
  if(!(expr->is_empty())) expr->code(s, env);

  //code for when expression is void
  int not_void_label = num_labels++;
  emit_bne(ACC, ZERO, not_void_label, s);
  emit_load_string(ACC,stringtable.lookup_string((*class_nodes_instance)[current_self]->get_filename()->get_string()),s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_dispatch_abort", s);
  emit_label_ref(not_void_label, s);
  s << LABEL;


  std::string dispatch_class_str;
  Symbol type = expr->get_type();
  //TODO: will the expression ever be empty?
    //if(!(expr->is_empty()) && type != SELF_TYPE) expr->code(s, env);
  CgenNodeP node; 
  if(type == SELF_TYPE) type = current_self;
  node = class_nodes_instance->at(type);
  /* 
  //search up the inheritance path for the method 
  std::vector<CgenNodeP> *path = node->get_full_inheritance_path();
  for(int i = 0; i < (int)(path->size()); i++) {
    //TODO: modularize name_str?
    std::string name_str = path->at(i)->name->get_string();
    name_str += ".";
    name_str += name->get_string();
      if(path->at(i)->dispatch_indeces.find(name_str) 
         != path->at(i)->dispatch_indeces.end()) { 
        dispatch_class_str = path->at(i)->name->get_string();  
        node = path->at(i);
	break;
      }
  }
  
  dispatch_class_str += ".";
  */
  dispatch_class_str = name->get_string();

  char address[dispatch_class_str.length()+1];
  strcpy(address, dispatch_class_str.c_str());

  int dispatch_index = (node->dispatch_indeces).find(dispatch_class_str)->second;

  emit_load(T1, 2, ACC, s); //2 is where the dispatch table pointer is
  emit_load(T1, dispatch_index, T1, s); 
  emit_jalr(T1, s);

  // - num_actuals because the called method already decremented the 
  // stack to account for the use of formal parameters
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top -num_actuals), s);
  stack_top += (stack_init_offset-stack_top); 
  
  env->exitscope();
}

void cond_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  pred->code(s, env); 
  emit_load(T1, 3, ACC, s);

  int else_label = num_labels++;
  
  //then_exp section
  emit_beqz(T1, else_label, s);
  then_exp->code(s, env);
  int end_label = num_labels++;
  emit_branch(end_label, s); // if eval then_exp, jump over else_exp code

  //else_exp section
  emit_label_ref(else_label, s);
  s << LABEL;
  else_exp->code(s, env);

  //label the end of the cond section
  emit_label_ref(end_label, s);
  s << LABEL;

}

void loop_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  int pred_label = num_labels++;
  emit_label_ref(pred_label, s);
  s << LABEL;

  pred->code(s, env);
  emit_load(T1, 3, ACC, s);

  int loop_end_label = num_labels++;
  emit_beqz(T1, loop_end_label, s);

  body->code(s, env);
  emit_branch(pred_label, s);
  emit_label_ref(loop_end_label, s);
  s << LABEL;
  emit_move(ACC, ZERO, s);
}

bool path_contains(std::vector<CgenNodeP>* path, Symbol name) {
  for(size_t i = 0; i < path->size(); i++) {
    if(path->at(i)->name == name) return true;
  }
  return false;
}

void typcase_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;

  expr->code(s, env);
  //CASE ON VOID ERROR CODE HERE
  int non_void_label = num_labels++;
  emit_bne(ACC, ZERO, non_void_label, s);  
  emit_load_string(ACC,stringtable.lookup_string((*class_nodes_instance)[current_self]->get_filename()->get_string()),s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_case_abort2", s);
  //new label for non-error code
  emit_label_ref(non_void_label, s);
  s << LABEL;

  int expr0_offset = stack_top;
  emit_store(ACC, stack_top--, FP, s);//store expr0 on stack
  emit_addiu(SP, SP, -4, s);

  int branches_remaining = cases->len();
  int num_branches = cases->len(); 
  //map for storing correct label for each branch
  std::map<Symbol, int>* branch_labels = new std::map<Symbol, int>();
  //map for storing list of descendants for each branch
  std::map<Symbol, std::vector<CgenNodeP>* > descendant_lists;// = new std::vector<Symbol, std::vector<CgenNodeP>* >();
  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case branch_case = cases->nth(i);
    Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
    add_to_env(env, branch_type, FP, stack_top);
    int branch_label = num_labels++;
    branch_labels->insert(std::make_pair(branch_type, branch_label));

    //initiate descendant lists
    std::vector<CgenNodeP>* ancestor = new std::vector<CgenNodeP>();
    ancestor->push_back((*class_nodes_instance)[branch_type]);
    descendant_lists.insert(std::make_pair(branch_type, ancestor));

    stack_top--;
  }

  emit_addiu(SP, SP, -4*(num_branches), s); 

  while(branches_remaining > 0) {
    branches_remaining = cases->len();

    for(std::map<Symbol, std::vector<CgenNodeP>* >::iterator it = descendant_lists.begin(); 
	it != descendant_lists.end(); it++) {
      std::vector<CgenNodeP>* list = it->second;
      std::vector<CgenNodeP>* next_descendants = new std::vector<CgenNodeP>();

      for(int j = 0; j < (int) list->size(); j++) {
        CgenNodeP node_p = list->at(j);
        int child_tag = (*class_tags_instance)[node_p->get_name()];
        emit_load_imm(T1, child_tag, s);
        emit_load(T2, expr0_offset, FP, s);
        emit_load(T2, 0, T2, s);
	int match_label = (*branch_labels)[it->first];
        emit_beq(T2, T1, match_label, s); 	
        List<CgenNode>* children = node_p->get_children();
	//cout << "BRANCH " << it->first << endl;
	//cout << "PARENT " << node_p->get_name() << endl;
	while(children != NULL) {
          next_descendants->push_back(children->hd());

	  //cout << "CHILD " << children->hd()->get_name() << endl;
	  children = children->tl(); 
	  //cout << "SIZE: " << descendant_lists.size() << endl;
	}
      }
      delete it->second;
      it->second = next_descendants;
      //descendant_lists.insert(std::make_pair(it->first, next_descendants));
      if(next_descendants->size() == 0) branches_remaining--;
    }
    //cout << "***** NEXT ROUND *****" << endl;
  }

  //if no match found, this code will be executed
  //otherwise, execution will have jumped to one of the branch labels
  //--expr->code(s, env);
  emit_load(T1,expr0_offset, FP, s);
  emit_load(T2, 0, ACC, s);
  emit_jal("_case_abort", s);

  int end_label = num_labels++;//label for code after branch is coded

  //code each branch with its own label
  for(int i = cases->first(); cases->more(i); i = cases->next(i))
  {
    Case branch_case = cases->nth(i);
    Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
    int branch_label = (*branch_labels)[branch_type];
    emit_label_def(branch_label, s);

    //--expr->code(s, env);
    emit_load(T1,expr0_offset, FP, s);
    //--emit_store(ACC, get_pair_from_env(env, branch_type).second, FP, s);
    emit_store(T1, get_pair_from_env(env, branch_type).second, FP, s);

    Symbol branch_name = ((branch_class*)(branch_case))->name;
    add_to_env(env, branch_name, FP, get_pair_from_env(env, branch_type).second);
    //branch_case->code(s, env);  
    ((branch_class*)branch_case)->expr->code(s, env);
    emit_branch(end_label, s);
  }

  //end section after coding branch
  emit_label_def(end_label, s);

  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top = stack_init_offset; 

  env->exitscope();
}

/*
void typcase_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;

  expr->code(s, env);
  //CASE ON VOID ERROR CODE HERE
  int non_void_label = num_labels++;
  emit_bne(ACC, ZERO, non_void_label, s);  
  emit_load_string(ACC,stringtable.lookup_string((*class_nodes_instance)[current_self]->get_filename()->get_string()),s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_case_abort2", s);
  //new label for non-error code
  emit_label_ref(non_void_label, s);
  s << LABEL;

  int expr0_offset = stack_top;
  //--emit_load(T1, 0, ACC, s);
  //--emit_store(T1, stack_top--, FP, s);//store expr0 tag on stack
  emit_store(ACC, stack_top--, FP, s);//store expr0 on stack
  emit_addiu(SP, SP, -4, s);
  //Case correct_case = NULL; 
  int branches_remaining = cases->len();
  int num_branches = cases->len();
  //bool done_checking = false;
  int round = 0; 
  //int object_tag = (*class_tags_instance)[Object];

 //FIRST: only need to add to env??
 
  //map for storing correct label for each branch
  std::map<Symbol, int>* branch_labels = new std::map<Symbol, int>();
  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case branch_case = cases->nth(i);
    Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
    add_to_env(env, branch_type, FP, stack_top);
    int branch_label = num_labels++;
    branch_labels->insert(std::make_pair(branch_type, branch_label));
    stack_top--;
  }

  emit_addiu(SP, SP, -4*(num_branches), s); 
  
  while(branches_remaining > 0) {
    branches_remaining = cases->len();
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
      Case branch_case = cases->nth(i);
      Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
      List<CgenNode>* children = (*class_nodes_instance)[branch_type]->get_children();
      for(int r = 1; r < round; r++) {
        if(children == NULL) { 
	  branches_remaining--;
          break;	      
	}
        children = children->tl();
      } 
      int child_tag = -1;
      if(round == 0) {
        child_tag = (*class_tags_instance)[branch_type]; 
      } else if(children != NULL) { 
        Symbol child_type = children->hd()->get_name();
	cout << "PARENT: " << branch_type << "CHILD: " << child_type << endl;
        child_tag = (*class_tags_instance)[child_type];
	cout << "TAG: " << child_tag << endl;
      }
      
      //Symbol branch_name = ((branch_class*)(branch_case))->name;
      int offset = get_pair_from_env(env, branch_type).second;
      emit_load_imm(T1, child_tag, s);
      emit_store(T1, offset, FP, s);        
    } //finished storing current tags for all branches

    //now check for any matches
    
    emit_load(T1,expr0_offset, FP, s);
    emit_load(T1, 0, T1, s); //--load expr0 tag into $T1
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
      Case branch_case = cases->nth(i);
      Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
      int match_found_label = (*branch_labels)[branch_type];
      //branch_labels->insert(std::make_pair(branch_name, match_found_label));
      int offset = get_pair_from_env(env, branch_type).second;
      emit_load(T2, offset, FP, s);
      emit_beq(T1, T2, match_found_label, s);
    }
    round++;   //increase round count for level down inheritance
  } 

  //if no match found, this code will be executed
  //otherwise, execution will have jumped to one of the branch labels
  //--expr->code(s, env);
  emit_load(T1,expr0_offset, FP, s);
  emit_load(T2, 0, ACC, s);
  emit_jal("_case_abort", s);

  int end_label = num_labels++;//label for code after branch is coded

  //code each branch with its own label
  for(int i = cases->first(); cases->more(i); i = cases->next(i))
  {
    Case branch_case = cases->nth(i);
    Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
    int branch_label = (*branch_labels)[branch_type];
    emit_label_def(branch_label, s);

    //--expr->code(s, env);
    emit_load(T1,expr0_offset, FP, s);
    //--emit_store(ACC, get_pair_from_env(env, branch_type).second, FP, s);
    emit_store(T1, get_pair_from_env(env, branch_type).second, FP, s);

    Symbol branch_name = ((branch_class*)(branch_case))->name;
    add_to_env(env, branch_name, FP, get_pair_from_env(env, branch_type).second);
    //branch_case->code(s, env);  
    ((branch_class*)branch_case)->expr->code(s, env);
    emit_branch(end_label, s);
  }

  //end section after coding branch
  emit_label_def(end_label, s);

  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top = stack_init_offset; 

  env->exitscope();
}
*/
/*void typcase_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  Symbol type = expr->get_type();
  if(type == SELF_TYPE) type = current_self;

  //experiment with List of children nodes 
  //experiment with List of children nodes 
  //experiment with List of children nodes 
  //experiment with List of children nodes 
  List<CgenNode>* children = (*class_nodes_instance)[type]->get_children();
  for(; children != NULL; children = children->tl()) { 
	  cout << children->hd()->get_name() << endl;
  }

  CgenNodeP class_node = (*class_nodes_instance)[type];
  std::vector<CgenNodeP> *path = class_node->get_full_inheritance_path();
//  cout << "TYPE: " << type << endl;
  Case correct_case = NULL; 
  int min = -1;

  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case branch_case = cases->nth(i);
    Symbol branch_type = ((branch_class*)(branch_case))->type_decl;
    std::vector<CgenNodeP> *branch_path = 
	    class_nodes_instance->at(branch_type)->get_full_inheritance_path();
    
    for(int j = 0; j < (int)path->size(); j++) {
      //cout << "Branch type: " << branch_type << " Path: " 
	      //<< branch_path->at(j)->name << endl;
      //if((type == branch_path->at(j)->name) && (min == -1 || j < min)) {
      if(path->at(j)->name == branch_type && (min == -1 || j < min)) 
      {
	//cout << "CASE MET" << endl;
        correct_case = branch_case;
	min = j;
	break;
      } 
    }  
  }
  //cout << "CORRECT CASE: " << ((branch_class*)(correct_case))->type_decl;

  if(correct_case == NULL) {
    //emit_bne(ACC, ZERO, num_labels, s);  
    //emit_load_string(ACC,stringtable.lookup_string((*class_nodes_instance)[current_self]->get_filename()->get_string()),s);
    expr->code(s, env);
    //emit_load_imm(T2, (*class_tags_instance)[type], s);
    emit_load(T2, 0, ACC, s);
    emit_jal("_case_abort", s);

    return;
  }
  env->enterscope();
  int stack_init_offset = stack_top;
  
  //env->addid(((branch_class*)correct_case)->name, new int(stack_top));
  add_to_env(env, ((branch_class*)correct_case)->name, FP, stack_top);

  expr->code(s, env);

  //CASE ON VOID ERROR CODE HERE
  emit_bne(ACC, ZERO, num_labels, s);  
  emit_load_string(ACC,stringtable.lookup_string((*class_nodes_instance)[current_self]->get_filename()->get_string()),s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_case_abort2", s);
  //new label for non-error code
  emit_label_ref(num_labels++, s);
  s << LABEL;


  emit_store(ACC, stack_top, FP, s); 
  stack_top--;
  emit_addiu(SP, SP, -4, s);

  ((branch_class*)correct_case)->expr->code(s, env);

  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

  env->exitscope();
}
*/

void block_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    expr->code(s, env);
  }
}

void let_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;

  //env->addid(identifier, new int(stack_top));
  add_to_env(env, identifier, FP, stack_top);

  if(init->is_empty() != true) {
    init->code(s, env);
  } else { 
    if(type_decl == Int) emit_load_int(ACC,inttable.lookup_string("0"),s);
    if(type_decl == Str) emit_load_string(ACC,stringtable.lookup_string("") ,s);
    if(type_decl == Bool) emit_load_bool(ACC, BoolConst(false), s);
  }
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s); 

  body->code(s, env);

  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; //restore stack to original state 

  env->exitscope();

}

void plus_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;
 
  //code e1 and push to stack 
  e1->code(s, env);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s); 
  
  e2->code(s, env);
  char* copy = "Object.copy";
  emit_jal(copy, s);
  emit_load(T2, 3, ACC, s); //put e2 value in T2
  //emit_store(ACC, stack_top--, FP, s);

  emit_load(T3, stack_top + 1, FP, s); //put e1 object in T3
  emit_load(T1, 3, T3, s); //put e1 value in T1

  emit_add(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);

  //restore stack
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

  env->exitscope();
}

void sub_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;
 
  //code e1 and push to stack 
  e1->code(s, env);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s); 
  
  e2->code(s, env);
  char* copy = "Object.copy";
  emit_jal(copy, s);
  emit_load(T2, 3, ACC, s); //put e2 value in T2
  //emit_store(ACC, stack_top--, FP, s);

  emit_load(T3, stack_top + 1, FP, s); //put e1 object in T3
  emit_load(T1, 3, T3, s); //put e1 value in T1

  emit_sub(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);

  //restore stack
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

  env->exitscope();

}

void mul_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;
 
  //code e1 and push to stack 
  e1->code(s, env);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s); 
  
  e2->code(s, env);
  char* copy = "Object.copy";
  emit_jal(copy, s);
  emit_load(T2, 3, ACC, s); //put e2 value in T2
  //emit_store(ACC, stack_top--, FP, s);

  emit_load(T3, stack_top + 1, FP, s); //put e1 object in T3
  emit_load(T1, 3, T3, s); //put e1 value in T1

  emit_mul(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);

  //restore stack
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

  env->exitscope();

}

void divide_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  env->enterscope();
  int stack_init_offset = stack_top;
 
  //code e1 and push to stack 
  e1->code(s, env);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s); 
  
  e2->code(s, env);
  char* copy = "Object.copy";
  emit_jal(copy, s);
  emit_load(T2, 3, ACC, s); //put e2 value in T2
  //emit_store(ACC, stack_top--, FP, s);

  emit_load(T3, stack_top + 1, FP, s); //put e1 object in T3
  emit_load(T1, 3, T3, s); //put e1 value in T1

  emit_div(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);

  //restore stack
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

  env->exitscope();
}

void neg_class::code(ostream &s, SymbolTable<Symbol, std::string> *env){
  e1->code(s, env);
  char* copy = "Object.copy";
  emit_jal(copy, s);
  emit_load(T1, 3, ACC, s);
  emit_neg(T1, T1, s);
  emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  int stack_init_offset = stack_top;

  e1->code(s, env);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s);

  e2->code(s, env);
  emit_load(T2, 3, ACC, s);
  emit_load(T1, stack_top+1, FP, s);
  emit_load(T1, 3, T1, s);

  int true_label = num_labels++;
  emit_blt(T1, T2, true_label, s);
  emit_load_bool(ACC, BoolConst(false), s);
  int end_label = num_labels++; 
  emit_branch(end_label, s);

  emit_label_ref(true_label, s);
  s << LABEL;
  emit_load_bool(ACC, BoolConst(true), s);

  emit_label_ref(end_label, s);
  s << LABEL;

  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

}

void eq_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  int stack_init_offset = stack_top;

  e1->code(s, env);
  //emit_move(T1, ACC, s);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s); 

  e2->code(s, env);
  emit_move(T2, ACC, s);
  //emit_move(ACC, ZERO, s);
  emit_load(T1, stack_top + 1, FP, s); //put e1 object in T1

  if(e1->type == Str || e1->type == Int || e1->type == Bool) {
    emit_load_bool(ACC, BoolConst(true), s);
    emit_load_bool(A1, BoolConst(false), s);
    emit_jal("equality_test", s);
  } else {
    emit_load_bool(ACC, BoolConst(false), s);
    
    int skip_equal_label = num_labels++;
    emit_bne(T1, T2, skip_equal_label, s);
    emit_load_bool(ACC, BoolConst(true), s); //will be skipped if not equal
    emit_label_def(skip_equal_label, s);//label to skip to
  }

  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 
}

void leq_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  int stack_init_offset = stack_top;
  e1->code(s, env);
  emit_store(ACC, stack_top--, FP, s);
  emit_addiu(SP, SP, -4, s);

  e2->code(s, env);
  emit_move(T2, ACC, s);
  emit_load(T2, 3, T2, s);

  emit_load(T1, stack_top + 1, FP, s);
  emit_load(T1, 3, T1, s);
  
  emit_load_bool(ACC, BoolConst(true), s);
  int leq_label = num_labels++;
  emit_bleq(T1, T2, leq_label, s);
  emit_load_bool(ACC, BoolConst(false), s);
  emit_label_def(leq_label, s);
  
  emit_addiu(SP, SP, 4*(stack_init_offset-stack_top), s);
  stack_top += 1; 

}

void comp_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  e1->code(s, env);
  emit_load(T1, 3, ACC, s);
  emit_load_bool(ACC, BoolConst(true), s);
  int label = num_labels++;
  emit_beqz(T1, label, s);
  emit_load_bool(ACC, BoolConst(false), s);
  emit_label_def(label, s);
}

void int_const_class::code(ostream& s, SymbolTable<Symbol, std::string> *env)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, SymbolTable<Symbol, std::string> *env)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, SymbolTable<Symbol, std::string> *env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  //char proto_address[type_name->get_string().length()+strlen(PROTOBJ_SUFFIX+1];

  if(type_name == SELF_TYPE) {
    emit_load_address(T1, "class_objTab", s);
    emit_load(T2, 0, SELF, s); // class tag in $T2
    
    emit_load_imm("$t3", 8, s);
    emit_mul(T2, T2, "$t3", s);

    emit_addu(T1, T1, T2, s);

    //emit_push(T1, s);

    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);

    emit_load_address(T1, "class_objTab", s);
    emit_load(T2, 0, SELF, s); // class tag in $T2
    
    emit_load_imm("$t3", 8, s);
    emit_mul(T2, T2, "$t3", s);

    emit_addu(T1, T1, T2, s);

    emit_load(T1, 1, T1, s);
    emit_jalr(T1, s); 

    return;
  }

  std::stringstream ss1, ss2, ss3;
  ss1 << type_name->get_string() << PROTOBJ_SUFFIX;
  emit_load_address(ACC, (char*)(ss1.str().c_str()), s);
  ss2 << std::string("Object.copy");
  emit_jal((char*)(ss2.str().c_str()), s);
  ss3 << type_name->get_string() << CLASSINIT_SUFFIX;
  emit_jal((char*)(ss3.str().c_str()), s);
}

void isvoid_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  e1->code(s, env);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(true), s);
  int label = num_labels++;
  emit_beqz(T1, label, s);
  emit_load_bool(ACC, BoolConst(false), s);
  emit_label_def(label, s);
}

void no_expr_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
	emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, SymbolTable<Symbol, std::string> *env) {
  if(name == self) {
    emit_move(ACC, SELF, s);
    return;
  }
  std::pair<std::string, int> pair = get_pair_from_env(env, name);
  //cout << "FIRST: " << pair.first << endl;
  if(pair.first == SELF) emit_load(ACC, pair.second, SELF, s); 
  else emit_load(ACC, pair.second, FP, s);
}
