/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 33 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"

/* doh.h uses #pragma GCC poison with GCC to prevent direct calls to certain
 * standard C library functions being introduced, but those cause errors due
 * to checks like `#if defined YYMALLOC || defined malloc` in the bison
 * template code.  We can't easily arrange to include headers after that
 * template code, so instead we disable the problematic poisoning for this
 * file.
 */
#define DOH_NO_POISON_MALLOC_FREE

#include "swig.h"
#include "cparse.h"
#include "preprocessor.h"
#include <ctype.h>

#define YYMALLOC Malloc
#define YYFREE Free

/* -----------------------------------------------------------------------------
 *                               Externals
 * ----------------------------------------------------------------------------- */

int  yyparse(void);

/* NEW Variables */

static void    *top = 0;      /* Top of the generated parse tree */
static int      unnamed = 0;  /* Unnamed datatype counter */
static Hash    *classes = 0;        /* Hash table of classes */
static Hash    *classes_typedefs = 0; /* Hash table of typedef classes: typedef struct X {...} Y; */
static Symtab  *prev_symtab = 0;
static Node    *current_class = 0;
String  *ModuleName = 0;
static Node    *module_node = 0;
static String  *Classprefix = 0;  
static String  *Namespaceprefix = 0;
static int      inclass = 0;
static Node    *currentOuterClass = 0; /* for nested classes */
static String  *last_cpptype = 0;
static int      inherit_list = 0;
static Parm    *template_parameters = 0;
static int      parsing_template_declaration = 0;
static int      extendmode   = 0;
static int      compact_default_args = 0;
static int      template_reduce = 0;
static int      cparse_externc = 0;
int		ignore_nested_classes = 0;
int		kwargs_supported = 0;

/* -----------------------------------------------------------------------------
 *                            Doxygen Comment Globals
 * ----------------------------------------------------------------------------- */
static String *currentDeclComment = NULL; /* Comment of C/C++ declaration. */

/* -----------------------------------------------------------------------------
 *                            Assist Functions
 * ----------------------------------------------------------------------------- */


 
/* Called by the parser (yyparse) when an error is found.*/
static void yyerror (const char *e) {
  (void)e;
}

static Node *new_node(const_String_or_char_ptr tag) {
  Node *n = Swig_cparse_new_node(tag);
  return n;
}

/* Copies a node.  Does not copy tree links or symbol table data (except for
   sym:name) */

static Node *copy_node(Node *n) {
  Node *nn;
  Iterator k;
  nn = NewHash();
  Setfile(nn,Getfile(n));
  Setline(nn,Getline(n));
  for (k = First(n); k.key; k = Next(k)) {
    String *ci;
    String *key = k.key;
    char *ckey = Char(key);
    if ((strcmp(ckey,"nextSibling") == 0) ||
	(strcmp(ckey,"previousSibling") == 0) ||
	(strcmp(ckey,"parentNode") == 0) ||
	(strcmp(ckey,"lastChild") == 0)) {
      continue;
    }
    if (Strncmp(key,"csym:",5) == 0) continue;
    /* We do copy sym:name.  For templates */
    if ((strcmp(ckey,"sym:name") == 0) || 
	(strcmp(ckey,"sym:weak") == 0) ||
	(strcmp(ckey,"sym:typename") == 0)) {
      String *ci = Copy(k.item);
      Setattr(nn,key, ci);
      Delete(ci);
      continue;
    }
    if (strcmp(ckey,"sym:symtab") == 0) {
      Setattr(nn,"sym:needs_symtab", "1");
    }
    /* We don't copy any other symbol table attributes */
    if (strncmp(ckey,"sym:",4) == 0) {
      continue;
    }
    /* If children.  We copy them recursively using this function */
    if (strcmp(ckey,"firstChild") == 0) {
      /* Copy children */
      Node *cn = k.item;
      while (cn) {
	Node *copy = copy_node(cn);
	appendChild(nn,copy);
	Delete(copy);
	cn = nextSibling(cn);
      }
      continue;
    }
    /* We don't copy the symbol table.  But we drop an attribute 
       requires_symtab so that functions know it needs to be built */

    if (strcmp(ckey,"symtab") == 0) {
      /* Node defined a symbol table. */
      Setattr(nn,"requires_symtab","1");
      continue;
    }
    /* Can't copy nodes */
    if (strcmp(ckey,"node") == 0) {
      continue;
    }
    if ((strcmp(ckey,"parms") == 0) || (strcmp(ckey,"pattern") == 0) || (strcmp(ckey,"throws") == 0)
	|| (strcmp(ckey,"kwargs") == 0)) {
      ParmList *pl = CopyParmList(k.item);
      Setattr(nn,key,pl);
      Delete(pl);
      continue;
    }
    if (strcmp(ckey,"nested:outer") == 0) { /* don't copy outer classes links, they will be updated later */
      Setattr(nn, key, k.item);
      continue;
    }
    /* defaultargs will be patched back in later in update_defaultargs() */
    if (strcmp(ckey,"defaultargs") == 0) {
      Setattr(nn, "needs_defaultargs", "1");
      continue;
    }
    /* same for abstracts, which contains pointers to the source node children, and so will need to be patch too */
    if (strcmp(ckey,"abstracts") == 0) {
      SetFlag(nn, "needs_abstracts");
      continue;
    }
    /* Looks okay.  Just copy the data using Copy */
    ci = Copy(k.item);
    Setattr(nn, key, ci);
    Delete(ci);
  }
  return nn;
}

static void set_comment(Node *n, String *comment) {
  String *name;
  Parm *p;
  if (!n || !comment)
    return;

  if (Getattr(n, "doxygen"))
    Append(Getattr(n, "doxygen"), comment);
  else {
    Setattr(n, "doxygen", comment);
    /* This is the first comment, populate it with @params, if any */
    p = Getattr(n, "parms");
    while (p) {
      if (Getattr(p, "doxygen"))
	Printv(comment, "\n@param ", Getattr(p, "name"), Getattr(p, "doxygen"), NIL);
      p=nextSibling(p);
    }
  }
  
  /* Append same comment to every generated overload */
  name = Getattr(n, "name");
  if (!name)
    return;
  n = nextSibling(n);
  while (n && Getattr(n, "name") && Strcmp(Getattr(n, "name"), name) == 0) {
    Setattr(n, "doxygen", comment);
    n = nextSibling(n);
  }
}

/* -----------------------------------------------------------------------------
 *                              Variables
 * ----------------------------------------------------------------------------- */

static int cplus_mode  = 0;

/* C++ modes */

#define  CPLUS_PUBLIC    1
#define  CPLUS_PRIVATE   2
#define  CPLUS_PROTECTED 3

/* storage classes */

#define SWIG_STORAGE_CLASS_EXTERNC	0x0001
#define SWIG_STORAGE_CLASS_EXTERNCPP	0x0002
#define SWIG_STORAGE_CLASS_EXTERN	0x0004
#define SWIG_STORAGE_CLASS_STATIC	0x0008
#define SWIG_STORAGE_CLASS_TYPEDEF	0x0010
#define SWIG_STORAGE_CLASS_VIRTUAL	0x0020
#define SWIG_STORAGE_CLASS_FRIEND	0x0040
#define SWIG_STORAGE_CLASS_EXPLICIT	0x0080
#define SWIG_STORAGE_CLASS_CONSTEXPR	0x0100
#define SWIG_STORAGE_CLASS_THREAD_LOCAL	0x0200

/* Test if multiple bits are set in x. */
static int multiple_bits_set(unsigned x) { return (x & (x - 1)) != 0; }

static const char* storage_class_string(int c) {
  switch (c) {
    case SWIG_STORAGE_CLASS_EXTERNC:
      return "extern \"C\"";
    case SWIG_STORAGE_CLASS_EXTERNCPP:
      return "extern \"C++\"";
    case SWIG_STORAGE_CLASS_EXTERN:
      return "extern";
    case SWIG_STORAGE_CLASS_STATIC:
      return "static";
    case SWIG_STORAGE_CLASS_TYPEDEF:
      return "typedef";
    case SWIG_STORAGE_CLASS_VIRTUAL:
      return "virtual";
    case SWIG_STORAGE_CLASS_FRIEND:
      return "friend";
    case SWIG_STORAGE_CLASS_EXPLICIT:
      return "explicit";
    case SWIG_STORAGE_CLASS_CONSTEXPR:
      return "constexpr";
    case SWIG_STORAGE_CLASS_THREAD_LOCAL:
      return "thread_local";
  }
  assert(0);
  return "<unknown>";
}

/* include types */
static int   import_mode = 0;

void SWIG_cparse_set_compact_default_args(int defargs) {
  compact_default_args = defargs;
}

int SWIG_cparse_template_reduce(int treduce) {
  template_reduce = treduce;
  return treduce;  
}

/* -----------------------------------------------------------------------------
 *                           Assist functions
 * ----------------------------------------------------------------------------- */

static int promote_type(int t) {
  if (t <= T_UCHAR || t == T_CHAR || t == T_WCHAR) return T_INT;
  return t;
}

/* Perform type-promotion for binary operators */
static int promote(int t1, int t2) {
  t1 = promote_type(t1);
  t2 = promote_type(t2);
  return t1 > t2 ? t1 : t2;
}

static String *yyrename = 0;

/* Forward renaming operator */

static String *resolve_create_node_scope(String *cname, int is_class_definition, int *errored);


Hash *Swig_cparse_features(void) {
  static Hash   *features_hash = 0;
  if (!features_hash) features_hash = NewHash();
  return features_hash;
}

/* -----------------------------------------------------------------------------
 * feature_identifier_fix()
 *
 * If a template, return template with all template parameters fully resolved.
 *
 * This is a copy and modification of typemap_identifier_fix.
 * ----------------------------------------------------------------------------- */

static String *feature_identifier_fix(String *s) {
  String *tp = SwigType_istemplate_templateprefix(s);
  if (tp) {
    String *ts, *ta, *tq;
    ts = SwigType_templatesuffix(s);
    ta = SwigType_templateargs(s);
    tq = Swig_symbol_type_qualify(ta,0);
    Append(tp,tq);
    Append(tp,ts);
    Delete(ts);
    Delete(ta);
    Delete(tq);
    return tp;
  } else {
    return NewString(s);
  }
}

static void set_access_mode(Node *n) {
  if (cplus_mode == CPLUS_PUBLIC)
    Setattr(n, "access", "public");
  else if (cplus_mode == CPLUS_PROTECTED)
    Setattr(n, "access", "protected");
  else
    Setattr(n, "access", "private");
}

static void restore_access_mode(Node *n) {
  String *mode = Getattr(n, "access");
  if (Strcmp(mode, "private") == 0)
    cplus_mode = CPLUS_PRIVATE;
  else if (Strcmp(mode, "protected") == 0)
    cplus_mode = CPLUS_PROTECTED;
  else
    cplus_mode = CPLUS_PUBLIC;
}

/* Generate the symbol table name for an object */
/* This is a bit of a mess. Need to clean up */
static String *add_oldname = 0;



static String *make_name(Node *n, String *name,SwigType *decl) {
  String *made_name = 0;
  int destructor = name && (*(Char(name)) == '~');

  if (yyrename) {
    String *s = NewString(yyrename);
    Delete(yyrename);
    yyrename = 0;
    if (destructor  && (*(Char(s)) != '~')) {
      Insert(s,0,"~");
    }
    return s;
  }

  if (!name) return 0;

  if (parsing_template_declaration)
    SetFlag(n, "parsing_template_declaration");
  made_name = Swig_name_make(n, Namespaceprefix, name, decl, add_oldname);
  Delattr(n, "parsing_template_declaration");

  return made_name;
}

/* Generate an unnamed identifier */
static String *make_unnamed(void) {
  unnamed++;
  return NewStringf("$unnamed%d$",unnamed);
}

static int is_operator(String *name) {
  return Strncmp(name,"operator ", 9) == 0;
}

/* Add declaration list to symbol table */
static int  add_only_one = 0;

static void add_symbols(Node *n) {
  String *decl;
  String *wrn = 0;

  if (inclass && n) {
    cparse_normalize_void(n);
  }
  while (n) {
    String *symname = 0;
    String *old_prefix = 0;
    Symtab *old_scope = 0;
    int isfriend = inclass && Strstr(Getattr(n, "storage"), "friend") != NULL;
    int iscdecl = Cmp(nodeType(n),"cdecl") == 0;
    int only_csymbol = 0;
    
    if (inclass) {
      String *name = Getattr(n, "name");
      if (isfriend) {
	/* Friends methods in a class are declared in the namespace enclosing the class (outer most class if a nested class) */
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	Node *outer = currentOuterClass;
	Symtab *namespace_symtab;
	old_prefix = Namespaceprefix;
	old_scope = Swig_symbol_current();

	assert(outer);
	while (Getattr(outer, "nested:outer")) {
	  outer = Getattr(outer, "nested:outer");
	}
	namespace_symtab = Getattr(outer, "sym:symtab");
	if (!namespace_symtab)
	  namespace_symtab = Getattr(outer, "unofficial:symtab");
	Swig_symbol_setscope(namespace_symtab);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);

	if (!prefix) {
	  /* To check - this should probably apply to operators too */
	  if (name && !is_operator(name) && Namespaceprefix) {
	    String *friendusing = NewStringf("using namespace %s;", Namespaceprefix);
	    Setattr(n, "friendusing", friendusing);
	    Delete(friendusing);
	  }
	} else {
	  /* Qualified friend declarations should not be possible as they are ignored in the parse tree */
	  assert(0);
	}
      } else if (Equal(nodeType(n), "using")) {
	String *uname = Getattr(n, "uname");
	Node *cls = currentOuterClass;
	String *nprefix = 0;
	String *nlast = 0;
	Swig_scopename_split(uname, &nprefix, &nlast);
	if (Swig_item_in_list(Getattr(cls, "baselist"), nprefix) || Swig_item_in_list(Getattr(cls, "protectedbaselist"), nprefix) || Swig_item_in_list(Getattr(cls, "privatebaselist"), nprefix)) {
	  String *plain_name = SwigType_istemplate(nprefix) ? SwigType_templateprefix(nprefix) : nprefix;
	  if (Equal(nlast, plain_name)) {
	    /* Using declaration looks like it is using a constructor in an immediate base class - change the constructor name for this class.
	     * C++11 requires using declarations for inheriting base constructors to be in the immediate base class.
	     * Note that we don't try and look up the constructor in the base class as the constructor may be an implicit/implied constructor and hence not exist. */
	    Symtab *stab = Swig_symbol_current();
	    String *nname = Getattr(stab, "name");
	    Setattr(n, "name", nname);
	    SetFlag(n, "usingctor");
	  }
	}
      } else {
	/* for member functions, we need to remove the redundant
	   class scope if provided, as in
	   
	   struct Foo {
	   int Foo::method(int a);
	   };
	   
	*/
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	if (prefix) {
	  if (Classprefix && (Equal(prefix,Classprefix))) {
	    String *base = Swig_scopename_last(name);
	    Setattr(n,"name",base);
	    Delete(base);
	  }
	  Delete(prefix);
	}
      }
    }

    if (!isfriend && (inclass || extendmode)) {
      Setattr(n,"ismember","1");
    }

    if (extendmode) {
      if (!Getattr(n, "template"))
        SetFlag(n,"isextendmember");
    }

    if (!isfriend && inclass) {
      if ((cplus_mode != CPLUS_PUBLIC)) {
	only_csymbol = 1;
	if (cplus_mode == CPLUS_PROTECTED) {
	  Setattr(n,"access", "protected");
	  only_csymbol = !Swig_need_protected(n);
	} else {
	  Setattr(n,"access", "private");
	  /* private are needed only when they are pure virtuals - why? */
	  if ((Cmp(Getattr(n,"storage"),"virtual") == 0) && (Cmp(Getattr(n,"value"),"0") == 0)) {
	    only_csymbol = 0;
	  }
	  if (Cmp(nodeType(n),"destructor") == 0) {
	    /* Needed for "unref" feature */
	    only_csymbol = 0;
	  }
	}
      } else {
	Setattr(n, "access", "public");
      }
    } else if (extendmode && !inclass) {
      Setattr(n, "access", "public");
    }

    if (Getattr(n,"sym:name")) {
      n = nextSibling(n);
      continue;
    }
    decl = Getattr(n,"decl");
    if (!SwigType_isfunction(decl)) {
      String *name = Getattr(n,"name");
      String *makename = Getattr(n,"parser:makename");
      if (iscdecl) {	
	String *storage = Getattr(n, "storage");
	if (Cmp(storage,"typedef") == 0) {
	  Setattr(n,"kind","typedef");
	} else {
	  SwigType *type = Getattr(n,"type");
	  String *value = Getattr(n,"value");
	  Setattr(n,"kind","variable");
	  if (value && Len(value)) {
	    Setattr(n,"hasvalue","1");
	  }
	  if (type) {
	    SwigType *ty;
	    SwigType *tmp = 0;
	    if (decl) {
	      ty = tmp = Copy(type);
	      SwigType_push(ty,decl);
	    } else {
	      ty = type;
	    }
	    if (storage && (Strstr(storage, "constexpr") || (Strstr(storage, "static") && !SwigType_ismutable(ty)))) {
	      SetFlag(n, "hasconsttype");
	    }
	    Delete(tmp);
	  }
	  if (!type) {
	    Printf(stderr,"notype name %s\n", name);
	  }
	}
      }
      Swig_features_get(Swig_cparse_features(), Namespaceprefix, name, 0, n);
      if (makename) {
	symname = make_name(n, makename,0);
        Delattr(n,"parser:makename"); /* temporary information, don't leave it hanging around */
      } else {
        makename = name;
	symname = make_name(n, makename,0);
      }
      
      if (!symname) {
	symname = Copy(Getattr(n,"unnamed"));
      }
      if (symname) {
	if (parsing_template_declaration)
	  SetFlag(n, "parsing_template_declaration");
	wrn = Swig_name_warning(n, Namespaceprefix, symname,0);
	Delattr(n, "parsing_template_declaration");
      }
    } else {
      String *name = Getattr(n,"name");
      SwigType *fdecl = Copy(decl);
      SwigType *fun = SwigType_pop_function(fdecl);
      if (iscdecl) {	
	Setattr(n,"kind","function");
      }
      
      Swig_features_get(Swig_cparse_features(),Namespaceprefix,name,fun,n);

      symname = make_name(n, name,fun);
      if (parsing_template_declaration)
	SetFlag(n, "parsing_template_declaration");
      wrn = Swig_name_warning(n, Namespaceprefix,symname,fun);
      Delattr(n, "parsing_template_declaration");
      
      Delete(fdecl);
      Delete(fun);
      
    }
    if (!symname) {
      n = nextSibling(n);
      continue;
    }

    if (GetFlag(n, "valueignored")) {
      SWIG_WARN_NODE_BEGIN(n);
      Swig_warning(WARN_PARSE_ASSIGNED_VALUE, Getfile(n), Getline(n), "Value assigned to %s not used due to limited parsing implementation.\n", SwigType_namestr(Getattr(n, "name")));
      SWIG_WARN_NODE_END(n);
    }

    if (cparse_cplusplus) {
      String *value = Getattr(n, "value");
      if (value && Strcmp(value, "delete") == 0) {
	/* C++11 deleted definition / deleted function */
        SetFlag(n,"deleted");
        SetFlag(n,"feature:ignore");
      }
      if (SwigType_isrvalue_reference(Getattr(n, "refqualifier"))) {
	/* Ignore rvalue ref-qualifiers by default
	 * Use Getattr instead of GetFlag to handle explicit ignore and explicit not ignore */
	if (!(Getattr(n, "feature:ignore") || Strncmp(symname, "$ignore", 7) == 0)) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(WARN_TYPE_RVALUE_REF_QUALIFIER_IGNORED, Getfile(n), Getline(n),
	      "Method with rvalue ref-qualifier %s ignored.\n", Swig_name_decl(n));
	  SWIG_WARN_NODE_END(n);
	  SetFlag(n, "feature:ignore");
	}
      }
      if (Equal(Getattr(n, "type"), "auto")) {
	/* Ignore functions with an auto return type and no trailing return type
	 * Use Getattr instead of GetFlag to handle explicit ignore and explicit not ignore */
	if (!(Getattr(n, "feature:ignore") || Strncmp(symname, "$ignore", 7) == 0)) {
	  SWIG_WARN_NODE_BEGIN(n);
	  if (SwigType_isfunction(Getattr(n, "decl")))
	    Swig_warning(WARN_CPP14_AUTO, Getfile(n), Getline(n), "Unable to deduce auto return type for '%s' (ignored).\n", Swig_name_decl(n));
	  else
	    Swig_warning(WARN_CPP11_AUTO, Getfile(n), Getline(n), "Unable to deduce auto type for variable '%s' (ignored).\n", Swig_name_decl(n));
	  SWIG_WARN_NODE_END(n);
	  SetFlag(n, "feature:ignore");
	}
      }
    }
    if (only_csymbol || GetFlag(n, "feature:ignore") || Strncmp(symname, "$ignore", 7) == 0) {
      /* Only add to C symbol table and continue */
      Swig_symbol_add(0, n);
      if (!only_csymbol && !GetFlag(n, "feature:ignore")) {
	/* Print the warning attached to $ignore name, if any */
        char *c = Char(symname) + 7;
	if (strlen(c)) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n), Getline(n), "%s\n",c+1);
	  SWIG_WARN_NODE_END(n);
	}
	/* If the symbol was ignored via "rename" and is visible, set also feature:ignore*/
	SetFlag(n, "feature:ignore");
      }
      if (!GetFlag(n, "feature:ignore") && Strcmp(symname,"$ignore") == 0) {
	/* Add feature:ignore if the symbol was explicitly ignored, regardless of visibility */
	SetFlag(n, "feature:ignore");
      }
    } else {
      Node *c;
      if ((wrn) && (Len(wrn))) {
	String *metaname = symname;
	if (!Getmeta(metaname,"already_warned")) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n),Getline(n), "%s\n", wrn);
	  SWIG_WARN_NODE_END(n);
	  Setmeta(metaname,"already_warned","1");
	}
      }
      c = Swig_symbol_add(symname,n);

      if (c != n) {
	/* symbol conflict attempting to add in the new symbol */
	if (Getattr(n,"sym:weak")) {
	  Setattr(n,"sym:name",symname);
	} else {
	  Swig_symbol_conflict_warn(n, c, symname, inclass);
        }
      }
    }
    /* restore the class scope if needed */
    if (isfriend) {
      Swig_symbol_setscope(old_scope);
      if (old_prefix) {
	Delete(Namespaceprefix);
	Namespaceprefix = old_prefix;
      }
    }
    Delete(symname);

    if (add_only_one) return;
    n = nextSibling(n);
  }
}


/* add symbols a parse tree node copy */

static void add_symbols_copy(Node *n) {
  int    emode = 0;
  while (n) {
    if (Equal(nodeType(n), "access")) {
      String *kind = Getattr(n,"kind");
      if (Strcmp(kind,"public") == 0) {
	cplus_mode = CPLUS_PUBLIC;
      } else if (Strcmp(kind,"private") == 0) {
	cplus_mode = CPLUS_PRIVATE;
      } else if (Strcmp(kind,"protected") == 0) {
	cplus_mode = CPLUS_PROTECTED;
      }
      n = nextSibling(n);
      continue;
    }

    add_oldname = Getattr(n,"sym:name");
    if ((add_oldname) || (Getattr(n,"sym:needs_symtab"))) {
      int old_inclass = -1;
      Node *oldCurrentOuterClass = 0;
      if (add_oldname) {
	DohIncref(add_oldname);
	/*  Disable this, it prevents %rename to work with templates */
	/* If already renamed, we used that name  */
	/*
	if (Strcmp(add_oldname, Getattr(n,"name")) != 0) {
	  Delete(yyrename);
	  yyrename = Copy(add_oldname);
	}
	*/
      }
      Delattr(n,"sym:needs_symtab");
      Delattr(n,"sym:name");

      add_only_one = 1;
      add_symbols(n);

      if (Getattr(n,"partialargs")) {
	Swig_symbol_cadd(Getattr(n,"partialargs"),n);
      }
      add_only_one = 0;
      if (Equal(nodeType(n), "class")) {
	/* add_symbols() above sets "sym:symtab", so "unofficial:symtab" is not required */
	old_inclass = inclass;
	oldCurrentOuterClass = currentOuterClass;
	inclass = 1;
	currentOuterClass = n;
	if (Strcmp(Getattr(n,"kind"),"class") == 0) {
	  cplus_mode = CPLUS_PRIVATE;
	} else {
	  cplus_mode = CPLUS_PUBLIC;
	}
      }
      if (Equal(nodeType(n), "extend")) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }

      if (Getattr(n, "requires_symtab")) {
	Swig_symbol_newscope();
	Swig_symbol_setscopename(Getattr(n, "name"));
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }

      add_symbols_copy(firstChild(n));

      if (Equal(nodeType(n), "extend")) {
	cplus_mode = emode;
      }
      if (Getattr(n,"requires_symtab")) {
	Setattr(n,"symtab", Swig_symbol_popscope());
	Delattr(n,"requires_symtab");
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (add_oldname) {
	Delete(add_oldname);
	add_oldname = 0;
      }
      if (Equal(nodeType(n), "class")) {
	inclass = old_inclass;
	currentOuterClass = oldCurrentOuterClass;
      }
    } else {
      if (Equal(nodeType(n), "extend")) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (Equal(nodeType(n), "extend")) {
	cplus_mode = emode;
      }
    }
    n = nextSibling(n);
  }
}

/* Add in the "defaultargs" attribute for functions in instantiated templates.
 * n should be any instantiated template (class or start of linked list of functions). */
static void update_defaultargs(Node *n) {
  if (n) {
    Node *firstdefaultargs = n;
    update_defaultargs(firstChild(n));
    n = nextSibling(n);
    /* recursively loop through nodes of all types, but all we really need are the overloaded functions */
    while (n) {
      update_defaultargs(firstChild(n));
      if (!Getattr(n, "defaultargs")) {
	if (Getattr(n, "needs_defaultargs")) {
	  Setattr(n, "defaultargs", firstdefaultargs);
	  Delattr(n, "needs_defaultargs");
	} else {
	  firstdefaultargs = n;
	}
      } else {
	/* Functions added in with %extend (for specialized template classes) will already have default args patched up */
	assert(Getattr(n, "defaultargs") == firstdefaultargs);
      }
      n = nextSibling(n);
    }
  }
}

/* Check a set of declarations to see if any are pure-abstract */

static List *pure_abstracts(Node *n) {
  List *abstracts = 0;
  while (n) {
    if (Cmp(nodeType(n),"cdecl") == 0) {
      String *decl = Getattr(n,"decl");
      if (SwigType_isfunction(decl)) {
	String *init = Getattr(n,"value");
	if (Cmp(init,"0") == 0) {
	  if (!abstracts) {
	    abstracts = NewList();
	  }
	  Append(abstracts,n);
	  SetFlag(n,"abstract");
	}
      }
    } else if (Cmp(nodeType(n),"destructor") == 0) {
      if (Cmp(Getattr(n,"value"),"0") == 0) {
	if (!abstracts) {
	  abstracts = NewList();
	}
	Append(abstracts,n);
	SetFlag(n,"abstract");
      }
    }
    n = nextSibling(n);
  }
  return abstracts;
}

/* Recompute the "abstracts" attribute for the classes in instantiated templates, similarly to update_defaultargs() above. */
static void update_abstracts(Node *n) {
  for (; n; n = nextSibling(n)) {
    Node* const child = firstChild(n);
    if (!child)
      continue;

    update_abstracts(child);

    if (Getattr(n, "needs_abstracts")) {
      Setattr(n, "abstracts", pure_abstracts(child));
      Delattr(n, "needs_abstracts");
    }
  }
}

/* Make a classname */

static String *make_class_name(String *name) {
  String *nname = 0;
  String *prefix;
  if (Namespaceprefix) {
    nname= NewStringf("%s::%s", Namespaceprefix, name);
  } else {
    nname = NewString(name);
  }
  prefix = SwigType_istemplate_templateprefix(nname);
  if (prefix) {
    String *args, *qargs;
    args   = SwigType_templateargs(nname);
    qargs  = Swig_symbol_type_qualify(args,0);
    Append(prefix,qargs);
    Delete(nname);
    Delete(args);
    Delete(qargs);
    nname = prefix;
  }
  return nname;
}

/* Use typedef name as class name */

static void add_typedef_name(Node *n, Node *declnode, String *oldName, Symtab *cscope, String *scpname) {
  String *class_rename = 0;
  SwigType *decl = Getattr(declnode, "decl");
  if (!decl || !Len(decl)) {
    String *cname;
    String *tdscopename;
    String *class_scope = Swig_symbol_qualifiedscopename(cscope);
    String *name = Getattr(declnode, "name");
    cname = Copy(name);
    Setattr(n, "tdname", cname);
    tdscopename = class_scope ? NewStringf("%s::%s", class_scope, name) : Copy(name);
    class_rename = Getattr(n, "class_rename");
    if (class_rename && (Strcmp(class_rename, oldName) == 0))
      Setattr(n, "class_rename", NewString(name));
    if (!classes_typedefs) classes_typedefs = NewHash();
    if (!Equal(scpname, tdscopename) && !Getattr(classes_typedefs, tdscopename)) {
      Setattr(classes_typedefs, tdscopename, n);
    }
    Setattr(n, "decl", decl);
    Delete(class_scope);
    Delete(cname);
    Delete(tdscopename);
  }
}

/* If the class name is qualified.  We need to create or lookup namespace entries */

static Symtab *set_scope_to_global(void) {
  Symtab *symtab = Swig_symbol_global_scope();
  Swig_symbol_setscope(symtab);
  return symtab;
}
 
/* Remove the block braces, { and }, if the 'noblock' attribute is set.
 * Node *kw can be either a Hash or Parmlist. */
static String *remove_block(Node *kw, const String *inputcode) {
  String *modified_code = 0;
  while (kw) {
   String *name = Getattr(kw,"name");
   if (name && (Cmp(name,"noblock") == 0)) {
     char *cstr = Char(inputcode);
     int len = Len(inputcode);
     if (len && cstr[0] == '{') {
       --len; ++cstr; 
       if (len && cstr[len - 1] == '}') { --len; }
       /* we now remove the extra spaces */
       while (len && isspace((int)cstr[0])) { --len; ++cstr; }
       while (len && isspace((int)cstr[len - 1])) { --len; }
       modified_code = NewStringWithSize(cstr, len);
       break;
     }
   }
   kw = nextSibling(kw);
  }
  return modified_code;
}

/*
#define RESOLVE_DEBUG 1
*/
static Node *nscope = 0;
static Node *nscope_inner = 0;

/* Remove the scope prefix from cname and return the base name without the prefix.
 * The scopes required for the symbol name are resolved and/or created, if required.
 * For example AA::BB::CC as input returns CC and creates the namespace AA then inner 
 * namespace BB in the current scope. */
static String *resolve_create_node_scope(String *cname_in, int is_class_definition, int *errored) {
  Symtab *gscope = 0;
  Node *cname_node = 0;
  String *cname = cname_in;
  String *last = Swig_scopename_last(cname);
  nscope = 0;
  nscope_inner = 0;  
  *errored = 0;

  if (Strncmp(cname, "::", 2) == 0) {
    if (is_class_definition) {
      Swig_error(cparse_file, cparse_line, "Using the unary scope operator :: in class definition '%s' is invalid.\n", SwigType_namestr(cname));
      *errored = 1;
      return last;
    }
    cname = NewString(Char(cname) + 2);
  }
  if (is_class_definition) {
    /* Only lookup symbols which are in scope via a using declaration but not via a using directive.
       For example find y via 'using x::y' but not y via a 'using namespace x'. */
    cname_node = Swig_symbol_clookup_no_inherit(cname, 0);
    if (!cname_node) {
      Node *full_lookup_node = Swig_symbol_clookup(cname, 0);
      if (full_lookup_node) {
       /* This finds a symbol brought into scope via both a using directive and a using declaration. */
	Node *last_node = Swig_symbol_clookup_no_inherit(last, 0);
	if (last_node == full_lookup_node)
	  cname_node = last_node;
      }
    }
  } else {
    /* For %template, the template needs to be in scope via any means. */
    cname_node = Swig_symbol_clookup(cname, 0);
  }
#if RESOLVE_DEBUG
  if (!cname_node)
    Printf(stdout, "symbol does not yet exist (%d): [%s]\n", is_class_definition, cname_in);
  else
    Printf(stdout, "symbol does exist (%d): [%s]\n", is_class_definition, cname_in);
#endif

  if (cname_node) {
    /* The symbol has been defined already or is in another scope.
       If it is a weak symbol, it needs replacing and if it was brought into the current scope,
       the scope needs adjusting appropriately for the new symbol.
       Similarly for defined templates. */
    Symtab *symtab = Getattr(cname_node, "sym:symtab");
    Node *sym_weak = Getattr(cname_node, "sym:weak");
    if ((symtab && sym_weak) || Equal(nodeType(cname_node), "template")) {
      /* Check if the scope is the current scope */
      String *current_scopename = Swig_symbol_qualifiedscopename(0);
      String *found_scopename = Swig_symbol_qualifiedscopename(symtab);
      if (!current_scopename)
	current_scopename = NewString("");
      if (!found_scopename)
	found_scopename = NewString("");

      {
	int fail = 1;
	List *current_scopes = Swig_scopename_tolist(current_scopename);
	List *found_scopes = Swig_scopename_tolist(found_scopename);
        Iterator cit = First(current_scopes);
	Iterator fit = First(found_scopes);
#if RESOLVE_DEBUG
Printf(stdout, "comparing current: [%s] found: [%s]\n", current_scopename, found_scopename);
#endif
	for (; fit.item && cit.item; fit = Next(fit), cit = Next(cit)) {
	  String *current = cit.item;
	  String *found = fit.item;
#if RESOLVE_DEBUG
	  Printf(stdout, "  looping %s %s\n", current, found);
#endif
	  if (Strcmp(current, found) != 0)
	    break;
	}

	if (!cit.item) {
	  String *subscope = NewString("");
	  for (; fit.item; fit = Next(fit)) {
	    if (Len(subscope) > 0)
	      Append(subscope, "::");
	    Append(subscope, fit.item);
	  }
	  if (Len(subscope) > 0)
	    cname = NewStringf("%s::%s", subscope, last);
	  else
	    cname = Copy(last);
#if RESOLVE_DEBUG
	  Printf(stdout, "subscope to create: [%s] cname: [%s]\n", subscope, cname);
#endif
	  fail = 0;
	  Delete(subscope);
	} else {
	  if (is_class_definition) {
	    if (!fit.item) {
	      /* It is valid to define a new class with the same name as one forward declared in a parent scope */
	      fail = 0;
	    } else if (Swig_scopename_check(cname)) {
	      /* Classes defined with scope qualifiers must have a matching forward declaration in matching scope */
	      fail = 1;
	    } else {
	      /* This may let through some invalid cases */
	      fail = 0;
	    }
#if RESOLVE_DEBUG
	    Printf(stdout, "scope for class definition, fail: %d\n", fail);
#endif
	  } else {
#if RESOLVE_DEBUG
	    Printf(stdout, "no matching base scope for template\n");
#endif
	    fail = 1;
	  }
	}

	Delete(found_scopes);
	Delete(current_scopes);

	if (fail) {
	  String *cname_resolved = NewStringf("%s::%s", found_scopename, last);
	  Swig_error(cparse_file, cparse_line, "'%s' resolves to '%s' and was incorrectly instantiated in scope '%s' instead of within scope '%s'.\n",
	    SwigType_namestr(cname_in), SwigType_namestr(cname_resolved), SwigType_namestr(current_scopename), SwigType_namestr(found_scopename));
	  *errored = 1;
	  Delete(cname_resolved);
	}
      }

      Delete(current_scopename);
      Delete(found_scopename);
    }
  } else if (!is_class_definition) {
    /* A template instantiation requires a template to be found in scope */
    Swig_error(cparse_file, cparse_line, "Template '%s' undefined.\n", SwigType_namestr(cname_in));
    *errored = 1;
  }

  if (*errored)
    return last;

  if (Swig_scopename_check(cname) && !*errored) {
    Node   *ns;
    String *prefix = Swig_scopename_prefix(cname);
    if (Len(prefix) == 0) {
      String *base = Copy(last);
      /* Use the global scope, but we need to add a 'global' namespace.  */
      if (!gscope) gscope = set_scope_to_global();
      /* note that this namespace is not the "unnamed" one,
	 and we don't use Setattr(nscope,"name", ""),
	 because the unnamed namespace is private */
      nscope = new_node("namespace");
      Setattr(nscope,"symtab", gscope);;
      nscope_inner = nscope;
      Delete(last);
      return base;
    }
    /* Try to locate the scope */
    ns = Swig_symbol_clookup(prefix,0);
    if (!ns) {
      Swig_error(cparse_file, cparse_line, "Undefined scope '%s'\n", SwigType_namestr(prefix));
      *errored = 1;
    } else {
      Symtab *nstab = Getattr(ns,"symtab");
      if (!nstab) {
	Swig_error(cparse_file, cparse_line, "'%s' is not defined as a valid scope.\n", SwigType_namestr(prefix));
	*errored = 1;
	ns = 0;
      } else {
	/* Check if the node scope is the current scope */
	String *tname = Swig_symbol_qualifiedscopename(0);
	String *nname = Swig_symbol_qualifiedscopename(nstab);
	if (tname && (Strcmp(tname,nname) == 0)) {
	  ns = 0;
	  cname = Copy(last);
	}
	Delete(tname);
	Delete(nname);
      }
      if (ns) {
	/* we will try to create a new node using the namespaces we
	   can find in the scope name */
	List *scopes = Swig_scopename_tolist(prefix);
	String *sname;
	Iterator si;

	for (si = First(scopes); si.item; si = Next(si)) {
	  Node *ns1,*ns2;
	  sname = si.item;
	  ns1 = Swig_symbol_clookup(sname,0);
	  assert(ns1);
	  if (Strcmp(nodeType(ns1),"namespace") == 0) {
	    if (Getattr(ns1,"alias")) {
	      ns1 = Getattr(ns1,"namespace");
	    }
	  } else {
	    /* now this last part is a class */
	    si = Next(si);
	    /*  or a nested class tree, which is unrolled here */
	    for (; si.item; si = Next(si)) {
	      if (si.item) {
		Printf(sname,"::%s",si.item);
	      }
	    }
	    /* we get the 'inner' class */
	    nscope_inner = Swig_symbol_clookup(sname,0);
	    /* set the scope to the inner class */
	    Swig_symbol_setscope(Getattr(nscope_inner,"symtab"));
	    /* save the last namespace prefix */
	    Delete(Namespaceprefix);
	    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	    /* and return the node name, including the inner class prefix */
	    break;
	  }
	  /* here we just populate the namespace tree as usual */
	  ns2 = new_node("namespace");
	  Setattr(ns2,"name",sname);
	  Setattr(ns2,"symtab", Getattr(ns1,"symtab"));
	  add_symbols(ns2);
	  Swig_symbol_setscope(Getattr(ns1,"symtab"));
	  Delete(Namespaceprefix);
	  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	  if (nscope_inner) {
	    if (Getattr(nscope_inner,"symtab") != Getattr(ns2,"symtab")) {
	      appendChild(nscope_inner,ns2);
	      Delete(ns2);
	    }
	  }
	  nscope_inner = ns2;
	  if (!nscope) nscope = ns2;
	}
	cname = Copy(last);
	Delete(scopes);
      }
    }
    Delete(prefix);
  }
  Delete(last);

  return cname;
}
 
/* look for simple typedef name in typedef list */
static String *try_to_find_a_name_for_unnamed_structure(const String *storage, Node *decls) {
  String *name = 0;
  Node *n = decls;
  if (storage && Equal(storage, "typedef")) {
    for (; n; n = nextSibling(n)) {
      if (!Len(Getattr(n, "decl"))) {
	name = Copy(Getattr(n, "name"));
	break;
      }
    }
  }
  return name;
}

/* traverse copied tree segment, and update outer class links*/
static void update_nested_classes(Node *n)
{
  Node *c = firstChild(n);
  while (c) {
    if (Getattr(c, "nested:outer"))
      Setattr(c, "nested:outer", n);
    update_nested_classes(c);
    c = nextSibling(c);
  }
}

/* -----------------------------------------------------------------------------
 * nested_forward_declaration()
 * 
 * Nested struct handling for C++ code if the nested classes are disabled.
 * Create the nested class/struct/union as a forward declaration.
 * ----------------------------------------------------------------------------- */

static Node *nested_forward_declaration(const String *storage, const String *kind, String *sname, String *name, Node *cpp_opt_declarators) {
  Node *nn = 0;

  if (sname) {
    /* Add forward declaration of the nested type */
    Node *n = new_node("classforward");
    Setattr(n, "kind", kind);
    Setattr(n, "name", sname);
    Setattr(n, "storage", storage);
    Setattr(n, "sym:weak", "1");
    SetFlag(n, "nested:forward");
    add_symbols(n);
    nn = n;
  }

  /* Add any variable instances. Also add in any further typedefs of the nested type.
     Note that anonymous typedefs (eg typedef struct {...} a, b;) are treated as class forward declarations */
  if (cpp_opt_declarators) {
    int storage_typedef = (storage && Equal(storage, "typedef"));
    int variable_of_anonymous_type = !sname && !storage_typedef;
    if (!variable_of_anonymous_type) {
      int anonymous_typedef = !sname && storage_typedef;
      Node *n = cpp_opt_declarators;
      SwigType *type = name;
      while (n) {
	Setattr(n, "type", type);
	Setattr(n, "storage", storage);
	if (anonymous_typedef) {
	  Setattr(n, "nodeType", "classforward");
	  Setattr(n, "sym:weak", "1");
	}
	n = nextSibling(n);
      }
      add_symbols(cpp_opt_declarators);

      if (nn) {
	set_nextSibling(nn, cpp_opt_declarators);
      } else {
	nn = cpp_opt_declarators;
      }
    }
  }

  if (!currentOuterClass || !GetFlag(currentOuterClass, "nested")) {
    if (nn && Equal(nodeType(nn), "classforward")) {
      Node *n = nn;
      if (!GetFlag(n, "feature:ignore")) {
	SWIG_WARN_NODE_BEGIN(n);
	Swig_warning(WARN_PARSE_NAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (%s ignored)\n", kind, SwigType_namestr(sname ? sname : name));
	SWIG_WARN_NODE_END(n);
      }
    } else {
      Swig_warning(WARN_PARSE_UNNAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (ignored).\n", kind);
    }
  }

  return nn;
}


Node *Swig_cparse(File *f) {
  scanner_file(f);
  top = 0;
  if (yyparse() == 2) {
    Swig_error(cparse_file, cparse_line, "Parser exceeded stack depth or ran out of memory\n");
    Exit(EXIT_FAILURE);
  }
  return (Node *)top;
}

static void single_new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {
  String *fname;
  String *name;
  String *fixname;
  SwigType *t = Copy(type);

  /* Printf(stdout, "single_new_feature: [%s] [%s] [%s] [%s] [%s] [%s]\n", featurename, val, declaratorid, t, ParmList_str_defaultargs(declaratorparms), qualifier); */

  fname = NewStringf("feature:%s",featurename);
  if (declaratorid) {
    fixname = feature_identifier_fix(declaratorid);
  } else {
    fixname = NewStringEmpty();
  }
  if (Namespaceprefix) {
    name = NewStringf("%s::%s",Namespaceprefix, fixname);
  } else {
    name = fixname;
  }

  if (declaratorparms) Setmeta(val,"parms",declaratorparms);
  if (!Len(t)) t = 0;
  if (t) {
    if (qualifier) SwigType_push(t,qualifier);
    if (SwigType_isfunction(t)) {
      SwigType *decl = SwigType_pop_function(t);
      if (SwigType_ispointer(t)) {
	String *nname = NewStringf("*%s",name);
	Swig_feature_set(Swig_cparse_features(), nname, decl, fname, val, featureattribs);
	Delete(nname);
      } else {
	Swig_feature_set(Swig_cparse_features(), name, decl, fname, val, featureattribs);
      }
      Delete(decl);
    } else if (SwigType_ispointer(t)) {
      String *nname = NewStringf("*%s",name);
      Swig_feature_set(Swig_cparse_features(),nname,0,fname,val, featureattribs);
      Delete(nname);
    }
  } else {
    /* Global feature, that is, feature not associated with any particular symbol */
    Swig_feature_set(Swig_cparse_features(),name,0,fname,val, featureattribs);
  }
  Delete(fname);
  Delete(name);
}

/* Add a new feature to the Hash. Additional features are added if the feature has a parameter list (declaratorparms)
 * and one or more of the parameters have a default argument. An extra feature is added for each defaulted parameter,
 * simulating the equivalent overloaded method. */
static void new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {

  ParmList *declparms = declaratorparms;

  /* remove the { and } braces if the noblock attribute is set */
  String *newval = remove_block(featureattribs, val);
  val = newval ? newval : val;

  /* Add the feature */
  single_new_feature(featurename, val, featureattribs, declaratorid, type, declaratorparms, qualifier);

  /* Add extra features if there are default parameters in the parameter list */
  if (type) {
    while (declparms) {
      if (ParmList_has_defaultargs(declparms)) {

        /* Create a parameter list for the new feature by copying all
           but the last (defaulted) parameter */
        ParmList* newparms = CopyParmListMax(declparms, ParmList_len(declparms)-1);

        /* Create new declaration - with the last parameter removed */
        SwigType *newtype = Copy(type);
        Delete(SwigType_pop_function(newtype)); /* remove the old parameter list from newtype */
        SwigType_add_function(newtype,newparms);

        single_new_feature(featurename, Copy(val), featureattribs, declaratorid, newtype, newparms, qualifier);
        declparms = newparms;
      } else {
        declparms = 0;
      }
    }
  }
}

/* check if a function declaration is a plain C object */
static int is_cfunction(Node *n) {
  if (!cparse_cplusplus || cparse_externc)
    return 1;
  if (Swig_storage_isexternc(n)) {
    return 1;
  }
  return 0;
}

/* If the Node is a function with parameters, check to see if any of the parameters
 * have default arguments. If so create a new function for each defaulted argument. 
 * The additional functions form a linked list of nodes with the head being the original Node n. */
static void default_arguments(Node *n) {
  Node *function = n;

  if (function) {
    ParmList *varargs = Getattr(function,"feature:varargs");
    if (varargs) {
      /* Handles the %varargs directive by looking for "feature:varargs" and 
       * substituting ... with an alternative set of arguments.  */
      Parm     *p = Getattr(function,"parms");
      Parm     *pp = 0;
      while (p) {
	SwigType *t = Getattr(p,"type");
	if (Strcmp(t,"v(...)") == 0) {
	  if (pp) {
	    ParmList *cv = Copy(varargs);
	    set_nextSibling(pp,cv);
	    Delete(cv);
	  } else {
	    ParmList *cv =  Copy(varargs);
	    Setattr(function,"parms", cv);
	    Delete(cv);
	  }
	  break;
	}
	pp = p;
	p = nextSibling(p);
      }
    }

    /* Do not add in functions if kwargs is being used or if user wants old default argument wrapping
       (one wrapped method per function irrespective of number of default arguments) */
    if (compact_default_args 
	|| is_cfunction(function) 
	|| GetFlag(function,"feature:compactdefaultargs") 
	|| (GetFlag(function,"feature:kwargs") && kwargs_supported)) {
      ParmList *p = Getattr(function,"parms");
      if (p) 
        Setattr(p,"compactdefargs", "1"); /* mark parameters for special handling */
      function = 0; /* don't add in extra methods */
    }
  }

  while (function) {
    ParmList *parms = Getattr(function,"parms");
    if (ParmList_has_defaultargs(parms)) {

      /* Create a parameter list for the new function by copying all
         but the last (defaulted) parameter */
      ParmList* newparms = CopyParmListMax(parms,ParmList_len(parms)-1);

      /* Create new function and add to symbol table */
      {
	SwigType *ntype = Copy(nodeType(function));
	char *cntype = Char(ntype);
        Node *new_function = new_node(ntype);
        SwigType *decl = Copy(Getattr(function,"decl"));
        int constqualifier = SwigType_isconst(decl);
	String *ccode = Copy(Getattr(function,"code"));
	String *cstorage = Copy(Getattr(function,"storage"));
	String *cvalue = Copy(Getattr(function,"value"));
	SwigType *ctype = Copy(Getattr(function,"type"));
	String *cthrow = Copy(Getattr(function,"throw"));

        Delete(SwigType_pop_function(decl)); /* remove the old parameter list from decl */
        SwigType_add_function(decl,newparms);
        if (constqualifier)
          SwigType_add_qualifier(decl,"const");

        Setattr(new_function,"name", Getattr(function,"name"));
        Setattr(new_function,"code", ccode);
        Setattr(new_function,"decl", decl);
        Setattr(new_function,"parms", newparms);
        Setattr(new_function,"storage", cstorage);
        Setattr(new_function,"value", cvalue);
        Setattr(new_function,"type", ctype);
        Setattr(new_function,"throw", cthrow);
        if (GetFlag(function, "feature:ignore"))
          SetFlag(new_function, "feature:ignore");

	Delete(ccode);
	Delete(cstorage);
	Delete(cvalue);
	Delete(ctype);
	Delete(cthrow);
	Delete(decl);

        {
          Node *throws = Getattr(function,"throws");
	  ParmList *pl = CopyParmList(throws);
          if (throws) Setattr(new_function,"throws",pl);
	  Delete(pl);
        }

        /* copy specific attributes for global (or in a namespace) template functions - these are not class template methods */
        if (strcmp(cntype,"template") == 0) {
          Node *templatetype = Getattr(function,"templatetype");
          Node *symtypename = Getattr(function,"sym:typename");
          Parm *templateparms = Getattr(function,"templateparms");
          if (templatetype) {
	    Node *tmp = Copy(templatetype);
	    Setattr(new_function,"templatetype",tmp);
	    Delete(tmp);
	  }
          if (symtypename) {
	    Node *tmp = Copy(symtypename);
	    Setattr(new_function,"sym:typename",tmp);
	    Delete(tmp);
	  }
          if (templateparms) {
	    Parm *tmp = CopyParmList(templateparms);
	    Setattr(new_function,"templateparms",tmp);
	    Delete(tmp);
	  }
        } else if (strcmp(cntype,"constructor") == 0) {
          /* only copied for constructors as this is not a user defined feature - it is hard coded in the parser */
          if (GetFlag(function,"feature:new")) SetFlag(new_function,"feature:new");
        }

        add_symbols(new_function);
        /* mark added functions as ones with overloaded parameters and point to the parsed method */
        Setattr(new_function,"defaultargs", n);

        /* Point to the new function, extending the linked list */
        set_nextSibling(function, new_function);
	Delete(new_function);
        function = new_function;
	
	Delete(ntype);
      }
    } else {
      function = 0;
    }
  }
}

/* -----------------------------------------------------------------------------
 * mark_nodes_as_extend()
 *
 * Used by the %extend to mark subtypes with "feature:extend".
 * template instances declared within %extend are skipped
 * ----------------------------------------------------------------------------- */

static void mark_nodes_as_extend(Node *n) {
  for (; n; n = nextSibling(n)) {
    if (Getattr(n, "template") && Strcmp(nodeType(n), "class") == 0)
      continue;
    /* Fix me: extend is not a feature. Replace with isextendmember? */
    Setattr(n, "feature:extend", "1");
    mark_nodes_as_extend(firstChild(n));
  }
}

/* -----------------------------------------------------------------------------
 * add_qualifier_to_declarator()
 *
 * Normally the qualifier is pushed on to the front of the type.
 * Adding a qualifier to a pointer to member function is a special case.
 * For example       : typedef double (Cls::*pmf)(void) const;
 * The qualifier is  : q(const).
 * The declarator is : m(Cls).f(void).
 * We need           : m(Cls).q(const).f(void).
 * ----------------------------------------------------------------------------- */

static String *add_qualifier_to_declarator(SwigType *type, SwigType *qualifier) {
  int is_pointer_to_member_function = 0;
  String *decl = Copy(type);
  String *poppedtype = NewString("");
  assert(qualifier);

  while (decl) {
    if (SwigType_ismemberpointer(decl)) {
      String *memberptr = SwigType_pop(decl);
      if (SwigType_isfunction(decl)) {
	is_pointer_to_member_function = 1;
	SwigType_push(decl, qualifier);
	SwigType_push(decl, memberptr);
	Insert(decl, 0, poppedtype);
	Delete(memberptr);
	break;
      } else {
	Append(poppedtype, memberptr);
      }
      Delete(memberptr);
    } else {
      String *popped = SwigType_pop(decl);
      if (!popped)
	break;
      Append(poppedtype, popped);
      Delete(popped);
    }
  }

  if (!is_pointer_to_member_function) {
    Delete(decl);
    decl = Copy(type);
    SwigType_push(decl, qualifier);
  }

  Delete(poppedtype);
  return decl;
}


#line 1648 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* END  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_ID = 3,                         /* ID  */
  YYSYMBOL_HBLOCK = 4,                     /* HBLOCK  */
  YYSYMBOL_POUND = 5,                      /* POUND  */
  YYSYMBOL_STRING = 6,                     /* STRING  */
  YYSYMBOL_WSTRING = 7,                    /* WSTRING  */
  YYSYMBOL_INCLUDE = 8,                    /* INCLUDE  */
  YYSYMBOL_IMPORT = 9,                     /* IMPORT  */
  YYSYMBOL_INSERT = 10,                    /* INSERT  */
  YYSYMBOL_CHARCONST = 11,                 /* CHARCONST  */
  YYSYMBOL_WCHARCONST = 12,                /* WCHARCONST  */
  YYSYMBOL_NUM_INT = 13,                   /* NUM_INT  */
  YYSYMBOL_NUM_DOUBLE = 14,                /* NUM_DOUBLE  */
  YYSYMBOL_NUM_FLOAT = 15,                 /* NUM_FLOAT  */
  YYSYMBOL_NUM_LONGDOUBLE = 16,            /* NUM_LONGDOUBLE  */
  YYSYMBOL_NUM_UNSIGNED = 17,              /* NUM_UNSIGNED  */
  YYSYMBOL_NUM_LONG = 18,                  /* NUM_LONG  */
  YYSYMBOL_NUM_ULONG = 19,                 /* NUM_ULONG  */
  YYSYMBOL_NUM_LONGLONG = 20,              /* NUM_LONGLONG  */
  YYSYMBOL_NUM_ULONGLONG = 21,             /* NUM_ULONGLONG  */
  YYSYMBOL_NUM_BOOL = 22,                  /* NUM_BOOL  */
  YYSYMBOL_TYPEDEF = 23,                   /* TYPEDEF  */
  YYSYMBOL_TYPE_INT = 24,                  /* TYPE_INT  */
  YYSYMBOL_TYPE_UNSIGNED = 25,             /* TYPE_UNSIGNED  */
  YYSYMBOL_TYPE_SHORT = 26,                /* TYPE_SHORT  */
  YYSYMBOL_TYPE_LONG = 27,                 /* TYPE_LONG  */
  YYSYMBOL_TYPE_FLOAT = 28,                /* TYPE_FLOAT  */
  YYSYMBOL_TYPE_DOUBLE = 29,               /* TYPE_DOUBLE  */
  YYSYMBOL_TYPE_CHAR = 30,                 /* TYPE_CHAR  */
  YYSYMBOL_TYPE_WCHAR = 31,                /* TYPE_WCHAR  */
  YYSYMBOL_TYPE_VOID = 32,                 /* TYPE_VOID  */
  YYSYMBOL_TYPE_SIGNED = 33,               /* TYPE_SIGNED  */
  YYSYMBOL_TYPE_BOOL = 34,                 /* TYPE_BOOL  */
  YYSYMBOL_TYPE_COMPLEX = 35,              /* TYPE_COMPLEX  */
  YYSYMBOL_TYPE_NON_ISO_INT8 = 36,         /* TYPE_NON_ISO_INT8  */
  YYSYMBOL_TYPE_NON_ISO_INT16 = 37,        /* TYPE_NON_ISO_INT16  */
  YYSYMBOL_TYPE_NON_ISO_INT32 = 38,        /* TYPE_NON_ISO_INT32  */
  YYSYMBOL_TYPE_NON_ISO_INT64 = 39,        /* TYPE_NON_ISO_INT64  */
  YYSYMBOL_LPAREN = 40,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 41,                    /* RPAREN  */
  YYSYMBOL_COMMA = 42,                     /* COMMA  */
  YYSYMBOL_SEMI = 43,                      /* SEMI  */
  YYSYMBOL_EXTERN = 44,                    /* EXTERN  */
  YYSYMBOL_LBRACE = 45,                    /* LBRACE  */
  YYSYMBOL_RBRACE = 46,                    /* RBRACE  */
  YYSYMBOL_PERIOD = 47,                    /* PERIOD  */
  YYSYMBOL_ELLIPSIS = 48,                  /* ELLIPSIS  */
  YYSYMBOL_CONST_QUAL = 49,                /* CONST_QUAL  */
  YYSYMBOL_VOLATILE = 50,                  /* VOLATILE  */
  YYSYMBOL_REGISTER = 51,                  /* REGISTER  */
  YYSYMBOL_STRUCT = 52,                    /* STRUCT  */
  YYSYMBOL_UNION = 53,                     /* UNION  */
  YYSYMBOL_EQUAL = 54,                     /* EQUAL  */
  YYSYMBOL_SIZEOF = 55,                    /* SIZEOF  */
  YYSYMBOL_ALIGNOF = 56,                   /* ALIGNOF  */
  YYSYMBOL_MODULE = 57,                    /* MODULE  */
  YYSYMBOL_LBRACKET = 58,                  /* LBRACKET  */
  YYSYMBOL_RBRACKET = 59,                  /* RBRACKET  */
  YYSYMBOL_LLBRACKET = 60,                 /* LLBRACKET  */
  YYSYMBOL_RRBRACKET = 61,                 /* RRBRACKET  */
  YYSYMBOL_BEGINFILE = 62,                 /* BEGINFILE  */
  YYSYMBOL_ENDOFFILE = 63,                 /* ENDOFFILE  */
  YYSYMBOL_CONSTANT = 64,                  /* CONSTANT  */
  YYSYMBOL_RENAME = 65,                    /* RENAME  */
  YYSYMBOL_NAMEWARN = 66,                  /* NAMEWARN  */
  YYSYMBOL_EXTEND = 67,                    /* EXTEND  */
  YYSYMBOL_PRAGMA = 68,                    /* PRAGMA  */
  YYSYMBOL_FEATURE = 69,                   /* FEATURE  */
  YYSYMBOL_VARARGS = 70,                   /* VARARGS  */
  YYSYMBOL_ENUM = 71,                      /* ENUM  */
  YYSYMBOL_CLASS = 72,                     /* CLASS  */
  YYSYMBOL_TYPENAME = 73,                  /* TYPENAME  */
  YYSYMBOL_PRIVATE = 74,                   /* PRIVATE  */
  YYSYMBOL_PUBLIC = 75,                    /* PUBLIC  */
  YYSYMBOL_PROTECTED = 76,                 /* PROTECTED  */
  YYSYMBOL_COLON = 77,                     /* COLON  */
  YYSYMBOL_STATIC = 78,                    /* STATIC  */
  YYSYMBOL_VIRTUAL = 79,                   /* VIRTUAL  */
  YYSYMBOL_FRIEND = 80,                    /* FRIEND  */
  YYSYMBOL_THROW = 81,                     /* THROW  */
  YYSYMBOL_CATCH = 82,                     /* CATCH  */
  YYSYMBOL_EXPLICIT = 83,                  /* EXPLICIT  */
  YYSYMBOL_STATIC_ASSERT = 84,             /* STATIC_ASSERT  */
  YYSYMBOL_CONSTEXPR = 85,                 /* CONSTEXPR  */
  YYSYMBOL_THREAD_LOCAL = 86,              /* THREAD_LOCAL  */
  YYSYMBOL_DECLTYPE = 87,                  /* DECLTYPE  */
  YYSYMBOL_AUTO = 88,                      /* AUTO  */
  YYSYMBOL_NOEXCEPT = 89,                  /* NOEXCEPT  */
  YYSYMBOL_OVERRIDE = 90,                  /* OVERRIDE  */
  YYSYMBOL_FINAL = 91,                     /* FINAL  */
  YYSYMBOL_USING = 92,                     /* USING  */
  YYSYMBOL_NAMESPACE = 93,                 /* NAMESPACE  */
  YYSYMBOL_NATIVE = 94,                    /* NATIVE  */
  YYSYMBOL_INLINE = 95,                    /* INLINE  */
  YYSYMBOL_TYPEMAP = 96,                   /* TYPEMAP  */
  YYSYMBOL_ECHO = 97,                      /* ECHO  */
  YYSYMBOL_APPLY = 98,                     /* APPLY  */
  YYSYMBOL_CLEAR = 99,                     /* CLEAR  */
  YYSYMBOL_SWIGTEMPLATE = 100,             /* SWIGTEMPLATE  */
  YYSYMBOL_FRAGMENT = 101,                 /* FRAGMENT  */
  YYSYMBOL_WARN = 102,                     /* WARN  */
  YYSYMBOL_LESSTHAN = 103,                 /* LESSTHAN  */
  YYSYMBOL_GREATERTHAN = 104,              /* GREATERTHAN  */
  YYSYMBOL_DELETE_KW = 105,                /* DELETE_KW  */
  YYSYMBOL_DEFAULT = 106,                  /* DEFAULT  */
  YYSYMBOL_LESSTHANOREQUALTO = 107,        /* LESSTHANOREQUALTO  */
  YYSYMBOL_GREATERTHANOREQUALTO = 108,     /* GREATERTHANOREQUALTO  */
  YYSYMBOL_EQUALTO = 109,                  /* EQUALTO  */
  YYSYMBOL_NOTEQUALTO = 110,               /* NOTEQUALTO  */
  YYSYMBOL_LESSEQUALGREATER = 111,         /* LESSEQUALGREATER  */
  YYSYMBOL_ARROW = 112,                    /* ARROW  */
  YYSYMBOL_QUESTIONMARK = 113,             /* QUESTIONMARK  */
  YYSYMBOL_TYPES = 114,                    /* TYPES  */
  YYSYMBOL_PARMS = 115,                    /* PARMS  */
  YYSYMBOL_NONID = 116,                    /* NONID  */
  YYSYMBOL_DSTAR = 117,                    /* DSTAR  */
  YYSYMBOL_DCNOT = 118,                    /* DCNOT  */
  YYSYMBOL_TEMPLATE = 119,                 /* TEMPLATE  */
  YYSYMBOL_OPERATOR = 120,                 /* OPERATOR  */
  YYSYMBOL_CONVERSIONOPERATOR = 121,       /* CONVERSIONOPERATOR  */
  YYSYMBOL_PARSETYPE = 122,                /* PARSETYPE  */
  YYSYMBOL_PARSEPARM = 123,                /* PARSEPARM  */
  YYSYMBOL_PARSEPARMS = 124,               /* PARSEPARMS  */
  YYSYMBOL_DOXYGENSTRING = 125,            /* DOXYGENSTRING  */
  YYSYMBOL_DOXYGENPOSTSTRING = 126,        /* DOXYGENPOSTSTRING  */
  YYSYMBOL_LOR = 127,                      /* LOR  */
  YYSYMBOL_LAND = 128,                     /* LAND  */
  YYSYMBOL_OR = 129,                       /* OR  */
  YYSYMBOL_XOR = 130,                      /* XOR  */
  YYSYMBOL_AND = 131,                      /* AND  */
  YYSYMBOL_LSHIFT = 132,                   /* LSHIFT  */
  YYSYMBOL_RSHIFT = 133,                   /* RSHIFT  */
  YYSYMBOL_PLUS = 134,                     /* PLUS  */
  YYSYMBOL_MINUS = 135,                    /* MINUS  */
  YYSYMBOL_STAR = 136,                     /* STAR  */
  YYSYMBOL_SLASH = 137,                    /* SLASH  */
  YYSYMBOL_MODULO = 138,                   /* MODULO  */
  YYSYMBOL_UMINUS = 139,                   /* UMINUS  */
  YYSYMBOL_NOT = 140,                      /* NOT  */
  YYSYMBOL_LNOT = 141,                     /* LNOT  */
  YYSYMBOL_CAST = 142,                     /* CAST  */
  YYSYMBOL_DCOLON = 143,                   /* DCOLON  */
  YYSYMBOL_YYACCEPT = 144,                 /* $accept  */
  YYSYMBOL_program = 145,                  /* program  */
  YYSYMBOL_interface = 146,                /* interface  */
  YYSYMBOL_declaration = 147,              /* declaration  */
  YYSYMBOL_swig_directive = 148,           /* swig_directive  */
  YYSYMBOL_extend_directive = 149,         /* extend_directive  */
  YYSYMBOL_150_1 = 150,                    /* $@1  */
  YYSYMBOL_apply_directive = 151,          /* apply_directive  */
  YYSYMBOL_clear_directive = 152,          /* clear_directive  */
  YYSYMBOL_constant_directive = 153,       /* constant_directive  */
  YYSYMBOL_echo_directive = 154,           /* echo_directive  */
  YYSYMBOL_stringtype = 155,               /* stringtype  */
  YYSYMBOL_fname = 156,                    /* fname  */
  YYSYMBOL_fragment_directive = 157,       /* fragment_directive  */
  YYSYMBOL_include_directive = 158,        /* include_directive  */
  YYSYMBOL_159_2 = 159,                    /* @2  */
  YYSYMBOL_includetype = 160,              /* includetype  */
  YYSYMBOL_inline_directive = 161,         /* inline_directive  */
  YYSYMBOL_insert_directive = 162,         /* insert_directive  */
  YYSYMBOL_module_directive = 163,         /* module_directive  */
  YYSYMBOL_native_directive = 164,         /* native_directive  */
  YYSYMBOL_pragma_directive = 165,         /* pragma_directive  */
  YYSYMBOL_pragma_arg = 166,               /* pragma_arg  */
  YYSYMBOL_pragma_lang = 167,              /* pragma_lang  */
  YYSYMBOL_rename_directive = 168,         /* rename_directive  */
  YYSYMBOL_rename_namewarn = 169,          /* rename_namewarn  */
  YYSYMBOL_feature_directive = 170,        /* feature_directive  */
  YYSYMBOL_stringbracesemi = 171,          /* stringbracesemi  */
  YYSYMBOL_featattr = 172,                 /* featattr  */
  YYSYMBOL_varargs_directive = 173,        /* varargs_directive  */
  YYSYMBOL_varargs_parms = 174,            /* varargs_parms  */
  YYSYMBOL_typemap_directive = 175,        /* typemap_directive  */
  YYSYMBOL_typemap_type = 176,             /* typemap_type  */
  YYSYMBOL_tm_list = 177,                  /* tm_list  */
  YYSYMBOL_tm_list_builder = 178,          /* tm_list_builder  */
  YYSYMBOL_typemap_parm = 179,             /* typemap_parm  */
  YYSYMBOL_types_directive = 180,          /* types_directive  */
  YYSYMBOL_template_directive = 181,       /* template_directive  */
  YYSYMBOL_warn_directive = 182,           /* warn_directive  */
  YYSYMBOL_c_declaration = 183,            /* c_declaration  */
  YYSYMBOL_184_3 = 184,                    /* $@3  */
  YYSYMBOL_c_decl = 185,                   /* c_decl  */
  YYSYMBOL_c_decl_tail = 186,              /* c_decl_tail  */
  YYSYMBOL_initializer = 187,              /* initializer  */
  YYSYMBOL_cpp_alternate_rettype = 188,    /* cpp_alternate_rettype  */
  YYSYMBOL_cpp_lambda_decl = 189,          /* cpp_lambda_decl  */
  YYSYMBOL_lambda_introducer = 190,        /* lambda_introducer  */
  YYSYMBOL_lambda_template = 191,          /* lambda_template  */
  YYSYMBOL_lambda_body = 192,              /* lambda_body  */
  YYSYMBOL_lambda_tail = 193,              /* lambda_tail  */
  YYSYMBOL_194_4 = 194,                    /* $@4  */
  YYSYMBOL_c_enum_key = 195,               /* c_enum_key  */
  YYSYMBOL_c_enum_inherit = 196,           /* c_enum_inherit  */
  YYSYMBOL_c_enum_forward_decl = 197,      /* c_enum_forward_decl  */
  YYSYMBOL_c_enum_decl = 198,              /* c_enum_decl  */
  YYSYMBOL_c_constructor_decl = 199,       /* c_constructor_decl  */
  YYSYMBOL_cpp_declaration = 200,          /* cpp_declaration  */
  YYSYMBOL_cpp_class_decl = 201,           /* cpp_class_decl  */
  YYSYMBOL_202_5 = 202,                    /* @5  */
  YYSYMBOL_203_6 = 203,                    /* @6  */
  YYSYMBOL_cpp_opt_declarators = 204,      /* cpp_opt_declarators  */
  YYSYMBOL_cpp_forward_class_decl = 205,   /* cpp_forward_class_decl  */
  YYSYMBOL_cpp_template_decl = 206,        /* cpp_template_decl  */
  YYSYMBOL_207_7 = 207,                    /* $@7  */
  YYSYMBOL_cpp_template_possible = 208,    /* cpp_template_possible  */
  YYSYMBOL_template_parms = 209,           /* template_parms  */
  YYSYMBOL_template_parms_builder = 210,   /* template_parms_builder  */
  YYSYMBOL_templateparameter = 211,        /* templateparameter  */
  YYSYMBOL_cpp_using_decl = 212,           /* cpp_using_decl  */
  YYSYMBOL_cpp_namespace_decl = 213,       /* cpp_namespace_decl  */
  YYSYMBOL_214_8 = 214,                    /* @8  */
  YYSYMBOL_215_9 = 215,                    /* @9  */
  YYSYMBOL_cpp_members = 216,              /* cpp_members  */
  YYSYMBOL_cpp_members_builder = 217,      /* cpp_members_builder  */
  YYSYMBOL_cpp_member_no_dox = 218,        /* cpp_member_no_dox  */
  YYSYMBOL_cpp_member = 219,               /* cpp_member  */
  YYSYMBOL_220_10 = 220,                   /* $@10  */
  YYSYMBOL_cpp_constructor_decl = 221,     /* cpp_constructor_decl  */
  YYSYMBOL_cpp_destructor_decl = 222,      /* cpp_destructor_decl  */
  YYSYMBOL_cpp_conversion_operator = 223,  /* cpp_conversion_operator  */
  YYSYMBOL_cpp_catch_decl = 224,           /* cpp_catch_decl  */
  YYSYMBOL_cpp_static_assert = 225,        /* cpp_static_assert  */
  YYSYMBOL_cpp_protection_decl = 226,      /* cpp_protection_decl  */
  YYSYMBOL_cpp_swig_directive = 227,       /* cpp_swig_directive  */
  YYSYMBOL_cpp_vend = 228,                 /* cpp_vend  */
  YYSYMBOL_anonymous_bitfield = 229,       /* anonymous_bitfield  */
  YYSYMBOL_anon_bitfield_type = 230,       /* anon_bitfield_type  */
  YYSYMBOL_storage_class = 231,            /* storage_class  */
  YYSYMBOL_storage_class_list = 232,       /* storage_class_list  */
  YYSYMBOL_storage_class_raw = 233,        /* storage_class_raw  */
  YYSYMBOL_parms = 234,                    /* parms  */
  YYSYMBOL_rawparms = 235,                 /* rawparms  */
  YYSYMBOL_parm_no_dox = 236,              /* parm_no_dox  */
  YYSYMBOL_parm = 237,                     /* parm  */
  YYSYMBOL_valparms = 238,                 /* valparms  */
  YYSYMBOL_valparms_builder = 239,         /* valparms_builder  */
  YYSYMBOL_valparm = 240,                  /* valparm  */
  YYSYMBOL_def_args = 241,                 /* def_args  */
  YYSYMBOL_parameter_declarator = 242,     /* parameter_declarator  */
  YYSYMBOL_plain_declarator = 243,         /* plain_declarator  */
  YYSYMBOL_declarator = 244,               /* declarator  */
  YYSYMBOL_notso_direct_declarator = 245,  /* notso_direct_declarator  */
  YYSYMBOL_direct_declarator = 246,        /* direct_declarator  */
  YYSYMBOL_abstract_declarator = 247,      /* abstract_declarator  */
  YYSYMBOL_direct_abstract_declarator = 248, /* direct_abstract_declarator  */
  YYSYMBOL_pointer = 249,                  /* pointer  */
  YYSYMBOL_cv_ref_qualifier = 250,         /* cv_ref_qualifier  */
  YYSYMBOL_ref_qualifier = 251,            /* ref_qualifier  */
  YYSYMBOL_type_qualifier = 252,           /* type_qualifier  */
  YYSYMBOL_type_qualifier_raw = 253,       /* type_qualifier_raw  */
  YYSYMBOL_type = 254,                     /* type  */
  YYSYMBOL_rawtype = 255,                  /* rawtype  */
  YYSYMBOL_type_right = 256,               /* type_right  */
  YYSYMBOL_decltype = 257,                 /* decltype  */
  YYSYMBOL_258_11 = 258,                   /* @11  */
  YYSYMBOL_decltypeexpr = 259,             /* decltypeexpr  */
  YYSYMBOL_primitive_type = 260,           /* primitive_type  */
  YYSYMBOL_primitive_type_list = 261,      /* primitive_type_list  */
  YYSYMBOL_type_specifier = 262,           /* type_specifier  */
  YYSYMBOL_definetype = 263,               /* definetype  */
  YYSYMBOL_default_delete = 264,           /* default_delete  */
  YYSYMBOL_deleted_definition = 265,       /* deleted_definition  */
  YYSYMBOL_explicit_default = 266,         /* explicit_default  */
  YYSYMBOL_ename = 267,                    /* ename  */
  YYSYMBOL_constant_directives = 268,      /* constant_directives  */
  YYSYMBOL_optional_ignored_defines = 269, /* optional_ignored_defines  */
  YYSYMBOL_enumlist = 270,                 /* enumlist  */
  YYSYMBOL_enumlist_item = 271,            /* enumlist_item  */
  YYSYMBOL_edecl_with_dox = 272,           /* edecl_with_dox  */
  YYSYMBOL_edecl = 273,                    /* edecl  */
  YYSYMBOL_etype = 274,                    /* etype  */
  YYSYMBOL_expr = 275,                     /* expr  */
  YYSYMBOL_exprmem = 276,                  /* exprmem  */
  YYSYMBOL_exprsimple = 277,               /* exprsimple  */
  YYSYMBOL_valexpr = 278,                  /* valexpr  */
  YYSYMBOL_exprnum = 279,                  /* exprnum  */
  YYSYMBOL_exprcompound = 280,             /* exprcompound  */
  YYSYMBOL_variadic_opt = 281,             /* variadic_opt  */
  YYSYMBOL_inherit = 282,                  /* inherit  */
  YYSYMBOL_raw_inherit = 283,              /* raw_inherit  */
  YYSYMBOL_284_12 = 284,                   /* $@12  */
  YYSYMBOL_base_list = 285,                /* base_list  */
  YYSYMBOL_base_specifier = 286,           /* base_specifier  */
  YYSYMBOL_287_13 = 287,                   /* @13  */
  YYSYMBOL_288_14 = 288,                   /* @14  */
  YYSYMBOL_access_specifier = 289,         /* access_specifier  */
  YYSYMBOL_templcpptype = 290,             /* templcpptype  */
  YYSYMBOL_cpptype = 291,                  /* cpptype  */
  YYSYMBOL_classkey = 292,                 /* classkey  */
  YYSYMBOL_classkeyopt = 293,              /* classkeyopt  */
  YYSYMBOL_opt_virtual = 294,              /* opt_virtual  */
  YYSYMBOL_virt_specifier_seq = 295,       /* virt_specifier_seq  */
  YYSYMBOL_virt_specifier_seq_opt = 296,   /* virt_specifier_seq_opt  */
  YYSYMBOL_class_virt_specifier_opt = 297, /* class_virt_specifier_opt  */
  YYSYMBOL_exception_specification = 298,  /* exception_specification  */
  YYSYMBOL_qualifiers_exception_specification = 299, /* qualifiers_exception_specification  */
  YYSYMBOL_cpp_const = 300,                /* cpp_const  */
  YYSYMBOL_ctor_end = 301,                 /* ctor_end  */
  YYSYMBOL_ctor_initializer = 302,         /* ctor_initializer  */
  YYSYMBOL_mem_initializer_list = 303,     /* mem_initializer_list  */
  YYSYMBOL_mem_initializer = 304,          /* mem_initializer  */
  YYSYMBOL_less_valparms_greater = 305,    /* less_valparms_greater  */
  YYSYMBOL_identifier = 306,               /* identifier  */
  YYSYMBOL_idstring = 307,                 /* idstring  */
  YYSYMBOL_idstringopt = 308,              /* idstringopt  */
  YYSYMBOL_idcolon = 309,                  /* idcolon  */
  YYSYMBOL_idcolontail = 310,              /* idcolontail  */
  YYSYMBOL_idtemplate = 311,               /* idtemplate  */
  YYSYMBOL_idtemplatetemplate = 312,       /* idtemplatetemplate  */
  YYSYMBOL_idcolonnt = 313,                /* idcolonnt  */
  YYSYMBOL_idcolontailnt = 314,            /* idcolontailnt  */
  YYSYMBOL_attribute = 315,                /* attribute  */
  YYSYMBOL_attribute_notopt = 316,         /* attribute_notopt  */
  YYSYMBOL_attribute_item = 317,           /* attribute_item  */
  YYSYMBOL_attribute_nspace = 318,         /* attribute_nspace  */
  YYSYMBOL_attribute_list = 319,           /* attribute_list  */
  YYSYMBOL_attribute_element = 320,        /* attribute_element  */
  YYSYMBOL_string = 321,                   /* string  */
  YYSYMBOL_wstring = 322,                  /* wstring  */
  YYSYMBOL_stringbrace = 323,              /* stringbrace  */
  YYSYMBOL_options = 324,                  /* options  */
  YYSYMBOL_kwargs = 325,                   /* kwargs  */
  YYSYMBOL_kwargs_builder = 326,           /* kwargs_builder  */
  YYSYMBOL_stringnum = 327                 /* stringnum  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;


/* Second part of user prologue.  */
#line 1890 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"


// Default-initialised instances of token types to avoid uninitialised fields.
// The compiler will initialise all fields to zero or NULL for us.

static const struct Decl default_decl;
static const struct Define default_dtype;

/* C++ decltype/auto type deduction. */
static SwigType *deduce_type(const struct Define *dtype) {
  Node *n;
  if (!dtype->val) return NULL;
  n = Swig_symbol_clookup(dtype->val, 0);
  if (n) {
    if (Strcmp(nodeType(n),"enumitem") == 0) {
      /* For an enumitem, the "type" attribute gives us the underlying integer
       * type - we want the "type" attribute from the enum itself, which is
       * "parentNode".
       */
      n = Getattr(n, "parentNode");
    }
    return Getattr(n, "type");
  } else if (dtype->type != T_AUTO && dtype->type != T_UNKNOWN) {
    /* Try to deduce the type from the T_* type code. */
    String *deduced_type = NewSwigType(dtype->type);
    if (Len(deduced_type) > 0) return deduced_type;
    Delete(deduced_type);
  }
  return NULL;
}

// Append scanner_ccode to expr.  Some cleaning up of the code may be done.
static void append_expr_from_scanner(String *expr) {
  if (Strchr(scanner_ccode, '"') == NULL) {
    // Append scanner_ccode, changing any whitespace character to a space.
    int len = Len(scanner_ccode);
    for (int i = 0; i < len; ++i) {
      char ch = Char(scanner_ccode)[i];
      if (isspace((unsigned char)ch)) ch = ' ';
      Putc(ch, expr);
    }
  } else {
    // The code contains a double quote so leave it be as changing a
    // backslash-escaped linefeed character (i.e. `\` followed by byte 0x0a)
    // in a string literal into a space will insert a space into the string
    // literal's value.  An expression containing a double quote won't work if
    // used in a context where a swig_type_info is generated as the typename
    // gets substituted into a string literal without any escaping which will
    // result in invalid code due to the double quotes.
    Append(expr, scanner_ccode);
  }
  Clear(scanner_ccode);
}

static Node *new_enum_node(SwigType *enum_base_type) {
  Node *n = new_node("enum");
  if (enum_base_type) {
    switch (SwigType_type(enum_base_type)) {
      case T_USER:
	// We get T_USER if the underlying type is a typedef.  Unfortunately we
	// aren't able to resolve a typedef at this point, so we have to assume
	// it's a typedef to an integral or boolean type.
	break;
      case T_BOOL:
      case T_SCHAR:
      case T_UCHAR:
      case T_SHORT:
      case T_USHORT:
      case T_INT:
      case T_UINT:
      case T_LONG:
      case T_ULONG:
      case T_LONGLONG:
      case T_ULONGLONG:
      case T_CHAR:
      case T_WCHAR:
	break;
      default:
	Swig_error(cparse_file, cparse_line, "Underlying type of enum must be an integral type\n");
    }
    Setattr(n, "enumbase", enum_base_type);
  }
  return n;
}


#line 2096 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  62
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   9371

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  144
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  184
/* YYNRULES -- Number of rules.  */
#define YYNRULES  649
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1186

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   398


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX) YY_CAST (yysymbol_kind_t, YYX)

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  1986,  1986,  1998,  2002,  2005,  2008,  2011,  2014,  2019,
    2028,  2033,  2042,  2047,  2048,  2049,  2050,  2051,  2060,  2076,
    2087,  2088,  2089,  2090,  2091,  2092,  2093,  2094,  2095,  2096,
    2097,  2098,  2099,  2100,  2101,  2102,  2103,  2104,  2105,  2112,
    2112,  2194,  2204,  2218,  2236,  2254,  2272,  2276,  2287,  2296,
    2309,  2316,  2320,  2331,  2341,  2356,  2369,  2369,  2430,  2431,
    2438,  2456,  2490,  2495,  2505,  2511,  2529,  2572,  2579,  2606,
    2612,  2619,  2620,  2623,  2624,  2631,  2677,  2723,  2734,  2737,
    2764,  2770,  2778,  2784,  2792,  2793,  2794,  2797,  2803,  2810,
    2846,  2847,  2881,  2900,  2910,  2925,  2947,  2952,  2955,  2966,
    2977,  2982,  2995,  3007,  3251,  3261,  3268,  3269,  3273,  3273,
    3306,  3312,  3322,  3334,  3341,  3430,  3485,  3554,  3600,  3620,
    3638,  3642,  3669,  3673,  3684,  3685,  3691,  3692,  3693,  3694,
    3698,  3699,  3703,  3707,  3712,  3717,  3728,  3737,  3746,  3757,
    3762,  3765,  3768,  3772,  3773,  3773,  3785,  3788,  3791,  3800,
    3803,  3810,  3836,  3869,  3970,  4030,  4031,  4032,  4033,  4034,
    4035,  4044,  4044,  4294,  4294,  4452,  4453,  4465,  4487,  4487,
    4691,  4697,  4703,  4709,  4715,  4716,  4717,  4718,  4721,  4722,
    4725,  4728,  4733,  4736,  4747,  4755,  4763,  4771,  4796,  4807,
    4817,  4848,  4848,  4906,  4906,  4928,  4955,  4958,  4963,  4966,
    4971,  4977,  4980,  5010,  5011,  5028,  5029,  5030,  5031,  5032,
    5033,  5034,  5040,  5041,  5042,  5043,  5044,  5045,  5046,  5047,
    5048,  5049,  5051,  5052,  5056,  5060,  5060,  5079,  5120,  5164,
    5188,  5214,  5241,  5269,  5297,  5305,  5312,  5319,  5327,  5335,
    5338,  5340,  5341,  5342,  5343,  5344,  5345,  5346,  5347,  5350,
    5354,  5359,  5366,  5369,  5370,  5371,  5373,  5379,  5421,  5424,
    5425,  5433,  5434,  5444,  5445,  5446,  5447,  5448,  5449,  5450,
    5457,  5469,  5470,  5474,  5479,  5485,  5492,  5499,  5504,  5516,
    5524,  5525,  5526,  5532,  5540,  5545,  5548,  5559,  5587,  5597,
    5600,  5608,  5614,  5620,  5626,  5632,  5640,  5663,  5680,  5699,
    5718,  5723,  5731,  5740,  5749,  5753,  5762,  5773,  5784,  5796,
    5806,  5820,  5829,  5839,  5849,  5859,  5871,  5883,  5896,  5907,
    5918,  5930,  5944,  5949,  5955,  5967,  5975,  5986,  5997,  6008,
    6027,  6033,  6046,  6054,  6061,  6068,  6079,  6091,  6102,  6114,
    6125,  6136,  6156,  6170,  6175,  6181,  6187,  6193,  6202,  6211,
    6212,  6221,  6230,  6236,  6242,  6247,  6253,  6261,  6272,  6283,
    6294,  6299,  6304,  6307,  6324,  6342,  6352,  6357,  6361,  6366,
    6373,  6376,  6381,  6388,  6392,  6398,  6402,  6408,  6409,  6410,
    6416,  6422,  6426,  6427,  6431,  6436,  6442,  6443,  6444,  6445,
    6447,  6450,  6453,  6456,  6456,  6473,  6476,  6489,  6515,  6516,
    6570,  6574,  6578,  6582,  6586,  6590,  6594,  6598,  6602,  6606,
    6610,  6614,  6618,  6622,  6628,  6629,  6632,  6633,  6637,  6645,
    6654,  6655,  6658,  6659,  6663,  6664,  6674,  6678,  6683,  6689,
    6699,  6710,  6721,  6726,  6731,  6737,  6746,  6754,  6772,  6790,
    6791,  6818,  6822,  6828,  6832,  6838,  6842,  6848,  6852,  6861,
    6862,  6863,  6869,  6875,  6881,  6893,  6901,  6909,  6916,  6926,
    6933,  6934,  6937,  6947,  6986,  6996,  7006,  7016,  7027,  7038,
    7059,  7082,  7083,  7084,  7085,  7086,  7087,  7088,  7089,  7090,
    7091,  7094,  7099,  7104,  7109,  7114,  7119,  7124,  7129,  7134,
    7139,  7144,  7149,  7154,  7159,  7168,  7179,  7184,  7189,  7206,
    7211,  7216,  7221,  7226,  7231,  7236,  7241,  7246,  7251,  7256,
    7261,  7266,  7271,  7280,  7290,  7295,  7300,  7306,  7321,  7328,
    7354,  7365,  7370,  7375,  7402,  7405,  7410,  7413,  7413,  7414,
    7417,  7434,  7443,  7443,  7462,  7462,  7481,  7482,  7483,  7486,
    7491,  7498,  7499,  7506,  7515,  7518,  7521,  7526,  7527,  7530,
    7531,  7534,  7537,  7540,  7543,  7548,  7549,  7554,  7557,  7562,
    7567,  7571,  7575,  7581,  7586,  7592,  7597,  7600,  7607,  7608,
    7613,  7623,  7633,  7639,  7645,  7651,  7665,  7666,  7669,  7670,
    7671,  7672,  7675,  7687,  7693,  7702,  7703,  7704,  7707,  7708,
    7709,  7712,  7713,  7716,  7720,  7724,  7727,  7730,  7733,  7738,
    7744,  7748,  7751,  7755,  7761,  7764,  7771,  7772,  7780,  7784,
    7788,  7791,  7794,  7797,  7803,  7807,  7810,  7814,  7820,  7821,
    7825,  7826,  7838,  7851,  7852,  7855,  7859,  7866,  7870,  7878,
    7886,  7891,  7893,  7899,  7905,  7911,  7914,  7915,  7919,  7922,
    7935,  7939,  7944,  7951,  7960,  7965,  7972,  7977,  7986,  7987
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "END", "error", "\"invalid token\"", "ID", "HBLOCK", "POUND", "STRING",
  "WSTRING", "INCLUDE", "IMPORT", "INSERT", "CHARCONST", "WCHARCONST",
  "NUM_INT", "NUM_DOUBLE", "NUM_FLOAT", "NUM_LONGDOUBLE", "NUM_UNSIGNED",
  "NUM_LONG", "NUM_ULONG", "NUM_LONGLONG", "NUM_ULONGLONG", "NUM_BOOL",
  "TYPEDEF", "TYPE_INT", "TYPE_UNSIGNED", "TYPE_SHORT", "TYPE_LONG",
  "TYPE_FLOAT", "TYPE_DOUBLE", "TYPE_CHAR", "TYPE_WCHAR", "TYPE_VOID",
  "TYPE_SIGNED", "TYPE_BOOL", "TYPE_COMPLEX", "TYPE_NON_ISO_INT8",
  "TYPE_NON_ISO_INT16", "TYPE_NON_ISO_INT32", "TYPE_NON_ISO_INT64",
  "LPAREN", "RPAREN", "COMMA", "SEMI", "EXTERN", "LBRACE", "RBRACE",
  "PERIOD", "ELLIPSIS", "CONST_QUAL", "VOLATILE", "REGISTER", "STRUCT",
  "UNION", "EQUAL", "SIZEOF", "ALIGNOF", "MODULE", "LBRACKET", "RBRACKET",
  "LLBRACKET", "RRBRACKET", "BEGINFILE", "ENDOFFILE", "CONSTANT", "RENAME",
  "NAMEWARN", "EXTEND", "PRAGMA", "FEATURE", "VARARGS", "ENUM", "CLASS",
  "TYPENAME", "PRIVATE", "PUBLIC", "PROTECTED", "COLON", "STATIC",
  "VIRTUAL", "FRIEND", "THROW", "CATCH", "EXPLICIT", "STATIC_ASSERT",
  "CONSTEXPR", "THREAD_LOCAL", "DECLTYPE", "AUTO", "NOEXCEPT", "OVERRIDE",
  "FINAL", "USING", "NAMESPACE", "NATIVE", "INLINE", "TYPEMAP", "ECHO",
  "APPLY", "CLEAR", "SWIGTEMPLATE", "FRAGMENT", "WARN", "LESSTHAN",
  "GREATERTHAN", "DELETE_KW", "DEFAULT", "LESSTHANOREQUALTO",
  "GREATERTHANOREQUALTO", "EQUALTO", "NOTEQUALTO", "LESSEQUALGREATER",
  "ARROW", "QUESTIONMARK", "TYPES", "PARMS", "NONID", "DSTAR", "DCNOT",
  "TEMPLATE", "OPERATOR", "CONVERSIONOPERATOR", "PARSETYPE", "PARSEPARM",
  "PARSEPARMS", "DOXYGENSTRING", "DOXYGENPOSTSTRING", "LOR", "LAND", "OR",
  "XOR", "AND", "LSHIFT", "RSHIFT", "PLUS", "MINUS", "STAR", "SLASH",
  "MODULO", "UMINUS", "NOT", "LNOT", "CAST", "DCOLON", "$accept",
  "program", "interface", "declaration", "swig_directive",
  "extend_directive", "$@1", "apply_directive", "clear_directive",
  "constant_directive", "echo_directive", "stringtype", "fname",
  "fragment_directive", "include_directive", "@2", "includetype",
  "inline_directive", "insert_directive", "module_directive",
  "native_directive", "pragma_directive", "pragma_arg", "pragma_lang",
  "rename_directive", "rename_namewarn", "feature_directive",
  "stringbracesemi", "featattr", "varargs_directive", "varargs_parms",
  "typemap_directive", "typemap_type", "tm_list", "tm_list_builder",
  "typemap_parm", "types_directive", "template_directive",
  "warn_directive", "c_declaration", "$@3", "c_decl", "c_decl_tail",
  "initializer", "cpp_alternate_rettype", "cpp_lambda_decl",
  "lambda_introducer", "lambda_template", "lambda_body", "lambda_tail",
  "$@4", "c_enum_key", "c_enum_inherit", "c_enum_forward_decl",
  "c_enum_decl", "c_constructor_decl", "cpp_declaration", "cpp_class_decl",
  "@5", "@6", "cpp_opt_declarators", "cpp_forward_class_decl",
  "cpp_template_decl", "$@7", "cpp_template_possible", "template_parms",
  "template_parms_builder", "templateparameter", "cpp_using_decl",
  "cpp_namespace_decl", "@8", "@9", "cpp_members", "cpp_members_builder",
  "cpp_member_no_dox", "cpp_member", "$@10", "cpp_constructor_decl",
  "cpp_destructor_decl", "cpp_conversion_operator", "cpp_catch_decl",
  "cpp_static_assert", "cpp_protection_decl", "cpp_swig_directive",
  "cpp_vend", "anonymous_bitfield", "anon_bitfield_type", "storage_class",
  "storage_class_list", "storage_class_raw", "parms", "rawparms",
  "parm_no_dox", "parm", "valparms", "valparms_builder", "valparm",
  "def_args", "parameter_declarator", "plain_declarator", "declarator",
  "notso_direct_declarator", "direct_declarator", "abstract_declarator",
  "direct_abstract_declarator", "pointer", "cv_ref_qualifier",
  "ref_qualifier", "type_qualifier", "type_qualifier_raw", "type",
  "rawtype", "type_right", "decltype", "@11", "decltypeexpr",
  "primitive_type", "primitive_type_list", "type_specifier", "definetype",
  "default_delete", "deleted_definition", "explicit_default", "ename",
  "constant_directives", "optional_ignored_defines", "enumlist",
  "enumlist_item", "edecl_with_dox", "edecl", "etype", "expr", "exprmem",
  "exprsimple", "valexpr", "exprnum", "exprcompound", "variadic_opt",
  "inherit", "raw_inherit", "$@12", "base_list", "base_specifier", "@13",
  "@14", "access_specifier", "templcpptype", "cpptype", "classkey",
  "classkeyopt", "opt_virtual", "virt_specifier_seq",
  "virt_specifier_seq_opt", "class_virt_specifier_opt",
  "exception_specification", "qualifiers_exception_specification",
  "cpp_const", "ctor_end", "ctor_initializer", "mem_initializer_list",
  "mem_initializer", "less_valparms_greater", "identifier", "idstring",
  "idstringopt", "idcolon", "idcolontail", "idtemplate",
  "idtemplatetemplate", "idcolonnt", "idcolontailnt", "attribute",
  "attribute_notopt", "attribute_item", "attribute_nspace",
  "attribute_list", "attribute_element", "string", "wstring",
  "stringbrace", "options", "kwargs", "kwargs_builder", "stringnum", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1065)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-620)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     402,  7745,  7847,   378,    91,  7186, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,    43,    43,
     122,   387,    64,    64,   182, -1065, -1065,   147,   191,  8542,
     365, -1065,   338,  8758, -1065,  1310,   795, -1065, -1065, -1065,
    4318, -1065,   365,   191, -1065,    95,  8542,    43, -1065,   359,
   -1065,  8257, -1065,   255, -1065, -1065, -1065,   323, -1065,   353,
    8396, -1065, -1065,   353,   388,   407,   437,   442,   450,   251,
     195,   462,   274,   472,   630,  8614,  8614,   491,   523,   562,
     533,  8830, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065,   353, -1065, -1065, -1065, -1065, -1065, -1065,
     864, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
   -1065,  1567, -1065, -1065, -1065,    75,   582, -1065, -1065, -1065,
   -1065, -1065, -1065,   347,  2754, -1065, -1065, -1065, -1065, -1065,
     795,  6844, -1065,  2890,  3434,    59,   249,   970,   795,   365,
   -1065, -1065,   552,    67,   552,   258,   151,   499,   795, -1065,
   -1065, -1065,    75,   413, -1065, -1065, -1065, -1065,   597, -1065,
      93, -1065,   582,   582,   582,    79,  1657,   575,   146,   369,
      75,    75,   582,  8053,  8257, -1065,   365,   365,    55, -1065,
     218,   604,    75, -1065, -1065,   582, -1065, -1065,   646,  8257,
     613,  1190,   601,   626, -1065,   582,   562,   646,  8257, -1065,
   -1065,   365,  8155,   365,   365, -1065, -1065,   365,   173,   562,
    1942,   352,   481,   582,   476,   564,   554, -1065,    44, -1065,
   -1065, -1065, -1065, -1065, -1065,  8686,  1933, -1065,   609, -1065,
   -1065, -1065, -1065, -1065, -1065,    21,   637,   674,   646,  2504,
      75, -1065, -1065,    95,    30, -1065, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,  6426,  3659,
     688,   691,  6426,  6426,  6426,  6426,  6426,  6426, -1065,   642,
     694, -1065,   708,    66,  3809,    68, -1065,    32, -1065, -1065,
     529,   577,   795,  6958,  1601,  1601,   713,   720,   355,   647,
   -1065,   719, -1065,  3809, -1065, -1065,  6557,   739,  6958,   144,
     365,   540,   258, -1065, -1065,   144,   540,   258, -1065,   795,
   -1065, -1065,  8257,  3570, -1065,  8257,  3706,   144,  1793,  1820,
     795,   540,   258, -1065,   666,  1472, -1065, -1065,    95,   781,
    7949, -1065,   753,   748,   749,   760,   764, -1065, -1065, -1065,
    1965,   552,   553,  3026, -1065, -1065, -1065, -1065,   365,   767,
     756,   774,   785,   794, -1065,   798,   808,   810, -1065,  8758,
   -1065,   365, -1065,   800,   814, -1065, -1065,   815,  8614, -1065,
   -1065,   560, -1065, -1065,  8614, -1065,   825, -1065,   692,   384,
     836,   229,   776,   778,   846, -1065, -1065,    42,   854, -1065,
   -1065, -1065, -1065,   139,   172,   172,  1131,   443,   796,   857,
     342,   144,   144,   859,  8257,   144,   953,  1890,   797,  1441,
    9024,   428,  1657,   365,  1691,   424,   562, -1065, -1065, -1065,
     582, -1065,   582,   870, -1065,  2471,   191, -1065,   912,   918,
    6814,   833,    30, -1065,   878, -1065, -1065, -1065,  2066, -1065,
   -1065, -1065, -1065, -1065, -1065,  2754, -1065,  3842,  3978,  4114,
    4250,  6426,  6426,  4386,  4522,  4658,  4794,  4930,  5066,  5202,
    5338,  5474,  5610,  5746,  5882,   937,   944, -1065, -1065, -1065,
      53,    53,  1831,   831,   587, -1065,   625, -1065, -1065,    53,
      53,   638,   848,  1188, -1065, -1065,  8257,  1363,    22,   540,
   -1065,  8257,  6018,   540,   909, -1065,  7020,   922, -1065,  7693,
     540,   144,   540,   258, -1065,   144,   540,   258, -1065,   795,
     599,   144,   919,  1904,   540,   795, -1065, -1065,  8257, -1065,
    8257,   395,  1452, -1065,   582,   923,  8257,   925,   924, -1065,
     657,  2340,   933,  8257,  1657,   930, -1065, -1065,  1190,  7300,
     934, -1065,  1933,  8614,   938,   936,  8257, -1065,  1005,   942,
     582,  8257,   408, -1065, -1065,  8155,   895,  8155, -1065,  8257,
   -1065,  1188,  1494,  1319,    34, -1065,   949,   144,   144,  1815,
     365,   365, -1065,  1183,   136,   917,   327,  7072,  1183, -1065,
     951, -1065,   451,   646, -1065, -1065, -1065, -1065, -1065,   958,
     959,  6698,  6154,  3162,  3298,  6562,   220,  6290,    75, -1065,
   -1065,  1028, -1065,  1028, -1065,  1765, -1065,  1765,  1151,  3673,
   -1065,  4081, -1065,  2360, -1065,  2881, -1065,  2081, -1065,  2066,
   -1065,   824, -1065,   824, -1065,   726, -1065,   726, -1065, -1065,
   -1065, -1065, -1065, -1065,   963,   966,   848,   795, -1065, -1065,
   -1065,   574,   979,   981,  1188,   984,   685,   848, -1065,   334,
     985, -1065,  7795,  1183, -1065,   438, -1065,   540,   540,   144,
     982,  1909,   540,   258,   540,   144,   144,   540, -1065, -1065,
   -1065, -1065,   646, -1065, -1065,   384, -1065,   973, -1065,   988,
   -1065, -1065, -1065, -1065,   646,   977,   227,   990,   702, -1065,
    1183, -1065,   997, -1065, -1065,  7414,  8758,   656,  8257, -1065,
    1001,   902, -1065,   126,   943, -1065,  1006,  1003, -1065, -1065,
   -1065,  1010, -1065,   646, -1065,   947,   365,    77, -1065,  1011,
   -1065,  1188,  1183,   225,   144, -1065,  1015, -1065, -1065,  1016,
      41,   967,   974, -1065, -1065,   632, -1065,   438, -1065, -1065,
   -1065,    40,  2091,  9095,   631,  1022,   796,    43,   978, -1065,
   -1065, -1065,   989, -1065, -1065, -1065,  1029,  2721,  6426,  6426,
    8758,  6426,  1035,  1037,  1041,  3129,  1042,  6426, -1065, -1065,
   -1065, -1065,  1043,  1044, -1065, -1065,   747, -1065, -1065, -1065,
   -1065,   540,   144,   144,   540,   540,   540,  1452,   438,  2624,
    1452,   582,  1047, -1065,  1183,  1048, -1065, -1065,  1657,   193,
   -1065,  8614, -1065,  1053,   438,    87,    75,   242, -1065,  2754,
     291, -1065,  8257,   446,  1049,  8927, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065,  1960, -1065,  7528,  1054, -1065,  8257,  7642,
    8257,  6426, -1065, -1065, -1065, -1065, -1065, -1065,  9167,  1055,
   -1065,  1008,  1061, -1065, -1065,  1045,  1749,   340, -1065,  1064,
   -1065,   816,  2624,  1070, -1065, -1065, -1065, -1065, -1065,  6426,
    6426, -1065, -1065, -1065,  3945, -1065, -1065, -1065,   540,   540,
   -1065, -1065,   552, -1065, -1065,  1072,  1058,  1065,  1067,  9171,
   -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
   -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
    1074,  8294,   992, -1065, -1065, -1065, -1065, -1065, -1065, -1065,
   -1065,  1567,   646,   990,  1592,   408, -1065,  1079, -1065,  1083,
   -1065, -1065, -1065,   126, -1065, -1065,   126,  1026, -1065, -1065,
    1090,   621,  8758,  8155,    44,  8468, -1065, -1065,  1091, -1065,
    1100,  3265,   723, -1065, -1065,   171, -1065,  1045, -1065,    70,
    1101,   132,  8257,  3026,  1092,  1075, -1065,  2232,  6426,   505,
   -1065,   978, -1065, -1065, -1065,   365, -1065,  1105, -1065, -1065,
   -1065,  1115, -1065, -1065, -1065, -1065, -1065, -1065,  9252, -1065,
   -1065,  6770, -1065, -1065,  1183, -1065, -1065, -1065, -1065, -1065,
    1116,  1126, -1065,   552,  1190,  1066,  1657,  8758,  1734, -1065,
     723, -1065, -1065,   403,  8257, -1065,   516, -1065,    75,  1045,
      43,  1118,  1614,   121, -1065,  1132,  1134,  1130,   714,   365,
     648,  1133,  3809, -1065,  1657, -1065, -1065, -1065, -1065,    64,
     978,  1636,  2624, -1065,  2624,  1102,  1106,    75,  1108,  1109,
      39,  1139, -1065, -1065, -1065,  1147, -1065,   156,   115,  7072,
   -1065,   505,  1136, -1065, -1065, -1065,    43, -1065, -1065,  6426,
   -1065,  1183,  1045, -1065,  1045,   664, -1065,  1152,  1172,  1167,
     386, -1065, -1065, -1065,  1183, -1065,   365, -1065, -1065,  1183,
    1170,  1171,  1178,  6426, -1065, -1065, -1065,  2368,  8257,  1184,
    1185,   101,  1186, -1065,  1183,  1193, -1065, -1065,  3809,   403,
   -1065, -1065, -1065, -1065, -1065,   365, -1065, -1065, -1065,   403,
      64,   403,  1636, -1065,  8257,  3401,  1199,  8257,  8257,  8257,
    1189,  1749,    76, -1065,   505,  1194,   505, -1065,   505, -1065,
    1202, -1065,  1183,  1203,  1208,  1211,  8257, -1065,  8758,   516,
   -1065, -1065, -1065, -1065,  1183, -1065,   757,  1183,  1183,  1183,
    1214,  1181, -1065, -1065, -1065, -1065,  3026, -1065, -1065, -1065,
    1183,   516,  1213, -1065, -1065, -1065
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
      12,     0,     0,     0,     0,     0,     4,   585,   400,   408,
     401,   402,   405,   406,   403,   404,   388,   407,   387,   409,
     410,   411,   412,   413,   279,   377,   378,   379,   619,   619,
     624,   146,   525,   525,     0,   586,   587,     0,   597,     0,
       0,   280,     0,     0,   375,   292,   382,   392,   386,   397,
     398,   541,     0,   604,   390,   595,     0,   620,     6,     0,
       8,   277,     1,    17,    62,    58,    59,     0,    16,   640,
       0,    78,    79,   640,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    10,    11,     9,    13,    20,    21,    22,    23,
      24,    25,    26,   640,    27,    28,    29,    30,    31,    32,
       0,    33,    34,    35,    36,    37,    38,    14,   105,   110,
     107,   106,    18,    15,   155,   156,   157,   158,   159,   160,
     113,   258,   618,   542,   543,     0,     0,   148,   147,   524,
     539,   540,   393,     0,   284,   598,   282,   389,     3,   376,
     381,   277,   385,     0,     0,   597,   525,   525,   369,     0,
     295,   278,   292,   304,   292,   349,   525,   330,   383,   399,
     391,   605,     0,     0,   593,   281,   621,     5,     0,   270,
     271,    19,     0,     0,     0,     0,     0,   380,   604,   548,
       0,     0,     0,   277,   277,   235,     0,     0,     0,   193,
     604,     0,     0,    60,    61,     0,    48,   631,    49,   277,
       0,   300,     0,    96,    97,   592,     0,   104,   277,   128,
     127,     0,   181,     0,     0,   135,   126,     0,   130,     0,
       0,     0,     0,     0,   304,     0,   330,   264,   261,   263,
     265,   266,   267,   268,   269,     0,   257,   259,     0,   418,
     419,   589,   416,   417,   588,   627,     0,   625,   590,     0,
       0,   599,   606,   596,   585,   635,   453,   454,   471,   472,
     473,   474,   475,   476,   477,   478,   479,   480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   287,     0,
     283,   285,   440,   380,     0,   450,   460,   439,   449,   461,
     451,   452,   384,   277,   525,   525,     0,     0,   525,   390,
     291,   289,   415,   414,   439,   360,     0,     0,   277,   524,
       0,   306,   351,   353,   322,   524,   305,   350,   352,   368,
     331,   293,   277,     0,   294,   277,     0,   524,   525,   525,
     367,   301,   344,   343,   322,   354,   603,   602,   601,     0,
     277,   273,   272,     0,   644,     0,   641,    66,    47,    46,
       0,   292,   304,     0,   545,   546,   544,   547,     0,     0,
      70,    88,     0,     0,    90,     0,     0,     0,   188,     0,
      12,     0,   191,     0,     0,    95,   630,     0,     0,    99,
     297,   304,   298,    42,     0,   591,     0,    52,     0,    51,
       0,     0,     0,     0,   180,   182,   187,   541,     0,   129,
     170,   132,   131,     0,     0,     0,     0,   588,     0,     0,
       0,     0,     0,     0,   277,     0,     0,     0,   322,     0,
       0,   262,     0,   421,     0,   529,   261,   260,   623,   628,
       0,   622,     0,     0,   394,     0,     0,   594,     0,     0,
       0,   460,     0,   455,     0,   459,   456,   457,   469,   520,
     519,   470,   521,   522,   584,     0,   523,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   634,   633,   632,
     525,   525,   525,   390,     0,   330,     0,   365,   362,   525,
     525,     0,   330,   354,   290,   361,   277,   525,   390,   315,
     323,   277,     0,   314,     0,   339,     0,     0,   358,     0,
     311,   524,   303,   348,   346,   524,   302,   347,   345,   366,
     356,     0,     0,     0,   307,   355,   600,     7,   277,   274,
     277,     0,     0,   639,     0,     0,   277,     0,     0,    73,
       0,     0,     0,     0,     0,     0,   189,   190,   300,     0,
       0,    12,   258,     0,   100,     0,   277,    98,     0,     0,
       0,     0,     0,   134,   133,   181,   168,     0,   184,   277,
      56,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,   108,   569,   330,   150,   604,   277,   569,   527,
       0,   526,   391,   262,   629,   626,   396,   395,   607,   445,
     441,   462,     0,     0,     0,   369,     0,     0,     0,   286,
     516,   498,   515,   497,   511,   493,   512,   494,   517,     0,
     510,   492,   509,   491,   505,   487,   506,   488,   504,   486,
     507,   489,   508,   490,   499,   481,   500,   482,   501,   483,
     502,   484,   503,   485,   447,   443,     0,   354,   334,   333,
     332,   356,     0,     0,   355,     0,     0,   322,   324,   354,
       0,   327,     0,   341,   340,   363,   359,   313,   312,     0,
       0,     0,   308,   357,   316,     0,     0,   310,   276,   275,
      64,    65,    63,   646,   649,   648,   642,   645,    44,     0,
      43,    39,    72,    69,    71,     0,   648,    88,     0,    91,
     569,   234,     0,   194,   195,     0,     0,     0,   277,    41,
       0,     0,   612,   610,     0,    55,     0,     0,   638,    85,
     637,     0,   102,   636,    84,     0,     0,   619,   183,     0,
      12,     0,   569,     0,     0,   341,     0,   172,    12,     0,
     560,   551,   552,   374,   373,   565,   372,   370,   561,   566,
     568,     0,     0,     0,     0,     0,   390,   619,   550,   163,
     167,   557,   529,   446,   442,   463,     0,     0,     0,     0,
     368,     0,     0,     0,     0,     0,     0,     0,   448,   444,
     335,   337,     0,     0,   342,   325,     0,   329,   328,   296,
     364,   317,     0,     0,   309,   321,   320,     0,   341,     0,
       0,     0,     0,    82,   569,     0,   111,   192,     0,   604,
      93,     0,    92,     0,   341,     0,     0,     0,   608,   284,
       0,    50,   277,     0,     0,     0,   174,   175,   178,   177,
     169,   176,   179,   258,   171,     0,     0,    77,   277,     0,
     277,     0,   563,   554,   553,   567,   371,   117,     0,     0,
     139,   141,     0,   149,   151,   425,   569,   292,   549,   528,
     530,   532,     0,     0,   513,   495,   466,   465,   464,     0,
       0,   514,   496,   458,   518,   336,   338,   326,   319,   318,
     647,   643,   292,   200,   221,     0,     0,     0,     0,   619,
     246,   247,   240,   248,   219,   215,   244,   239,   241,   242,
     243,   245,   220,   216,   217,   203,   210,   209,   213,   212,
       0,   619,   222,   201,   204,   205,   208,   214,   206,   207,
     218,   258,   648,    88,     0,     0,    89,     0,    67,     0,
     101,   299,   613,   611,   617,   616,   615,     0,    53,    54,
       0,   292,     0,   181,   261,     0,    57,    76,     0,   109,
       0,     0,   556,   119,   140,     0,   118,   422,   424,   432,
       0,   426,   277,     0,   566,   577,   154,     0,     0,     0,
     124,   550,   537,   536,   538,     0,   534,     0,   161,   468,
     467,     0,   225,   237,   236,   238,   223,    40,   619,   202,
     224,     0,    87,    83,   569,    80,    68,    94,   609,   614,
       0,     0,   186,   292,   300,     0,     0,     0,     0,   173,
     559,   564,   555,   292,   277,   142,     0,   423,     0,   425,
     619,   436,     0,   425,   428,   427,     0,     0,     0,     0,
       0,     0,   125,   123,     0,   120,   122,   114,   531,   525,
     550,     0,     0,    45,     0,   388,   387,     0,     0,   386,
     390,     0,   103,    86,   185,     0,   168,   330,     0,   277,
     562,     0,     0,   144,   143,   138,   619,   433,   434,     0,
     152,   569,   425,   429,   425,     0,   574,     0,   576,   578,
       0,   570,   571,   115,   569,   533,     0,   165,   164,   569,
       0,     0,     0,     0,   211,    81,   112,     0,   277,     0,
       0,     0,     0,   116,   569,     0,   435,   437,   438,   292,
     431,   430,   572,   573,   575,     0,   580,   582,   583,   292,
     525,   292,     0,   226,   277,     0,     0,   277,   277,   277,
       0,   569,     0,   145,     0,   579,     0,   535,     0,   162,
       0,   252,   569,     0,     0,     0,   277,   227,     0,     0,
     153,   581,   121,   166,   569,   233,     0,   569,   569,   569,
       0,     0,   136,   228,   249,   251,     0,   231,   230,   229,
     569,     0,     0,   232,   137,   250
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1065, -1065,  -338, -1065, -1065, -1065, -1065,    20,    51,     5,
      54,  -497, -1065,    60,    61, -1065, -1065, -1065,    63, -1065,
   -1065,    71, -1065, -1065,    81, -1065,    82,  -665,  -650,    83,
   -1065,    84, -1065,  -339, -1065,   -59,    89,    92,    94,    97,
   -1065,   494,  -635,  -904,  -406, -1065, -1065, -1065, -1042, -1064,
   -1065,   -78, -1065, -1065, -1065, -1065, -1065,    35, -1065, -1065,
     125,    36,    48, -1065, -1065,  -538, -1065,   681,   100, -1065,
   -1065, -1065,  -824, -1065,  -829,   345, -1065,   522, -1065,   526,
     108, -1065, -1065, -1065,  -293, -1065, -1065,  -523, -1065,  1023,
      11, -1065,   308,    28,   439, -1065,   805,   -34, -1065,  -535,
     -27,  1336,   -28,   -38,   915,    88,  -621,   534,   -40,    80,
     -66,    -1,   -31,   -64, -1065, -1065,   -76,  1240, -1065,  -342,
    -137, -1065, -1065, -1065,   325,   264,  -704, -1065, -1065,   269,
   -1065,   729, -1065,   546,  -135,  -484, -1065,   -30,   527, -1065,
   -1065, -1065,   317, -1065, -1065, -1065,  -200,   -77, -1065, -1065,
     253,  -706, -1065, -1065,  -722,   634,  -417,   164, -1065, -1065,
     184,    -2,  1165,   -44, -1065,   957,  -199,  -138,  1142, -1065,
    -354,     3,    23, -1065, -1065,   881,   872,  1344, -1065,   603,
      31,  -159, -1065,  -504
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     4,     5,    94,    95,    96,   809,   900,   901,   902,
     903,   397,   398,   904,   905,   740,   103,   104,   906,   106,
     107,   907,   703,   191,   908,   110,   909,   732,   552,   910,
     373,   911,   384,   212,   213,   214,   912,   913,   914,   915,
     748,   118,  1047,   979,   223,   119,   861,   965,  1026,  1075,
    1115,    40,   764,   120,   121,   122,   123,   916,  1052,   872,
    1098,   917,   918,   737,   840,   403,   404,   405,   919,   128,
     561,   380,   920,   921,   922,   923,  1054,   924,   925,   926,
     927,   130,   928,   929,  1165,   930,  1058,   245,   246,   247,
     306,   179,    41,   180,   289,   290,   291,   980,   161,   389,
     390,   321,   234,   307,   165,   235,   755,   756,    43,    44,
     292,   187,    46,    47,   259,   444,    48,    49,    50,   311,
     251,   252,   253,   595,   968,   969,   970,   971,  1029,  1030,
    1117,   313,   295,   296,   314,   298,   299,   343,   600,   601,
     768,   869,   870,   985,  1050,   986,    51,    52,   367,   368,
     871,   758,  1023,   772,   759,   760,  1166,   976,  1040,  1088,
    1089,   171,    53,   354,   396,    54,   174,    55,   263,   724,
     828,   931,    56,    57,   136,   256,   257,   300,   301,   734,
     184,   355,   356,   696
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      45,    45,   140,   141,   186,   262,   168,   164,   131,   297,
      99,   160,   150,   224,   227,   226,   312,   163,   162,   211,
     211,   547,   407,   712,   590,    97,   210,   225,   132,    42,
      59,   133,   134,   855,   346,   262,   145,   735,    45,   716,
     124,   125,   559,   813,   852,   693,   385,   707,   987,   565,
     207,   132,   132,   126,   800,    45,    98,   812,   694,   100,
      45,   439,   317,   668,   447,   101,   102,   694,   105,     7,
     996,   419,   178,     7,  -288,   668,   108,   448,     7,   358,
     176,   851,  1104,   233,  -292,   857,   109,   111,   112,   113,
       7,    62,   255,   303,   114,  1172,   153,   115,   378,   116,
    1159,   139,   117,    30,   189,   127,   151,   332,  -292,   379,
     302,   154,   139,   129,   152,   485,  -256,  1184,   329,  1071,
     153,  1025,   359,   149,   154,   333,   323,   328,   331,  1181,
     334,   751,   752,   166,   229,   350,  -288,    30,   353,   669,
     357,  1139,   449,   293,   974,   386,  -292,     7,   371,   536,
      45,   744,   858,   145,     7,  1108,    35,    36,   362,   361,
      35,    36,   144,   430,   440,    35,    36,   433,   435,   996,
    -292,   395,   288,   392,  1033,     7,   761,    35,    36,   434,
     486,   767,    37,   391,   420,    70,   155,   892,  1158,   423,
     762,   318,    45,    45,   156,  1028,   835,   157,     7,   337,
     363,   580,   158,   941,   374,   375,   159,   942,    45,   154,
    1107,  1024,   360,   172,   135,  1144,  1025,    45,   351,   352,
     387,    45,   142,   715,   717,  1146,   312,  1148,  1100,   400,
    1101,   386,  1140,   386,    35,    36,   938,   340,   173,   308,
     199,    35,    36,  1109,   826,     7,  1110,  1082,   149,   144,
     406,   158,     7,   429,     7,   164,  1022,  1034,  1035,   160,
      37,   781,    35,    36,    38,   163,   162,    37,   847,   827,
    1005,    38,   381,   429,   323,   328,   494,   496,   203,   338,
     501,  -590,   339,  1002,   320,    35,    36,   340,    37,   318,
     143,   320,   155,   815,   144,   948,   144,   319,   335,   166,
     529,   411,    45,   891,   412,   535,   933,   154,   524,   528,
     890,    37,   159,   558,  1070,    38,   336,    45,   416,   204,
     955,   144,   211,   694,   196,   846,   694,   545,   211,  1083,
     297,    45,    35,    36,    45,   567,   949,     7,   148,    35,
      36,    35,    36,   514,   197,     7,   517,   146,   782,    45,
       7,   783,   224,   591,   226,     7,   340,   573,     7,   177,
     574,   539,   945,   182,   175,    37,   225,    37,     7,    38,
    -420,    38,  -420,   578,   420,   407,   181,   407,  1120,    60,
    1121,   166,   149,    25,    26,    27,   494,   496,   501,   320,
     386,   492,   420,   183,   977,   151,   255,   935,   255,   690,
     421,   207,   845,   139,  -420,   593,   507,   598,  1001,   149,
     849,   726,   728,   154,   207,  1015,     7,   978,    61,   974,
     862,   364,   365,    45,    35,    36,  1127,     7,   190,   571,
     144,  1128,    35,    36,   386,   586,  1113,    35,    36,   137,
     691,   366,    35,    36,   608,    35,    36,   192,   416,   975,
      37,   729,   962,   730,    38,    35,    36,   153,    37,   138,
     323,   328,    38,   664,   293,   663,   260,   261,    37,   524,
     528,    37,    38,   592,   320,   155,   662,   193,   158,  1065,
     978,    37,   194,   499,     7,    38,   500,    25,    26,    27,
     195,   340,   320,   288,   770,   159,  -558,   211,    28,    29,
     697,   599,   202,    35,    36,    45,  1043,   705,   583,  1160,
      45,  1162,   205,  1163,    35,    36,   424,   665,    32,    33,
     392,   420,   670,   731,     1,     2,     3,   710,  -558,   422,
     391,   215,   260,   347,   333,   386,   487,    45,   616,    45,
      37,   741,   771,   663,    38,    45,   144,  1044,  1045,   688,
    1046,   689,    45,   757,   662,   742,  1073,   699,   757,  1074,
    -604,  -604,   131,   216,    99,    45,   753,     7,   207,   754,
      45,    35,    36,   218,    45,   780,    45,   720,    45,    97,
     511,   709,   132,   488,   489,     7,  -604,  1061,   207,  1008,
     739,   166,  1009,   546,   124,   125,    45,    37,   512,   727,
     566,    38,     7,   406,   420,   406,   153,   126,   765,   149,
      98,   333,   425,   100,   303,   149,   345,   535,   333,   101,
     102,   320,   105,   152,     7,   312,   793,   424,   658,   535,
     108,  1037,   154,   757,   206,   757,   207,   792,   349,   318,
     109,   111,   112,   113,   393,   333,   166,   679,   114,   382,
     818,   115,   386,   116,    35,    36,   117,   154,   388,   127,
     728,   702,   207,   207,  1119,   424,   659,   129,   394,   166,
     757,   429,    35,    36,   864,   153,   865,  1129,   424,   660,
      37,   814,  1131,   333,    38,   416,   438,   249,   250,    35,
      36,  1091,   426,  1092,   297,   427,   333,  1142,   441,   820,
     340,   730,   757,   793,   320,     7,   728,  1122,   207,  1123,
     821,    35,    36,   749,   792,    37,   442,    45,   131,    38,
      99,   750,   751,   752,   975,   511,   795,   680,   456,   823,
     681,   457,   863,   569,   570,    97,   465,    37,   132,   320,
     843,    38,   360,   512,   149,   729,   464,   730,   466,   150,
     124,   125,   166,   407,   497,   211,   951,   224,   227,   226,
     132,   498,   939,   126,   503,   862,    98,   705,   757,   100,
     867,   225,   837,   838,   757,   101,   102,   504,   105,   506,
     224,   537,   226,   530,   757,   839,   108,   511,   887,   541,
     132,   937,    35,    36,   225,   540,   109,   111,   112,   113,
    1174,   543,  1175,   542,   114,   512,   544,   115,   549,   116,
     550,  1176,   117,   751,   752,   127,   551,   731,    37,   249,
     250,   149,   155,   129,   451,   455,   757,   553,   293,   166,
     231,    45,   132,   232,  1182,   554,   312,   149,   158,   555,
     312,   562,   159,   950,    25,    26,    27,    45,   131,    45,
      99,   556,   131,   557,    99,   563,   564,   288,   991,   958,
     149,   960,   482,   483,   484,    97,   568,     7,   132,    97,
     967,  1173,   132,   294,  1177,  1178,  1179,   572,   435,   575,
     124,   125,   576,   316,   124,   125,  1014,  1183,   577,  1018,
     982,   983,   984,   126,   579,   132,    98,   126,   582,   100,
      98,  1087,   585,   100,   230,   101,   102,  1004,   105,   101,
     102,   606,   105,   581,   589,   609,   108,  1012,   618,  1102,
     108,   610,   132,   433,   435,  1059,   109,   111,   112,   113,
     109,   111,   112,   113,   114,  1018,   617,   115,   114,   116,
     654,   115,   117,   116,   132,   127,   117,   655,   657,   127,
     673,  1068,    45,   129,    35,    36,     7,   129,   480,   481,
     482,   483,   484,   675,   757,   661,   698,   685,   700,   701,
     312,    45,   967,     7,   708,   711,   392,   714,   718,  1064,
      37,   406,   719,  1036,   155,   725,   391,   736,   445,   593,
     745,   598,   231,   420,   763,   232,   769,   147,   773,   774,
     158,   587,   167,   788,   159,  1081,   789,   450,     7,   170,
     318,   458,   459,   460,   461,   462,   463,  1094,   325,  1095,
     790,   132,   791,    45,  1099,   794,   797,   807,   154,   808,
     802,   810,   811,  1078,   967,  1072,   198,   201,   967,   312,
     816,   757,   824,    35,    36,   825,   829,   830,   228,   831,
     832,   833,   844,   132,   757,   848,   850,   868,   853,   757,
      35,    36,   516,   866,   854,   519,   599,   236,    45,    37,
     874,   322,   327,    38,   757,   132,   879,   132,   880,  1116,
    1112,   342,   881,   883,   885,   886,    37,   967,   934,   967,
      38,   936,  1171,   320,   940,    35,    36,   957,   963,   132,
    1147,   757,   166,   952,   966,  1099,   981,    45,   309,    70,
     320,   964,   757,   324,   324,   988,   330,   992,  1000,  1136,
     997,   721,  1006,   344,   757,   722,  1007,   757,   757,   757,
    1010,  1011,  1019,    45,     7,   993,    45,    45,    45,   471,
     757,  1020,   994,   236,   995,  1150,  1038,  1032,  1153,  1154,
    1155,  1051,  1039,   376,   377,    45,  1111,   416,  1053,  1062,
     478,   479,   480,   481,   482,   483,   484,  1170,   167,  1063,
    1066,   360,  1079,  1086,  1084,  1085,  1093,  1114,   401,  -255,
     408,   409,  1105,  -254,   410,  1103,  -253,   418,   324,   324,
    1106,     7,   428,     7,   294,  1124,   621,   623,   625,   627,
     628,   629,   631,   633,   635,   637,   639,   641,   643,   645,
     647,   649,   651,   653,  1125,  1126,  1132,  1133,  1134,   322,
     327,    35,    36,   342,  1137,  1138,  1025,  1141,   151,  1156,
     151,   836,    25,    26,    27,   188,  1143,    25,    26,    27,
    1152,   672,  1161,  1164,  1167,   200,   154,    37,   154,  1168,
     167,   155,  1169,   523,   527,  1180,  1185,  1149,   738,   841,
     493,   495,   495,   842,   749,   502,   999,   340,   947,   437,
     619,   159,   750,   751,   752,   508,   324,   510,    35,    36,
      35,    36,   324,   478,   479,   480,   481,   482,   483,   484,
     169,   856,  1027,  1077,   324,   324,   324,  1076,  1048,   873,
     248,   254,   324,  1096,    37,  1157,    37,   799,   155,  1145,
     155,   753,   604,     7,   754,   348,   156,   418,   156,   157,
     822,   157,     7,   605,   158,   548,   158,     0,   159,     0,
     159,     0,     0,     0,     0,     0,     0,     0,   560,     0,
     775,   777,   633,   639,   649,     0,   785,   254,   254,   254,
     151,     0,     0,     0,     0,   369,   370,   254,   152,   420,
       0,     0,     0,     0,   153,     0,     7,   383,   154,     0,
     254,   495,   495,   495,     0,     0,     0,   584,   324,   324,
     254,     0,   324,   324,   324,     0,   324,   228,     0,   594,
     147,   236,   602,     0,     0,   417,     0,     0,   254,     0,
      35,    36,     0,   318,     0,   322,   327,   342,     0,    35,
      36,   139,     0,     0,   523,   527,     0,     0,     0,     0,
       0,   154,   342,     0,     0,   446,    37,     0,   208,     0,
     155,     0,     0,   217,     0,    37,     0,     0,   156,    38,
       0,   157,     0,     0,     7,   683,   158,     0,     0,   656,
     159,     0,     0,    35,    36,   340,     0,     0,   207,   320,
     167,     0,     0,     0,   667,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     7,     0,     0,   324,    37,
     258,   420,   324,    38,     0,     0,     0,   324,   324,   531,
     324,   499,     0,   326,   500,     0,     0,     7,     0,   340,
     207,     0,   341,   320,     0,     0,     0,   876,   877,     0,
     878,   236,   420,     0,     0,   167,   884,     0,     0,     0,
     531,    25,    26,    27,     0,     0,   258,   258,   258,     0,
       0,    35,    36,     0,   360,     0,   258,     0,   167,   236,
     324,     0,     0,     0,   324,   324,   324,   746,   747,   258,
       0,     0,     0,     0,   766,     0,     0,    37,   294,   258,
     399,    38,    35,    36,     0,     0,     0,     0,   326,   532,
       0,   341,   533,   413,   258,     0,   683,   258,     0,     0,
     961,   320,   431,     0,    35,    36,     0,     0,    37,     0,
     237,     0,    38,     0,     0,     7,     0,     0,   596,     0,
     532,     0,     0,   533,     7,   254,     0,   254,   989,   990,
      37,   238,   320,     0,   155,     0,     0,     7,     0,     0,
       0,   167,   231,     0,     0,   232,   324,     0,     0,     0,
     158,     0,   360,     0,   159,  1003,   324,     0,   324,     7,
       0,   151,   324,   324,     0,   239,   240,   241,     0,   139,
     242,     0,   243,   244,   360,   509,     0,  1080,     0,   154,
       7,   513,     0,     0,     0,   236,     0,     0,     0,     0,
       0,     0,     0,   520,   522,   526,   360,     0,     0,  1097,
       0,   534,    35,    36,     0,     0,     0,     0,     0,     0,
       0,    35,    36,   834,     7,     0,     0,   360,   167,     0,
       0,   324,     0,     0,    35,    36,     0,  1042,    37,   254,
       0,     0,   155,     0,     0,     0,   254,    37,     0,     0,
     231,   155,     0,   232,     0,     0,    35,    36,   158,     0,
      37,   597,   159,   723,   155,   254,     0,     7,     0,     0,
       0,   159,   231,     0,     0,   232,     0,    35,    36,     0,
     158,     0,    37,     0,   159,     0,   155,   509,   513,   324,
     324,   520,   522,   526,   231,   534,     0,   232,     0,     0,
       0,     0,   158,    37,  1069,   236,   159,   155,     0,     0,
     603,    35,    36,   786,   258,   231,   258,     0,   232,   972,
       0,     0,   228,   158,     0,     0,     7,   159,    25,    26,
      27,     0,     0,   973,     0,     0,     0,    37,  1118,     0,
       0,   155,     0,     0,     0,   228,     0,     0,     7,   231,
       0,     0,   232,     7,    35,    36,     0,   158,     0,     0,
     749,   159,  1135,   318,     7,     0,     0,     0,   750,   751,
     752,   521,     0,   666,     0,     0,     0,     0,     0,     0,
      37,   154,     0,     0,   155,   420,     0,   677,     0,     0,
     318,   678,   231,   679,     0,   232,   682,   684,   525,   687,
     158,   303,   467,   468,   159,     0,   471,   753,   154,   139,
     754,   819,     0,    35,    36,   692,   695,     0,   258,   154,
       0,   236,     0,     7,   704,   706,     0,   478,   479,   480,
     481,   482,   483,   484,     0,    35,    36,     7,  1013,    37,
      35,    36,     7,    38,   258,     0,   733,     0,     0,   666,
       0,    35,    36,   677,   678,   682,   743,     0,     0,     0,
     420,    37,     0,   320,     0,    38,    37,     0,   588,     0,
      38,     0,  1049,   680,   420,     7,   681,    37,   207,   420,
       0,    38,   686,     0,     0,   320,   237,   803,  1060,   499,
     320,     0,   500,     0,     0,     0,     0,   340,     7,     0,
       0,   167,     0,  1067,     0,   236,   254,   436,     0,     0,
      35,    36,     0,   237,     0,     0,     0,     0,     0,   236,
     943,   944,   946,     0,    35,    36,  1090,     0,     0,    35,
      36,   236,     0,     0,   954,   796,    37,     0,   236,     0,
      38,   239,   240,   241,     0,   801,   242,   804,   243,   244,
      37,   805,   806,     0,    38,    37,   766,     0,     0,    38,
     320,     0,    35,    36,     0,     0,     0,     0,   239,   240,
     241,     0,     0,   242,   320,   243,   244,   249,   250,   320,
       0,     0,   733,  1130,     0,    35,    36,     0,    37,     0,
       0,   733,    38,     0,     0,     0,     0,     0,     0,     0,
     414,     0,     0,   415,     0,     0,     0,     0,   158,     0,
     796,    37,  1090,     0,     0,    38,     0,     0,     0,   236,
       0,     0,   859,   414,   264,     0,   415,   207,   265,     0,
       0,   158,   266,   267,   268,   269,   270,   271,   272,   273,
     274,   275,   276,   277,     0,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,   278,     0,     0,  1031,     0,     0,     0,   888,   889,
      25,    26,    27,    28,    29,     0,   279,   280,     0,   860,
       0,   695,     0,     0,   932,   258,     0,     0,     0,     0,
       0,     0,    31,    32,    33,     0,     0,     0,     0,     0,
       0,     0,     0,   467,   468,   469,   470,   471,    34,     0,
     281,    35,    36,     0,     0,     0,     0,     0,   467,   468,
     469,   470,   471,  1031,     0,     0,   249,   250,   478,   479,
     480,   481,   482,   483,   484,     0,     0,    37,     0,     0,
       0,    38,   477,   478,   479,   480,   481,   482,   483,   484,
       0,     0,   282,     0,     0,   283,   284,   285,     0,     0,
       0,   286,   287,  1041,     0,   264,     0,     0,   207,   265,
       0,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,     0,     0,     0,     0,   310,     0,   733,
       0,    25,    26,    27,    28,    29,     0,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   603,     0,
       0,     0,     0,    31,    32,    33,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   249,   250,     0,
       0,     0,     0,     7,     0,     0,   207,     0,    37,     0,
       0,     0,    38,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   282,     0,     0,   283,   284,   285,   859,
       0,   264,   286,   287,   207,   265,     0,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,     0,
       0,     0,     0,     0,     0,     0,     0,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,     0,     0,     0,
      35,    36,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     0,     0,   249,   250,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,     0,     0,     0,     0,     0,   467,   468,   469,
     470,   471,     0,   249,   250,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     0,     0,     0,    38,   475,
     476,   477,   478,   479,   480,   481,   482,   483,   484,   282,
       0,     0,   283,   284,   285,   443,     0,   264,   286,   287,
     207,   265,   607,     0,     0,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,     0,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   467,   468,
     469,   470,   471,     0,   472,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,   893,     0,  -619,    64,     0,
       0,     0,    65,    66,    67,   282,     0,     0,   283,   284,
     285,     0,     0,     0,   286,   287,     0,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,     0,     0,     0,   894,  -619,     0,
    -198,     0,     0,  -619,  -619,  -619,  -619,  -619,     0,     0,
       0,     0,     0,     0,    30,     0,     0,     0,    70,    71,
      72,   895,    74,    75,    76,  -619,  -619,  -619,   896,   897,
     898,     0,  -619,  -619,  -619,     0,    77,  -619,    78,  -619,
    -619,  -619,  -619,     0,  -619,  -619,    79,     0,     0,     0,
      83,    84,    85,    86,    87,    88,    89,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    90,     0,
    -619,     0,     0,    91,  -619,  -619,     0,     0,     0,   899,
       0,     0,     0,     0,     0,     0,     0,   264,     0,     0,
     207,   265,   875,     0,  -619,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,    24,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,     0,    30,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   467,   468,
     469,   470,   471,     0,   472,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,   282,     0,     0,   283,   284,
     285,     0,     0,   264,   286,   287,   207,   265,     0,     0,
       0,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,     0,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
     278,     0,     0,     0,     0,   310,     0,     0,     0,    25,
      26,    27,    28,    29,     0,   279,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,   281,
      35,    36,     0,     0,     0,     0,     0,     0,   467,   468,
     469,   470,   471,     0,     0,   249,   250,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    37,     0,     0,     0,
      38,   476,   477,   478,   479,   480,   481,   482,   483,   484,
       0,   282,     0,     0,   283,   284,   285,     0,     0,   264,
     286,   287,   207,   265,     0,     0,     0,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,   278,     0,     0,     0,
       0,     0,     0,     0,     0,    25,    26,    27,    28,    29,
       0,   279,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,   281,    35,    36,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   249,   250,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    37,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     283,   284,   285,     0,     0,   264,   286,   287,   207,   265,
     882,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,   778,     0,     0,     0,     0,     0,     0,
     632,    25,    26,    27,    28,    29,     0,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   467,   468,   469,   470,
     471,     0,   472,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,   473,   474,   475,   476,
     477,   478,   479,   480,   481,   482,   483,   484,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    37,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,   283,   284,   285,     0,
       0,   264,   286,   287,   207,   265,  1021,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,   779,
       0,     0,     0,     0,     0,     0,   638,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   467,   468,   469,   470,   471,     0,   472,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,   473,   474,   475,   476,   477,   478,   479,   480,
     481,   482,   483,   484,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   283,   284,   285,     0,     0,   264,   286,   287,
     207,   265,     0,     0,  1151,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,     0,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   467,   468,
     469,   470,   471,     0,   472,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   283,   284,
     285,     0,     0,   264,   286,   287,   207,   265,     0,     0,
       0,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,     0,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
     278,     0,     0,     0,     0,     0,     0,     0,     0,    25,
      26,    27,    28,    29,     0,   279,   280,     0,     0,   515,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,   281,
      35,    36,   452,     0,     0,   207,   265,     0,     0,     0,
     266,   267,   268,   269,   270,   271,   272,   273,   274,   275,
     276,   277,     0,     0,     0,     0,    37,     0,     0,     0,
      38,     0,     0,     0,     0,     0,     0,     0,     0,   453,
       0,   282,     0,     0,   283,   284,   285,   454,     0,   264,
     286,   287,   207,   265,   279,   280,     0,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,   278,     0,   281,     0,
     787,     0,     0,     0,     0,    25,    26,    27,    28,    29,
       0,   279,   280,     0,     0,   518,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
     467,   468,   469,   470,   471,     0,   472,     0,     0,     0,
       0,     0,     0,    34,     0,   281,    35,    36,     0,     0,
     473,   474,   475,   476,   477,   478,   479,   480,   481,   482,
     483,   484,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    37,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     283,   284,   285,     0,     0,   264,   286,   287,   207,   265,
       0,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,     0,     0,     0,     0,     0,     0,     0,
     620,    25,    26,    27,    28,    29,     0,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   467,   468,   469,   470,
     471,     0,   472,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,   473,   474,   475,   476,
     477,   478,   479,   480,   481,   482,   483,   484,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    37,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,   283,   284,   285,     0,
       0,   264,   286,   287,   207,   265,     0,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,     0,
       0,     0,     0,     0,     0,     0,   622,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   467,   468,   469,   470,   471,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,   473,   474,   475,   476,   477,   478,   479,   480,
     481,   482,   483,   484,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   283,   284,   285,     0,     0,   264,   286,   287,
     207,   265,     0,     0,     0,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,   624,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   467,   468,
     469,   470,   471,     0,     0,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,     0,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   283,   284,
     285,     0,     0,   264,   286,   287,   207,   265,     0,     0,
       0,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,     0,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
     278,     0,     0,     0,     0,     0,     0,     0,   626,    25,
      26,    27,    28,    29,     0,   279,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,   281,
      35,    36,     8,     9,    10,    11,    12,    13,    14,    15,
       0,    17,     0,    19,    20,    21,    22,    23,     0,     0,
       0,     0,     0,     0,     0,     0,    37,     0,     0,     0,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,   283,   284,   285,     0,     0,   264,
     286,   287,   207,   265,     0,     0,     0,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,   278,     0,     0,     0,
       0,     0,     0,     0,   630,    25,    26,    27,    28,    29,
       0,   279,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,   281,    35,    36,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    37,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     283,   284,   285,     0,     0,   264,   286,   287,   207,   265,
       0,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,     0,     0,     0,     0,     0,     0,     0,
     632,    25,    26,    27,    28,    29,     0,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    37,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,   283,   284,   285,     0,
       0,   264,   286,   287,   207,   265,     0,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,     0,
       0,     0,     0,     0,     0,     0,   634,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   283,   284,   285,     0,     0,   264,   286,   287,
     207,   265,     0,     0,     0,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,   636,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   283,   284,
     285,     0,     0,   264,   286,   287,   207,   265,     0,     0,
       0,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,     0,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
     278,     0,     0,     0,     0,     0,     0,     0,   638,    25,
      26,    27,    28,    29,     0,   279,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,   281,
      35,    36,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    37,     0,     0,     0,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,   283,   284,   285,     0,     0,   264,
     286,   287,   207,   265,     0,     0,     0,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,   278,     0,     0,     0,
       0,     0,     0,     0,   640,    25,    26,    27,    28,    29,
       0,   279,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,   281,    35,    36,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    37,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     283,   284,   285,     0,     0,   264,   286,   287,   207,   265,
       0,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,     0,     0,     0,     0,     0,     0,     0,
     642,    25,    26,    27,    28,    29,     0,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    37,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,   283,   284,   285,     0,
       0,   264,   286,   287,   207,   265,     0,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,     0,
       0,     0,     0,     0,     0,     0,   644,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   283,   284,   285,     0,     0,   264,   286,   287,
     207,   265,     0,     0,     0,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,   646,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   283,   284,
     285,     0,     0,   264,   286,   287,   207,   265,     0,     0,
       0,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,     0,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
     278,     0,     0,     0,     0,     0,     0,     0,   648,    25,
      26,    27,    28,    29,     0,   279,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,   281,
      35,    36,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    37,     0,     0,     0,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,   283,   284,   285,     0,     0,   264,
     286,   287,   207,   265,     0,     0,     0,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,   278,     0,     0,     0,
       0,     0,     0,     0,   650,    25,    26,    27,    28,    29,
       0,   279,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,   281,    35,    36,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    37,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     283,   284,   285,     0,     0,   264,   286,   287,   207,   265,
       0,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,     0,     0,     0,     0,     0,     0,     0,
     652,    25,    26,    27,    28,    29,     0,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    37,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,   283,   284,   285,     0,
       0,   264,   286,   287,   207,   265,     0,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,     0,
       0,     0,     0,     0,     0,     0,     0,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,   671,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   283,   284,   285,     0,     0,   264,   286,   287,
     207,   265,     0,     0,     0,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   278,     0,     0,     0,     0,     0,
       0,     0,   776,    25,    26,    27,    28,    29,     0,   279,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    34,     0,   281,    35,    36,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   283,   284,
     285,     0,     0,   264,   286,   287,   207,   265,     0,     0,
       0,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,     0,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
     278,     0,     0,     0,     0,     0,     0,     0,   784,    25,
      26,    27,    28,    29,     0,   279,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,   281,
      35,    36,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    37,     0,     0,     0,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,   283,   284,   285,     0,     0,   264,
     286,   287,   207,   265,     0,     0,     0,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,   278,     0,     0,     0,
       0,     0,     0,     0,     0,    25,    26,    27,    28,    29,
       0,   279,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,   281,    35,    36,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    37,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     283,   284,   285,     0,     0,   264,   286,   287,   207,   265,
       0,     0,     0,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,   278,     0,     0,     0,     0,     0,     0,     0,
     648,    25,    26,    27,    28,    29,   505,   279,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
       0,   281,    35,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   467,   468,   469,   470,   471,     0,
     472,     0,     0,     0,     0,     0,     0,     0,    37,     0,
       0,     0,    38,     0,   473,   474,   475,   476,   477,   478,
     479,   480,   481,   482,   483,   484,   283,   284,     0,     0,
       0,   264,   286,   287,   207,   265,     0,     0,     0,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   278,     0,
       0,     0,     0,     0,     0,     0,     0,    25,    26,    27,
      28,    29,     0,   279,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     7,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,   281,    35,    36,
       0,     0,     0,     0,     8,     9,    10,    11,    12,    13,
      14,    15,  1055,    17,  1056,    19,    20,    21,    22,    23,
       0,     0,     0,     0,    37,     0,     0,     0,    38,    25,
      26,    27,    28,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   286,   287,
       0,    31,    32,    33,     0,     0,     0,     7,     0,     0,
       0,     0,     0,     0,     0,   611,     0,    34,   432,     0,
      35,    36,     0,     0,     0,     0,     0,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   303,     0,    37,     0,     0,     0,
      38,  1017,    24,    25,    26,    27,    28,    29,     0,     0,
       0,     0,   154,     0,    30,     0,     0,     0,     0,     0,
    1057,     0,     0,     0,     0,    31,    32,    33,   612,     0,
       0,   467,   468,   469,   470,   471,     0,   472,     0,     0,
       0,    34,     0,     0,    35,    36,     0,     0,     0,     0,
       0,   473,   613,   475,   476,   614,   478,   479,   480,   481,
     615,   483,   484,     0,     0,     0,     0,     0,     0,     0,
      37,     7,     0,     0,    38,     0,     0,     0,     0,    39,
       0,     0,   304,     0,     0,   305,     0,     0,     0,     0,
     158,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,   303,     0,
       0,     0,     0,     0,     0,     0,    24,    25,    26,    27,
      28,    29,     0,     0,     0,     0,   154,     0,    30,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,     0,    35,    36,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    37,     7,     0,     0,    38,   674,
       0,     0,     0,    39,     0,     0,   490,     0,     0,   491,
       0,     0,     0,     0,   158,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,     0,     0,     0,     0,     0,     0,     0,     0,
      24,    25,    26,    27,    28,    29,     0,   467,   468,   469,
     470,   471,    30,   472,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,     0,   473,   474,   475,
     476,   477,   478,   479,   480,   481,   482,   483,   484,    34,
       0,     0,    35,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,    63,    37,  -619,
      64,     0,    38,     0,    65,    66,    67,    39,     0,     0,
     414,     0,     0,   415,     0,     0,     0,     0,   158,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,     0,     0,     0,    68,
    -619,     0,     0,     0,     0,  -619,  -619,  -619,  -619,  -619,
       0,     0,     0,    69,     0,     0,    30,     0,     0,     0,
      70,    71,    72,    73,    74,    75,    76,  -619,  -619,  -619,
       0,     0,     0,     0,  -619,  -619,  -619,     0,    77,  -619,
      78,  -619,  -619,  -619,  -619,     0,  -619,  -619,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      90,    63,  -619,  -619,    64,    91,  -619,     0,    65,    66,
      67,    92,    93,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
       0,     0,     0,    68,  -619,     0,   713,     0,     0,  -619,
    -619,  -619,  -619,  -619,     0,     0,     0,    69,     0,     0,
      30,     0,     0,     0,    70,    71,    72,    73,    74,    75,
      76,  -619,  -619,  -619,     0,     0,     0,     0,  -619,  -619,
    -619,     0,    77,  -619,    78,  -619,  -619,  -619,  -619,     0,
    -619,  -619,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    90,    63,  -619,  -619,    64,    91,
    -619,     0,    65,    66,    67,    92,    93,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,     0,     0,     0,    68,  -619,     0,
     817,     0,     0,  -619,  -619,  -619,  -619,  -619,     0,     0,
       0,    69,     0,     0,    30,     0,     0,     0,    70,    71,
      72,    73,    74,    75,    76,  -619,  -619,  -619,     0,     0,
       0,     0,  -619,  -619,  -619,     0,    77,  -619,    78,  -619,
    -619,  -619,  -619,     0,  -619,  -619,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    90,    63,
    -619,  -619,    64,    91,  -619,     0,    65,    66,    67,    92,
      93,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,     0,     0,
       0,    68,  -619,     0,     0,     0,     0,  -619,  -619,  -619,
    -619,  -619,     0,     0,     0,    69,     0,     0,    30,     0,
       0,   956,    70,    71,    72,    73,    74,    75,    76,  -619,
    -619,  -619,     0,     0,     0,     0,  -619,  -619,  -619,     0,
      77,  -619,    78,  -619,  -619,  -619,  -619,     0,  -619,  -619,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    90,    63,  -619,  -619,    64,    91,  -619,     0,
      65,    66,    67,    92,    93,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,     0,     0,     0,    68,  -619,     0,   959,     0,
       0,  -619,  -619,  -619,  -619,  -619,     0,     0,     0,    69,
       0,     0,    30,     0,     0,     0,    70,    71,    72,    73,
      74,    75,    76,  -619,  -619,  -619,     0,     0,     0,     0,
    -619,  -619,  -619,     0,    77,  -619,    78,  -619,  -619,  -619,
    -619,     0,  -619,  -619,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,     0,     6,     0,     7,     0,
       0,     0,   676,     0,     0,     0,    90,     0,  -619,     0,
       0,    91,  -619,     0,     0,     0,     0,    92,    93,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,     0,     0,     0,     0,     0,
       0,     0,     0,    24,    25,    26,    27,    28,    29,     0,
     467,   468,   469,   470,   471,    30,   472,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,     0,
     473,   474,   475,   476,   477,   478,   479,   480,   481,   482,
     483,   484,    34,     0,     0,    35,    36,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       7,     0,     0,     0,   798,     0,     0,     0,     0,     0,
       0,    37,     0,     0,     0,    38,     0,     0,     0,     0,
      39,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,     0,     0,     0,
       0,     0,     0,     0,     0,    24,    25,    26,    27,    28,
      29,     0,   467,   468,   469,   470,   471,    30,   472,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,     0,   473,   474,   475,   476,   477,   478,   479,   480,
     481,   482,   483,   484,    34,     0,     0,    35,    36,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     7,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    37,     0,     0,     0,    38,     0,     0,
       0,     0,    39,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,     0,
       0,     0,     0,     0,     0,     0,     0,    24,    25,    26,
      27,    28,    29,     0,     0,     0,     0,     0,     0,    30,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    34,     0,     0,    35,
      36,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     7,     0,     0,     0,
       0,     0,     0,     0,     0,    37,   372,     0,     0,    38,
       0,     0,     0,     0,    39,   538,     0,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,     0,     0,     0,     0,     0,     0,     0,
       0,    24,    25,    26,    27,    28,    29,     0,     0,     0,
       0,     0,     0,    30,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      34,     0,     0,    35,    36,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     7,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    37,
       0,     0,     0,    38,     0,     0,     0,     0,    39,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,     0,     0,     0,     0,     0,
       0,     0,     0,    24,    25,    26,    27,    28,    29,     0,
       0,     0,     0,     0,     0,    30,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    34,     0,     0,    35,    36,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       7,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    37,     0,     0,   402,    38,     0,     0,     0,     0,
      39,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,     0,    64,     0,
       0,     0,    65,    66,    67,    24,    25,    26,    27,    28,
      29,     0,     0,     0,     0,     0,     0,    30,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,     0,     0,     0,     0,     0,     0,   894,     0,     0,
    -196,     0,     0,     0,    34,     0,     0,    35,    36,     0,
       0,     0,     0,     0,    30,     0,     0,     0,    70,    71,
      72,   895,    74,    75,    76,     0,     0,     0,   896,   897,
     898,     0,     0,    37,     0,     0,    77,    38,    78,     0,
       0,     0,    39,     0,     0,     0,    79,     0,     0,     0,
      83,    84,    85,    86,    87,    88,    89,   185,     0,     7,
       0,     0,     0,     0,     0,     0,     0,     0,    90,     0,
       0,     0,     0,    91,     0,     0,     0,     0,     0,   998,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    25,    26,    27,    28,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     7,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,     0,    35,    36,     0,     0,
       0,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,     0,     0,
       0,     0,    37,     0,     0,     0,    38,    25,    26,    27,
      28,    29,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     0,     0,     7,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,  1016,     0,    35,    36,
       0,     0,     0,     0,     0,     0,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,     0,     0,    37,     0,     0,     0,    38,  1017,
      24,    25,    26,    27,    28,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,     0,     7,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
       0,     0,    35,    36,     0,     0,     0,     0,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,   209,     0,     0,     0,    37,     0,
       0,     0,    38,    25,    26,    27,    28,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,     0,     7,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    34,     0,     0,    35,    36,     0,     0,     0,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,     0,     0,     0,     0,
      37,     0,     0,     0,    38,    25,    26,    27,    28,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,     7,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,   432,     0,    35,    36,     0,     0,
       0,     0,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,     0,     0,
       0,     0,    37,     0,     0,     0,    38,    25,    26,    27,
      28,    29,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,     7,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,     0,     0,    35,    36,
       0,     0,     0,     0,     8,     9,    10,    11,    12,    13,
      14,    15,   219,    17,   220,    19,    20,    21,    22,    23,
       0,     0,     0,     0,    37,     0,     0,     0,    38,   221,
       0,     0,    28,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,     0,     0,
      35,    36,     0,     0,     0,     0,     0,     0,     0,     0,
       7,     0,     0,   222,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    37,     0,     0,     0,
      38,     8,     9,    10,    11,    12,    13,    14,    15,   219,
      17,   220,    19,    20,    21,    22,    23,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,    28,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    34,     0,     0,    35,    36,     0,
       0,     0,     0,     0,     0,     0,     0,     7,     0,     0,
     953,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    37,     0,     0,     0,    38,     8,     9,
      10,    11,    12,    13,    14,    15,   219,    17,   220,    19,
      20,    21,    22,    23,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,    28,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,     7,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    34,     0,     0,    35,    36,     0,     0,     0,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,     0,     0,     0,     0,     0,
      37,     0,     0,     0,    38,     0,     0,    28,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,     0,
       7,     0,     0,     0,     0,    64,     0,     0,     0,    65,
      66,    67,    34,     0,     0,    35,    36,     0,     0,     0,
       0,     8,     9,    10,    11,    12,    13,    14,    15,   219,
      17,   220,    19,    20,    21,    22,    23,     0,     0,     0,
       0,    37,     0,     0,   894,    38,   221,  -199,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    30,     0,     0,     0,    70,    71,    72,    31,    74,
      75,    76,     0,     0,     0,   896,   897,   898,     0,     0,
       0,     0,     0,    77,    34,    78,    64,    35,    36,     0,
      65,    66,    67,    79,     0,     0,     0,    83,    84,    85,
      86,    87,    88,    89,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    37,     0,    90,     0,    38,     0,     0,
      91,     0,     0,     0,     0,   894,     0,     0,  -197,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    30,     0,     0,     0,    70,    71,    72,     0,
      74,    75,    76,     0,     0,     0,   896,   897,   898,     0,
       0,     0,     0,     0,    77,     0,    78,     0,     0,     0,
       0,     0,     0,     0,    79,     0,     0,     0,    83,    84,
      85,    86,    87,    88,    89,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    90,     0,     0,     0,
       0,    91
};

static const yytype_int16 yycheck[] =
{
       1,     2,    32,    33,    70,   143,    46,    45,     5,   144,
       5,    45,    43,    91,    91,    91,   153,    45,    45,    85,
      86,   363,   222,   558,   430,     5,    85,    91,     5,     1,
       2,    28,    29,   755,   172,   173,    38,   575,    39,   562,
       5,     5,   380,   708,   750,   542,   205,   551,   872,   388,
       6,    28,    29,     5,   675,    56,     5,   707,   542,     5,
      61,    40,     3,    41,   263,     5,     5,   551,     5,     3,
     899,   230,    61,     3,    42,    41,     5,    47,     3,     0,
      57,    40,    43,   110,    42,    45,     5,     5,     5,     5,
       3,     0,   136,    40,     5,  1159,    54,     5,    43,     5,
    1142,    48,     5,    60,    73,     5,    40,    40,    42,    54,
     150,    58,    48,     5,    48,    47,    77,  1181,   158,  1023,
      54,    45,    43,    43,    58,    58,   156,   157,   162,  1171,
     164,    90,    91,    45,   103,    42,   104,    60,   182,   117,
     184,    40,   112,   144,   866,     6,   104,     3,   192,   348,
     151,   117,   112,   155,     3,    40,    90,    91,   186,   186,
      90,    91,   103,   119,   143,    90,    91,   245,   245,   998,
     104,   215,   144,   211,    42,     3,   593,    90,    91,   245,
     112,   598,   116,   211,    40,    64,   120,   808,   112,   233,
      54,    40,   193,   194,   128,   125,   119,   131,     3,    48,
      54,    62,   136,   824,   193,   194,   140,   120,   209,    58,
      54,    40,    40,   118,    92,  1119,    45,   218,   125,   126,
     209,   222,    40,   561,   563,  1129,   363,  1131,  1052,   218,
    1054,     6,   131,     6,    90,    91,    43,   136,   143,   151,
      45,    90,    91,   128,   118,     3,   131,   126,   168,   103,
     222,   136,     3,   117,     3,   293,   962,   125,   126,   293,
     116,    41,    90,    91,   120,   293,   293,   116,    43,   143,
     935,   120,    54,   117,   304,   305,   304,   305,     4,   128,
     308,    54,   131,   933,   140,    90,    91,   136,   116,    40,
     143,   140,   120,   710,   103,     4,   103,    48,    40,   211,
     340,   128,   303,   807,   131,   345,   810,    58,   338,   339,
     807,   116,   140,   379,  1020,   120,    58,   318,   230,    45,
     843,   103,   388,   807,    73,   742,   810,   361,   394,  1033,
     465,   332,    90,    91,   335,   394,    45,     3,     0,    90,
      91,    90,    91,   332,    93,     3,   335,    39,   128,   350,
       3,   131,   430,   430,   430,     3,   136,   128,     3,     0,
     131,   350,   120,    40,    56,   116,   430,   116,     3,   120,
      43,   120,    45,   407,    40,   575,   121,   577,  1082,     1,
    1084,   293,   302,    49,    50,    51,   414,   415,   416,   140,
       6,   303,    40,    40,    54,    40,   440,   814,   442,     4,
      48,     6,   740,    48,    77,   432,   318,   434,   931,   329,
     748,   570,     4,    58,     6,   953,     3,    77,    40,  1141,
     762,    52,    53,   424,    90,    91,    40,     3,    40,    45,
     103,    45,    90,    91,     6,   424,  1071,    90,    91,    52,
      45,    72,    90,    91,   446,    90,    91,    40,   360,   866,
     116,    43,   858,    45,   120,    90,    91,    54,   116,    72,
     490,   491,   120,   503,   465,   503,   119,   120,   116,   499,
     500,   116,   120,    45,   140,   120,   503,    40,   136,  1014,
      77,   116,    40,   128,     3,   120,   131,    49,    50,    51,
      40,   136,   140,   465,    43,   140,    45,   563,    52,    53,
     544,    77,    40,    90,    91,   506,     1,   551,   420,  1144,
     511,  1146,    40,  1148,    90,    91,    40,   506,    72,    73,
     558,    40,   511,   115,   122,   123,   124,   554,    77,    48,
     558,    40,   119,   120,    58,     6,     7,   538,   450,   540,
     116,   581,    91,   581,   120,   546,   103,    42,    43,   538,
      45,   540,   553,   593,   581,   582,    40,   546,   598,    43,
     117,   118,   559,    40,   559,   566,   128,     3,     6,   131,
     571,    90,    91,    40,   575,   615,   577,   566,   579,   559,
      40,   553,   559,     6,     7,     3,   143,  1004,     6,   943,
     579,   503,   946,    40,   559,   559,   597,   116,    58,   571,
      40,   120,     3,   575,    40,   577,    54,   559,   597,   529,
     559,    58,    48,   559,    40,   535,   117,   657,    58,   559,
     559,   140,   559,    48,     3,   762,   664,    40,    41,   669,
     559,   973,    58,   673,     4,   675,     6,   664,    41,    40,
     559,   559,   559,   559,    43,    58,   558,    48,   559,    45,
     716,   559,     6,   559,    90,    91,   559,    58,    45,   559,
       4,     4,     6,     6,  1081,    40,    41,   559,    42,   581,
     710,   117,    90,    91,    43,    54,    45,  1094,    40,    41,
     116,   708,  1099,    58,   120,   597,    77,   105,   106,    90,
      91,    43,   128,    45,   829,   131,    58,  1114,    61,    43,
     136,    45,   742,   741,   140,     3,     4,    43,     6,    45,
      54,    90,    91,    81,   741,   116,    42,   718,   715,   120,
     715,    89,    90,    91,  1141,    40,    41,   128,    40,   718,
     131,    40,   763,    41,    42,   715,    42,   116,   715,   140,
     737,   120,    40,    58,   664,    43,   104,    45,    40,   780,
     715,   715,   664,   953,    41,   821,   833,   835,   835,   835,
     737,    41,   821,   715,   117,  1107,   715,   811,   808,   715,
     767,   835,   737,   737,   814,   715,   715,    58,   715,    40,
     858,     0,   858,   117,   824,   737,   715,    40,    41,    41,
     767,   818,    90,    91,   858,    42,   715,   715,   715,   715,
      43,    41,    45,    54,   715,    58,    42,   715,    41,   715,
      54,    54,   715,    90,    91,   715,    42,   115,   116,   105,
     106,   741,   120,   715,   278,   279,   866,    42,   829,   741,
     128,   832,   809,   131,  1176,    41,   973,   757,   136,    41,
     977,    41,   140,   832,    49,    50,    51,   848,   845,   850,
     845,    43,   849,    43,   849,    41,    41,   829,   892,   848,
     780,   850,   136,   137,   138,   845,    41,     3,   845,   849,
     865,  1164,   849,   144,  1167,  1168,  1169,    41,   955,   103,
     845,   845,   104,   154,   849,   849,   952,  1180,    42,   955,
      74,    75,    76,   845,    40,   872,   845,   849,    41,   845,
     849,  1038,    43,   849,    40,   845,   845,   934,   845,   849,
     849,    41,   849,   117,   117,     3,   845,   951,    40,  1057,
     849,     3,   899,  1001,  1001,  1001,   845,   845,   845,   845,
     849,   849,   849,   849,   845,  1001,   103,   845,   849,   845,
       3,   849,   845,   849,   921,   845,   849,     3,   117,   849,
      41,  1017,   953,   845,    90,    91,     3,   849,   134,   135,
     136,   137,   138,    41,  1004,   117,    43,    48,    43,    45,
    1107,   972,   967,     3,    41,    45,  1014,    43,    40,  1013,
     116,   953,    46,   972,   120,    43,  1014,    92,   259,  1016,
      41,  1018,   128,    40,    77,   131,    45,    40,    40,    40,
     136,    48,    45,    40,   140,  1032,    40,   278,     3,    52,
      40,   282,   283,   284,   285,   286,   287,  1044,    48,  1049,
      41,   998,    41,  1024,  1051,    41,    41,    54,    58,    41,
      48,    54,    42,  1030,  1029,  1024,    79,    80,  1033,  1176,
      43,  1081,    41,    90,    91,   143,   103,    41,    91,    46,
      40,   104,    41,  1030,  1094,    40,    40,    79,    91,  1099,
      90,    91,   333,    41,    90,   336,    77,   110,  1069,   116,
      41,   156,   157,   120,  1114,  1052,    41,  1054,    41,  1076,
    1069,   166,    41,    41,    41,    41,   116,  1082,    41,  1084,
     120,    43,  1158,   140,    41,    90,    91,    43,    43,  1076,
    1130,  1141,  1014,    54,    43,  1132,    42,  1108,   151,    64,
     140,   103,  1152,   156,   157,    45,   159,    45,   126,  1108,
      46,   116,    43,   166,  1164,   120,    43,  1167,  1168,  1169,
     104,    41,    41,  1134,     3,    77,  1137,  1138,  1139,   111,
    1180,    41,    77,   186,    77,  1134,    54,    46,  1137,  1138,
    1139,    46,    77,   196,   197,  1156,  1068,  1069,    43,    43,
     132,   133,   134,   135,   136,   137,   138,  1156,   211,    43,
     104,    40,    54,    43,    42,    41,    43,    41,   221,    77,
     223,   224,    43,    77,   227,    77,    77,   230,   231,   232,
      43,     3,   235,     3,   465,    43,   467,   468,   469,   470,
     471,   472,   473,   474,   475,   476,   477,   478,   479,   480,
     481,   482,   483,   484,    42,    48,    46,    46,    40,   304,
     305,    90,    91,   308,    40,    40,    45,    41,    40,    40,
      40,   737,    49,    50,    51,    70,    43,    49,    50,    51,
      41,   512,    48,    41,    41,    80,    58,   116,    58,    41,
     293,   120,    41,   338,   339,    41,    43,  1132,   577,   737,
     303,   304,   305,   737,    81,   308,   921,   136,   829,   246,
     465,   140,    89,    90,    91,   318,   319,   320,    90,    91,
      90,    91,   325,   132,   133,   134,   135,   136,   137,   138,
      50,   757,   967,  1029,   337,   338,   339,  1028,   981,   772,
     135,   136,   345,  1050,   116,  1141,   116,   673,   120,  1125,
     120,   128,   440,     3,   131,   173,   128,   360,   128,   131,
     717,   131,     3,   442,   136,   368,   136,    -1,   140,    -1,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   381,    -1,
     611,   612,   613,   614,   615,    -1,   617,   182,   183,   184,
      40,    -1,    -1,    -1,    -1,   190,   191,   192,    48,    40,
      -1,    -1,    -1,    -1,    54,    -1,     3,   202,    58,    -1,
     205,   414,   415,   416,    -1,    -1,    -1,   420,   421,   422,
     215,    -1,   425,   426,   427,    -1,   429,   430,    -1,   432,
     433,   434,   435,    -1,    -1,   230,    -1,    -1,   233,    -1,
      90,    91,    -1,    40,    -1,   490,   491,   492,    -1,    90,
      91,    48,    -1,    -1,   499,   500,    -1,    -1,    -1,    -1,
      -1,    58,   507,    -1,    -1,   260,   116,    -1,    84,    -1,
     120,    -1,    -1,    89,    -1,   116,    -1,    -1,   128,   120,
      -1,   131,    -1,    -1,     3,   530,   136,    -1,    -1,   492,
     140,    -1,    -1,    90,    91,   136,    -1,    -1,     6,   140,
     503,    -1,    -1,    -1,   507,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,     3,    -1,    -1,   521,   116,
     136,    40,   525,   120,    -1,    -1,    -1,   530,   531,    48,
     533,   128,    -1,   157,   131,    -1,    -1,     3,    -1,   136,
       6,    -1,   166,   140,    -1,    -1,    -1,   778,   779,    -1,
     781,   554,    40,    -1,    -1,   558,   787,    -1,    -1,    -1,
      48,    49,    50,    51,    -1,    -1,   182,   183,   184,    -1,
      -1,    90,    91,    -1,    40,    -1,   192,    -1,   581,   582,
     583,    -1,    -1,    -1,   587,   588,   589,   590,   591,   205,
      -1,    -1,    -1,    -1,   597,    -1,    -1,   116,   829,   215,
     216,   120,    90,    91,    -1,    -1,    -1,    -1,   232,   128,
      -1,   235,   131,   229,   230,    -1,   661,   233,    -1,    -1,
     851,   140,   238,    -1,    90,    91,    -1,    -1,   116,    -1,
      23,    -1,   120,    -1,    -1,     3,    -1,    -1,   433,    -1,
     128,    -1,    -1,   131,     3,   440,    -1,   442,   879,   880,
     116,    44,   140,    -1,   120,    -1,    -1,     3,    -1,    -1,
      -1,   664,   128,    -1,    -1,   131,   669,    -1,    -1,    -1,
     136,    -1,    40,    -1,   140,    43,   679,    -1,   681,     3,
      -1,    40,   685,   686,    -1,    78,    79,    80,    -1,    48,
      83,    -1,    85,    86,    40,   319,    -1,    43,    -1,    58,
       3,   325,    -1,    -1,    -1,   708,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   337,   338,   339,    40,    -1,    -1,    43,
      -1,   345,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,   736,     3,    -1,    -1,    40,   741,    -1,
      -1,   744,    -1,    -1,    90,    91,    -1,   978,   116,   544,
      -1,    -1,   120,    -1,    -1,    -1,   551,   116,    -1,    -1,
     128,   120,    -1,   131,    -1,    -1,    90,    91,   136,    -1,
     116,    40,   140,   568,   120,   570,    -1,     3,    -1,    -1,
      -1,   140,   128,    -1,    -1,   131,    -1,    90,    91,    -1,
     136,    -1,   116,    -1,   140,    -1,   120,   421,   422,   802,
     803,   425,   426,   427,   128,   429,    -1,   131,    -1,    -1,
      -1,    -1,   136,   116,    40,   818,   140,   120,    -1,    -1,
     436,    90,    91,   618,   440,   128,   442,    -1,   131,    40,
      -1,    -1,   835,   136,    -1,    -1,     3,   140,    49,    50,
      51,    -1,    -1,    54,    -1,    -1,    -1,   116,  1079,    -1,
      -1,   120,    -1,    -1,    -1,   858,    -1,    -1,     3,   128,
      -1,    -1,   131,     3,    90,    91,    -1,   136,    -1,    -1,
      81,   140,  1103,    40,     3,    -1,    -1,    -1,    89,    90,
      91,    48,    -1,   507,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    58,    -1,    -1,   120,    40,    -1,   521,    -1,    -1,
      40,   525,   128,    48,    -1,   131,   530,   531,    48,   533,
     136,    40,   107,   108,   140,    -1,   111,   128,    58,    48,
     131,   716,    -1,    90,    91,   541,   542,    -1,   544,    58,
      -1,   934,    -1,     3,   550,   551,    -1,   132,   133,   134,
     135,   136,   137,   138,    -1,    90,    91,     3,   951,   116,
      90,    91,     3,   120,   570,    -1,   572,    -1,    -1,   583,
      -1,    90,    91,   587,   588,   589,   582,    -1,    -1,    -1,
      40,   116,    -1,   140,    -1,   120,   116,    -1,    48,    -1,
     120,    -1,   985,   128,    40,     3,   131,   116,     6,    40,
      -1,   120,    48,    -1,    -1,   140,    23,    48,  1001,   128,
     140,    -1,   131,    -1,    -1,    -1,    -1,   136,     3,    -1,
      -1,  1014,    -1,  1016,    -1,  1018,   811,    44,    -1,    -1,
      90,    91,    -1,    23,    -1,    -1,    -1,    -1,    -1,  1032,
     825,   826,   827,    -1,    90,    91,  1039,    -1,    -1,    90,
      91,  1044,    -1,    -1,    44,   669,   116,    -1,  1051,    -1,
     120,    78,    79,    80,    -1,   679,    83,   681,    85,    86,
     116,   685,   686,    -1,   120,   116,  1069,    -1,    -1,   120,
     140,    -1,    90,    91,    -1,    -1,    -1,    -1,    78,    79,
      80,    -1,    -1,    83,   140,    85,    86,   105,   106,   140,
      -1,    -1,   708,  1096,    -1,    90,    91,    -1,   116,    -1,
      -1,   717,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,   131,    -1,    -1,    -1,    -1,   136,    -1,
     744,   116,  1125,    -1,    -1,   120,    -1,    -1,    -1,  1132,
      -1,    -1,     1,   128,     3,    -1,   131,     6,     7,    -1,
      -1,   136,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    -1,   969,    -1,    -1,    -1,   802,   803,
      49,    50,    51,    52,    53,    -1,    55,    56,    -1,    58,
      -1,   807,    -1,    -1,   810,   811,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,   108,   109,   110,   111,    87,    -1,
      89,    90,    91,    -1,    -1,    -1,    -1,    -1,   107,   108,
     109,   110,   111,  1028,    -1,    -1,   105,   106,   132,   133,
     134,   135,   136,   137,   138,    -1,    -1,   116,    -1,    -1,
      -1,   120,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,   131,    -1,    -1,   134,   135,   136,    -1,    -1,
      -1,   140,   141,     1,    -1,     3,    -1,    -1,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    45,    -1,   935,
      -1,    49,    50,    51,    52,    53,    -1,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   954,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,     3,    -1,    -1,     6,    -1,   116,    -1,
      -1,    -1,   120,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,   131,    -1,    -1,   134,   135,   136,     1,
      -1,     3,   140,   141,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,   109,
     110,   111,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   131,
      -1,    -1,   134,   135,   136,     1,    -1,     3,   140,   141,
       6,     7,    41,    -1,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,   107,   108,
     109,   110,   111,    -1,   113,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,     1,    -1,     3,     4,    -1,
      -1,    -1,     8,     9,    10,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,    -1,   140,   141,    -1,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    43,    44,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    60,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    -1,    78,    79,    80,    -1,    82,    83,    84,    85,
      86,    87,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      96,    97,    98,    99,   100,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,    -1,
     116,    -1,    -1,   119,   120,   121,    -1,    -1,    -1,   125,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
       6,     7,    41,    -1,   140,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    -1,    60,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,   107,   108,
     109,   110,   111,    -1,   113,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,   125,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,     3,   140,   141,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,    49,
      50,    51,    52,    53,    -1,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
     109,   110,   111,    -1,    -1,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,   131,    -1,    -1,   134,   135,   136,    -1,    -1,     3,
     140,   141,     6,     7,    -1,    -1,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
     134,   135,   136,    -1,    -1,     3,   140,   141,     6,     7,
      41,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    -1,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,   107,   108,   109,   110,
     111,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,   134,   135,   136,    -1,
      -1,     3,   140,   141,     6,     7,    41,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,   107,   108,   109,   110,   111,    -1,   113,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,   134,   135,   136,    -1,    -1,     3,   140,   141,
       6,     7,    -1,    -1,    43,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,   107,   108,
     109,   110,   111,    -1,   113,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,     3,   140,   141,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    53,    -1,    55,    56,    -1,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      90,    91,     3,    -1,    -1,     6,     7,    -1,    -1,    -1,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,
      -1,   131,    -1,    -1,   134,   135,   136,    48,    -1,     3,
     140,   141,     6,     7,    55,    56,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    89,    -1,
      77,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    55,    56,    -1,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
     107,   108,   109,   110,   111,    -1,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
     134,   135,   136,    -1,    -1,     3,   140,   141,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    -1,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,   107,   108,   109,   110,
     111,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,   134,   135,   136,    -1,
      -1,     3,   140,   141,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,   107,   108,   109,   110,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,   134,   135,   136,    -1,    -1,     3,   140,   141,
       6,     7,    -1,    -1,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,   107,   108,
     109,   110,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,     3,   140,   141,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    -1,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      90,    91,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    33,    -1,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,   134,   135,   136,    -1,    -1,     3,
     140,   141,     6,     7,    -1,    -1,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
     134,   135,   136,    -1,    -1,     3,   140,   141,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    -1,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,   134,   135,   136,    -1,
      -1,     3,   140,   141,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,   134,   135,   136,    -1,    -1,     3,   140,   141,
       6,     7,    -1,    -1,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,     3,   140,   141,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    -1,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,   134,   135,   136,    -1,    -1,     3,
     140,   141,     6,     7,    -1,    -1,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
     134,   135,   136,    -1,    -1,     3,   140,   141,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    -1,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,   134,   135,   136,    -1,
      -1,     3,   140,   141,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,   134,   135,   136,    -1,    -1,     3,   140,   141,
       6,     7,    -1,    -1,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,     3,   140,   141,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    -1,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,   134,   135,   136,    -1,    -1,     3,
     140,   141,     6,     7,    -1,    -1,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
     134,   135,   136,    -1,    -1,     3,   140,   141,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    -1,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,   134,   135,   136,    -1,
      -1,     3,   140,   141,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,   134,   135,   136,    -1,    -1,     3,   140,   141,
       6,     7,    -1,    -1,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    -1,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    89,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,   134,   135,
     136,    -1,    -1,     3,   140,   141,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    -1,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,   134,   135,   136,    -1,    -1,     3,
     140,   141,     6,     7,    -1,    -1,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    89,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
     134,   135,   136,    -1,    -1,     3,   140,   141,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    59,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    89,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,   108,   109,   110,   111,    -1,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    -1,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   134,   135,    -1,    -1,
      -1,     3,   140,   141,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    89,    90,    91,
      -1,    -1,    -1,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    49,
      50,    51,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,   141,
      -1,    71,    72,    73,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    41,    -1,    87,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,   116,    -1,    -1,    -1,
     120,   121,    48,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    -1,    58,    -1,    60,    -1,    -1,    -1,    -1,    -1,
     140,    -1,    -1,    -1,    -1,    71,    72,    73,   104,    -1,
      -1,   107,   108,   109,   110,   111,    -1,   113,    -1,    -1,
      -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,     3,    -1,    -1,   120,    -1,    -1,    -1,    -1,   125,
      -1,    -1,   128,    -1,    -1,   131,    -1,    -1,    -1,    -1,
     136,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    -1,    58,    -1,    60,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,     3,    -1,    -1,   120,    59,
      -1,    -1,    -1,   125,    -1,    -1,   128,    -1,    -1,   131,
      -1,    -1,    -1,    -1,   136,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    -1,   107,   108,   109,
     110,   111,    60,   113,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,    87,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     0,     1,   116,     3,
       4,    -1,   120,    -1,     8,     9,    10,   125,    -1,    -1,
     128,    -1,    -1,   131,    -1,    -1,    -1,    -1,   136,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    43,
      44,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    57,    -1,    -1,    60,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      -1,    -1,    -1,    -1,    78,    79,    80,    -1,    82,    83,
      84,    85,    86,    87,    88,    -1,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,     1,   116,     3,     4,   119,   120,    -1,     8,     9,
      10,   125,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    43,    44,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    -1,    -1,    -1,    57,    -1,    -1,
      60,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    -1,    -1,    -1,    -1,    78,    79,
      80,    -1,    82,    83,    84,    85,    86,    87,    88,    -1,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,     1,   116,     3,     4,   119,
     120,    -1,     8,     9,    10,   125,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    43,    44,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    57,    -1,    -1,    60,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    -1,    -1,
      -1,    -1,    78,    79,    80,    -1,    82,    83,    84,    85,
      86,    87,    88,    -1,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,     1,
     116,     3,     4,   119,   120,    -1,     8,     9,    10,   125,
     126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    43,    44,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    57,    -1,    -1,    60,    -1,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    -1,    -1,    -1,    -1,    78,    79,    80,    -1,
      82,    83,    84,    85,    86,    87,    88,    -1,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,     1,   116,     3,     4,   119,   120,    -1,
       8,     9,    10,   125,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    43,    44,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    -1,    -1,    -1,    57,
      -1,    -1,    60,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    -1,    -1,    -1,    -1,
      78,    79,    80,    -1,    82,    83,    84,    85,    86,    87,
      88,    -1,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,    -1,     1,    -1,     3,    -1,
      -1,    -1,    59,    -1,    -1,    -1,   114,    -1,   116,    -1,
      -1,   119,   120,    -1,    -1,    -1,    -1,   125,   126,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    -1,
     107,   108,   109,   110,   111,    60,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
       3,    -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,   116,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,
     125,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    -1,   107,   108,   109,   110,   111,    60,   113,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    -1,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,    87,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    -1,    -1,
      -1,    -1,   125,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   116,    13,    -1,    -1,   120,
      -1,    -1,    -1,    -1,   125,   126,    -1,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,
      -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,   125,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   116,    -1,    -1,   119,   120,    -1,    -1,    -1,    -1,
     125,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,     4,    -1,
      -1,    -1,     8,     9,    10,    48,    49,    50,    51,    52,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,
      46,    -1,    -1,    -1,    87,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    60,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    -1,    -1,    -1,    74,    75,
      76,    -1,    -1,   116,    -1,    -1,    82,   120,    84,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    92,    -1,    -1,    -1,
      96,    97,    98,    99,   100,   101,   102,     1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,    -1,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,   125,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,   116,    -1,    -1,    -1,   120,   121,
      48,    49,    50,    51,    52,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,   116,    -1,
      -1,    -1,   120,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,   116,    -1,    -1,    -1,   120,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    49,
      -1,    -1,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
     120,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   116,    -1,    -1,    -1,   120,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,    -1,   120,    -1,    -1,    52,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,
       3,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,     8,
       9,    10,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,   116,    -1,    -1,    43,   120,    49,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    60,    -1,    -1,    -1,    64,    65,    66,    71,    68,
      69,    70,    -1,    -1,    -1,    74,    75,    76,    -1,    -1,
      -1,    -1,    -1,    82,    87,    84,     4,    90,    91,    -1,
       8,     9,    10,    92,    -1,    -1,    -1,    96,    97,    98,
      99,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   116,    -1,   114,    -1,   120,    -1,    -1,
     119,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    60,    -1,    -1,    -1,    64,    65,    66,    -1,
      68,    69,    70,    -1,    -1,    -1,    74,    75,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    84,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    96,    97,
      98,    99,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,
      -1,   119
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   122,   123,   124,   145,   146,     1,     3,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    48,    49,    50,    51,    52,    53,
      60,    71,    72,    73,    87,    90,    91,   116,   120,   125,
     195,   236,   237,   252,   253,   255,   256,   257,   260,   261,
     262,   290,   291,   306,   309,   311,   316,   317,     1,   237,
       1,    40,     0,     1,     4,     8,     9,    10,    43,    57,
      64,    65,    66,    67,    68,    69,    70,    82,    84,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     114,   119,   125,   126,   147,   148,   149,   151,   152,   153,
     154,   157,   158,   160,   161,   162,   163,   164,   165,   168,
     169,   170,   173,   175,   180,   181,   182,   183,   185,   189,
     197,   198,   199,   200,   201,   205,   206,   212,   213,   224,
     225,   315,   316,   315,   315,    92,   318,    52,    72,    48,
     281,   281,    40,   143,   103,   305,   236,   309,     0,   253,
     256,    40,    48,    54,    58,   120,   128,   131,   136,   140,
     241,   242,   244,   246,   247,   248,   249,   309,   252,   261,
     309,   305,   118,   143,   310,   236,   316,     0,   234,   235,
     237,   121,    40,    40,   324,     1,   254,   255,   306,   324,
      40,   167,    40,    40,    40,    40,    73,    93,   309,    45,
     306,   309,    40,     4,    45,    40,     4,     6,   321,    40,
     179,   254,   177,   178,   179,    40,    40,   321,    40,    32,
      34,    49,   103,   188,   195,   257,   260,   291,   309,   324,
      40,   128,   131,   244,   246,   249,   309,    23,    44,    78,
      79,    80,    83,    85,    86,   231,   232,   233,   306,   105,
     106,   264,   265,   266,   306,   307,   319,   320,   321,   258,
     119,   120,   311,   312,     3,     7,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    40,    55,
      56,    89,   131,   134,   135,   136,   140,   141,   237,   238,
     239,   240,   254,   255,   275,   276,   277,   278,   279,   280,
     321,   322,   252,    40,   128,   131,   234,   247,   249,   309,
      45,   263,   264,   275,   278,    59,   275,     3,    40,    48,
     140,   245,   248,   281,   309,    48,   245,   248,   281,   252,
     309,   241,    40,    58,   241,    40,    58,    48,   128,   131,
     136,   245,   248,   281,   309,   117,   311,   120,   312,    41,
      42,   125,   126,   307,   307,   325,   326,   307,     0,    43,
      40,   244,   246,    54,    52,    53,    72,   292,   293,   306,
     306,   307,    13,   174,   234,   234,   309,   309,    43,    54,
     215,    54,    45,   306,   176,   325,     6,   234,    45,   243,
     244,   246,   247,    43,    42,   307,   308,   155,   156,   321,
     234,   309,   119,   209,   210,   211,   237,   290,   309,   309,
     309,   128,   131,   321,   128,   131,   249,   306,   309,   325,
      40,    48,    48,   307,    40,    48,   128,   131,   309,   117,
     119,   321,    88,   195,   254,   291,    44,   233,    77,    40,
     143,    61,    42,     1,   259,   275,   306,   310,    47,   112,
     275,   277,     3,    40,    48,   277,    40,    40,   275,   275,
     275,   275,   275,   275,   104,    42,    40,   107,   108,   109,
     110,   111,   113,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,    47,   112,     7,     6,     7,
     128,   131,   249,   309,   246,   309,   246,    41,    41,   128,
     131,   246,   309,   117,    58,    59,    40,   249,   309,   245,
     309,    40,    58,   245,   234,    59,   275,   234,    59,   275,
     245,    48,   245,   248,   281,    48,   245,   248,   281,   252,
     117,    48,   128,   131,   245,   252,   310,     0,   126,   234,
      42,    41,    54,    41,    42,   241,    40,   263,   309,    41,
      54,    42,   172,    42,    41,    41,    43,    43,   254,   146,
     309,   214,    41,    41,    41,   177,    40,   179,    41,    41,
      42,    45,    41,   128,   131,   103,   104,    42,   241,    40,
      62,   117,    41,   249,   309,    43,   234,    48,    48,   117,
     188,   291,    45,   244,   309,   267,   306,    40,   244,    77,
     282,   283,   309,   321,   320,   319,    41,    41,   305,     3,
       3,    41,   104,   128,   131,   136,   249,   103,    40,   240,
      48,   275,    48,   275,    48,   275,    48,   275,   275,   275,
      48,   275,    48,   275,    48,   275,    48,   275,    48,   275,
      48,   275,    48,   275,    48,   275,    48,   275,    48,   275,
      48,   275,    48,   275,     3,     3,   309,   117,    41,    41,
      41,   117,   244,   247,   252,   234,   245,   309,    41,   117,
     234,    59,   275,    41,    59,    41,    59,   245,   245,    48,
     128,   131,   245,   248,   245,    48,    48,   245,   234,   234,
       4,    45,   321,   155,   279,   321,   327,   307,    43,   234,
      43,    45,     4,   166,   321,   307,   321,   327,    41,   237,
     244,    45,   243,    46,    43,   146,   231,   177,    40,    46,
     234,   116,   120,   306,   313,    43,   325,   237,     4,    43,
      45,   115,   171,   321,   323,   209,    92,   207,   211,   234,
     159,   252,   244,   321,   117,    41,   309,   309,   184,    81,
      89,    90,    91,   128,   131,   250,   251,   252,   295,   298,
     299,   300,    54,    77,   196,   234,   309,   300,   284,    45,
      43,    91,   297,    40,    40,   275,    48,   275,    41,    41,
     252,    41,   128,   131,    48,   275,   306,    77,    40,    40,
      41,    41,   244,   247,    41,    41,   245,    41,    59,   299,
     250,   245,    48,    48,   245,   245,   245,    54,    41,   150,
      54,    42,   172,   171,   244,   300,    43,    46,   254,   306,
      43,    54,   323,   234,    41,   143,   118,   143,   314,   103,
      41,    46,    40,   104,   309,   119,   185,   201,   205,   206,
     208,   221,   223,   315,    41,   146,   300,    43,    40,   146,
      40,    40,   295,    91,    90,   298,   251,    45,   112,     1,
      58,   190,   263,   256,    43,    45,    41,   315,    79,   285,
     286,   294,   203,   282,    41,    41,   275,   275,   275,    41,
      41,    41,    41,    41,   275,    41,    41,    41,   245,   245,
     155,   327,   250,     1,    43,    67,    74,    75,    76,   125,
     151,   152,   153,   154,   157,   158,   162,   165,   168,   170,
     173,   175,   180,   181,   182,   183,   201,   205,   206,   212,
     216,   217,   218,   219,   221,   222,   223,   224,   226,   227,
     229,   315,   321,   327,    41,   300,    43,   244,    43,   179,
      41,   250,   120,   306,   306,   120,   306,   238,     4,    45,
     234,   291,    54,   103,    44,   231,    63,    43,   234,    46,
     234,   275,   188,    43,   103,   191,    43,   153,   268,   269,
     270,   271,    40,    54,   298,   300,   301,    54,    77,   187,
     241,    42,    74,    75,    76,   287,   289,   216,    45,   275,
     275,   241,    45,    77,    77,    77,   218,    46,   125,   219,
     126,   231,   172,    43,   244,   171,    43,    43,   314,   314,
     104,    41,   241,   309,   254,   209,    88,   121,   254,    41,
      41,    41,   295,   296,    40,    45,   192,   268,   125,   272,
     273,   306,    46,    42,   125,   126,   234,   263,    54,    77,
     302,     1,   275,     1,    42,    43,    45,   186,   286,   309,
     288,    46,   202,    43,   220,    32,    34,   140,   230,   260,
     309,   300,    43,    43,   241,   243,   104,   309,   254,    40,
     295,   187,   234,    40,    43,   193,   273,   269,   315,    54,
      43,   244,   126,   270,    42,    41,    43,   264,   303,   304,
     309,    43,    45,    43,   244,   281,   294,    43,   204,   244,
     216,   216,   311,    77,    43,    43,    43,    54,    40,   128,
     131,   249,   234,   186,    41,   194,   315,   274,   275,   300,
     270,   270,    43,    45,    43,    42,    48,    40,    45,   300,
     309,   300,    46,    46,    40,   275,   234,    40,    40,    40,
     131,    41,   300,    43,   187,   304,   187,   281,   187,   204,
     234,    43,    41,   234,   234,   234,    40,   301,   112,   192,
     186,    48,   186,   186,    41,   228,   300,    41,    41,    41,
     234,   254,   193,   228,    43,    45,    54,   228,   228,   228,
      41,   192,   263,   228,   193,    43
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   144,   145,   145,   145,   145,   145,   145,   145,   146,
     146,   146,   146,   147,   147,   147,   147,   147,   147,   147,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   150,
     149,   151,   152,   153,   153,   153,   153,   153,   154,   154,
     155,   156,   156,   157,   157,   157,   159,   158,   160,   160,
     161,   161,   162,   162,   162,   162,   163,   164,   164,   165,
     165,   166,   166,   167,   167,   168,   168,   168,   169,   169,
     170,   170,   170,   170,   171,   171,   171,   172,   172,   173,
     174,   174,   175,   175,   175,   176,   177,   178,   178,   179,
     179,   179,   180,   181,   182,   183,   183,   183,   184,   183,
     183,   183,   183,   183,   185,   185,   185,   185,   185,   185,
     186,   186,   186,   186,   187,   187,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   189,   189,   189,   190,
     191,   191,   192,   193,   194,   193,   195,   195,   195,   196,
     196,   197,   198,   198,   199,   200,   200,   200,   200,   200,
     200,   202,   201,   203,   201,   204,   204,   205,   207,   206,
     206,   206,   206,   206,   208,   208,   208,   208,   208,   208,
     209,   209,   210,   210,   211,   211,   211,   211,   212,   212,
     212,   214,   213,   215,   213,   213,   216,   216,   216,   216,
     216,   217,   217,   218,   218,   218,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   218,
     218,   218,   219,   219,   219,   220,   219,   221,   222,   223,
     223,   223,   223,   223,   224,   225,   226,   226,   226,   227,
     227,   227,   227,   227,   227,   227,   227,   227,   227,   228,
     228,   228,   229,   230,   230,   230,   230,   231,   231,   232,
     232,   233,   233,   233,   233,   233,   233,   233,   233,   233,
     234,   235,   235,   235,   235,   235,   235,   235,   236,   236,
     237,   237,   237,   238,   238,   239,   239,   240,   240,   241,
     241,   241,   241,   242,   242,   242,   242,   243,   243,   243,
     243,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   245,   245,   245,   245,   245,   245,   245,   245,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   248,   248,
     248,   248,   248,   248,   248,   248,   249,   249,   249,   249,
     250,   250,   250,   251,   251,   252,   252,   253,   253,   253,
     254,   255,   255,   255,   255,   255,   256,   256,   256,   256,
     256,   256,   256,   258,   257,   259,   259,   260,   261,   261,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   263,   263,   264,   264,   265,   266,
     267,   267,   268,   268,   269,   269,   270,   270,   270,   270,
     270,   270,   270,   271,   272,   272,   273,   273,   274,   275,
     275,   276,   276,   276,   276,   276,   276,   276,   276,   277,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   277,
     278,   278,   278,   278,   278,   278,   278,   278,   278,   278,
     278,   279,   279,   279,   279,   279,   279,   279,   279,   279,
     279,   280,   280,   280,   280,   280,   280,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   280,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   280,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   280,   280,   280,   280,
     280,   280,   280,   280,   281,   281,   282,   284,   283,   283,
     285,   285,   287,   286,   288,   286,   289,   289,   289,   290,
     290,   291,   291,   291,   292,   292,   292,   293,   293,   294,
     294,   295,   295,   295,   295,   296,   296,   297,   297,   298,
     298,   298,   298,   298,   298,   299,   299,   299,   300,   300,
     301,   301,   301,   301,   301,   301,   302,   302,   303,   303,
     303,   303,   304,   304,   305,   306,   306,   306,   307,   307,
     307,   308,   308,   309,   309,   309,   309,   309,   309,   309,
     310,   310,   310,   310,   311,   311,   312,   312,   313,   313,
     313,   313,   313,   313,   314,   314,   314,   314,   315,   315,
     316,   316,   317,   318,   318,   319,   319,   320,   320,   320,
     321,   321,   322,   322,   322,   322,   323,   323,   323,   324,
     324,   325,   326,   326,   326,   326,   326,   326,   327,   327
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     3,     2,     3,     2,     5,     2,     2,
       2,     2,     0,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       8,     5,     3,     5,     5,     9,     3,     3,     2,     2,
       4,     1,     1,     7,     7,     5,     0,     7,     1,     1,
       2,     2,     1,     5,     5,     5,     3,     7,     8,     5,
       3,     1,     1,     3,     0,     4,     7,     6,     1,     1,
       8,    10,     6,     8,     1,     1,     5,     5,     0,     7,
       1,     3,     6,     6,     8,     1,     1,     1,     3,     2,
       3,     6,     5,     9,     2,     1,     1,     1,     0,     7,
       1,     6,    10,     1,     8,     9,    10,     6,     7,     7,
       1,     5,     1,     1,     1,     2,     1,     1,     1,     2,
       1,     2,     2,     3,     3,     1,    13,    15,     9,     1,
       1,     0,     1,     1,     0,     3,     1,     2,     2,     2,
       0,     6,     9,    12,     7,     1,     1,     1,     1,     1,
       1,     0,    11,     0,     9,     1,     4,     5,     0,     6,
       3,     6,     5,     8,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     3,     2,     7,     6,     1,     3,     4,
       4,     0,     6,     0,     5,     5,     1,     2,     0,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     0,     5,     7,     8,     9,
       9,     9,    10,     8,     5,     2,     2,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       4,     2,     6,     1,     1,     1,     1,     1,     0,     1,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     3,     4,     4,     0,     2,     1,
       1,     2,     2,     1,     0,     1,     3,     1,     1,     2,
       3,     2,     0,     2,     2,     1,     5,     1,     1,     5,
       0,     2,     3,     3,     1,     2,     2,     3,     4,     5,
       4,     3,     4,     4,     3,     3,     4,     5,     6,     6,
       5,     5,     1,     2,     3,     4,     5,     3,     4,     4,
       1,     2,     4,     4,     4,     5,     6,     5,     6,     3,
       4,     4,     5,     2,     2,     3,     3,     3,     3,     1,
       2,     2,     2,     2,     2,     3,     3,     4,     3,     4,
       2,     3,     3,     4,     5,     3,     3,     2,     2,     1,
       1,     2,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     2,     1,     2,     3,     2,     1,     1,     1,     2,
       1,     2,     1,     0,     4,     2,     2,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     2,     1,     0,     1,     2,     2,     3,
       4,     4,     1,     3,     2,     3,     1,     3,     1,     1,
       1,     3,     4,     3,     4,     3,     4,     3,     4,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     5,     2,
       1,     1,     3,     4,     5,     5,     5,     6,     6,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     5,     5,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     5,     5,     3,     3,     3,     5,     2,
       2,     2,     2,     2,     1,     0,     1,     0,     3,     0,
       1,     3,     0,     4,     0,     6,     1,     1,     1,     2,
       2,     1,     2,     2,     1,     1,     1,     1,     0,     1,
       0,     1,     1,     2,     2,     1,     0,     1,     0,     4,
       1,     1,     5,     2,     4,     1,     1,     2,     1,     0,
       3,     3,     4,     4,     3,     4,     2,     0,     1,     3,
       2,     4,     2,     2,     3,     1,     1,     1,     1,     1,
       1,     1,     0,     2,     4,     1,     3,     1,     2,     3,
       3,     2,     2,     2,     1,     2,     1,     3,     2,     4,
       1,     3,     1,     3,     3,     2,     2,     2,     1,     0,
       1,     2,     4,     3,     0,     1,     3,     1,     2,     3,
       2,     1,     2,     2,     2,     1,     1,     1,     1,     3,
       0,     1,     3,     5,     1,     3,     3,     5,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= END)
    {
      yychar = END;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: interface  */
#line 1986 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
                   if (!classes) classes = NewHash();
		   Setattr((yyvsp[0].node),"classes",classes); 
		   Setattr((yyvsp[0].node),"name",ModuleName);
		   
		   if ((!module_node) && ModuleName) {
		     module_node = new_node("module");
		     Setattr(module_node,"name",ModuleName);
		   }
		   Setattr((yyvsp[0].node),"module",module_node);
	           top = (yyvsp[0].node);
               }
#line 5539 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 3: /* program: PARSETYPE parm END  */
#line 1998 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
                 top = Copy(Getattr((yyvsp[-1].p),"type"));
		 Delete((yyvsp[-1].p));
               }
#line 5548 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 4: /* program: PARSETYPE error  */
#line 2002 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
                 top = 0;
               }
#line 5556 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 5: /* program: PARSEPARM parm END  */
#line 2005 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
                 top = (yyvsp[-1].p);
               }
#line 5564 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 6: /* program: PARSEPARM error  */
#line 2008 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
                 top = 0;
               }
#line 5572 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 7: /* program: PARSEPARMS LPAREN parms RPAREN END  */
#line 2011 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
                 top = (yyvsp[-2].pl);
               }
#line 5580 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 8: /* program: PARSEPARMS error  */
#line 2014 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
                 top = 0;
               }
#line 5588 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 9: /* interface: interface declaration  */
#line 2019 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {  
                   /* add declaration to end of linked list (the declaration isn't always a single declaration, sometimes it is a linked list itself) */
                   if (currentDeclComment != NULL) {
		     set_comment((yyvsp[0].node), currentDeclComment);
		     currentDeclComment = NULL;
                   }                                      
                   appendChild((yyvsp[-1].node),(yyvsp[0].node));
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 5602 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 10: /* interface: interface DOXYGENSTRING  */
#line 2028 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		   Delete(currentDeclComment);
                   currentDeclComment = (yyvsp[0].str); 
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 5612 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 11: /* interface: interface DOXYGENPOSTSTRING  */
#line 2033 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 {
                   Node *node = lastChild((yyvsp[-1].node));
                   if (node) {
                     set_comment(node, (yyvsp[0].str));
		   } else {
		     Delete((yyvsp[0].str));
		   }
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 5626 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 12: /* interface: %empty  */
#line 2042 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
                   (yyval.node) = new_node("top");
               }
#line 5634 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 16: /* declaration: SEMI  */
#line 2050 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                      { (yyval.node) = 0; }
#line 5640 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 17: /* declaration: error  */
#line 2051 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
		  if (cparse_unknown_directive) {
		      Swig_error(cparse_file, cparse_line, "Unknown directive '%s'.\n", cparse_unknown_directive);
		  } else {
		      Swig_error(cparse_file, cparse_line, "Syntax error in input(1).\n");
		  }
		  Exit(EXIT_FAILURE);
               }
#line 5653 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 18: /* declaration: c_constructor_decl  */
#line 2060 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    { 
                  if ((yyval.node)) {
   		      add_symbols((yyval.node));
                  }
                  (yyval.node) = (yyvsp[0].node); 
	       }
#line 5664 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 19: /* declaration: error CONVERSIONOPERATOR  */
#line 2076 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
                  (yyval.node) = 0;
		  Delete((yyvsp[0].str));
                  skip_decl();
               }
#line 5674 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 39: /* $@1: %empty  */
#line 2112 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                             {
               Node *cls;
	       String *clsname;
	       extendmode = 1;
	       cplus_mode = CPLUS_PUBLIC;
	       if (!classes) classes = NewHash();
	       if (!classes_typedefs) classes_typedefs = NewHash();
	       clsname = make_class_name((yyvsp[-1].str));
	       cls = Getattr(classes,clsname);
	       if (!cls) {
	         cls = Getattr(classes_typedefs, clsname);
		 if (!cls) {
		   /* No previous definition. Create a new scope */
		   Node *am = Getattr(Swig_extend_hash(),clsname);
		   if (!am) {
		     Swig_symbol_newscope();
		     Swig_symbol_setscopename((yyvsp[-1].str));
		     prev_symtab = 0;
		   } else {
		     prev_symtab = Swig_symbol_setscope(Getattr(am,"symtab"));
		   }
		   current_class = 0;
		 } else {
		   /* Previous typedef class definition.  Use its symbol table.
		      Deprecated, just the real name should be used. 
		      Note that %extend before the class typedef never worked, only %extend after the class typedef. */
		   prev_symtab = Swig_symbol_setscope(Getattr(cls, "symtab"));
		   current_class = cls;
		   SWIG_WARN_NODE_BEGIN(cls);
		   Swig_warning(WARN_PARSE_EXTEND_NAME, cparse_file, cparse_line, "Deprecated %%extend name used - the %s name '%s' should be used instead of the typedef name '%s'.\n", Getattr(cls, "kind"), SwigType_namestr(Getattr(cls, "name")), (yyvsp[-1].str));
		   SWIG_WARN_NODE_END(cls);
		 }
	       } else {
		 /* Previous class definition.  Use its symbol table */
		 prev_symtab = Swig_symbol_setscope(Getattr(cls,"symtab"));
		 current_class = cls;
	       }
	       Classprefix = NewString((yyvsp[-1].str));
	       Namespaceprefix= Swig_symbol_qualifiedscopename(0);
	       Delete(clsname);
	     }
#line 5720 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 40: /* extend_directive: EXTEND options classkeyopt idcolon LBRACE $@1 cpp_members RBRACE  */
#line 2152 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
               String *clsname;
	       extendmode = 0;
               (yyval.node) = new_node("extend");
	       Setattr((yyval.node),"symtab",Swig_symbol_popscope());
	       if (prev_symtab) {
		 Swig_symbol_setscope(prev_symtab);
	       }
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
               clsname = make_class_name((yyvsp[-4].str));
	       Setattr((yyval.node),"name",clsname);

	       mark_nodes_as_extend((yyvsp[-1].node));
	       if (current_class) {
		 /* We add the extension to the previously defined class */
		 appendChild((yyval.node), (yyvsp[-1].node));
		 appendChild(current_class,(yyval.node));
	       } else {
		 /* We store the extensions in the extensions hash */
		 Node *am = Getattr(Swig_extend_hash(),clsname);
		 if (am) {
		   /* Append the members to the previous extend methods */
		   appendChild(am, (yyvsp[-1].node));
		 } else {
		   appendChild((yyval.node), (yyvsp[-1].node));
		   Setattr(Swig_extend_hash(),clsname,(yyval.node));
		 }
	       }
	       current_class = 0;
	       Delete(Classprefix);
	       Delete(clsname);
	       Classprefix = 0;
	       prev_symtab = 0;
	       (yyval.node) = 0;

	     }
#line 5761 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 41: /* apply_directive: APPLY typemap_parm LBRACE tm_list RBRACE  */
#line 2194 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                           {
                    (yyval.node) = new_node("apply");
                    Setattr((yyval.node),"pattern",Getattr((yyvsp[-3].p),"pattern"));
		    appendChild((yyval.node),(yyvsp[-1].p));
               }
#line 5771 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 42: /* clear_directive: CLEAR tm_list SEMI  */
#line 2204 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
		 (yyval.node) = new_node("clear");
		 appendChild((yyval.node),(yyvsp[-1].p));
               }
#line 5780 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 43: /* constant_directive: CONSTANT identifier EQUAL definetype SEMI  */
#line 2218 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                {
		 SwigType *type = NewSwigType((yyvsp[-1].dtype).type);
		 if (Len(type) > 0) {
		   (yyval.node) = new_node("constant");
		   Setattr((yyval.node), "name", (yyvsp[-3].id));
		   Setattr((yyval.node), "type", type);
		   Setattr((yyval.node), "value", (yyvsp[-1].dtype).val);
		   if ((yyvsp[-1].dtype).stringval) Setattr((yyval.node), "stringval", (yyvsp[-1].dtype).stringval);
		   if ((yyvsp[-1].dtype).numval) Setattr((yyval.node), "numval", (yyvsp[-1].dtype).numval);
		   Setattr((yyval.node), "storage", "%constant");
		   SetFlag((yyval.node), "feature:immutable");
		   add_symbols((yyval.node));
		   Delete(type);
		 } else {
		   Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE, cparse_file, cparse_line, "Unsupported constant value (ignored)\n");
		   (yyval.node) = 0;
		 }
	       }
#line 5803 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 44: /* constant_directive: CONSTANT type declarator def_args SEMI  */
#line 2236 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                        {
		 SwigType_push((yyvsp[-3].type), (yyvsp[-2].decl).type);
		 /* Sneaky callback function trick */
		 if (SwigType_isfunction((yyvsp[-3].type))) {
		   SwigType_add_pointer((yyvsp[-3].type));
		 }
		 (yyval.node) = new_node("constant");
		 Setattr((yyval.node), "name", (yyvsp[-2].decl).id);
		 Setattr((yyval.node), "type", (yyvsp[-3].type));
		 Setattr((yyval.node), "value", (yyvsp[-1].dtype).val);
		 if ((yyvsp[-1].dtype).stringval) Setattr((yyval.node), "stringval", (yyvsp[-1].dtype).stringval);
		 if ((yyvsp[-1].dtype).numval) Setattr((yyval.node), "numval", (yyvsp[-1].dtype).numval);
		 Setattr((yyval.node), "storage", "%constant");
		 SetFlag((yyval.node), "feature:immutable");
		 add_symbols((yyval.node));
               }
#line 5824 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 45: /* constant_directive: CONSTANT type direct_declarator LPAREN parms RPAREN cv_ref_qualifier def_args SEMI  */
#line 2254 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                    {
		 SwigType_add_function((yyvsp[-7].type), (yyvsp[-4].pl));
		 SwigType_push((yyvsp[-7].type), (yyvsp[-2].dtype).qualifier);
		 SwigType_push((yyvsp[-7].type), (yyvsp[-6].decl).type);
		 /* Sneaky callback function trick */
		 if (SwigType_isfunction((yyvsp[-7].type))) {
		   SwigType_add_pointer((yyvsp[-7].type));
		 }
		 (yyval.node) = new_node("constant");
		 Setattr((yyval.node), "name", (yyvsp[-6].decl).id);
		 Setattr((yyval.node), "type", (yyvsp[-7].type));
		 Setattr((yyval.node), "value", (yyvsp[-1].dtype).val);
		 if ((yyvsp[-1].dtype).stringval) Setattr((yyval.node), "stringval", (yyvsp[-1].dtype).stringval);
		 if ((yyvsp[-1].dtype).numval) Setattr((yyval.node), "numval", (yyvsp[-1].dtype).numval);
		 Setattr((yyval.node), "storage", "%constant");
		 SetFlag((yyval.node), "feature:immutable");
		 add_symbols((yyval.node));
	       }
#line 5847 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 46: /* constant_directive: CONSTANT error SEMI  */
#line 2272 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
		 Swig_warning(WARN_PARSE_BAD_VALUE,cparse_file,cparse_line,"Bad constant value (ignored).\n");
		 (yyval.node) = 0;
	       }
#line 5856 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 47: /* constant_directive: CONSTANT error END  */
#line 2276 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		 Swig_error(cparse_file,cparse_line,"Missing semicolon (';') after %%constant.\n");
		 Exit(EXIT_FAILURE);
	       }
#line 5865 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 48: /* echo_directive: ECHO HBLOCK  */
#line 2287 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		 char temp[64];
		 Replace((yyvsp[0].str),"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace((yyvsp[0].str),"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", (yyvsp[0].str));
		 Delete((yyvsp[0].str));
                 (yyval.node) = 0;
	       }
#line 5879 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 49: /* echo_directive: ECHO string  */
#line 2296 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		 char temp[64];
		 String *s = (yyvsp[0].str);
		 Replace(s,"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace(s,"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", s);
		 Delete(s);
                 (yyval.node) = 0;
               }
#line 5894 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 50: /* stringtype: string LBRACE parm RBRACE  */
#line 2309 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {		 
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[-3].str));
		 Setattr((yyval.node),"type",Getattr((yyvsp[-1].p),"type"));
               }
#line 5904 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 51: /* fname: string  */
#line 2316 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[0].str));
              }
#line 5913 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 53: /* fragment_directive: FRAGMENT LPAREN fname COMMA kwargs RPAREN HBLOCK  */
#line 2331 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                     {
                   Hash *p = (yyvsp[-2].node);
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-4].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-4].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Setattr((yyval.node),"code",(yyvsp[0].str));
		   Delete((yyvsp[0].str));
                 }
#line 5928 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 54: /* fragment_directive: FRAGMENT LPAREN fname COMMA kwargs RPAREN LBRACE  */
#line 2341 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
		   Hash *p = (yyvsp[-2].node);
		   String *code;
		   if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-4].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-4].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code",code);
		   Delete(code);
                 }
#line 5948 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 55: /* fragment_directive: FRAGMENT LPAREN fname RPAREN SEMI  */
#line 2356 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                     {
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-2].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-2].node),"type"));
		   Setattr((yyval.node),"emitonly","1");
		 }
#line 5959 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 56: /* @2: %empty  */
#line 2369 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                             {
		     (yyval.loc).filename = Copy(cparse_file);
		     (yyval.loc).line = cparse_line;
		     scanner_set_location((yyvsp[-1].str),1);
                     if ((yyvsp[-2].node)) { 
		       String *maininput = Getattr((yyvsp[-2].node), "maininput");
		       if (maininput)
		         scanner_set_main_input_file(NewString(maininput));
		     }
               }
#line 5974 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 57: /* include_directive: includetype options string BEGINFILE @2 interface ENDOFFILE  */
#line 2378 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
                     String *mname = 0;
                     (yyval.node) = (yyvsp[-1].node);
		     scanner_set_location((yyvsp[-2].loc).filename, (yyvsp[-2].loc).line + 1);
		     Delete((yyvsp[-2].loc).filename);
		     switch ((yyvsp[-6].includetype)) {
		       case INCLUDE_INCLUDE:
			 set_nodeType((yyval.node), "include");
			 break;
		       case INCLUDE_IMPORT:
			 mname = (yyvsp[-5].node) ? Getattr((yyvsp[-5].node), "module") : 0;
			 set_nodeType((yyval.node), "import");
			 if (import_mode) --import_mode;
			 break;
		     }
		     
		     Setattr((yyval.node),"name",(yyvsp[-4].str));
		     /* Search for the module (if any) */
		     {
			 Node *n = firstChild((yyval.node));
			 while (n) {
			     if (Strcmp(nodeType(n),"module") == 0) {
			         if (mname) {
				   Setattr(n,"name", mname);
				   mname = 0;
				 }
				 Setattr((yyval.node),"module",Getattr(n,"name"));
				 break;
			     }
			     n = nextSibling(n);
			 }
			 if (mname) {
			   /* There is no module node in the import
			      node, ie, you imported a .h file
			      directly.  We are forced then to create
			      a new import node with a module node.
			   */			      
			   Node *nint = new_node("import");
			   Node *mnode = new_node("module");
			   Setattr(mnode,"name", mname);
                           Setattr(mnode,"options",(yyvsp[-5].node));
			   appendChild(nint,mnode);
			   Delete(mnode);
			   appendChild(nint,firstChild((yyval.node)));
			   (yyval.node) = nint;
			   Setattr((yyval.node),"module",mname);
			 }
		     }
		     Setattr((yyval.node),"options",(yyvsp[-5].node));
               }
#line 6029 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 58: /* includetype: INCLUDE  */
#line 2430 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.includetype) = INCLUDE_INCLUDE; }
#line 6035 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 59: /* includetype: IMPORT  */
#line 2431 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.includetype) = INCLUDE_IMPORT; ++import_mode;}
#line 6041 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 60: /* inline_directive: INLINE HBLOCK  */
#line 2438 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
                 String *cpps;
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   (yyval.node) = 0;
		 } else {
		   (yyval.node) = new_node("insert");
		   Setattr((yyval.node),"code",(yyvsp[0].str));
		   /* Need to run through the preprocessor */
		   Seek((yyvsp[0].str),0,SEEK_SET);
		   Setline((yyvsp[0].str),cparse_start_line);
		   Setfile((yyvsp[0].str),cparse_file);
		   cpps = Preprocessor_parse((yyvsp[0].str));
		   scanner_start_inline(cpps, cparse_start_line);
		   Delete(cpps);
		 }
		 Delete((yyvsp[0].str));
	       }
#line 6064 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 61: /* inline_directive: INLINE LBRACE  */
#line 2456 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
                 String *cpps;
		 int start_line = cparse_line;
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		 }
		 if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		 if (Namespaceprefix) {
		   (yyval.node) = 0;
		 } else {
		   String *code;
                   (yyval.node) = new_node("insert");
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code", code);
		   Delete(code);		   
		   cpps=Copy(scanner_ccode);
		   scanner_start_inline(cpps, start_line);
		   Delete(cpps);
		 }
               }
#line 6091 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 62: /* insert_directive: HBLOCK  */
#line 2490 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          {
                 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"code",(yyvsp[0].str));
		 Delete((yyvsp[0].str));
	       }
#line 6101 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 63: /* insert_directive: INSERT LPAREN idstring RPAREN string  */
#line 2495 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
		 String *code = NewStringEmpty();
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Setattr((yyval.node),"code",code);
		 if (Swig_insert_file((yyvsp[0].str),code) < 0) {
		   Swig_error(cparse_file, cparse_line, "Couldn't find '%s'.\n", (yyvsp[0].str));
		   (yyval.node) = 0;
		 } 
               }
#line 6116 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 64: /* insert_directive: INSERT LPAREN idstring RPAREN HBLOCK  */
#line 2505 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Setattr((yyval.node),"code",(yyvsp[0].str));
		 Delete((yyvsp[0].str));
               }
#line 6127 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 65: /* insert_directive: INSERT LPAREN idstring RPAREN LBRACE  */
#line 2511 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
		 String *code;
		 if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Delitem(scanner_ccode,0);
		 Delitem(scanner_ccode,DOH_END);
		 code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code", code);
		 Delete(code);
	       }
#line 6143 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 66: /* module_directive: MODULE options idstring  */
#line 2529 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
                 (yyval.node) = new_node("module");
		 if ((yyvsp[-1].node)) {
		   Setattr((yyval.node),"options",(yyvsp[-1].node));
		   if (Getattr((yyvsp[-1].node),"directors")) {
		     Wrapper_director_mode_set(1);
		     if (!cparse_cplusplus) {
		       Swig_error(cparse_file, cparse_line, "Directors are not supported for C code and require the -c++ option\n");
		     }
		   } 
		   if (Getattr((yyvsp[-1].node),"dirprot")) {
		     Wrapper_director_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[-1].node),"allprotected")) {
		     Wrapper_all_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[-1].node),"templatereduce")) {
		     template_reduce = 1;
		   }
		   if (Getattr((yyvsp[-1].node),"notemplatereduce")) {
		     template_reduce = 0;
		   }
		 }
		 if (!ModuleName) ModuleName = NewString((yyvsp[0].id));
		 if (!import_mode) {
		   /* first module included, we apply global
		      ModuleName, which can be modify by -module */
		   String *mname = Copy(ModuleName);
		   Setattr((yyval.node),"name",mname);
		   Delete(mname);
		 } else { 
		   /* import mode, we just pass the idstring */
		   Setattr((yyval.node),"name",(yyvsp[0].id));   
		 }		 
		 if (!module_node) module_node = (yyval.node);
	       }
#line 6184 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 67: /* native_directive: NATIVE LPAREN identifier RPAREN storage_class identifier SEMI  */
#line 2572 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                  {
                 (yyval.node) = new_node("native");
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 Setattr((yyval.node),"wrap:name",(yyvsp[-1].id));
		 Delete((yyvsp[-2].str));
	         add_symbols((yyval.node));
	       }
#line 6196 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 68: /* native_directive: NATIVE LPAREN identifier RPAREN storage_class type declarator SEMI  */
#line 2579 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                    {
		 if (!SwigType_isfunction((yyvsp[-1].decl).type)) {
		   Swig_error(cparse_file,cparse_line,"%%native declaration '%s' is not a function.\n", (yyvsp[-1].decl).id);
		   (yyval.node) = 0;
		 } else {
		     Delete(SwigType_pop_function((yyvsp[-1].decl).type));
		     /* Need check for function here */
		     SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		     (yyval.node) = new_node("native");
	             Setattr((yyval.node),"name",(yyvsp[-5].id));
		     Setattr((yyval.node),"wrap:name",(yyvsp[-1].decl).id);
		     Setattr((yyval.node),"type",(yyvsp[-2].type));
		     Setattr((yyval.node),"parms",(yyvsp[-1].decl).parms);
		     Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		 }
		 Delete((yyvsp[-3].str));
	         add_symbols((yyval.node));
	       }
#line 6219 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 69: /* pragma_directive: PRAGMA pragma_lang identifier EQUAL pragma_arg  */
#line 2606 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                  {
                 (yyval.node) = new_node("pragma");
		 Setattr((yyval.node),"lang",(yyvsp[-3].id));
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
		 Setattr((yyval.node),"value",(yyvsp[0].str));
	       }
#line 6230 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 70: /* pragma_directive: PRAGMA pragma_lang identifier  */
#line 2612 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
		(yyval.node) = new_node("pragma");
		Setattr((yyval.node),"lang",(yyvsp[-1].id));
		Setattr((yyval.node),"name",(yyvsp[0].id));
	      }
#line 6240 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 73: /* pragma_lang: LPAREN identifier RPAREN  */
#line 2623 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         { (yyval.id) = (yyvsp[-1].id); }
#line 6246 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 74: /* pragma_lang: %empty  */
#line 2624 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       { (yyval.id) = "swig"; }
#line 6252 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 75: /* rename_directive: rename_namewarn declarator idstring SEMI  */
#line 2631 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
                SwigType *t = (yyvsp[-2].decl).type;
		Hash *kws = NewHash();
		String *fixname;
		fixname = feature_identifier_fix((yyvsp[-2].decl).id);
		Setattr(kws,"name",(yyvsp[-1].id));
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[-3].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[-3].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[-3].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[-2].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[-3].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[-2].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
#line 6303 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 76: /* rename_directive: rename_namewarn LPAREN kwargs RPAREN declarator cpp_const SEMI  */
#line 2677 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                               {
		String *fixname;
		Hash *kws = (yyvsp[-4].node);
		SwigType *t = (yyvsp[-2].decl).type;
		fixname = feature_identifier_fix((yyvsp[-2].decl).id);
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if ((yyvsp[-1].dtype).qualifier) SwigType_push(t,(yyvsp[-1].dtype).qualifier);
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[-6].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[-6].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[-6].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[-2].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[-6].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[-2].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
#line 6354 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 77: /* rename_directive: rename_namewarn LPAREN kwargs RPAREN string SEMI  */
#line 2723 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 {
		if ((yyvsp[-5].intvalue)) {
		  Swig_name_rename_add(Namespaceprefix,(yyvsp[-1].str),0,(yyvsp[-3].node),0);
		} else {
		  Swig_name_namewarn_add(Namespaceprefix,(yyvsp[-1].str),0,(yyvsp[-3].node));
		}
		(yyval.node) = 0;
		scanner_clear_rename();
              }
#line 6368 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 78: /* rename_namewarn: RENAME  */
#line 2734 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		    (yyval.intvalue) = 1;
                }
#line 6376 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 79: /* rename_namewarn: NAMEWARN  */
#line 2737 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
                    (yyval.intvalue) = 0;
                }
#line 6384 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 80: /* feature_directive: FEATURE LPAREN idstring featattr RPAREN declarator cpp_const stringbracesemi  */
#line 2764 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                 {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-5].id), val, (yyvsp[-4].node), (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6395 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 81: /* feature_directive: FEATURE LPAREN idstring COMMA stringnum featattr RPAREN declarator cpp_const SEMI  */
#line 2770 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                      {
                    String *val = Len((yyvsp[-5].str)) ? (yyvsp[-5].str) : 0;
                    new_feature((yyvsp[-7].id), val, (yyvsp[-4].node), (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6406 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 82: /* feature_directive: FEATURE LPAREN idstring featattr RPAREN stringbracesemi  */
#line 2778 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                            {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-3].id), val, (yyvsp[-2].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6417 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 83: /* feature_directive: FEATURE LPAREN idstring COMMA stringnum featattr RPAREN SEMI  */
#line 2784 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                 {
                    String *val = Len((yyvsp[-3].str)) ? (yyvsp[-3].str) : 0;
                    new_feature((yyvsp[-5].id), val, (yyvsp[-2].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6428 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 85: /* stringbracesemi: SEMI  */
#line 2793 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       { (yyval.str) = 0; }
#line 6434 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 86: /* stringbracesemi: PARMS LPAREN parms RPAREN SEMI  */
#line 2794 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 { (yyval.str) = (yyvsp[-2].pl); }
#line 6440 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 87: /* featattr: COMMA idstring EQUAL stringnum featattr  */
#line 2797 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                              {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[-3].id));
		  Setattr((yyval.node),"value",(yyvsp[-1].str));
		  if ((yyvsp[0].node)) set_nextSibling((yyval.node), (yyvsp[0].node));
		}
#line 6451 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 88: /* featattr: %empty  */
#line 2803 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		  (yyval.node) = 0;
		}
#line 6459 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 89: /* varargs_directive: VARARGS LPAREN varargs_parms RPAREN declarator cpp_const SEMI  */
#line 2810 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                  {
                 Parm *val;
		 String *name;
		 SwigType *t;
		 if (Namespaceprefix) name = NewStringf("%s::%s", Namespaceprefix, (yyvsp[-2].decl).id);
		 else name = NewString((yyvsp[-2].decl).id);
		 val = (yyvsp[-4].pl);
		 if ((yyvsp[-2].decl).parms) {
		   Setmeta(val,"parms",(yyvsp[-2].decl).parms);
		 }
		 t = (yyvsp[-2].decl).type;
		 if (!Len(t)) t = 0;
		 if (t) {
		   if ((yyvsp[-1].dtype).qualifier) SwigType_push(t,(yyvsp[-1].dtype).qualifier);
		   if (SwigType_isfunction(t)) {
		     SwigType *decl = SwigType_pop_function(t);
		     if (SwigType_ispointer(t)) {
		       String *nname = NewStringf("*%s",name);
		       Swig_feature_set(Swig_cparse_features(), nname, decl, "feature:varargs", val, 0);
		       Delete(nname);
		     } else {
		       Swig_feature_set(Swig_cparse_features(), name, decl, "feature:varargs", val, 0);
		     }
		     Delete(decl);
		   } else if (SwigType_ispointer(t)) {
		     String *nname = NewStringf("*%s",name);
		     Swig_feature_set(Swig_cparse_features(),nname,0,"feature:varargs",val, 0);
		     Delete(nname);
		   }
		 } else {
		   Swig_feature_set(Swig_cparse_features(),name,0,"feature:varargs",val, 0);
		 }
		 Delete(name);
		 (yyval.node) = 0;
              }
#line 6499 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 91: /* varargs_parms: NUM_INT COMMA parm  */
#line 2847 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     { 
		  int i;
		  int n;
		  Parm *p;
		  n = atoi(Char((yyvsp[-2].dtype).val));
		  if (n <= 0) {
		    Swig_error(cparse_file, cparse_line,"Argument count in %%varargs must be positive.\n");
		    (yyval.pl) = 0;
		  } else {
		    String *name = Getattr((yyvsp[0].p), "name");
		    (yyval.pl) = Copy((yyvsp[0].p));
		    if (name)
		      Setattr((yyval.pl), "name", NewStringf("%s%d", name, n));
		    for (i = 1; i < n; i++) {
		      p = Copy((yyvsp[0].p));
		      name = Getattr(p, "name");
		      if (name)
		        Setattr(p, "name", NewStringf("%s%d", name, n-i));
		      set_nextSibling(p,(yyval.pl));
		      Delete((yyval.pl));
		      (yyval.pl) = p;
		    }
		  }
                }
#line 6528 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 92: /* typemap_directive: TYPEMAP LPAREN typemap_type RPAREN tm_list stringbrace  */
#line 2881 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                            {
		   (yyval.node) = 0;
		   if ((yyvsp[-3].tmap).method) {
		     String *code = 0;
		     (yyval.node) = new_node("typemap");
		     Setattr((yyval.node),"method",(yyvsp[-3].tmap).method);
		     if ((yyvsp[-3].tmap).kwargs) {
		       ParmList *kw = (yyvsp[-3].tmap).kwargs;
                       code = remove_block(kw, (yyvsp[0].str));
		       Setattr((yyval.node),"kwargs", (yyvsp[-3].tmap).kwargs);
		     }
		     code = code ? code : NewString((yyvsp[0].str));
		     Setattr((yyval.node),"code", code);
		     Delete(code);
		     appendChild((yyval.node),(yyvsp[-1].p));
		     Delete((yyvsp[-3].tmap).kwargs);
		     Delete((yyvsp[-3].tmap).method);
		   }
	       }
#line 6552 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 93: /* typemap_directive: TYPEMAP LPAREN typemap_type RPAREN tm_list SEMI  */
#line 2900 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 {
		 (yyval.node) = 0;
		 if ((yyvsp[-3].tmap).method) {
		   (yyval.node) = new_node("typemap");
		   Setattr((yyval.node),"method",(yyvsp[-3].tmap).method);
		   appendChild((yyval.node),(yyvsp[-1].p));
		   Delete((yyvsp[-3].tmap).method);
		 }
		 Delete((yyvsp[-3].tmap).kwargs);
	       }
#line 6567 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 94: /* typemap_directive: TYPEMAP LPAREN typemap_type RPAREN tm_list EQUAL typemap_parm SEMI  */
#line 2910 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                    {
		   (yyval.node) = 0;
		   if ((yyvsp[-5].tmap).method) {
		     (yyval.node) = new_node("typemapcopy");
		     Setattr((yyval.node),"method",(yyvsp[-5].tmap).method);
		     Setattr((yyval.node),"pattern", Getattr((yyvsp[-1].p),"pattern"));
		     appendChild((yyval.node),(yyvsp[-3].p));
		     Delete((yyvsp[-5].tmap).method);
		   }
		   Delete((yyvsp[-5].tmap).kwargs);
	       }
#line 6583 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 95: /* typemap_type: kwargs  */
#line 2925 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		 String *name = Getattr((yyvsp[0].node), "name");
		 Hash *p = nextSibling((yyvsp[0].node));
		 (yyval.tmap).method = name;
		 (yyval.tmap).kwargs = p;
		 if (Getattr((yyvsp[0].node), "value")) {
		   Swig_error(cparse_file, cparse_line,
			      "%%typemap method shouldn't have a value specified.\n");
		 }
		 while (p) {
		   if (!Getattr(p, "value")) {
		     Swig_error(cparse_file, cparse_line,
				"%%typemap attribute '%s' is missing its value.  If this is specifying the target language, that's no longer supported: use #ifdef SWIG<LANG> instead.\n",
				Getattr(p, "name"));
		     /* Set to empty value to avoid segfaults later. */
		     Setattr(p, "value", NewStringEmpty());
		   }
		   p = nextSibling(p);
		 }
                }
#line 6608 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 96: /* tm_list: tm_list_builder  */
#line 2947 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
		 (yyval.p) = (yyvsp[0].pbuilder).parms;
	       }
#line 6616 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 97: /* tm_list_builder: typemap_parm  */
#line 2952 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
                 (yyval.pbuilder).parms = (yyval.pbuilder).last = (yyvsp[0].p);
	       }
#line 6624 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 98: /* tm_list_builder: tm_list_builder COMMA typemap_parm  */
#line 2955 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                        {
		 // Build a linked list in the order specified, but avoiding
		 // a right recursion rule because "Right recursion uses up
		 // space on the Bison stack in proportion to the number of
		 // elements in the sequence".
		 set_nextSibling((yyvsp[-2].pbuilder).last, (yyvsp[0].p));
		 (yyval.pbuilder).parms = (yyvsp[-2].pbuilder).parms;
		 (yyval.pbuilder).last = (yyvsp[0].p);
	       }
#line 6638 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 99: /* typemap_parm: type plain_declarator  */
#line 2966 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
                  Parm *parm;
		  SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		  (yyval.p) = new_node("typemapitem");
		  parm = NewParmWithoutFileLineInfo((yyvsp[-1].type),(yyvsp[0].decl).id);
		  Setattr((yyval.p),"pattern",parm);
		  Setattr((yyval.p),"parms", (yyvsp[0].decl).parms);
		  Delete(parm);
		  /*		  $$ = NewParmWithoutFileLineInfo($type,$plain_declarator.id);
				  Setattr($$,"parms",$plain_declarator.parms); */
                }
#line 6654 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 100: /* typemap_parm: LPAREN parms RPAREN  */
#line 2977 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
                  (yyval.p) = new_node("typemapitem");
		  Setattr((yyval.p),"pattern",(yyvsp[-1].pl));
		  /*		  Setattr($$,"multitype",$parms); */
               }
#line 6664 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 101: /* typemap_parm: LPAREN parms RPAREN LPAREN parms RPAREN  */
#line 2982 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                      {
		 (yyval.p) = new_node("typemapitem");
		 Setattr((yyval.p),"pattern", (yyvsp[-4].pl));
		 /*                 Setattr($$,"multitype",$in); */
		 Setattr((yyval.p),"parms",(yyvsp[-1].pl));
               }
#line 6675 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 102: /* types_directive: TYPES LPAREN parms RPAREN stringbracesemi  */
#line 2995 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
                   (yyval.node) = new_node("types");
		   Setattr((yyval.node),"parms",(yyvsp[-2].pl));
                   if ((yyvsp[0].str))
		     Setattr((yyval.node),"convcode",NewString((yyvsp[0].str)));
               }
#line 6686 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 103: /* template_directive: SWIGTEMPLATE LPAREN idstringopt RPAREN idcolonnt LESSTHAN valparms GREATERTHAN SEMI  */
#line 3007 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                        {
                  Parm *p;
		  Node *n = 0;
		  Node *outer_class = currentOuterClass;
		  Symtab *tscope = 0;
		  String *symname = (yyvsp[-6].id) ? NewString((yyvsp[-6].id)) : 0;
		  int errored_flag = 0;
		  String *idcolonnt;

		  (yyval.node) = 0;

		  tscope = Swig_symbol_current();          /* Get the current scope */

		  /* If the class name is qualified, we need to create or lookup namespace entries */
		  idcolonnt = resolve_create_node_scope((yyvsp[-4].str), 0, &errored_flag);

		  if (!errored_flag) {
		    if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0)
		      outer_class = nscope_inner;

		    /*
		      We use the new namespace entry 'nscope' only to
		      emit the template node. The template parameters are
		      resolved in the current 'tscope'.

		      This is closer to the C++ (typedef) behavior.
		    */
		    n = Swig_cparse_template_locate(idcolonnt, (yyvsp[-2].p), symname, tscope);
		  }

		  /* Patch the argument types to respect namespaces */
		  p = (yyvsp[-2].p);
		  while (p) {
		    SwigType *value = Getattr(p,"value");
		    if (!value) {
		      SwigType *ty = Getattr(p,"type");
		      if (ty) {
			SwigType *rty = 0;
			int reduce = template_reduce;
			if (reduce || !SwigType_ispointer(ty)) {
			  rty = Swig_symbol_typedef_reduce(ty,tscope);
			  if (!reduce) reduce = SwigType_ispointer(rty);
			}
			ty = reduce ? Swig_symbol_type_qualify(rty,tscope) : Swig_symbol_type_qualify(ty,tscope);
			Setattr(p,"type",ty);
			Delete(ty);
			Delete(rty);
		      }
		    } else {
		      value = Swig_symbol_type_qualify(value,tscope);
		      Setattr(p,"value",value);
		      Delete(value);
		    }

		    p = nextSibling(p);
		  }

		  /* Look for the template */
		  {
                    Node *nn = n;
                    Node *linklistend = 0;
                    Node *linkliststart = 0;
                    while (nn) {
                      Node *templnode = 0;
                      if (GetFlag(nn, "instantiate")) {
			Delattr(nn, "instantiate");
			{
			  int nnisclass = (Strcmp(Getattr(nn, "templatetype"), "class") == 0); /* class template not a classforward nor function template */
			  Parm *tparms = Getattr(nn, "templateparms");
			  int specialized = !tparms; /* fully specialized (an explicit specialization) */
			  String *tname = Copy(idcolonnt);
			  Node *primary_template = Swig_symbol_clookup(tname, 0);

			  /* Expand the template */
			  ParmList *temparms = Swig_cparse_template_parms_expand((yyvsp[-2].p), primary_template, nn);

                          templnode = copy_node(nn);
			  update_nested_classes(templnode); /* update classes nested within template */
                          /* We need to set the node name based on name used to instantiate */
                          Setattr(templnode,"name",tname);
			  Delete(tname);
                          if (!specialized) {
                            Delattr(templnode,"sym:typename");
                          } else {
                            Setattr(templnode,"sym:typename","1");
                          }
			  /* for now, nested %template is allowed only in the same scope as the template declaration */
                          if (symname && !(nnisclass && ((outer_class && (outer_class != Getattr(nn, "nested:outer")))
			    ||(extendmode && current_class && (current_class != Getattr(nn, "nested:outer")))))) {
			    /*
			       Comment this out for 1.3.28. We need to
			       re-enable it later but first we need to
			       move %ignore from using %rename to use
			       %feature(ignore).

			       String *symname = Swig_name_make(templnode, 0, symname, 0, 0);
			    */
                            Swig_cparse_template_expand(templnode, symname, temparms, tscope);
                            Setattr(templnode, "sym:name", symname);
                          } else {
                            static int cnt = 0;
                            String *nname = NewStringf("__dummy_%d__", cnt++);
                            Swig_cparse_template_expand(templnode,nname,temparms,tscope);
                            Setattr(templnode,"sym:name",nname);
                            SetFlag(templnode,"hidden");
			    Delete(nname);
                            Setattr(templnode,"feature:onlychildren", "typemap,typemapitem,typemapcopy,typedef,types,fragment,apply");
			    if (symname) {
			      Swig_warning(WARN_PARSE_NESTED_TEMPLATE, cparse_file, cparse_line, "Named nested template instantiations not supported. Processing as if no name was given to %%template().\n");
			    }
                          }
                          Delattr(templnode,"templatetype");
                          Setattr(templnode,"template",nn);
                          Setfile(templnode,cparse_file);
                          Setline(templnode,cparse_line);
                          Delete(temparms);
			  if (outer_class && nnisclass) {
			    SetFlag(templnode, "nested");
			    Setattr(templnode, "nested:outer", outer_class);
			  }
                          add_symbols_copy(templnode);

			  if (Equal(nodeType(templnode), "classforward") && !(GetFlag(templnode, "feature:ignore") || GetFlag(templnode, "hidden"))) {
			    SWIG_WARN_NODE_BEGIN(templnode);
			    /* A full template class definition is required in order to wrap a template class as a proxy class so this %template is ineffective. */
			    if (GetFlag(templnode, "nested:forward"))
			      Swig_warning(WARN_PARSE_TEMPLATE_NESTED, cparse_file, cparse_line, "Unsupported template nested class '%s' cannot be used to instantiate a full template class with name '%s'.\n", Swig_name_decl(templnode), Getattr(templnode, "sym:name"));
			    else
			      Swig_warning(WARN_PARSE_TEMPLATE_FORWARD, cparse_file, cparse_line, "Template forward class '%s' cannot be used to instantiate a full template class with name '%s'.\n", Swig_name_decl(templnode), Getattr(templnode, "sym:name"));
			    SWIG_WARN_NODE_END(templnode);
			  }

                          if (Strcmp(nodeType(templnode),"class") == 0) {

                            /* Identify pure abstract methods */
                            Setattr(templnode,"abstracts", pure_abstracts(firstChild(templnode)));

                            /* Set up inheritance in symbol table */
                            {
                              Symtab  *csyms;
                              List *baselist = Getattr(templnode,"baselist");
                              csyms = Swig_symbol_current();
                              Swig_symbol_setscope(Getattr(templnode,"symtab"));
                              if (baselist) {
                                List *bases = Swig_make_inherit_list(Getattr(templnode,"name"),baselist, Namespaceprefix);
                                if (bases) {
                                  Iterator s;
                                  for (s = First(bases); s.item; s = Next(s)) {
                                    Symtab *st = Getattr(s.item,"symtab");
                                    if (st) {
				      Setfile(st,Getfile(s.item));
				      Setline(st,Getline(s.item));
                                      Swig_symbol_inherit(st);
                                    }
                                  }
				  Delete(bases);
                                }
                              }
                              Swig_symbol_setscope(csyms);
                            }

                            /* Merge in %extend methods for this class.
			       This only merges methods within %extend for a template specialized class such as
			         template<typename T> class K {}; %extend K<int> { ... }
			       The copy_node() call above has already added in the generic %extend methods such as
			         template<typename T> class K {}; %extend K { ... } */

			    /* !!! This may be broken.  We may have to add the
			       %extend methods at the beginning of the class */
                            {
                              String *stmp = 0;
                              String *clsname;
                              Node *am;
                              if (Namespaceprefix) {
                                clsname = stmp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
                              } else {
                                clsname = Getattr(templnode,"name");
                              }
                              am = Getattr(Swig_extend_hash(),clsname);
                              if (am) {
                                Symtab *st = Swig_symbol_current();
                                Swig_symbol_setscope(Getattr(templnode,"symtab"));
                                /*			    Printf(stdout,"%s: %s %p %p\n", Getattr(templnode,"name"), clsname, Swig_symbol_current(), Getattr(templnode,"symtab")); */
                                Swig_extend_merge(templnode,am);
                                Swig_symbol_setscope(st);
				Swig_extend_append_previous(templnode,am);
                                Delattr(Swig_extend_hash(),clsname);
                              }
			      if (stmp) Delete(stmp);
                            }

                            /* Add to classes hash */
			    if (!classes)
			      classes = NewHash();

			    if (Namespaceprefix) {
			      String *temp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
			      Setattr(classes,temp,templnode);
			      Delete(temp);
			    } else {
			      String *qs = Swig_symbol_qualifiedscopename(templnode);
			      Setattr(classes, qs,templnode);
			      Delete(qs);
			    }
                          }
                        }

                        /* all the overloaded function templates are added into a linked list */
                        if (!linkliststart)
                          linkliststart = templnode;
                        if (nscope_inner) {
                          /* non-global namespace */
                          if (templnode) {
                            appendChild(nscope_inner,templnode);
			    Delete(templnode);
                            if (nscope) (yyval.node) = nscope;
                          }
                        } else {
                          /* global namespace */
                          if (!linklistend) {
                            (yyval.node) = templnode;
                          } else {
                            set_nextSibling(linklistend,templnode);
			    Delete(templnode);
                          }
                          linklistend = templnode;
                        }
                      }
                      nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded function templates. If a class template there will never be a sibling. */
                    }
                    update_defaultargs(linkliststart);
                    update_abstracts(linkliststart);
		  }
	          Swig_symbol_setscope(tscope);
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  Delete(symname);
                }
#line 6929 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 104: /* warn_directive: WARN string  */
#line 3251 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		  Swig_warning(0,cparse_file, cparse_line,"%s\n", (yyvsp[0].str));
		  (yyval.node) = 0;
               }
#line 6938 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 105: /* c_declaration: c_decl  */
#line 3261 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
                    (yyval.node) = (yyvsp[0].node); 
                    if ((yyval.node)) {
   		      add_symbols((yyval.node));
                      default_arguments((yyval.node));
   	            }
                }
#line 6950 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 108: /* $@3: %empty  */
#line 3273 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 {
		  if (Strcmp((yyvsp[-1].str),"C") == 0) {
		    cparse_externc = 1;
		  }
		}
#line 6960 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 109: /* c_declaration: attribute EXTERN string LBRACE $@3 interface RBRACE  */
#line 3277 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   {
		  cparse_externc = 0;
		  if (Strcmp((yyvsp[-4].str),"C") == 0) {
		    Node *n = firstChild((yyvsp[-1].node));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[-4].str));
		    appendChild((yyval.node),n);
		    while (n) {
		      String *s = Getattr(n, "storage");
		      if (s) {
			if (Strstr(s, "thread_local")) {
			  Insert(s,0,"externc ");
			} else if (!Equal(s, "typedef")) {
			  Setattr(n,"storage","externc");
			}
		      } else {
			Setattr(n,"storage","externc");
		      }
		      n = nextSibling(n);
		    }
		  } else {
		    if (!Equal((yyvsp[-4].str),"C++")) {
		      Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[-4].str));
		    }
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[-4].str));
		    appendChild((yyval.node),firstChild((yyvsp[-1].node)));
		  }
                }
#line 6994 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 110: /* c_declaration: cpp_lambda_decl  */
#line 3306 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
		  (yyval.node) = (yyvsp[0].node);
		  SWIG_WARN_NODE_BEGIN((yyval.node));
		  Swig_warning(WARN_CPP11_LAMBDA, cparse_file, cparse_line, "Lambda expressions and closures are not fully supported yet.\n");
		  SWIG_WARN_NODE_END((yyval.node));
		}
#line 7005 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 111: /* c_declaration: USING idcolon EQUAL type plain_declarator SEMI  */
#line 3312 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 {
		  /* Convert using statement to a typedef statement */
		  (yyval.node) = new_node("cdecl");
		  Setattr((yyval.node),"type",(yyvsp[-2].type));
		  Setattr((yyval.node),"storage","typedef");
		  Setattr((yyval.node),"name",(yyvsp[-4].str));
		  Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		  SetFlag((yyval.node),"typealias");
		  add_symbols((yyval.node));
		}
#line 7020 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 112: /* c_declaration: TEMPLATE LESSTHAN template_parms GREATERTHAN USING idcolon EQUAL type plain_declarator SEMI  */
#line 3322 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                              {
		  /* Convert alias template to a "template" typedef statement */
		  (yyval.node) = new_node("template");
		  Setattr((yyval.node),"type",(yyvsp[-2].type));
		  Setattr((yyval.node),"storage","typedef");
		  Setattr((yyval.node),"name",(yyvsp[-4].str));
		  Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		  Setattr((yyval.node),"templateparms",(yyvsp[-7].tparms));
		  Setattr((yyval.node),"templatetype","cdecl");
		  SetFlag((yyval.node),"aliastemplate");
		  add_symbols((yyval.node));
		}
#line 7037 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 114: /* c_decl: attribute storage_class type declarator cpp_const attribute initializer c_decl_tail  */
#line 3341 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                    {
	      String *decl = (yyvsp[-4].decl).type;
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-3].dtype).qualifier)
	        decl = add_qualifier_to_declarator((yyvsp[-4].decl).type, (yyvsp[-3].dtype).qualifier);
	      Setattr((yyval.node),"refqualifier",(yyvsp[-3].dtype).refqualifier);
	      Setattr((yyval.node),"type",(yyvsp[-5].type));
	      Setattr((yyval.node),"storage",(yyvsp[-6].str));
	      Setattr((yyval.node),"name",(yyvsp[-4].decl).id);
	      Setattr((yyval.node),"decl",decl);
	      Setattr((yyval.node),"parms",(yyvsp[-4].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
	      if ((yyvsp[-1].dtype).stringval) Setattr((yyval.node), "stringval", (yyvsp[-1].dtype).stringval);
	      if ((yyvsp[-1].dtype).numval) Setattr((yyval.node), "numval", (yyvsp[-1].dtype).numval);
	      Setattr((yyval.node),"throws",(yyvsp[-3].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[-3].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[-3].dtype).nexcept);
	      Setattr((yyval.node),"final",(yyvsp[-3].dtype).final);
              if ((yyvsp[-7].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-7].node));
              }
              if ((yyvsp[-2].node)) {
                Setattr((yyvsp[-5].type), "attribute", (yyvsp[-2].node));
              }
	      if ((yyvsp[-1].dtype).val && (yyvsp[-1].dtype).type) {
		/* store initializer type as it might be different to the declared type */
		SwigType *valuetype = NewSwigType((yyvsp[-1].dtype).type);
		if (Len(valuetype) > 0) {
		  Setattr((yyval.node), "valuetype", valuetype);
		} else {
		  /* If we can't determine the initializer type use the declared type. */
		  Setattr((yyval.node), "valuetype", (yyvsp[-5].type));
		}
		Delete(valuetype);
	      }
	      if (!(yyvsp[0].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[0].node);
		/* Inherit attributes */
		while (n) {
		  String *type = Copy((yyvsp[-5].type));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[-6].str));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[-1].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
	      }

	      if ((yyvsp[-4].decl).id) {
		/* Ignore all scoped declarations, could be 1. out of class function definition 2. friend function declaration 3. ... */
		String *p = Swig_scopename_prefix((yyvsp[-4].decl).id);
		if (p) {
		  /* This is a special case. If the scope name of the declaration exactly
		     matches that of the declaration, then we will allow it. Otherwise, delete. */
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-4].decl).id);
		    Setattr((yyval.node), "name", lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node), (yyvsp[0].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[0].node);
		  }
		  Delete(p);
		} else if (Strncmp((yyvsp[-4].decl).id, "::", 2) == 0) {
		  /* global scope declaration/definition ignored */
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[0].node);
		} else {
		  set_nextSibling((yyval.node), (yyvsp[0].node));
		}
	      } else {
		Swig_error(cparse_file, cparse_line, "Missing symbol name for global declaration\n");
		(yyval.node) = 0;
	      }

	      if ((yyvsp[-3].dtype).qualifier && (yyvsp[-6].str) && Strstr((yyvsp[-6].str), "static"))
		Swig_error(cparse_file, cparse_line, "Static function %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)));
	      Delete((yyvsp[-6].str));
           }
#line 7131 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 115: /* c_decl: attribute storage_class type declarator cpp_const attribute EQUAL error SEMI  */
#line 3430 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                {
	      String *decl = (yyvsp[-5].decl).type;
	      (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-4].dtype).qualifier)
	        decl = add_qualifier_to_declarator((yyvsp[-5].decl).type, (yyvsp[-4].dtype).qualifier);
	      Setattr((yyval.node), "refqualifier", (yyvsp[-4].dtype).refqualifier);
	      Setattr((yyval.node), "type", (yyvsp[-6].type));
	      Setattr((yyval.node), "storage", (yyvsp[-7].str));
	      Setattr((yyval.node), "name", (yyvsp[-5].decl).id);
	      Setattr((yyval.node), "decl", decl);
	      Setattr((yyval.node), "parms", (yyvsp[-5].decl).parms);

	      /* Set dummy value to avoid adding in code for handling missing value in later stages */
	      Setattr((yyval.node), "value", "*parse error*");
	      SetFlag((yyval.node), "valueignored");

	      Setattr((yyval.node), "throws", (yyvsp[-4].dtype).throws);
	      Setattr((yyval.node), "throw", (yyvsp[-4].dtype).throwf);
	      Setattr((yyval.node), "noexcept", (yyvsp[-4].dtype).nexcept);
	      Setattr((yyval.node), "final", (yyvsp[-4].dtype).final);

              if ((yyvsp[-8].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-8].node));
              }
              if ((yyvsp[-3].node)) {
                Setattr((yyvsp[-6].type), "attribute", (yyvsp[-3].node));
              }

	      if ((yyvsp[-5].decl).id) {
		/* Ignore all scoped declarations, could be 1. out of class function definition 2. friend function declaration 3. ... */
		String *p = Swig_scopename_prefix((yyvsp[-5].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-5].decl).id);
		    Setattr((yyval.node), "name", lstr);
		    Delete(lstr);
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = 0;
		  }
		  Delete(p);
		} else if (Strncmp((yyvsp[-5].decl).id, "::", 2) == 0) {
		  /* global scope declaration/definition ignored */
		  Delete((yyval.node));
		  (yyval.node) = 0;
		}
	      }

	      if ((yyvsp[-4].dtype).qualifier && (yyvsp[-7].str) && Strstr((yyvsp[-7].str), "static"))
		Swig_error(cparse_file, cparse_line, "Static function %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)));
	      Delete((yyvsp[-7].str));
	   }
#line 7189 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 116: /* c_decl: attribute storage_class AUTO declarator cpp_const ARROW cpp_alternate_rettype virt_specifier_seq_opt initializer c_decl_tail  */
#line 3485 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                                          {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-5].dtype).qualifier) SwigType_push((yyvsp[-6].decl).type, (yyvsp[-5].dtype).qualifier);
	      Setattr((yyval.node),"refqualifier",(yyvsp[-5].dtype).refqualifier);
	      Setattr((yyval.node),"type",(yyvsp[-3].type));
	      Setattr((yyval.node),"storage",(yyvsp[-8].str));
	      Setattr((yyval.node),"name",(yyvsp[-6].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[-6].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[-6].decl).parms);
	      Setattr((yyval.node),"throws",(yyvsp[-5].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[-5].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[-5].dtype).nexcept);
	      Setattr((yyval.node),"final",(yyvsp[-5].dtype).final);
	      if (!(yyvsp[0].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[0].node);
		while (n) {
		  String *type = Copy((yyvsp[-3].type));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[-8].str));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }

	      if ((yyvsp[-6].decl).id) {
		/* Ignore all scoped declarations, could be 1. out of class function definition 2. friend function declaration 3. ... */
		String *p = Swig_scopename_prefix((yyvsp[-6].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-6].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node), (yyvsp[0].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[0].node);
		  }
		  Delete(p);
		} else if (Strncmp((yyvsp[-6].decl).id, "::", 2) == 0) {
		  /* global scope declaration/definition ignored */
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[0].node);
		}
	      } else {
		set_nextSibling((yyval.node), (yyvsp[0].node));
	      }

              if ((yyvsp[-9].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-9].node));
              }

	      if ((yyvsp[-5].dtype).qualifier && (yyvsp[-8].str) && Strstr((yyvsp[-8].str), "static"))
		Swig_error(cparse_file, cparse_line, "Static function %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)));
	      Delete((yyvsp[-8].str));
           }
#line 7256 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 117: /* c_decl: attribute storage_class AUTO declarator cpp_const LBRACE  */
#line 3554 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                      {
	      if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);

              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-1].dtype).qualifier) SwigType_push((yyvsp[-2].decl).type, (yyvsp[-1].dtype).qualifier);
	      Setattr((yyval.node), "refqualifier", (yyvsp[-1].dtype).refqualifier);
	      Setattr((yyval.node), "type", NewString("auto"));
	      Setattr((yyval.node), "storage", (yyvsp[-4].str));
	      Setattr((yyval.node), "name", (yyvsp[-2].decl).id);
	      Setattr((yyval.node), "decl", (yyvsp[-2].decl).type);
	      Setattr((yyval.node), "parms", (yyvsp[-2].decl).parms);
	      Setattr((yyval.node), "throws", (yyvsp[-1].dtype).throws);
	      Setattr((yyval.node), "throw", (yyvsp[-1].dtype).throwf);
	      Setattr((yyval.node), "noexcept", (yyvsp[-1].dtype).nexcept);
	      Setattr((yyval.node), "final", (yyvsp[-1].dtype).final);

              if ((yyvsp[-5].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-5].node));
              }

	      if ((yyvsp[-2].decl).id) {
		/* Ignore all scoped declarations, could be 1. out of class function definition 2. friend function declaration 3. ... */
		String *p = Swig_scopename_prefix((yyvsp[-2].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-2].decl).id);
		    Setattr((yyval.node), "name", lstr);
		    Delete(lstr);
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = 0;
		  }
		  Delete(p);
		} else if (Strncmp((yyvsp[-2].decl).id, "::", 2) == 0) {
		  /* global scope declaration/definition ignored */
		  Delete((yyval.node));
		  (yyval.node) = 0;
		}
	      }

	      if ((yyvsp[-1].dtype).qualifier && (yyvsp[-4].str) && Strstr((yyvsp[-4].str), "static"))
		Swig_error(cparse_file, cparse_line, "Static function %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)));
	      Delete((yyvsp[-4].str));
	   }
#line 7306 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 118: /* c_decl: attribute storage_class AUTO idcolon EQUAL definetype SEMI  */
#line 3600 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                        {
	      SwigType *type = deduce_type(&(yyvsp[-1].dtype));
	      if (!type)
		type = NewString("auto");
	      (yyval.node) = new_node("cdecl");
	      Setattr((yyval.node), "type", type);
	      Setattr((yyval.node), "storage", (yyvsp[-5].str));
	      Setattr((yyval.node), "name", (yyvsp[-3].str));
	      Setattr((yyval.node), "decl", NewStringEmpty());
	      Setattr((yyval.node), "value", (yyvsp[-1].dtype).val);
	      if ((yyvsp[-1].dtype).stringval) Setattr((yyval.node), "stringval", (yyvsp[-1].dtype).stringval);
	      if ((yyvsp[-1].dtype).numval) Setattr((yyval.node), "numval", (yyvsp[-1].dtype).numval);
	      Setattr((yyval.node), "valuetype", type);
	      Delete((yyvsp[-5].str));
	      Delete(type);
              if ((yyvsp[-6].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-6].node));
              }
	   }
#line 7330 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 119: /* c_decl: attribute storage_class AUTO idcolon EQUAL error SEMI  */
#line 3620 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                   {
	      SwigType *type = NewString("auto");
	      (yyval.node) = new_node("cdecl");
	      Setattr((yyval.node), "type", type);
	      Setattr((yyval.node), "storage", (yyvsp[-5].str));
	      Setattr((yyval.node), "name", (yyvsp[-3].str));
	      Setattr((yyval.node), "decl", NewStringEmpty());
	      Setattr((yyval.node), "valuetype", type);
	      Delete((yyvsp[-5].str));
	      Delete(type);
              if ((yyvsp[-6].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-6].node));
              }
	   }
#line 7349 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 120: /* c_decl_tail: SEMI  */
#line 3638 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                      { 
                   (yyval.node) = 0;
                   Clear(scanner_ccode); 
               }
#line 7358 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 121: /* c_decl_tail: COMMA declarator cpp_const initializer c_decl_tail  */
#line 3642 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                        {
		 (yyval.node) = new_node("cdecl");
		 if ((yyvsp[-2].dtype).qualifier) SwigType_push((yyvsp[-3].decl).type,(yyvsp[-2].dtype).qualifier);
		 Setattr((yyval.node),"refqualifier",(yyvsp[-2].dtype).refqualifier);
		 Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
		 Setattr((yyval.node),"decl",(yyvsp[-3].decl).type);
		 Setattr((yyval.node),"parms",(yyvsp[-3].decl).parms);
		 Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		 if ((yyvsp[-1].dtype).stringval) Setattr((yyval.node), "stringval", (yyvsp[-1].dtype).stringval);
		 if ((yyvsp[-1].dtype).numval) Setattr((yyval.node), "numval", (yyvsp[-1].dtype).numval);
		 Setattr((yyval.node),"throws",(yyvsp[-2].dtype).throws);
		 Setattr((yyval.node),"throw",(yyvsp[-2].dtype).throwf);
		 Setattr((yyval.node),"noexcept",(yyvsp[-2].dtype).nexcept);
		 Setattr((yyval.node),"final",(yyvsp[-2].dtype).final);
		 if ((yyvsp[-1].dtype).bitfield) {
		   Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
		 }
		 if (!(yyvsp[0].node)) {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr((yyval.node),"code",code);
		     Delete(code);
		   }
		 } else {
		   set_nextSibling((yyval.node), (yyvsp[0].node));
		 }
	       }
#line 7390 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 122: /* c_decl_tail: LBRACE  */
#line 3669 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { 
                   if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
                   (yyval.node) = 0;
               }
#line 7399 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 123: /* c_decl_tail: error  */
#line 3673 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
		   (yyval.node) = 0;
		   if (yychar == RPAREN) {
		       Swig_error(cparse_file, cparse_line, "Unexpected closing parenthesis (')').\n");
		   } else {
		       Swig_error(cparse_file, cparse_line, "Syntax error - possibly a missing semicolon (';').\n");
		   }
		   Exit(EXIT_FAILURE);
               }
#line 7413 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 125: /* initializer: COLON expr  */
#line 3685 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		(yyval.dtype) = default_dtype;
		(yyval.dtype).bitfield = (yyvsp[0].dtype).val;
	      }
#line 7422 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 129: /* cpp_alternate_rettype: c_enum_key idcolon  */
#line 3694 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   {
		(yyval.type) = (yyvsp[0].str);
		Insert((yyval.type), 0, "enum ");
	      }
#line 7431 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 130: /* cpp_alternate_rettype: idcolon  */
#line 3698 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.type) = (yyvsp[0].str); }
#line 7437 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 131: /* cpp_alternate_rettype: idcolon AND  */
#line 3699 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
                (yyval.type) = (yyvsp[-1].str);
                SwigType_add_reference((yyval.type));
              }
#line 7446 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 132: /* cpp_alternate_rettype: idcolon LAND  */
#line 3703 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
                (yyval.type) = (yyvsp[-1].str);
                SwigType_add_rvalue_reference((yyval.type));
              }
#line 7455 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 133: /* cpp_alternate_rettype: CONST_QUAL idcolon AND  */
#line 3707 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
                (yyval.type) = (yyvsp[-1].str);
                SwigType_add_qualifier((yyval.type), "const");
                SwigType_add_reference((yyval.type));
              }
#line 7465 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 134: /* cpp_alternate_rettype: CONST_QUAL idcolon LAND  */
#line 3712 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
                (yyval.type) = (yyvsp[-1].str);
                SwigType_add_qualifier((yyval.type), "const");
                SwigType_add_rvalue_reference((yyval.type));
              }
#line 7475 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 136: /* cpp_lambda_decl: attribute storage_class AUTO idcolon EQUAL lambda_introducer lambda_template LPAREN parms RPAREN cpp_const lambda_body lambda_tail  */
#line 3728 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                                                     {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-9].str));
		  Delete((yyvsp[-11].str));
		  add_symbols((yyval.node));
                  if ((yyvsp[-12].node)) {
                    Setattr((yyval.node), "attribute", (yyvsp[-12].node));
                  }
	        }
#line 7489 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 137: /* cpp_lambda_decl: attribute storage_class AUTO idcolon EQUAL lambda_introducer lambda_template LPAREN parms RPAREN cpp_const ARROW type lambda_body lambda_tail  */
#line 3737 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                                                                {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-11].str));
		  Delete((yyvsp[-13].str));
		  add_symbols((yyval.node));
                  if ((yyvsp[-14].node)) {
                    Setattr((yyval.node), "attribute", (yyvsp[-14].node));
                  }
		}
#line 7503 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 138: /* cpp_lambda_decl: attribute storage_class AUTO idcolon EQUAL lambda_introducer lambda_template lambda_body lambda_tail  */
#line 3746 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                       {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-5].str));
		  Delete((yyvsp[-7].str));
		  add_symbols((yyval.node));
                  if ((yyvsp[-8].node)) {
                    Setattr((yyval.node), "attribute", (yyvsp[-8].node));
                  }
		}
#line 7517 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 139: /* lambda_introducer: LBRACKET  */
#line 3757 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		  if (skip_balanced('[',']') < 0) Exit(EXIT_FAILURE);
	        }
#line 7525 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 140: /* lambda_template: LESSTHAN  */
#line 3762 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		  if (skip_balanced('<','>') < 0) Exit(EXIT_FAILURE);
		}
#line 7533 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 142: /* lambda_body: LBRACE  */
#line 3768 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                     {
		  if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		}
#line 7541 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 144: /* $@4: %empty  */
#line 3773 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		  if (skip_balanced('(',')') < 0) Exit(EXIT_FAILURE);
		}
#line 7549 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 145: /* lambda_tail: LPAREN $@4 SEMI  */
#line 3775 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
		}
#line 7556 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 146: /* c_enum_key: ENUM  */
#line 3785 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                  {
		   (yyval.id) = "enum";
	      }
#line 7564 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 147: /* c_enum_key: ENUM CLASS  */
#line 3788 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		   (yyval.id) = "enum class";
	      }
#line 7572 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 148: /* c_enum_key: ENUM STRUCT  */
#line 3791 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
		   (yyval.id) = "enum struct";
	      }
#line 7580 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 149: /* c_enum_inherit: COLON type_right  */
#line 3800 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
                   (yyval.type) = (yyvsp[0].type);
              }
#line 7588 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 150: /* c_enum_inherit: %empty  */
#line 3803 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       { (yyval.type) = 0; }
#line 7594 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 151: /* c_enum_forward_decl: attribute storage_class c_enum_key ename c_enum_inherit SEMI  */
#line 3810 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                   {
		   SwigType *ty = 0;
		   int scopedenum = (yyvsp[-2].id) && !Equal((yyvsp[-3].id), "enum");
		   (yyval.node) = new_node("enumforward");
		   ty = NewStringf("enum %s", (yyvsp[-2].id));
		   Setattr((yyval.node),"enumkey",(yyvsp[-3].id));
		   if (scopedenum)
		     SetFlag((yyval.node), "scopedenum");
		   Setattr((yyval.node),"name",(yyvsp[-2].id));
		   Setattr((yyval.node), "enumbase", (yyvsp[-1].type));
		   Setattr((yyval.node),"type",ty);
		   Setattr((yyval.node),"sym:weak", "1");
		   Delete((yyvsp[-4].str));
		   add_symbols((yyval.node));
                   if ((yyvsp[-5].node)) {
                     Setattr((yyval.node), "attribute", (yyvsp[-5].node));
                  }
	      }
#line 7617 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 152: /* c_enum_decl: attribute storage_class c_enum_key ename c_enum_inherit LBRACE enumlist RBRACE SEMI  */
#line 3836 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                   {
		  SwigType *ty = 0;
		  int scopedenum = (yyvsp[-5].id) && !Equal((yyvsp[-6].id), "enum");
		  (yyval.node) = new_enum_node((yyvsp[-4].type));
		  ty = NewStringf("enum %s", (yyvsp[-5].id));
		  Setattr((yyval.node),"enumkey",(yyvsp[-6].id));
		  if (scopedenum)
		    SetFlag((yyval.node), "scopedenum");
		  Setattr((yyval.node),"name",(yyvsp[-5].id));
		  Setattr((yyval.node),"type",ty);
		  appendChild((yyval.node),(yyvsp[-2].node));
		  add_symbols((yyval.node));      /* Add to tag space */

		  if (scopedenum) {
		    Swig_symbol_newscope();
		    Swig_symbol_setscopename((yyvsp[-5].id));
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }

		  add_symbols((yyvsp[-2].node));      /* Add enum values to appropriate enum or enum class scope */

		  if (scopedenum) {
		    Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }
		  Delete((yyvsp[-7].str));

                  if ((yyvsp[-8].node)) {
                    Setattr((yyval.node), "attribute", (yyvsp[-8].node));
                  }
               }
#line 7655 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 153: /* c_enum_decl: attribute storage_class c_enum_key ename c_enum_inherit LBRACE enumlist RBRACE declarator cpp_const initializer c_decl_tail  */
#line 3869 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                                                             {
		 Node *n;
		 SwigType *ty = 0;
		 String   *unnamed = 0;
		 int       unnamedinstance = 0;
		 int scopedenum = (yyvsp[-8].id) && !Equal((yyvsp[-9].id), "enum");

		 (yyval.node) = new_enum_node((yyvsp[-7].type));
		 Setattr((yyval.node),"enumkey",(yyvsp[-9].id));
		 if (scopedenum)
		   SetFlag((yyval.node), "scopedenum");
		 if ((yyvsp[-8].id)) {
		   Setattr((yyval.node),"name",(yyvsp[-8].id));
		   ty = NewStringf("enum %s", (yyvsp[-8].id));
		 } else if ((yyvsp[-3].decl).id) {
		   unnamed = make_unnamed();
		   ty = NewStringf("enum %s", unnamed);
		   Setattr((yyval.node),"unnamed",unnamed);
                   /* name is not set for unnamed enum instances, e.g. enum { foo } Instance; */
		   if ((yyvsp[-10].str) && Cmp((yyvsp[-10].str), "typedef") == 0) {
		     Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
                   } else {
                     unnamedinstance = 1;
                   }
		   Setattr((yyval.node),"storage",(yyvsp[-10].str));
		 }
		 if ((yyvsp[-3].decl).id && Cmp((yyvsp[-10].str),"typedef") == 0) {
		   Setattr((yyval.node),"tdname",(yyvsp[-3].decl).id);
                   Setattr((yyval.node),"allows_typedef","1");
                 }
		 appendChild((yyval.node),(yyvsp[-5].node));
		 n = new_node("cdecl");
		 Setattr(n,"type",ty);
		 Setattr(n,"name",(yyvsp[-3].decl).id);
		 Setattr(n,"storage",(yyvsp[-10].str));
		 Setattr(n,"decl",(yyvsp[-3].decl).type);
		 Setattr(n,"parms",(yyvsp[-3].decl).parms);
		 Setattr(n,"unnamed",unnamed);

                 if (unnamedinstance) {
		   SwigType *cty = NewString("enum ");
		   Setattr((yyval.node),"type",cty);
		   SetFlag((yyval.node),"unnamedinstance");
		   SetFlag(n,"unnamedinstance");
		   Delete(cty);
                 }
		 if ((yyvsp[0].node)) {
		   Node *p = (yyvsp[0].node);
		   set_nextSibling(n,p);
		   while (p) {
		     SwigType *cty = Copy(ty);
		     Setattr(p,"type",cty);
		     Setattr(p,"unnamed",unnamed);
		     Setattr(p,"storage",(yyvsp[-10].str));
		     Delete(cty);
		     p = nextSibling(p);
		   }
		 } else {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr(n,"code",code);
		     Delete(code);
		   }
		 }

                 /* Ensure that typedef enum ABC {foo} XYZ; uses XYZ for sym:name, like structs.
                  * Note that class_rename/yyrename are bit of a mess so used this simple approach to change the name. */
                 if ((yyvsp[-3].decl).id && (yyvsp[-8].id) && Cmp((yyvsp[-10].str),"typedef") == 0) {
		   String *name = NewString((yyvsp[-3].decl).id);
                   Setattr((yyval.node), "parser:makename", name);
		   Delete(name);
                 }

		 add_symbols((yyval.node));       /* Add enum to tag space */
		 set_nextSibling((yyval.node),n);
		 Delete(n);

		 if (scopedenum) {
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[-8].id));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

		 add_symbols((yyvsp[-5].node));      /* Add enum values to appropriate enum or enum class scope */

		 if (scopedenum) {
		   Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

	         add_symbols(n);
		 Delete((yyvsp[-10].str));
		 Delete(unnamed);
                 if ((yyvsp[-11].node)) {
                   Setattr((yyval.node), "attribute", (yyvsp[-11].node));
                 }
	       }
#line 7759 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 154: /* c_constructor_decl: attribute storage_class type LPAREN parms RPAREN ctor_end  */
#line 3970 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                               {
                   /* This is a sick hack.  If the ctor_end has parameters,
                      and the parms parameter only has 1 parameter, this
                      could be a declaration of the form:

                         type (id)(parms)

			 Otherwise it's an error. */
                    int err = 0;
                    (yyval.node) = 0;

		    if ((ParmList_len((yyvsp[-2].pl)) == 1) && (!Swig_scopename_check((yyvsp[-4].type)))) {
		      SwigType *ty = Getattr((yyvsp[-2].pl),"type");
		      String *name = Getattr((yyvsp[-2].pl),"name");
		      err = 1;
		      if (!name) {
			(yyval.node) = new_node("cdecl");
			Setattr((yyval.node),"type",(yyvsp[-4].type));
			Setattr((yyval.node),"storage",(yyvsp[-5].str));
			Setattr((yyval.node),"name",ty);

			if ((yyvsp[0].decl).have_parms) {
			  SwigType *decl = NewStringEmpty();
			  SwigType_add_function(decl,(yyvsp[0].decl).parms);
			  Setattr((yyval.node),"decl",decl);
			  Setattr((yyval.node),"parms",(yyvsp[0].decl).parms);
			  if (Len(scanner_ccode)) {
			    String *code = Copy(scanner_ccode);
			    Setattr((yyval.node),"code",code);
			    Delete(code);
			  }
			}
			if ((yyvsp[0].decl).defarg)
			  Setattr((yyval.node), "value", (yyvsp[0].decl).defarg);
			if ((yyvsp[0].decl).stringdefarg)
			  Setattr((yyval.node), "stringval", (yyvsp[0].decl).stringdefarg);
			if ((yyvsp[0].decl).numdefarg)
			  Setattr((yyval.node), "numval", (yyvsp[0].decl).numdefarg);
			Setattr((yyval.node),"throws",(yyvsp[0].decl).throws);
			Setattr((yyval.node),"throw",(yyvsp[0].decl).throwf);
			Setattr((yyval.node),"noexcept",(yyvsp[0].decl).nexcept);
			Setattr((yyval.node),"final",(yyvsp[0].decl).final);
			err = 0;
		      }
		    }
		    Delete((yyvsp[-5].str));
		    if (err) {
		      Swig_error(cparse_file,cparse_line,"Syntax error in input(2).\n");
		      Exit(EXIT_FAILURE);
		    }
                    if ((yyvsp[-6].node)) {
                      Setattr((yyval.node), "attribute", (yyvsp[-6].node));
                    }
                }
#line 7818 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 161: /* @5: %empty  */
#line 4044 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                      {
                   String *prefix;
                   List *bases = 0;
		   Node *scope = 0;
		   int errored_flag = 0;
		   String *code;
		   (yyval.node) = new_node("class");
		   Setattr((yyval.node),"kind",(yyvsp[-4].type));
		   if ((yyvsp[-1].bases)) {
		     Setattr((yyval.node),"baselist", Getattr((yyvsp[-1].bases),"public"));
		     Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[-1].bases),"protected"));
		     Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[-1].bases),"private"));
		   }
		   Setattr((yyval.node),"allows_typedef","1");

		   /* Temporary unofficial symtab for use until add_symbols() adds "sym:symtab" */
		   Setattr((yyval.node), "unofficial:symtab", Swig_symbol_current());
		  
		   /* If the class name is qualified.  We need to create or lookup namespace/scope entries */
		   scope = resolve_create_node_scope((yyvsp[-3].str), 1, &errored_flag);
		   /* save nscope_inner to the class - it may be overwritten in nested classes*/
		   Setattr((yyval.node), "nested:innerscope", nscope_inner);
		   Setattr((yyval.node), "nested:nscope", nscope);
		   Setfile(scope,cparse_file);
		   Setline(scope,cparse_line);
		   Setattr((yyval.node), "name", scope);

		   if (currentOuterClass) {
		     SetFlag((yyval.node), "nested");
		     Setattr((yyval.node), "nested:outer", currentOuterClass);
		     set_access_mode((yyval.node));
		   }
		   Swig_features_get(Swig_cparse_features(), Namespaceprefix, Getattr((yyval.node), "name"), 0, (yyval.node));
		   /* save yyrename to the class attribute, to be used later in add_symbols()*/
		   Setattr((yyval.node), "class_rename", make_name((yyval.node), scope, 0));
		   Setattr((yyval.node), "Classprefix", scope);
		   Classprefix = NewString(scope);
		   /* Deal with inheritance  */
		   if ((yyvsp[-1].bases))
		     bases = Swig_make_inherit_list(scope, Getattr((yyvsp[-1].bases), "public"), Namespaceprefix);
		   prefix = SwigType_istemplate_templateprefix(scope);
		   if (prefix) {
		     String *fbase, *tbase;
		     if (Namespaceprefix) {
		       fbase = NewStringf("%s::%s", Namespaceprefix, scope);
		       tbase = NewStringf("%s::%s", Namespaceprefix, prefix);
		     } else {
		       fbase = Copy(scope);
		       tbase = Copy(prefix);
		     }
		     Swig_name_inherit(tbase,fbase);
		     Delete(fbase);
		     Delete(tbase);
		   }
                   if (Strcmp((yyvsp[-4].type), "class") == 0) {
		     cplus_mode = CPLUS_PRIVATE;
		   } else {
		     cplus_mode = CPLUS_PUBLIC;
		   }
		   if (!cparse_cplusplus) {
		     set_scope_to_global();
		   }
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename(scope);
		   Swig_inherit_base_symbols(bases);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   cparse_start_line = cparse_line;

		   /* If there are active template parameters, we need to make sure they are
                      placed in the class symbol table so we can catch shadows */

		   if (template_parameters) {
		     Parm *tp = template_parameters;
		     while(tp) {
		       String *tpname = Copy(Getattr(tp,"name"));
		       Node *tn = new_node("templateparm");
		       Setattr(tn,"name",tpname);
		       Swig_symbol_cadd(tpname,tn);
		       tp = nextSibling(tp);
		       Delete(tpname);
		     }
		   }
		   Delete(prefix);
		   inclass = 1;
		   currentOuterClass = (yyval.node);
		   if (cparse_cplusplusout) {
		     /* save the structure declaration to declare it in global scope for C++ to see */
		     code = get_raw_text_balanced('{', '}');
		     Setattr((yyval.node), "code", code);
		     Delete(code);
		   }
                   if ((yyvsp[-6].node)) {
                     Setattr((yyval.node), "attribute", (yyvsp[-6].node));
                   }
               }
#line 7919 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 162: /* cpp_class_decl: attribute storage_class cpptype idcolon class_virt_specifier_opt inherit LBRACE @5 cpp_members RBRACE cpp_opt_declarators  */
#line 4139 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                              {
		   Node *p;
		   SwigType *ty;
		   Symtab *cscope;
		   Node *am = 0;
		   String *scpname = 0;
		   (yyval.node) = currentOuterClass;
		   currentOuterClass = Getattr((yyval.node), "nested:outer");
		   nscope_inner = Getattr((yyval.node), "nested:innerscope");
		   nscope = Getattr((yyval.node), "nested:nscope");
		   Delattr((yyval.node), "nested:innerscope");
		   Delattr((yyval.node), "nested:nscope");
		   if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) { /* actual parent class for this class */
		     Node* forward_declaration = Swig_symbol_clookup_no_inherit(Getattr((yyval.node),"name"), Getattr(nscope_inner, "symtab"));
		     if (forward_declaration) {
		       Setattr((yyval.node), "access", Getattr(forward_declaration, "access"));
		     }
		     Setattr((yyval.node), "nested:outer", nscope_inner);
		     SetFlag((yyval.node), "nested");
                   }
		   if (!currentOuterClass)
		     inclass = 0;
		   cscope = Getattr((yyval.node), "unofficial:symtab");
		   Delattr((yyval.node), "unofficial:symtab");
		   
		   /* Check for pure-abstract class */
		   Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[-2].node)));
		   
		   /* This bit of code merges in a previously defined %extend directive (if any) */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     am = Getattr(Swig_extend_hash(), clsname);
		     if (am) {
		       Swig_extend_merge((yyval.node), am);
		       Delattr(Swig_extend_hash(), clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes, scpname, (yyval.node));

		   appendChild((yyval.node), (yyvsp[-2].node));
		   
		   if (am) 
		     Swig_extend_append_previous((yyval.node), am);

		   p = (yyvsp[0].node);
		   if (p && !nscope_inner) {
		     if (!cparse_cplusplus && currentOuterClass)
		       appendChild(currentOuterClass, p);
		     else
		      appendSibling((yyval.node), p);
		   }
		   
		   if (nscope_inner) {
		     ty = NewString(scpname); /* if the class is declared out of scope, let the declarator use fully qualified type*/
		   } else if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString(Getattr((yyvsp[-3].node), "name"));
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[-8].type), Getattr((yyvsp[-3].node), "name"));
		   }
		   while (p) {
		     Setattr(p, "storage", (yyvsp[-9].str));
		     Setattr(p, "type" ,ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(p, "hasconsttype");
		     }
		     p = nextSibling(p);
		   }
		   if ((yyvsp[0].node) && Cmp((yyvsp[-9].str),"typedef") == 0)
		     add_typedef_name((yyval.node), (yyvsp[0].node), Getattr((yyvsp[-3].node), "name"), cscope, scpname);
		   Delete(scpname);

		   if (cplus_mode != CPLUS_PUBLIC) {
		   /* we 'open' the class at the end, to allow %template
		      to add new members */
		     Node *pa = new_node("access");
		     Setattr(pa, "kind", "public");
		     cplus_mode = CPLUS_PUBLIC;
		     appendChild((yyval.node), pa);
		     Delete(pa);
		   }
		   if (currentOuterClass)
		     restore_access_mode((yyval.node));
		   Setattr((yyval.node), "symtab", Swig_symbol_popscope());
		   Classprefix = Getattr((yyval.node), "Classprefix");
		   Delattr((yyval.node), "Classprefix");
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   if (cplus_mode == CPLUS_PRIVATE) {
		     (yyval.node) = 0; /* skip private nested classes */
		   } else if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		     (yyval.node) = nested_forward_declaration((yyvsp[-9].str), (yyvsp[-8].type), Getattr((yyvsp[-3].node), "name"), Copy(Getattr((yyvsp[-3].node), "name")), (yyvsp[0].node));
		   } else if (nscope_inner) {
		     /* this is tricky */
		     /* we add the declaration in the original namespace */
		     if (Strcmp(nodeType(nscope_inner), "class") == 0 && cparse_cplusplus && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested"))
		       (yyval.node) = nested_forward_declaration((yyvsp[-9].str), (yyvsp[-8].type), Getattr((yyvsp[-3].node), "name"), Copy(Getattr((yyvsp[-3].node), "name")), (yyvsp[0].node));
		     appendChild(nscope_inner, (yyval.node));
		     Swig_symbol_setscope(Getattr(nscope_inner, "symtab"));
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     yyrename = Copy(Getattr((yyval.node), "class_rename"));
		     add_symbols((yyval.node));
		     Delattr((yyval.node), "class_rename");
		     /* but the variable definition in the current scope */
		     Swig_symbol_setscope(cscope);
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     add_symbols((yyvsp[0].node));
		     if (nscope) {
		       (yyval.node) = nscope; /* here we return recreated namespace tower instead of the class itself */
		       if ((yyvsp[0].node)) {
			 appendSibling((yyval.node), (yyvsp[0].node));
		       }
		     } else if (!SwigType_istemplate(ty) && template_parameters == 0) { /* for template we need the class itself */
		       (yyval.node) = (yyvsp[0].node);
		     }
		   } else {
		     Delete(yyrename);
		     yyrename = 0;
		     if (!cparse_cplusplus && currentOuterClass) { /* nested C structs go into global scope*/
		       Node *outer = currentOuterClass;
		       while (Getattr(outer, "nested:outer"))
			 outer = Getattr(outer, "nested:outer");
		       appendSibling(outer, (yyval.node));
		       Swig_symbol_setscope(cscope); /* declaration goes in the parent scope */
		       add_symbols((yyvsp[0].node));
		       set_scope_to_global();
		       Delete(Namespaceprefix);
		       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       if (!cparse_cplusplusout)
			 Delattr((yyval.node), "nested:outer");
		       Delattr((yyval.node), "class_rename");
		       (yyval.node) = 0;
		     } else {
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       add_symbols((yyvsp[0].node));
		       Delattr((yyval.node), "class_rename");
		     }
		   }
		   Delete(ty);
		   Swig_symbol_setscope(cscope);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   Classprefix = currentOuterClass ? Getattr(currentOuterClass, "Classprefix") : 0;
		   Delete((yyvsp[-9].str));
	       }
#line 8076 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 163: /* @6: %empty  */
#line 4294 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
	       String *unnamed;
	       String *code;
	       unnamed = make_unnamed();
	       (yyval.node) = new_node("class");
	       Setattr((yyval.node),"kind",(yyvsp[-2].type));
	       if ((yyvsp[-1].bases)) {
		 Setattr((yyval.node),"baselist", Getattr((yyvsp[-1].bases),"public"));
		 Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[-1].bases),"protected"));
		 Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[-1].bases),"private"));
	       }
	       Setattr((yyval.node),"storage",(yyvsp[-3].str));
	       Setattr((yyval.node),"unnamed",unnamed);
	       Setattr((yyval.node),"allows_typedef","1");

	       /* Temporary unofficial symtab for use until add_symbols() adds "sym:symtab" */
	       Setattr((yyval.node), "unofficial:symtab", Swig_symbol_current());

	       if (currentOuterClass) {
		 SetFlag((yyval.node), "nested");
		 Setattr((yyval.node), "nested:outer", currentOuterClass);
		 set_access_mode((yyval.node));
	       }
	       Swig_features_get(Swig_cparse_features(), Namespaceprefix, 0, 0, (yyval.node));
	       /* save yyrename to the class attribute, to be used later in add_symbols()*/
	       Setattr((yyval.node), "class_rename", make_name((yyval.node),0,0));
	       if (Strcmp((yyvsp[-2].type), "class") == 0) {
		 cplus_mode = CPLUS_PRIVATE;
	       } else {
		 cplus_mode = CPLUS_PUBLIC;
	       }
	       Swig_symbol_newscope();
	       cparse_start_line = cparse_line;
	       currentOuterClass = (yyval.node);
	       inclass = 1;
	       Classprefix = 0;
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       /* save the structure declaration to make a typedef for it later*/
	       code = get_raw_text_balanced('{', '}');
	       Setattr((yyval.node), "code", code);
	       Delete(code);
               if ((yyvsp[-4].node)) {
                 Setattr((yyval.node), "attribute", (yyvsp[-4].node));
               }
	     }
#line 8127 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 164: /* cpp_class_decl: attribute storage_class cpptype inherit LBRACE @6 cpp_members RBRACE cpp_opt_declarators  */
#line 4339 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
	       String *unnamed;
               List *bases = 0;
	       String *name = 0;
	       Node *n;
	       Symtab *cscope;
	       Classprefix = 0;
	       (void)(yyvsp[-3].node);
	       (yyval.node) = currentOuterClass;
	       currentOuterClass = Getattr((yyval.node), "nested:outer");
	       if (!currentOuterClass)
		 inclass = 0;
	       else
		 restore_access_mode((yyval.node));

	       cscope = Getattr((yyval.node), "unofficial:symtab");
	       Delattr((yyval.node), "unofficial:symtab");

	       unnamed = Getattr((yyval.node),"unnamed");
               /* Check for pure-abstract class */
	       Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[-2].node)));
	       n = (yyvsp[0].node);
	       if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		 String *name = n ? Copy(Getattr(n, "name")) : 0;
		 (yyval.node) = nested_forward_declaration((yyvsp[-7].str), (yyvsp[-6].type), 0, name, n);
	       } else if (n) {
	         appendSibling((yyval.node),n);
		 /* If a proper typedef name was given, we'll use it to set the scope name */
		 name = try_to_find_a_name_for_unnamed_structure((yyvsp[-7].str), n);
		 if (name) {
		   String *scpname = 0;
		   SwigType *ty;
		   Setattr((yyval.node),"tdname",name);
		   Setattr((yyval.node),"name",name);
		   Swig_symbol_setscopename(name);
		   if ((yyvsp[-5].bases))
		     bases = Swig_make_inherit_list(name,Getattr((yyvsp[-5].bases),"public"),Namespaceprefix);
		   Swig_inherit_base_symbols(bases);

		     /* If a proper name was given, we use that as the typedef, not unnamed */
		   Clear(unnamed);
		   Append(unnamed, name);
		   if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString(name);
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[-6].type),name);
		   }
		   while (n) {
		     Setattr(n,"storage",(yyvsp[-7].str));
		     Setattr(n, "type", ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(n,"hasconsttype");
		     }
		     n = nextSibling(n);
		   }
		   n = (yyvsp[0].node);

		   /* Check for previous extensions */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     Node *am = Getattr(Swig_extend_hash(),clsname);
		     if (am) {
		       /* Merge the extension into the symbol table */
		       Swig_extend_merge((yyval.node),am);
		       Swig_extend_append_previous((yyval.node),am);
		       Delattr(Swig_extend_hash(),clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes,scpname,(yyval.node));
		   Delete(scpname);
		 } else { /* no suitable name was found for a struct */
		   Setattr((yyval.node), "nested:unnamed", Getattr(n, "name")); /* save the name of the first declarator for later use in name generation*/
		   while (n) { /* attach unnamed struct to the declarators, so that they would receive proper type later*/
		     Setattr(n, "nested:unnamedtype", (yyval.node));
		     Setattr(n, "storage", (yyvsp[-7].str));
		     n = nextSibling(n);
		   }
		   n = (yyvsp[0].node);
		   Swig_symbol_setscopename("<unnamed>");
		 }
		 appendChild((yyval.node),(yyvsp[-2].node));
		 /* Pop the scope */
		 Setattr((yyval.node),"symtab",Swig_symbol_popscope());
		 if (name) {
		   Delete(yyrename);
		   yyrename = Copy(Getattr((yyval.node), "class_rename"));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   add_symbols((yyval.node));
		   add_symbols(n);
		   Delattr((yyval.node), "class_rename");
		 } else if (cparse_cplusplus)
		   (yyval.node) = 0; /* ignore unnamed structs for C++ */
		   Delete(unnamed);
	       } else { /* unnamed struct without declarator*/
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 add_symbols((yyvsp[-2].node));
		 Delete((yyval.node));
		 (yyval.node) = (yyvsp[-2].node); /* pass member list to outer class/namespace (instead of self)*/
	       }
	       Swig_symbol_setscope(cscope);
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       Classprefix = currentOuterClass ? Getattr(currentOuterClass, "Classprefix") : 0;
	       Delete((yyvsp[-7].str));
              }
#line 8243 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 165: /* cpp_opt_declarators: SEMI  */
#line 4452 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            { (yyval.node) = 0; }
#line 8249 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 166: /* cpp_opt_declarators: declarator cpp_const initializer c_decl_tail  */
#line 4453 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
                        (yyval.node) = new_node("cdecl");
                        Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
                        Setattr((yyval.node),"decl",(yyvsp[-3].decl).type);
                        Setattr((yyval.node),"parms",(yyvsp[-3].decl).parms);
			set_nextSibling((yyval.node), (yyvsp[0].node));
                    }
#line 8261 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 167: /* cpp_forward_class_decl: attribute storage_class cpptype idcolon SEMI  */
#line 4465 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                      {
	      if ((yyvsp[-3].str) && Strstr((yyvsp[-3].str), "friend")) {
		/* Ignore */
                (yyval.node) = 0; 
	      } else {
		(yyval.node) = new_node("classforward");
		Setattr((yyval.node),"kind",(yyvsp[-2].type));
		Setattr((yyval.node),"name",(yyvsp[-1].str));
		Setattr((yyval.node),"sym:weak", "1");
		add_symbols((yyval.node));
	      }
	      Delete((yyvsp[-3].str));
              if ((yyvsp[-4].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-4].node));
              }
             }
#line 8282 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 168: /* $@7: %empty  */
#line 4487 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 { 
		    if (currentOuterClass)
		      Setattr(currentOuterClass, "template_parameters", template_parameters);
		    template_parameters = (yyvsp[-1].tparms); 
		    parsing_template_declaration = 1;
		  }
#line 8293 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 169: /* cpp_template_decl: TEMPLATE LESSTHAN template_parms GREATERTHAN $@7 cpp_template_possible  */
#line 4492 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
			String *tname = 0;
			int     error = 0;

			/* check if we get a namespace node with a class declaration, and retrieve the class */
			Symtab *cscope = Swig_symbol_current();
			Symtab *sti = 0;
			Node *ntop = (yyvsp[0].node);
			Node *ni = ntop;
			SwigType *ntype = ni ? nodeType(ni) : 0;
			while (ni && Strcmp(ntype,"namespace") == 0) {
			  sti = Getattr(ni,"symtab");
			  ni = firstChild(ni);
			  ntype = nodeType(ni);
			}
			if (sti) {
			  Swig_symbol_setscope(sti);
			  Delete(Namespaceprefix);
			  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			  (yyval.node) = ni;
			} else {
			  (yyval.node) = (yyvsp[0].node);
			}

			if ((yyval.node)) tname = Getattr((yyval.node),"name");
			
			/* Check if the class is a template specialization */
			if (((yyval.node)) && (Strchr(tname,'<')) && (!is_operator(tname))) {
			  /* If a specialization.  Check if defined. */
			  Node *tempn = 0;
			  {
			    String *tbase = SwigType_templateprefix(tname);
			    tempn = Swig_symbol_clookup_local(tbase,0);
			    if (!tempn || (Strcmp(nodeType(tempn),"template") != 0)) {
			      SWIG_WARN_NODE_BEGIN(tempn);
			      Swig_warning(WARN_PARSE_TEMPLATE_SP_UNDEF, Getfile((yyval.node)),Getline((yyval.node)),"Specialization of non-template '%s'.\n", tbase);
			      SWIG_WARN_NODE_END(tempn);
			      tempn = 0;
			      error = 1;
			    }
			    Delete(tbase);
			  }
			  Setattr((yyval.node),"specialization","1");
			  Setattr((yyval.node),"templatetype",nodeType((yyval.node)));
			  set_nodeType((yyval.node),"template");
			  /* Template partial specialization */
			  if (tempn && ((yyvsp[-3].tparms)) && ((yyval.node))) {
			    ParmList *primary_templateparms = Getattr(tempn, "templateparms");
			    String *targs = SwigType_templateargs(tname); /* tname contains name and specialized template parameters, for example: X<(p.T,TT)> */
			    List *tlist = SwigType_parmlist(targs);
			    int specialization_parms_len = Len(tlist);
			    if (!Getattr((yyval.node),"sym:weak")) {
			      Setattr((yyval.node),"sym:typename","1");
			    }
			    Setattr((yyval.node), "primarytemplate", tempn);
			    Setattr((yyval.node), "templateparms", (yyvsp[-3].tparms));
			    Delattr((yyval.node), "specialization");
			    Setattr((yyval.node), "partialspecialization", "1");
			    
			    if (specialization_parms_len > ParmList_len(primary_templateparms)) {
			      Swig_error(Getfile((yyval.node)), Getline((yyval.node)), "Template partial specialization has more arguments than primary template %d %d.\n", specialization_parms_len, ParmList_len(primary_templateparms));
			      
			    } else if (specialization_parms_len < ParmList_numrequired(primary_templateparms)) {
			      Swig_error(Getfile((yyval.node)), Getline((yyval.node)), "Template partial specialization has fewer arguments than primary template %d %d.\n", specialization_parms_len, ParmList_len(primary_templateparms));
			    } else {
			      /* Create a specialized name with template parameters replaced with $ variables, such as, X<(T1,p.T2) => X<($1,p.$2)> */
			      Parm *p = (yyvsp[-3].tparms);
			      String *fname = NewString(tname);
			      String *ffname = 0;
			      ParmList *partialparms = 0;

			      char   tmp[32];
			      int i = 0;
			      while (p) {
				String *name = Getattr(p,"name");
				++i;
				if (!name) {
				  p = nextSibling(p);
				  continue;
				}
				sprintf(tmp, "$%d", i);
				Replaceid(fname, name, tmp);
				p = nextSibling(p);
			      }
			      /* Patch argument names with typedef */
			      {
				Iterator tt;
				Parm *parm_current = 0;
				List *tparms = SwigType_parmlist(fname);
				ffname = SwigType_templateprefix(fname);
				Append(ffname,"<(");
				for (tt = First(tparms); tt.item; ) {
				  SwigType *rtt = Swig_symbol_typedef_reduce(tt.item,0);
				  SwigType *ttr = Swig_symbol_type_qualify(rtt,0);

				  Parm *newp = NewParmWithoutFileLineInfo(ttr, 0);
				  if (partialparms)
				    set_nextSibling(parm_current, newp);
				  else
				    partialparms = newp;
				  parm_current = newp;

				  Append(ffname,ttr);
				  tt = Next(tt);
				  if (tt.item) Putc(',',ffname);
				  Delete(rtt);
				  Delete(ttr);
				}
				Delete(tparms);
				Append(ffname,")>");
			      }
			      {
				/* Replace each primary template parameter's name and value with $ variables, such as, class Y,class T=Y => class $1,class $2=$1 */
				ParmList *primary_templateparms_copy = CopyParmList(primary_templateparms);
				p = primary_templateparms_copy;
				i = 0;
				while (p) {
				  String *name = Getattr(p, "name");
				  Parm *pp = nextSibling(p);
				  ++i;
				  sprintf(tmp, "$%d", i);
				  while (pp) {
				    Replaceid(Getattr(pp, "value"), name, tmp);
				    pp = nextSibling(pp);
				  }
				  Setattr(p, "name", NewString(tmp));
				  p = nextSibling(p);
				}
				/* Modify partialparms by adding in missing default values ($ variables) from primary template parameters */
				partialparms = Swig_cparse_template_partialargs_expand(partialparms, tempn, primary_templateparms_copy);
				Delete(primary_templateparms_copy);
			      }
			      {
				Node *new_partial = NewHash();
				String *partials = Getattr(tempn,"partials");
				if (!partials) {
				  partials = NewList();
				  Setattr(tempn,"partials",partials);
				  Delete(partials);
				}
				/*			      Printf(stdout,"partial: fname = '%s', '%s'\n", fname, Swig_symbol_typedef_reduce(fname,0)); */
				Setattr(new_partial, "partialparms", partialparms);
				Setattr(new_partial, "templcsymname", ffname);
				Append(partials, new_partial);
			      }
			      Setattr((yyval.node),"partialargs",ffname);
			      Swig_symbol_cadd(ffname,(yyval.node));
			    }
			    Delete(tlist);
			    Delete(targs);
			  } else {
			    /* An explicit template specialization */
			    /* add default args from primary (unspecialized) template */
			    String *ty = Swig_symbol_template_deftype(tname,0);
			    String *fname = Swig_symbol_type_qualify(ty,0);
			    Swig_symbol_cadd(fname,(yyval.node));
			    Delete(ty);
			    Delete(fname);
			  }
			} else if ((yyval.node)) {
			  Setattr((yyval.node), "templatetype", nodeType((yyval.node)));
			  set_nodeType((yyval.node),"template");
			  Setattr((yyval.node),"templateparms", (yyvsp[-3].tparms));
			  if (!Getattr((yyval.node),"sym:weak")) {
			    Setattr((yyval.node),"sym:typename","1");
			  }
			  add_symbols((yyval.node));
			  default_arguments((yyval.node));
			  /* We also place a fully parameterized version in the symbol table */
			  {
			    Parm *p;
			    String *fname = NewStringf("%s<(", Getattr((yyval.node),"name"));
			    p = (yyvsp[-3].tparms);
			    while (p) {
			      String *n = Getattr(p,"name");
			      if (!n) n = Getattr(p,"type");
			      Append(fname,n);
			      p = nextSibling(p);
			      if (p) Putc(',',fname);
			    }
			    Append(fname,")>");
			    Swig_symbol_cadd(fname,(yyval.node));
			  }
			}
			(yyval.node) = ntop;
			Swig_symbol_setscope(cscope);
			Delete(Namespaceprefix);
			Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			if (error || (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0)) {
			  (yyval.node) = 0;
			}
			if (currentOuterClass)
			  template_parameters = Getattr(currentOuterClass, "template_parameters");
			else
			  template_parameters = 0;
			parsing_template_declaration = 0;
                }
#line 8495 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 170: /* cpp_template_decl: TEMPLATE cpptype idcolon  */
#line 4691 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
		}
#line 8504 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 171: /* cpp_template_decl: TEMPLATE cpp_alternate_rettype idcolon LPAREN parms RPAREN  */
#line 4697 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                             {
			Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
		}
#line 8513 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 172: /* cpp_template_decl: attribute EXTERN TEMPLATE cpptype idcolon  */
#line 4703 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
		  Swig_warning(WARN_PARSE_EXTERN_TEMPLATE, cparse_file, cparse_line, "Extern template ignored.\n");
                  (yyval.node) = 0; 
                }
#line 8522 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 173: /* cpp_template_decl: attribute EXTERN TEMPLATE cpp_alternate_rettype idcolon LPAREN parms RPAREN  */
#line 4709 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                              {
			Swig_warning(WARN_PARSE_EXTERN_TEMPLATE, cparse_file, cparse_line, "Extern template ignored.\n");
                  (yyval.node) = 0; 
		}
#line 8531 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 177: /* cpp_template_possible: cpp_template_decl  */
#line 4718 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		  (yyval.node) = 0;
                }
#line 8539 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 180: /* template_parms: template_parms_builder  */
#line 4725 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		 (yyval.tparms) = (yyvsp[0].pbuilder).parms;
	       }
#line 8547 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 181: /* template_parms: %empty  */
#line 4728 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		 (yyval.tparms) = 0;
	       }
#line 8555 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 182: /* template_parms_builder: templateparameter  */
#line 4733 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		    (yyval.pbuilder).parms = (yyval.pbuilder).last = (yyvsp[0].p);
		  }
#line 8563 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 183: /* template_parms_builder: template_parms_builder COMMA templateparameter  */
#line 4736 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                       {
		    // Build a linked list in the order specified, but avoiding
		    // a right recursion rule because "Right recursion uses up
		    // space on the Bison stack in proportion to the number of
		    // elements in the sequence".
		    set_nextSibling((yyvsp[-2].pbuilder).last, (yyvsp[0].p));
		    (yyval.pbuilder).parms = (yyvsp[-2].pbuilder).parms;
		    (yyval.pbuilder).last = (yyvsp[0].p);
		  }
#line 8577 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 184: /* templateparameter: templcpptype def_args  */
#line 4747 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		    (yyval.p) = NewParmWithoutFileLineInfo((yyvsp[-1].type), 0);
		    Setfile((yyval.p), cparse_file);
		    Setline((yyval.p), cparse_line);
		    Setattr((yyval.p), "value", (yyvsp[0].dtype).val);
		    if ((yyvsp[0].dtype).stringval) Setattr((yyval.p), "stringval", (yyvsp[0].dtype).stringval);
		    if ((yyvsp[0].dtype).numval) Setattr((yyval.p), "numval", (yyvsp[0].dtype).numval);
		  }
#line 8590 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 185: /* templateparameter: TEMPLATE LESSTHAN template_parms GREATERTHAN cpptype idcolon def_args  */
#line 4755 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                          {
		    (yyval.p) = NewParmWithoutFileLineInfo(NewStringf("template< %s > %s %s", ParmList_str_defaultargs((yyvsp[-4].tparms)), (yyvsp[-2].type), (yyvsp[-1].str)), (yyvsp[-1].str));
		    Setfile((yyval.p), cparse_file);
		    Setline((yyval.p), cparse_line);
		    if ((yyvsp[0].dtype).val) {
		      Setattr((yyval.p), "value", (yyvsp[0].dtype).val);
		    }
		  }
#line 8603 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 186: /* templateparameter: TEMPLATE LESSTHAN template_parms GREATERTHAN cpptype def_args  */
#line 4763 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                  {
		    (yyval.p) = NewParmWithoutFileLineInfo(NewStringf("template< %s > %s", ParmList_str_defaultargs((yyvsp[-3].tparms)), (yyvsp[-1].type)), 0);
		    Setfile((yyval.p), cparse_file);
		    Setline((yyval.p), cparse_line);
		    if ((yyvsp[0].dtype).val) {
		      Setattr((yyval.p), "value", (yyvsp[0].dtype).val);
		    }
		  }
#line 8616 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 187: /* templateparameter: parm  */
#line 4771 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		    Parm *p = (yyvsp[0].p);
		    String *name = Getattr(p, "name");
		    (yyval.p) = (yyvsp[0].p);

		    /* Correct the 'type name' parameter string, split into the appropriate "name" and "type" attributes */
		    if (!name) {
		      String *type = Getattr(p, "type");
		      if ((Strncmp(type, "class ", 6) == 0) || (Strncmp(type, "typename ", 9) == 0)) {
			/* A 'class T' parameter */
			const char *t = Strchr(type, ' ');
			Setattr(p, "name", t + 1);
			Setattr(p, "type", NewStringWithSize(type, (int)(t - Char(type))));
		      } else if ((Strncmp(type, "v.class ", 8) == 0) || (Strncmp(type, "v.typename ", 11) == 0)) {
			/* Variadic template args */
			const char *t = Strchr(type, ' ');
			Setattr(p, "name", t + 1);
			Setattr(p, "type", NewStringWithSize(type, (int)(t - Char(type))));
		      }
		    }
                  }
#line 8642 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 188: /* cpp_using_decl: USING idcolon SEMI  */
#line 4796 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
                  String *uname = Swig_symbol_type_qualify((yyvsp[-1].str),0);
                  /* Possible TODO: In testcase using_member_multiple_inherit class Susing3, uname is "Susing1::usingmethod" instead of "Susing2::usingmethod" */
		  String *name = Swig_scopename_last((yyvsp[-1].str));
                  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",uname);
		  Setattr((yyval.node),"name", name);
		  Delete(uname);
		  Delete(name);
		  add_symbols((yyval.node));
             }
#line 8658 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 189: /* cpp_using_decl: USING TYPENAME idcolon SEMI  */
#line 4807 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		  String *uname = Swig_symbol_type_qualify((yyvsp[-1].str),0);
		  String *name = Swig_scopename_last((yyvsp[-1].str));
		  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",uname);
		  Setattr((yyval.node),"name", name);
		  Delete(uname);
		  Delete(name);
		  add_symbols((yyval.node));
	     }
#line 8673 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 190: /* cpp_using_decl: USING NAMESPACE idcolon SEMI  */
#line 4817 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
	       Node *n = Swig_symbol_clookup((yyvsp[-1].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Nothing known about namespace '%s'\n", SwigType_namestr((yyvsp[-1].str)));
		 (yyval.node) = 0;
	       } else {

		 while (Strcmp(nodeType(n),"using") == 0) {
		   n = Getattr(n,"node");
		 }
		 if (n) {
		   if (Strcmp(nodeType(n),"namespace") == 0) {
		     Symtab *current = Swig_symbol_current();
		     Symtab *symtab = Getattr(n,"symtab");
		     (yyval.node) = new_node("using");
		     Setattr((yyval.node),"node",n);
		     Setattr((yyval.node),"namespace", (yyvsp[-1].str));
		     if (current != symtab) {
		       Swig_symbol_inherit(symtab);
		     }
		   } else {
		     Swig_error(cparse_file, cparse_line, "'%s' is not a namespace.\n", SwigType_namestr((yyvsp[-1].str)));
		     (yyval.node) = 0;
		   }
		 } else {
		   (yyval.node) = 0;
		 }
	       }
             }
#line 8707 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 191: /* @8: %empty  */
#line 4848 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
                Hash *h;
		Node *parent_ns = 0;
		List *scopes = Swig_scopename_tolist((yyvsp[-1].str));
		int ilen = Len(scopes);
		int i;

/*
Printf(stdout, "==== Namespace %s creation...\n", $idcolon);
*/
		(yyval.node) = 0;
		for (i = 0; i < ilen; i++) {
		  Node *ns = new_node("namespace");
		  Symtab *current_symtab = Swig_symbol_current();
		  String *scopename = Getitem(scopes, i);
		  Setattr(ns, "name", scopename);
		  (yyval.node) = ns;
		  if (parent_ns)
		    appendChild(parent_ns, ns);
		  parent_ns = ns;
		  h = Swig_symbol_clookup(scopename, 0);
		  if (h && (current_symtab == Getattr(h, "sym:symtab")) && (Strcmp(nodeType(h), "namespace") == 0)) {
/*
Printf(stdout, "  Scope %s [found C++17 style]\n", scopename);
*/
		    if (Getattr(h, "alias")) {
		      h = Getattr(h, "namespace");
		      Swig_warning(WARN_PARSE_NAMESPACE_ALIAS, cparse_file, cparse_line, "Namespace alias '%s' not allowed here. Assuming '%s'\n",
				   scopename, Getattr(h, "name"));
		      scopename = Getattr(h, "name");
		    }
		    Swig_symbol_setscope(Getattr(h, "symtab"));
		  } else {
/*
Printf(stdout, "  Scope %s [creating single scope C++17 style]\n", scopename);
*/
		    h = Swig_symbol_newscope();
		    Swig_symbol_setscopename(scopename);
		  }
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		}
		Delete(scopes);
             }
#line 8756 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 192: /* cpp_namespace_decl: NAMESPACE idcolon LBRACE @8 interface RBRACE  */
#line 4891 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
		Node *n = (yyvsp[-2].node);
		Node *top_ns = 0;
		do {
		  Setattr(n, "symtab", Swig_symbol_popscope());
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  add_symbols(n);
		  top_ns = n;
		  n = parentNode(n);
		} while(n);
		appendChild((yyvsp[-2].node), firstChild((yyvsp[-1].node)));
		Delete((yyvsp[-1].node));
		(yyval.node) = top_ns;
             }
#line 8776 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 193: /* @9: %empty  */
#line 4906 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
	       Hash *h;
	       (yyval.node) = Swig_symbol_current();
	       h = Swig_symbol_clookup("    ",0);
	       if (h && (Strcmp(nodeType(h),"namespace") == 0)) {
		 Swig_symbol_setscope(Getattr(h,"symtab"));
	       } else {
		 Swig_symbol_newscope();
		 /* we don't use "__unnamed__", but a long 'empty' name */
		 Swig_symbol_setscopename("    ");
	       }
	       Namespaceprefix = 0;
             }
#line 8794 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 194: /* cpp_namespace_decl: NAMESPACE LBRACE @9 interface RBRACE  */
#line 4918 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
	       (yyval.node) = (yyvsp[-1].node);
	       set_nodeType((yyval.node),"namespace");
	       Setattr((yyval.node),"unnamed","1");
	       Setattr((yyval.node),"symtab", Swig_symbol_popscope());
	       Swig_symbol_setscope((yyvsp[-2].node));
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       add_symbols((yyval.node));
             }
#line 8809 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 195: /* cpp_namespace_decl: NAMESPACE identifier EQUAL idcolon SEMI  */
#line 4928 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       {
	       /* Namespace alias */
	       Node *n;
	       (yyval.node) = new_node("namespace");
	       Setattr((yyval.node),"name",(yyvsp[-3].id));
	       Setattr((yyval.node),"alias",(yyvsp[-1].str));
	       n = Swig_symbol_clookup((yyvsp[-1].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Unknown namespace '%s'\n", SwigType_namestr((yyvsp[-1].str)));
		 (yyval.node) = 0;
	       } else {
		 if (Strcmp(nodeType(n),"namespace") != 0) {
		   Swig_error(cparse_file, cparse_line, "'%s' is not a namespace\n", SwigType_namestr((yyvsp[-1].str)));
		   (yyval.node) = 0;
		 } else {
		   while (Getattr(n,"alias")) {
		     n = Getattr(n,"namespace");
		   }
		   Setattr((yyval.node),"namespace",n);
		   add_symbols((yyval.node));
		   /* Set up a scope alias */
		   Swig_symbol_alias((yyvsp[-3].id),Getattr(n,"symtab"));
		 }
	       }
             }
#line 8839 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 196: /* cpp_members: cpp_members_builder  */
#line 4955 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
		 (yyval.node) = (yyvsp[0].nodebuilder).node;
	       }
#line 8847 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 197: /* cpp_members: cpp_members_builder DOXYGENSTRING  */
#line 4958 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
		 /* Quietly ignore misplaced doxygen string after a member, like Doxygen does */
		 (yyval.node) = (yyvsp[-1].nodebuilder).node;
		 Delete((yyvsp[0].str));
	       }
#line 8857 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 198: /* cpp_members: %empty  */
#line 4963 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		 (yyval.node) = 0;
	       }
#line 8865 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 199: /* cpp_members: DOXYGENSTRING  */
#line 4966 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
		 /* Quietly ignore misplaced doxygen string in empty class, like Doxygen does */
		 (yyval.node) = 0;
		 Delete((yyvsp[0].str));
	       }
#line 8875 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 200: /* cpp_members: error  */
#line 4971 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
		 Swig_error(cparse_file, cparse_line, "Syntax error in input(3).\n");
		 Exit(EXIT_FAILURE);
	       }
#line 8884 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 201: /* cpp_members_builder: cpp_member  */
#line 4977 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
	     (yyval.nodebuilder).node = (yyval.nodebuilder).last = (yyvsp[0].node);
	   }
#line 8892 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 202: /* cpp_members_builder: cpp_members_builder cpp_member  */
#line 4980 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                {
	     // Build a linked list in the order specified, but avoiding
	     // a right recursion rule because "Right recursion uses up
	     // space on the Bison stack in proportion to the number of
	     // elements in the sequence".
	     if ((yyvsp[0].node)) {
	       if ((yyvsp[-1].nodebuilder).node) {
		 Node *last = (yyvsp[-1].nodebuilder).last;
		 /* Advance to the last sibling. */
		 for (Node *p = last; p; p = nextSibling(p)) {
		   last = p;
		 }
		 set_nextSibling(last, (yyvsp[0].node));
		 set_previousSibling((yyvsp[0].node), last);
		 (yyval.nodebuilder).node = (yyvsp[-1].nodebuilder).node;
	       } else {
		 (yyval.nodebuilder).node = (yyval.nodebuilder).last = (yyvsp[0].node);
	       }
	     } else {
	       (yyval.nodebuilder) = (yyvsp[-1].nodebuilder);
	     }
	   }
#line 8919 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 204: /* cpp_member_no_dox: cpp_constructor_decl  */
#line 5011 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    { 
                 (yyval.node) = (yyvsp[0].node); 
		 if (extendmode && current_class) {
		   String *symname;
		   symname= make_name((yyval.node),Getattr((yyval.node),"name"), Getattr((yyval.node),"decl"));
		   if (Strcmp(symname,Getattr((yyval.node),"name")) == 0) {
		     /* No renaming operation.  Set name to class name */
		     Delete(yyrename);
		     yyrename = NewString(Getattr(current_class,"sym:name"));
		   } else {
		     Delete(yyrename);
		     yyrename = symname;
		   }
		 }
		 add_symbols((yyval.node));
                 default_arguments((yyval.node));
             }
#line 8941 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 211: /* cpp_member_no_dox: attribute storage_class idcolon SEMI  */
#line 5034 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
                (yyval.node) = 0; Delete((yyvsp[-2].str));
                if ((yyvsp[-3].node)) {
                  Setattr((yyval.node), "attribute", (yyvsp[-3].node));
                }
             }
#line 8952 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 218: /* cpp_member_no_dox: anonymous_bitfield  */
#line 5046 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  { (yyval.node) = 0; }
#line 8958 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 221: /* cpp_member_no_dox: SEMI  */
#line 5049 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                    { (yyval.node) = 0; }
#line 8964 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 223: /* cpp_member: DOXYGENSTRING cpp_member_no_dox  */
#line 5052 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                               {
	         (yyval.node) = (yyvsp[0].node);
		 set_comment((yyvsp[0].node), (yyvsp[-1].str));
	     }
#line 8973 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 224: /* cpp_member: cpp_member_no_dox DOXYGENPOSTSTRING  */
#line 5056 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
	         (yyval.node) = (yyvsp[-1].node);
		 set_comment((yyvsp[-1].node), (yyvsp[0].str));
	     }
#line 8982 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 225: /* $@10: %empty  */
#line 5060 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
	       extendmode = 1;
	       if (cplus_mode != CPLUS_PUBLIC) {
		 Swig_error(cparse_file,cparse_line,"%%extend can only be used in a public section\n");
	       }
	     }
#line 8993 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 226: /* cpp_member: EXTEND LBRACE $@10 cpp_members RBRACE  */
#line 5065 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
	       extendmode = 0;
	       (yyval.node) = new_node("extend");
	       mark_nodes_as_extend((yyvsp[-1].node));
	       appendChild((yyval.node), (yyvsp[-1].node));
	     }
#line 9004 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 227: /* cpp_constructor_decl: attribute storage_class type LPAREN parms RPAREN ctor_end  */
#line 5079 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                 {
	      /* Cannot be a constructor declaration/definition if parsed as a friend destructor/constructor
	         or a badly declared friend function without return type */
	      int isfriend = Strstr((yyvsp[-5].str), "friend") != NULL;
	      if (!isfriend && (inclass || extendmode)) {
	        String *name = SwigType_templateprefix((yyvsp[-4].type)); /* A constructor can optionally be declared with template parameters before C++20, strip these off */
		SwigType *decl = NewStringEmpty();
		(yyval.node) = new_node("constructor");
		Setattr((yyval.node),"storage",(yyvsp[-5].str));
		Setattr((yyval.node), "name", name);
		Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		SwigType_add_function(decl,(yyvsp[-2].pl));
		Setattr((yyval.node),"decl",decl);
		Setattr((yyval.node),"throws",(yyvsp[0].decl).throws);
		Setattr((yyval.node),"throw",(yyvsp[0].decl).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[0].decl).nexcept);
		Setattr((yyval.node),"final",(yyvsp[0].decl).final);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		SetFlag((yyval.node),"feature:new");
		if ((yyvsp[0].decl).defarg)
		  Setattr((yyval.node), "value", (yyvsp[0].decl).defarg);
		if ((yyvsp[0].decl).stringdefarg)
		  Setattr((yyval.node), "stringval", (yyvsp[0].decl).stringdefarg);
		if ((yyvsp[0].decl).numdefarg)
		  Setattr((yyval.node), "numval", (yyvsp[0].decl).numdefarg);
	      } else {
		(yyval.node) = 0;
              }
	      Delete((yyvsp[-5].str));
              if ((yyvsp[-6].node)) {
                Setattr((yyval.node), "attribute", (yyvsp[-6].node));
              }
              }
#line 9046 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 228: /* cpp_destructor_decl: attribute storage_class NOT idtemplate LPAREN parms RPAREN cpp_vend  */
#line 5120 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                          {
	       String *name = SwigType_templateprefix((yyvsp[-4].str)); /* A destructor can optionally be declared with template parameters before C++20, strip these off */
	       Insert(name, 0, "~");
	       (yyval.node) = new_node("destructor");
	       Setattr((yyval.node), "storage", (yyvsp[-6].str));
	       Setattr((yyval.node), "name", name);
	       Delete(name);
	       if (Len(scanner_ccode)) {
		 String *code = Copy(scanner_ccode);
		 Setattr((yyval.node), "code", code);
		 Delete(code);
	       }
	       {
		 String *decl = NewStringEmpty();
		 SwigType_add_function(decl, (yyvsp[-2].pl));
		 Setattr((yyval.node), "decl", decl);
		 Delete(decl);
	       }
	       Setattr((yyval.node), "throws", (yyvsp[0].dtype).throws);
	       Setattr((yyval.node), "throw", (yyvsp[0].dtype).throwf);
	       Setattr((yyval.node), "noexcept", (yyvsp[0].dtype).nexcept);
	       Setattr((yyval.node), "final", (yyvsp[0].dtype).final);
	       if ((yyvsp[0].dtype).val) {
		 if (Equal((yyvsp[0].dtype).val, "0")) {
		   if (!Strstr((yyvsp[-6].str), "virtual"))
		     Swig_error(cparse_file, cparse_line, "Destructor %s uses a pure specifier but is not virtual.\n", Swig_name_decl((yyval.node)));
		 } else if (!(Equal((yyvsp[0].dtype).val, "delete") || Equal((yyvsp[0].dtype).val, "default"))) {
		   Swig_error(cparse_file, cparse_line, "Destructor %s has an invalid pure specifier, only = 0 is allowed.\n", Swig_name_decl((yyval.node)));
		 }
		 Setattr((yyval.node), "value", (yyvsp[0].dtype).val);
	       }
	       /* TODO: check all storage decl-specifiers are valid */
	       if ((yyvsp[0].dtype).qualifier)
		 Swig_error(cparse_file, cparse_line, "Destructor %s %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)), SwigType_str((yyvsp[0].dtype).qualifier, 0));
	       add_symbols((yyval.node));
	       Delete((yyvsp[-6].str));
               if ((yyvsp[-7].node)) {
                 Setattr((yyval.node), "attribute", (yyvsp[-7].node));
               }
	      }
#line 9091 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 229: /* cpp_conversion_operator: attribute storage_class CONVERSIONOPERATOR type pointer LPAREN parms RPAREN cpp_vend  */
#line 5164 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                               {
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].str));

		 SwigType_add_function((yyvsp[-4].type),(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push((yyvsp[-4].type),(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",(yyvsp[-4].type));
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
                 if ((yyvsp[-8].node)) {
                   Setattr((yyval.node), "attribute", (yyvsp[-8].node));
                 }
   		 Delete((yyvsp[-6].str));
		 Delete((yyvsp[-7].str));
              }
#line 9120 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 230: /* cpp_conversion_operator: attribute storage_class CONVERSIONOPERATOR type AND LPAREN parms RPAREN cpp_vend  */
#line 5188 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                  {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].str));
		 decl = NewStringEmpty();
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
                 if ((yyvsp[-8].node)) {
                   Setattr((yyval.node), "attribute", (yyvsp[-8].node));
                 }
		 Delete((yyvsp[-6].str));
		 Delete((yyvsp[-7].str));
	       }
#line 9151 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 231: /* cpp_conversion_operator: attribute storage_class CONVERSIONOPERATOR type LAND LPAREN parms RPAREN cpp_vend  */
#line 5214 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                   {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].str));
		 decl = NewStringEmpty();
		 SwigType_add_rvalue_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
                 if ((yyvsp[-8].node)) {
                   Setattr((yyval.node), "attribute", (yyvsp[-8].node));
                 }
		 Delete((yyvsp[-6].str));
		 Delete((yyvsp[-7].str));
	       }
#line 9182 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 232: /* cpp_conversion_operator: attribute storage_class CONVERSIONOPERATOR type pointer AND LPAREN parms RPAREN cpp_vend  */
#line 5241 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                                          {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-6].type));
		 Setattr((yyval.node),"name",(yyvsp[-7].str));
		 Setattr((yyval.node),"storage",(yyvsp[-8].str));
		 decl = NewStringEmpty();
		 SwigType_add_pointer(decl);
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
                 if ((yyvsp[-9].node)) {
                   Setattr((yyval.node), "attribute", (yyvsp[-9].node));
                 }
		 Delete((yyvsp[-7].str));
		 Delete((yyvsp[-8].str));
	       }
#line 9214 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 233: /* cpp_conversion_operator: attribute storage_class CONVERSIONOPERATOR type LPAREN parms RPAREN cpp_vend  */
#line 5269 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                             {
		String *t = NewStringEmpty();
		(yyval.node) = new_node("cdecl");
		Setattr((yyval.node),"type",(yyvsp[-4].type));
		Setattr((yyval.node),"name",(yyvsp[-5].str));
		 Setattr((yyval.node),"storage",(yyvsp[-6].str));
		SwigType_add_function(t,(yyvsp[-2].pl));
		if ((yyvsp[0].dtype).qualifier) {
		  SwigType_push(t,(yyvsp[0].dtype).qualifier);
		}
		if ((yyvsp[0].dtype).val) {
		  Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		}
		Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		Setattr((yyval.node),"decl",t);
		Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		Setattr((yyval.node),"conversion_operator","1");
		add_symbols((yyval.node));
                if ((yyvsp[-7].node)) {
                  Setattr((yyval.node), "attribute", (yyvsp[-7].node));
                }
		Delete((yyvsp[-5].str));
		Delete((yyvsp[-6].str));
              }
#line 9243 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 234: /* cpp_catch_decl: CATCH LPAREN parms RPAREN LBRACE  */
#line 5297 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                  {
                 if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
                 (yyval.node) = 0;
               }
#line 9252 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 235: /* cpp_static_assert: STATIC_ASSERT LPAREN  */
#line 5305 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
                if (skip_balanced('(',')') < 0) Exit(EXIT_FAILURE);
                (yyval.node) = 0;
              }
#line 9261 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 236: /* cpp_protection_decl: PUBLIC COLON  */
#line 5312 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   { 
                (yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","public");
                cplus_mode = CPLUS_PUBLIC;
              }
#line 9271 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 237: /* cpp_protection_decl: PRIVATE COLON  */
#line 5319 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              { 
                (yyval.node) = new_node("access");
                Setattr((yyval.node),"kind","private");
		cplus_mode = CPLUS_PRIVATE;
	      }
#line 9281 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 238: /* cpp_protection_decl: PROTECTED COLON  */
#line 5327 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                { 
		(yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","protected");
		cplus_mode = CPLUS_PROTECTED;
	      }
#line 9291 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 249: /* cpp_vend: cpp_const SEMI  */
#line 5350 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                { 
                     Clear(scanner_ccode);
                     (yyval.dtype) = (yyvsp[-1].dtype);
               }
#line 9300 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 250: /* cpp_vend: cpp_const EQUAL definetype SEMI  */
#line 5354 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 { 
                     Clear(scanner_ccode);
                     (yyval.dtype) = (yyvsp[-3].dtype);
                     (yyval.dtype).val = (yyvsp[-1].dtype).val;
               }
#line 9310 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 251: /* cpp_vend: cpp_const LBRACE  */
#line 5359 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  { 
                     if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
                     (yyval.dtype) = (yyvsp[-1].dtype);
               }
#line 9319 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 252: /* anonymous_bitfield: attribute storage_class anon_bitfield_type COLON expr SEMI  */
#line 5366 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                 { Delete((yyvsp[-4].str)); }
#line 9325 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 256: /* anon_bitfield_type: idcolon  */
#line 5373 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.type) = (yyvsp[0].str); }
#line 9331 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 257: /* storage_class: storage_class_list  */
#line 5379 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		 String *r = NewStringEmpty();

		 /* Check for invalid combinations. */
		 if (multiple_bits_set((yyvsp[0].intvalue) & (SWIG_STORAGE_CLASS_EXTERN |
					     SWIG_STORAGE_CLASS_STATIC))) {
		   Swig_error(cparse_file, cparse_line, "Storage class can't be both 'static' and 'extern'");
		 }
		 if (multiple_bits_set((yyvsp[0].intvalue) & (SWIG_STORAGE_CLASS_EXTERNC |
					     SWIG_STORAGE_CLASS_EXTERN |
					     SWIG_STORAGE_CLASS_EXTERNCPP))) {
		   Swig_error(cparse_file, cparse_line, "Declaration can only be one of 'extern', 'extern \"C\"' and 'extern \"C++\"'");
		 }

		 if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_TYPEDEF) {
		   Append(r, "typedef ");
		 } else {
		   if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_EXTERNC)
		     Append(r, "externc ");
		   if ((yyvsp[0].intvalue) & (SWIG_STORAGE_CLASS_EXTERN|SWIG_STORAGE_CLASS_EXTERNCPP))
		     Append(r, "extern ");
		   if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_STATIC)
		     Append(r, "static ");
		 }
		 if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_VIRTUAL)
		   Append(r, "virtual ");
		 if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_FRIEND)
		   Append(r, "friend ");
		 if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_EXPLICIT)
		   Append(r, "explicit ");
		 if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_CONSTEXPR)
		   Append(r, "constexpr ");
		 if ((yyvsp[0].intvalue) & SWIG_STORAGE_CLASS_THREAD_LOCAL)
		   Append(r, "thread_local ");
		 if (Len(r) == 0) {
		   Delete(r);
		   (yyval.str) = 0;
		 } else {
		   Chop(r);
		   (yyval.str) = r;
		 }
	       }
#line 9378 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 258: /* storage_class: %empty  */
#line 5421 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.str) = 0; }
#line 9384 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 260: /* storage_class_list: storage_class_list storage_class_raw  */
#line 5425 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                          {
		  if ((yyvsp[-1].intvalue) & (yyvsp[0].intvalue)) {
		    Swig_error(cparse_file, cparse_line, "Repeated storage class or type specifier '%s'\n", storage_class_string((yyvsp[0].intvalue)));
		  }
		  (yyval.intvalue) = (yyvsp[-1].intvalue) | (yyvsp[0].intvalue);
	       }
#line 9395 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 261: /* storage_class_raw: EXTERN  */
#line 5433 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            { (yyval.intvalue) = SWIG_STORAGE_CLASS_EXTERN; }
#line 9401 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 262: /* storage_class_raw: EXTERN string  */
#line 5434 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
		   if (Strcmp((yyvsp[0].str),"C") == 0) {
		     (yyval.intvalue) = SWIG_STORAGE_CLASS_EXTERNC;
		   } else if (Strcmp((yyvsp[0].str),"C++") == 0) {
		     (yyval.intvalue) = SWIG_STORAGE_CLASS_EXTERNCPP;
		   } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[0].str));
		     (yyval.intvalue) = 0;
		   }
	       }
#line 9416 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 263: /* storage_class_raw: STATIC  */
#line 5444 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.intvalue) = SWIG_STORAGE_CLASS_STATIC; }
#line 9422 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 264: /* storage_class_raw: TYPEDEF  */
#line 5445 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.intvalue) = SWIG_STORAGE_CLASS_TYPEDEF; }
#line 9428 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 265: /* storage_class_raw: VIRTUAL  */
#line 5446 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.intvalue) = SWIG_STORAGE_CLASS_VIRTUAL; }
#line 9434 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 266: /* storage_class_raw: FRIEND  */
#line 5447 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.intvalue) = SWIG_STORAGE_CLASS_FRIEND; }
#line 9440 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 267: /* storage_class_raw: EXPLICIT  */
#line 5448 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          { (yyval.intvalue) = SWIG_STORAGE_CLASS_EXPLICIT; }
#line 9446 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 268: /* storage_class_raw: CONSTEXPR  */
#line 5449 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           { (yyval.intvalue) = SWIG_STORAGE_CLASS_CONSTEXPR; }
#line 9452 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 269: /* storage_class_raw: THREAD_LOCAL  */
#line 5450 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              { (yyval.intvalue) = SWIG_STORAGE_CLASS_THREAD_LOCAL; }
#line 9458 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 270: /* parms: rawparms  */
#line 5457 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          {
                 Parm *p;
		 (yyval.pl) = (yyvsp[0].pl);
		 p = (yyvsp[0].pl);
                 while (p) {
		   Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   p = nextSibling(p);
                 }
               }
#line 9472 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 271: /* rawparms: parm  */
#line 5469 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       { (yyval.pl) = (yyvsp[0].p); }
#line 9478 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 272: /* rawparms: parm DOXYGENPOSTSTRING  */
#line 5470 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		  set_comment((yyvsp[-1].p), (yyvsp[0].str));
		  (yyval.pl) = (yyvsp[-1].p);
		}
#line 9487 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 273: /* rawparms: parm DOXYGENSTRING  */
#line 5474 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
		  /* Misplaced doxygen string, attach it to previous parameter, like Doxygen does */
		  set_comment((yyvsp[-1].p), (yyvsp[0].str));
		  (yyval.pl) = (yyvsp[-1].p);
		}
#line 9497 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 274: /* rawparms: parm COMMA parms  */
#line 5479 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   {
		  if ((yyvsp[0].pl)) {
		    set_nextSibling((yyvsp[-2].p), (yyvsp[0].pl));
		  }
		  (yyval.pl) = (yyvsp[-2].p);
		}
#line 9508 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 275: /* rawparms: parm DOXYGENPOSTSTRING COMMA parms  */
#line 5485 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                     {
		  if ((yyvsp[0].pl)) {
		    set_nextSibling((yyvsp[-3].p), (yyvsp[0].pl));
		  }
		  set_comment((yyvsp[-3].p), (yyvsp[-2].str));
		  (yyval.pl) = (yyvsp[-3].p);
		}
#line 9520 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 276: /* rawparms: parm COMMA DOXYGENPOSTSTRING parms  */
#line 5492 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                     {
		  if ((yyvsp[0].pl)) {
		    set_nextSibling((yyvsp[-3].p), (yyvsp[0].pl));
		  }
		  set_comment((yyvsp[-3].p), (yyvsp[-1].str));
		  (yyval.pl) = (yyvsp[-3].p);
		}
#line 9532 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 277: /* rawparms: %empty  */
#line 5499 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		  (yyval.pl) = 0;
		}
#line 9540 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 278: /* parm_no_dox: rawtype parameter_declarator  */
#line 5504 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                               {
                   SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		   (yyval.p) = NewParmWithoutFileLineInfo((yyvsp[-1].type),(yyvsp[0].decl).id);
		   Setfile((yyval.p),cparse_file);
		   Setline((yyval.p),cparse_line);
		   if ((yyvsp[0].decl).defarg)
		     Setattr((yyval.p), "value", (yyvsp[0].decl).defarg);
		   if ((yyvsp[0].decl).stringdefarg)
		     Setattr((yyval.p), "stringval", (yyvsp[0].decl).stringdefarg);
		   if ((yyvsp[0].decl).numdefarg)
		     Setattr((yyval.p), "numval", (yyvsp[0].decl).numdefarg);
		}
#line 9557 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 279: /* parm_no_dox: ELLIPSIS  */
#line 5516 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		  SwigType *t = NewString("v(...)");
		  (yyval.p) = NewParmWithoutFileLineInfo(t, 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		}
#line 9568 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 281: /* parm: attribute_notopt parm_no_dox  */
#line 5525 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                               { (yyval.p) = (yyvsp[0].p); }
#line 9574 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 282: /* parm: DOXYGENSTRING parm_no_dox  */
#line 5526 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		  (yyval.p) = (yyvsp[0].p);
		  set_comment((yyvsp[0].p), (yyvsp[-1].str));
		}
#line 9583 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 283: /* valparms: valparms_builder  */
#line 5532 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
		 (yyval.p) = (yyvsp[0].pbuilder).parms;
                 for (Parm *p = (yyval.p); p; p = nextSibling(p)) {
		   if (Getattr(p,"type")) {
		     Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   }
                 }
	       }
#line 9596 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 284: /* valparms: %empty  */
#line 5540 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		 (yyval.p) = 0;
	       }
#line 9604 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 285: /* valparms_builder: valparm  */
#line 5545 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		    (yyval.pbuilder).parms = (yyval.pbuilder).last = (yyvsp[0].p);
		  }
#line 9612 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 286: /* valparms_builder: valparms_builder COMMA valparm  */
#line 5548 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       {
		    // Build a linked list in the order specified, but avoiding
		    // a right recursion rule because "Right recursion uses up
		    // space on the Bison stack in proportion to the number of
		    // elements in the sequence".
		    set_nextSibling((yyvsp[-2].pbuilder).last, (yyvsp[0].p));
		    (yyval.pbuilder).parms = (yyvsp[-2].pbuilder).parms;
		    (yyval.pbuilder).last = (yyvsp[0].p);
		  }
#line 9626 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 287: /* valparm: parm  */
#line 5559 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                      {
		  (yyval.p) = (yyvsp[0].p);
		  {
		    /* We need to make a possible adjustment for integer parameters. */
		    SwigType *type;
		    Node     *n = 0;

		    while (!n) {
		      type = Getattr((yyvsp[0].p),"type");
		      n = Swig_symbol_clookup(type,0);     /* See if we can find a node that matches the typename */
		      if ((n) && (Strcmp(nodeType(n),"cdecl") == 0)) {
			SwigType *decl = Getattr(n,"decl");
			if (!SwigType_isfunction(decl)) {
			  String *value = Getattr(n,"value");
			  if (value) {
			    String *v = Copy(value);
			    Setattr((yyvsp[0].p),"type",v);
			    Delete(v);
			    n = 0;
			  }
			}
		      } else {
			break;
		      }
		    }
		  }

               }
#line 9659 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 288: /* valparm: valexpr  */
#line 5587 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
                  (yyval.p) = NewParmWithoutFileLineInfo(0,0);
                  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		  Setattr((yyval.p),"value",(yyvsp[0].dtype).val);
		  if ((yyvsp[0].dtype).stringval) Setattr((yyval.p), "stringval", (yyvsp[0].dtype).stringval);
		  if ((yyvsp[0].dtype).numval) Setattr((yyval.p), "numval", (yyvsp[0].dtype).numval);
               }
#line 9672 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 289: /* def_args: EQUAL definetype  */
#line 5597 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  { 
                 (yyval.dtype) = (yyvsp[0].dtype);
               }
#line 9680 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 290: /* def_args: EQUAL definetype LBRACKET  */
#line 5600 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 if (skip_balanced('[', ']') < 0) Exit(EXIT_FAILURE);
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).type = T_UNKNOWN;
		 (yyval.dtype).val = (yyvsp[-1].dtype).val;
		 Append((yyval.dtype).val, scanner_ccode);
		 Clear(scanner_ccode);
               }
#line 9693 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 291: /* def_args: EQUAL LBRACE  */
#line 5608 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
		 if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewString(scanner_ccode);
		 (yyval.dtype).type = T_UNKNOWN;
	       }
#line 9704 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 292: /* def_args: %empty  */
#line 5614 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		 (yyval.dtype) = default_dtype;
                 (yyval.dtype).type = T_UNKNOWN;
               }
#line 9713 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 293: /* parameter_declarator: declarator def_args  */
#line 5620 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
                 (yyval.decl) = (yyvsp[-1].decl);
		 (yyval.decl).defarg = (yyvsp[0].dtype).val;
		 (yyval.decl).stringdefarg = (yyvsp[0].dtype).stringval;
		 (yyval.decl).numdefarg = (yyvsp[0].dtype).numval;
            }
#line 9724 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 294: /* parameter_declarator: abstract_declarator def_args  */
#line 5626 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
	      (yyval.decl) = (yyvsp[-1].decl);
	      (yyval.decl).defarg = (yyvsp[0].dtype).val;
	      (yyval.decl).stringdefarg = (yyvsp[0].dtype).stringval;
	      (yyval.decl).numdefarg = (yyvsp[0].dtype).numval;
            }
#line 9735 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 295: /* parameter_declarator: def_args  */
#line 5632 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
	      (yyval.decl) = default_decl;
	      (yyval.decl).defarg = (yyvsp[0].dtype).val;
	      (yyval.decl).stringdefarg = (yyvsp[0].dtype).stringval;
	      (yyval.decl).numdefarg = (yyvsp[0].dtype).numval;
            }
#line 9746 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 296: /* parameter_declarator: direct_declarator LPAREN parms RPAREN qualifiers_exception_specification  */
#line 5640 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                       {
	      SwigType *t;
	      (yyval.decl) = (yyvsp[-4].decl);
	      t = NewStringEmpty();
	      SwigType_add_function(t,(yyvsp[-2].pl));
	      if ((yyvsp[0].dtype).qualifier)
		SwigType_push(t, (yyvsp[0].dtype).qualifier);
	      if ((yyvsp[0].dtype).nexcept)
		SwigType_add_qualifier(t, "noexcept");
	      if (!(yyval.decl).have_parms) {
		(yyval.decl).parms = (yyvsp[-2].pl);
		(yyval.decl).have_parms = 1;
	      }
	      if (!(yyval.decl).type) {
		(yyval.decl).type = t;
	      } else {
		SwigType_push(t, (yyval.decl).type);
		Delete((yyval.decl).type);
		(yyval.decl).type = t;
	      }
	    }
#line 9772 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 297: /* plain_declarator: declarator  */
#line 5663 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
                 (yyval.decl) = (yyvsp[0].decl);
		 if (SwigType_isfunction((yyvsp[0].decl).type)) {
		   Delete(SwigType_pop_function((yyvsp[0].decl).type));
		 } else if (SwigType_isarray((yyvsp[0].decl).type)) {
		   SwigType *ta = SwigType_pop_arrays((yyvsp[0].decl).type);
		   if (SwigType_isfunction((yyvsp[0].decl).type)) {
		     Delete(SwigType_pop_function((yyvsp[0].decl).type));
		   } else {
		     (yyval.decl).parms = 0;
		   }
		   SwigType_push((yyvsp[0].decl).type,ta);
		   Delete(ta);
		 } else {
		   (yyval.decl).parms = 0;
		 }
            }
#line 9794 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 298: /* plain_declarator: abstract_declarator  */
#line 5680 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
              (yyval.decl) = (yyvsp[0].decl);
	      if (SwigType_isfunction((yyvsp[0].decl).type)) {
		Delete(SwigType_pop_function((yyvsp[0].decl).type));
	      } else if (SwigType_isarray((yyvsp[0].decl).type)) {
		SwigType *ta = SwigType_pop_arrays((yyvsp[0].decl).type);
		if (SwigType_isfunction((yyvsp[0].decl).type)) {
		  Delete(SwigType_pop_function((yyvsp[0].decl).type));
		} else {
		  (yyval.decl).parms = 0;
		}
		SwigType_push((yyvsp[0].decl).type,ta);
		Delete(ta);
	      } else {
		(yyval.decl).parms = 0;
	      }
            }
#line 9816 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 299: /* plain_declarator: direct_declarator LPAREN parms RPAREN cv_ref_qualifier  */
#line 5699 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                     {
	      SwigType *t;
	      (yyval.decl) = (yyvsp[-4].decl);
	      t = NewStringEmpty();
	      SwigType_add_function(t, (yyvsp[-2].pl));
	      if ((yyvsp[0].dtype).qualifier)
	        SwigType_push(t, (yyvsp[0].dtype).qualifier);
	      if (!(yyval.decl).have_parms) {
		(yyval.decl).parms = (yyvsp[-2].pl);
		(yyval.decl).have_parms = 1;
	      }
	      if (!(yyval.decl).type) {
		(yyval.decl).type = t;
	      } else {
		SwigType_push(t, (yyval.decl).type);
		Delete((yyval.decl).type);
		(yyval.decl).type = t;
	      }
	    }
#line 9840 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 300: /* plain_declarator: %empty  */
#line 5718 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                     {
	      (yyval.decl) = default_decl;
	    }
#line 9848 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 301: /* declarator: pointer notso_direct_declarator  */
#line 5723 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
              (yyval.decl) = (yyvsp[0].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[-1].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-1].type);
           }
#line 9861 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 302: /* declarator: pointer AND notso_direct_declarator  */
#line 5731 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_reference((yyvsp[-2].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 9875 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 303: /* declarator: pointer LAND notso_direct_declarator  */
#line 5740 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                  {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_rvalue_reference((yyvsp[-2].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 9889 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 304: /* declarator: direct_declarator  */
#line 5749 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
              (yyval.decl) = (yyvsp[0].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
#line 9898 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 305: /* declarator: AND notso_direct_declarator  */
#line 5753 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 9912 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 306: /* declarator: LAND notso_direct_declarator  */
#line 5762 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 9928 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 307: /* declarator: idcolon DSTAR notso_direct_declarator  */
#line 5773 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-2].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
#line 9944 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 308: /* declarator: pointer idcolon DSTAR notso_direct_declarator  */
#line 5784 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                           { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-2].str));
	     SwigType_push((yyvsp[-3].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-3].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-3].type);
	     Delete(t);
	   }
#line 9961 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 309: /* declarator: pointer idcolon DSTAR AND notso_direct_declarator  */
#line 5796 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                               { 
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-4].type),(yyvsp[-3].str));
	     SwigType_add_reference((yyvsp[-4].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-4].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-4].type);
	   }
#line 9976 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 310: /* declarator: idcolon DSTAR AND notso_direct_declarator  */
#line 5806 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 9992 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 311: /* declarator: pointer ELLIPSIS notso_direct_declarator  */
#line 5820 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       {
              (yyval.decl) = (yyvsp[0].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
	      SwigType_add_variadic((yyval.decl).type);
           }
#line 10006 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 312: /* declarator: pointer AND ELLIPSIS notso_direct_declarator  */
#line 5829 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                          {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_reference((yyvsp[-3].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-3].type);
	      SwigType_add_variadic((yyval.decl).type);
           }
#line 10021 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 313: /* declarator: pointer LAND ELLIPSIS notso_direct_declarator  */
#line 5839 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                           {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_rvalue_reference((yyvsp[-3].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-3].type);
	      SwigType_add_variadic((yyval.decl).type);
           }
#line 10036 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 314: /* declarator: AND ELLIPSIS notso_direct_declarator  */
#line 5849 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                  {
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     SwigType_add_variadic((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 10051 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 315: /* declarator: LAND ELLIPSIS notso_direct_declarator  */
#line 5859 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     SwigType_add_variadic((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 10068 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 316: /* declarator: idcolon DSTAR ELLIPSIS notso_direct_declarator  */
#line 5871 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     SwigType_add_variadic(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
#line 10085 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 317: /* declarator: pointer idcolon DSTAR ELLIPSIS notso_direct_declarator  */
#line 5883 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     SwigType_add_variadic(t);
	     SwigType_push((yyvsp[-4].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-4].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-4].type);
	     Delete(t);
	   }
#line 10103 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 318: /* declarator: pointer idcolon DSTAR AND ELLIPSIS notso_direct_declarator  */
#line 5896 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                        {
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-5].type),(yyvsp[-4].str));
	     SwigType_add_reference((yyvsp[-5].type));
	     SwigType_add_variadic((yyvsp[-5].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-5].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-5].type);
	   }
#line 10119 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 319: /* declarator: pointer idcolon DSTAR LAND ELLIPSIS notso_direct_declarator  */
#line 5907 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                         {
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-5].type),(yyvsp[-4].str));
	     SwigType_add_rvalue_reference((yyvsp[-5].type));
	     SwigType_add_variadic((yyvsp[-5].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-5].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-5].type);
	   }
#line 10135 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 320: /* declarator: idcolon DSTAR AND ELLIPSIS notso_direct_declarator  */
#line 5918 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                {
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-4].str));
	     SwigType_add_reference(t);
	     SwigType_add_variadic(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 10152 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 321: /* declarator: idcolon DSTAR LAND ELLIPSIS notso_direct_declarator  */
#line 5930 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 {
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-4].str));
	     SwigType_add_rvalue_reference(t);
	     SwigType_add_variadic(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 10169 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 322: /* notso_direct_declarator: idcolon  */
#line 5944 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
		 (yyval.decl) = default_decl;
                 (yyval.decl).id = Char((yyvsp[0].str));
                  }
#line 10179 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 323: /* notso_direct_declarator: NOT idcolon  */
#line 5949 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
		  (yyval.decl) = default_decl;
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[0].str)));
                  }
#line 10188 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 324: /* notso_direct_declarator: LPAREN idcolon RPAREN  */
#line 5955 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		  (yyval.decl) = default_decl;
                  (yyval.decl).id = Char((yyvsp[-1].str));
                  }
#line 10197 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 325: /* notso_direct_declarator: LPAREN pointer notso_direct_declarator RPAREN  */
#line 5967 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                      {
		    (yyval.decl) = (yyvsp[-1].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10210 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 326: /* notso_direct_declarator: LPAREN idcolon DSTAR notso_direct_declarator RPAREN  */
#line 5975 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                            {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
#line 10226 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 327: /* notso_direct_declarator: notso_direct_declarator LBRACKET RBRACKET  */
#line 5986 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                  { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10242 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 328: /* notso_direct_declarator: notso_direct_declarator LBRACKET expr RBRACKET  */
#line 5997 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                       { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10258 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 329: /* notso_direct_declarator: notso_direct_declarator LPAREN parms RPAREN  */
#line 6008 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 10280 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 330: /* direct_declarator: idcolon  */
#line 6027 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
		 (yyval.decl) = default_decl;
                 (yyval.decl).id = Char((yyvsp[0].str));
                  }
#line 10290 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 331: /* direct_declarator: NOT idcolon  */
#line 6033 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
		  (yyval.decl) = default_decl;
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[0].str)));
                  }
#line 10299 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 332: /* direct_declarator: LPAREN pointer direct_declarator RPAREN  */
#line 6046 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                {
		    (yyval.decl) = (yyvsp[-1].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10312 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 333: /* direct_declarator: LPAREN AND direct_declarator RPAREN  */
#line 6054 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
                    (yyval.decl) = (yyvsp[-1].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_reference((yyval.decl).type);
                  }
#line 10324 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 334: /* direct_declarator: LPAREN LAND direct_declarator RPAREN  */
#line 6061 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                             {
                    (yyval.decl) = (yyvsp[-1].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
#line 10336 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 335: /* direct_declarator: LPAREN idcolon DSTAR declarator RPAREN  */
#line 6068 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                           {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10352 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 336: /* direct_declarator: LPAREN idcolon DSTAR type_qualifier declarator RPAREN  */
#line 6079 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                          {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t, (yyvsp[-4].str));
		    SwigType_push(t, (yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10369 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 337: /* direct_declarator: LPAREN idcolon DSTAR abstract_declarator RPAREN  */
#line 6091 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t, (yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10385 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 338: /* direct_declarator: LPAREN idcolon DSTAR type_qualifier abstract_declarator RPAREN  */
#line 6102 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                   {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t, (yyvsp[-4].str));
		    SwigType_push(t, (yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10402 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 339: /* direct_declarator: direct_declarator LBRACKET RBRACKET  */
#line 6114 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10418 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 340: /* direct_declarator: direct_declarator LBRACKET expr RBRACKET  */
#line 6125 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10434 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 341: /* direct_declarator: direct_declarator LPAREN parms RPAREN  */
#line 6136 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                              {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 10456 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 342: /* direct_declarator: OPERATOR ID LPAREN parms RPAREN  */
#line 6156 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
		    (yyval.decl) = default_decl;
		    SwigType *t;
                    Append((yyvsp[-4].str), " "); /* intervening space is mandatory */
		    Append((yyvsp[-4].str), (yyvsp[-3].id));
		    (yyval.decl).id = Char((yyvsp[-4].str));
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    (yyval.decl).parms = (yyvsp[-1].pl);
		    (yyval.decl).have_parms = 1;
		    (yyval.decl).type = t;
		  }
#line 10473 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 343: /* abstract_declarator: pointer variadic_opt  */
#line 6170 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = (yyvsp[-1].type);
		    if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.decl).type);
                  }
#line 10483 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 344: /* abstract_declarator: pointer direct_abstract_declarator  */
#line 6175 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       { 
                     (yyval.decl) = (yyvsp[0].decl);
                     SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		     (yyval.decl).type = (yyvsp[-1].type);
		     Delete((yyvsp[0].decl).type);
                  }
#line 10494 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 345: /* abstract_declarator: pointer AND variadic_opt  */
#line 6181 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = (yyvsp[-2].type);
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.decl).type);
		  }
#line 10505 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 346: /* abstract_declarator: pointer LAND variadic_opt  */
#line 6187 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = (yyvsp[-2].type);
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.decl).type);
		  }
#line 10516 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 347: /* abstract_declarator: pointer AND direct_abstract_declarator  */
#line 6193 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                           {
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_reference((yyvsp[-2].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10530 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 348: /* abstract_declarator: pointer LAND direct_abstract_declarator  */
#line 6202 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                            {
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_rvalue_reference((yyvsp[-2].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10544 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 350: /* abstract_declarator: AND direct_abstract_declarator  */
#line 6212 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
		    (yyval.decl) = (yyvsp[0].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[0].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
		      Delete((yyvsp[0].decl).type);
		    }
                  }
#line 10558 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 351: /* abstract_declarator: LAND direct_abstract_declarator  */
#line 6221 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
		    (yyval.decl) = (yyvsp[0].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    if ((yyvsp[0].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
		      Delete((yyvsp[0].decl).type);
		    }
                  }
#line 10572 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 352: /* abstract_declarator: AND variadic_opt  */
#line 6230 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
		    (yyval.decl) = default_decl;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.decl).type);
                  }
#line 10583 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 353: /* abstract_declarator: LAND variadic_opt  */
#line 6236 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
		    (yyval.decl) = default_decl;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.decl).type);
                  }
#line 10594 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 354: /* abstract_declarator: idcolon DSTAR  */
#line 6242 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  { 
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_memberpointer((yyval.decl).type,(yyvsp[-1].str));
      	          }
#line 10604 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 355: /* abstract_declarator: idcolon DSTAR type_qualifier  */
#line 6247 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 {
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_memberpointer((yyval.decl).type, (yyvsp[-2].str));
		    SwigType_push((yyval.decl).type, (yyvsp[0].str));
		  }
#line 10615 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 356: /* abstract_declarator: pointer idcolon DSTAR  */
#line 6253 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          { 
		    (yyval.decl) = default_decl;
		    SwigType *t = NewStringEmpty();
                    (yyval.decl).type = (yyvsp[-2].type);
		    SwigType_add_memberpointer(t,(yyvsp[-1].str));
		    SwigType_push((yyval.decl).type,t);
		    Delete(t);
                  }
#line 10628 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 357: /* abstract_declarator: pointer idcolon DSTAR direct_abstract_declarator  */
#line 6261 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                     { 
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_memberpointer((yyvsp[-3].type),(yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-3].type);
                  }
#line 10642 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 358: /* direct_abstract_declarator: direct_abstract_declarator LBRACKET RBRACKET  */
#line 6272 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                              { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10658 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 359: /* direct_abstract_declarator: direct_abstract_declarator LBRACKET expr RBRACKET  */
#line 6283 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                          { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10674 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 360: /* direct_abstract_declarator: LBRACKET RBRACKET  */
#line 6294 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      { 
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_array((yyval.decl).type,"");
                  }
#line 10684 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 361: /* direct_abstract_declarator: LBRACKET expr RBRACKET  */
#line 6299 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           { 
		    (yyval.decl) = default_decl;
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_array((yyval.decl).type,(yyvsp[-1].dtype).val);
		  }
#line 10694 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 362: /* direct_abstract_declarator: LPAREN abstract_declarator RPAREN  */
#line 6304 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
                    (yyval.decl) = (yyvsp[-1].decl);
		  }
#line 10702 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 363: /* direct_abstract_declarator: direct_abstract_declarator LPAREN parms RPAREN  */
#line 6307 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                       {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
#line 10724 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 364: /* direct_abstract_declarator: direct_abstract_declarator LPAREN parms RPAREN cv_ref_qualifier  */
#line 6324 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                        {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-4].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[-2].pl));
		    SwigType_push(t, (yyvsp[0].dtype).qualifier);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-2].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
#line 10747 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 365: /* direct_abstract_declarator: LPAREN parms RPAREN  */
#line 6342 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		    (yyval.decl) = default_decl;
                    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_function((yyval.decl).type,(yyvsp[-1].pl));
		    (yyval.decl).parms = (yyvsp[-1].pl);
		    (yyval.decl).have_parms = 1;
                  }
#line 10759 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 366: /* pointer: pointer STAR type_qualifier  */
#line 6352 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
	     (yyval.type) = (yyvsp[-2].type);
             SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[0].str));
           }
#line 10769 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 367: /* pointer: pointer STAR  */
#line 6357 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
	     (yyval.type) = (yyvsp[-1].type);
	     SwigType_add_pointer((yyval.type));
	   }
#line 10778 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 368: /* pointer: STAR type_qualifier  */
#line 6361 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[0].str));
           }
#line 10788 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 369: /* pointer: STAR  */
#line 6366 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                  {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
           }
#line 10797 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 370: /* cv_ref_qualifier: type_qualifier  */
#line 6373 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
		  (yyval.dtype).qualifier = (yyvsp[0].str);
	       }
#line 10805 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 371: /* cv_ref_qualifier: type_qualifier ref_qualifier  */
#line 6376 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
		  (yyval.dtype).qualifier = (yyvsp[-1].str);
		  (yyval.dtype).refqualifier = (yyvsp[0].str);
		  SwigType_push((yyval.dtype).qualifier, (yyvsp[0].str));
	       }
#line 10815 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 372: /* cv_ref_qualifier: ref_qualifier  */
#line 6381 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
		  (yyval.dtype).qualifier = NewStringEmpty();
		  (yyval.dtype).refqualifier = (yyvsp[0].str);
		  SwigType_push((yyval.dtype).qualifier, (yyvsp[0].str));
	       }
#line 10825 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 373: /* ref_qualifier: AND  */
#line 6388 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                    {
	          (yyval.str) = NewStringEmpty();
	          SwigType_add_reference((yyval.str));
	       }
#line 10834 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 374: /* ref_qualifier: LAND  */
#line 6392 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                      {
	          (yyval.str) = NewStringEmpty();
	          SwigType_add_rvalue_reference((yyval.str));
	       }
#line 10843 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 375: /* type_qualifier: type_qualifier_raw  */
#line 6398 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
	          (yyval.str) = NewStringEmpty();
	          if ((yyvsp[0].id)) SwigType_add_qualifier((yyval.str),(yyvsp[0].id));
               }
#line 10852 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 376: /* type_qualifier: type_qualifier type_qualifier_raw  */
#line 6402 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       {
		  (yyval.str) = (yyvsp[-1].str);
	          if ((yyvsp[0].id)) SwigType_add_qualifier((yyval.str),(yyvsp[0].id));
               }
#line 10861 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 377: /* type_qualifier_raw: CONST_QUAL  */
#line 6408 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 { (yyval.id) = "const"; }
#line 10867 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 378: /* type_qualifier_raw: VOLATILE  */
#line 6409 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               { (yyval.id) = "volatile"; }
#line 10873 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 379: /* type_qualifier_raw: REGISTER  */
#line 6410 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               { (yyval.id) = 0; }
#line 10879 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 380: /* type: rawtype  */
#line 6416 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
                   (yyval.type) = (yyvsp[0].type);
                   Replace((yyval.type),"typename ","", DOH_REPLACE_ANY);
                }
#line 10888 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 381: /* rawtype: type_qualifier type_right  */
#line 6422 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
                   (yyval.type) = (yyvsp[0].type);
	           SwigType_push((yyval.type),(yyvsp[-1].str));
               }
#line 10897 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 383: /* rawtype: type_right type_qualifier  */
#line 6427 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		  (yyval.type) = (yyvsp[-1].type);
	          SwigType_push((yyval.type),(yyvsp[0].str));
	       }
#line 10906 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 384: /* rawtype: type_qualifier type_right type_qualifier  */
#line 6431 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                            {
		  (yyval.type) = (yyvsp[-1].type);
	          SwigType_push((yyval.type),(yyvsp[0].str));
	          SwigType_push((yyval.type),(yyvsp[-2].str));
	       }
#line 10916 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 385: /* rawtype: rawtype ELLIPSIS  */
#line 6436 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
		  (yyval.type) = (yyvsp[-1].type);
		  SwigType_add_variadic((yyval.type));
	       }
#line 10925 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 389: /* type_right: c_enum_key idcolon  */
#line 6445 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    { (yyval.type) = NewStringf("enum %s", (yyvsp[0].str)); }
#line 10931 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 390: /* type_right: idcolon  */
#line 6447 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   {
		  (yyval.type) = (yyvsp[0].str);
               }
#line 10939 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 391: /* type_right: cpptype idcolon  */
#line 6450 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 (yyval.type) = NewStringf("%s %s", (yyvsp[-1].type), (yyvsp[0].str));
               }
#line 10947 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 393: /* @11: %empty  */
#line 6456 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
		 (yyval.str) = get_raw_text_balanced('(', ')');
	       }
#line 10955 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 394: /* decltype: DECLTYPE LPAREN @11 decltypeexpr  */
#line 6458 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		 String *expr = (yyvsp[-1].str);
		 if ((yyvsp[0].type)) {
		   (yyval.type) = (yyvsp[0].type);
		 } else {
		   (yyval.type) = NewStringf("decltype%s", expr);
		   /* expr includes parentheses but don't include them in the warning message. */
		   Delitem(expr, 0);
		   Delitem(expr, DOH_END);
		   Swig_warning(WARN_CPP11_DECLTYPE, cparse_file, cparse_line, "Unable to deduce decltype for '%s'.\n", expr);
		 }
		 Delete(expr);
	       }
#line 10973 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 395: /* decltypeexpr: expr RPAREN  */
#line 6473 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		 (yyval.type) = deduce_type(&(yyvsp[-1].dtype));
	       }
#line 10981 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 396: /* decltypeexpr: error RPAREN  */
#line 6476 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
		 /* Avoid a parse error if we can't parse the expression
		  * decltype() is applied to.
		  *
		  * Set $$ to 0 here to trigger the decltype rule above to
		  * issue a warning.
		  */
		 (yyval.type) = 0;
		 if (skip_balanced('(',')') < 0) Exit(EXIT_FAILURE);
		 Clear(scanner_ccode);
	       }
#line 10997 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 397: /* primitive_type: primitive_type_list  */
#line 6489 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
		 String *type = (yyvsp[0].ptype).type;
		 if (!type) type = NewString("int");
		 if ((yyvsp[0].ptype).us) {
		   (yyval.type) = NewStringf("%s %s", (yyvsp[0].ptype).us, type);
		   Delete((yyvsp[0].ptype).us);
                   Delete(type);
		 } else {
                   (yyval.type) = type;
		 }
		 if (Cmp((yyval.type),"signed int") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("int");
                 } else if (Cmp((yyval.type),"signed long") == 0) {
		   Delete((yyval.type));
                   (yyval.type) = NewString("long");
                 } else if (Cmp((yyval.type),"signed short") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("short");
		 } else if (Cmp((yyval.type),"signed long long") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("long long");
		 }
               }
#line 11026 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 399: /* primitive_type_list: type_specifier primitive_type_list  */
#line 6516 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                        {
                    if ((yyvsp[-1].ptype).us && (yyvsp[0].ptype).us) {
		      Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[0].ptype).us);
		    }
                    (yyval.ptype) = (yyvsp[0].ptype);
                    if ((yyvsp[-1].ptype).us) (yyval.ptype).us = (yyvsp[-1].ptype).us;
		    if ((yyvsp[-1].ptype).type) {
		      if (!(yyvsp[0].ptype).type) (yyval.ptype).type = (yyvsp[-1].ptype).type;
		      else {
			int err = 0;
			if ((Cmp((yyvsp[-1].ptype).type,"long") == 0)) {
			  if ((Cmp((yyvsp[0].ptype).type,"long") == 0) || (Strncmp((yyvsp[0].ptype).type,"double",6) == 0)) {
			    (yyval.ptype).type = NewStringf("long %s", (yyvsp[0].ptype).type);
			  } else if (Cmp((yyvsp[0].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[-1].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if ((Cmp((yyvsp[-1].ptype).type,"short")) == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[-1].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"int") == 0) {
			  (yyval.ptype).type = (yyvsp[0].ptype).type;
			} else if (Cmp((yyvsp[-1].ptype).type,"double") == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"long") == 0) {
			    (yyval.ptype).type = NewString("long double");
			  } else if (Cmp((yyvsp[0].ptype).type,"_Complex") == 0) {
			    (yyval.ptype).type = NewString("double _Complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"float") == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"_Complex") == 0) {
			    (yyval.ptype).type = NewString("float _Complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"_Complex") == 0) {
			  (yyval.ptype).type = NewStringf("%s _Complex", (yyvsp[0].ptype).type);
			} else {
			  err = 1;
			}
			if (err) {
			  Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[-1].ptype).type);
			}
		      }
		    }
               }
#line 11082 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 400: /* type_specifier: TYPE_INT  */
#line 6570 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          { 
		    (yyval.ptype).type = NewString("int");
                    (yyval.ptype).us = 0;
               }
#line 11091 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 401: /* type_specifier: TYPE_SHORT  */
#line 6574 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            { 
                    (yyval.ptype).type = NewString("short");
                    (yyval.ptype).us = 0;
                }
#line 11100 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 402: /* type_specifier: TYPE_LONG  */
#line 6578 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           { 
                    (yyval.ptype).type = NewString("long");
                    (yyval.ptype).us = 0;
                }
#line 11109 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 403: /* type_specifier: TYPE_CHAR  */
#line 6582 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           { 
                    (yyval.ptype).type = NewString("char");
                    (yyval.ptype).us = 0;
                }
#line 11118 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 404: /* type_specifier: TYPE_WCHAR  */
#line 6586 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            { 
                    (yyval.ptype).type = NewString("wchar_t");
                    (yyval.ptype).us = 0;
                }
#line 11127 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 405: /* type_specifier: TYPE_FLOAT  */
#line 6590 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            { 
                    (yyval.ptype).type = NewString("float");
                    (yyval.ptype).us = 0;
                }
#line 11136 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 406: /* type_specifier: TYPE_DOUBLE  */
#line 6594 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             { 
                    (yyval.ptype).type = NewString("double");
                    (yyval.ptype).us = 0;
                }
#line 11145 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 407: /* type_specifier: TYPE_SIGNED  */
#line 6598 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             { 
                    (yyval.ptype).us = NewString("signed");
                    (yyval.ptype).type = 0;
                }
#line 11154 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 408: /* type_specifier: TYPE_UNSIGNED  */
#line 6602 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               { 
                    (yyval.ptype).us = NewString("unsigned");
                    (yyval.ptype).type = 0;
                }
#line 11163 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 409: /* type_specifier: TYPE_COMPLEX  */
#line 6606 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              { 
                    (yyval.ptype).type = NewString("_Complex");
                    (yyval.ptype).us = 0;
                }
#line 11172 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 410: /* type_specifier: TYPE_NON_ISO_INT8  */
#line 6610 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   { 
                    (yyval.ptype).type = NewString("__int8");
                    (yyval.ptype).us = 0;
                }
#line 11181 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 411: /* type_specifier: TYPE_NON_ISO_INT16  */
#line 6614 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    { 
                    (yyval.ptype).type = NewString("__int16");
                    (yyval.ptype).us = 0;
                }
#line 11190 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 412: /* type_specifier: TYPE_NON_ISO_INT32  */
#line 6618 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    { 
                    (yyval.ptype).type = NewString("__int32");
                    (yyval.ptype).us = 0;
                }
#line 11199 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 413: /* type_specifier: TYPE_NON_ISO_INT64  */
#line 6622 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    { 
                    (yyval.ptype).type = NewString("__int64");
                    (yyval.ptype).us = 0;
                }
#line 11208 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 418: /* deleted_definition: DELETE_KW  */
#line 6637 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewString("delete");
		  (yyval.dtype).type = T_STRING;
		}
#line 11218 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 419: /* explicit_default: DEFAULT  */
#line 6645 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewString("default");
		  (yyval.dtype).type = T_STRING;
		}
#line 11228 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 421: /* ename: %empty  */
#line 6655 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.id) = 0; }
#line 11234 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 426: /* enumlist: enumlist_item  */
#line 6674 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
		  Setattr((yyvsp[0].node),"_last",(yyvsp[0].node));
		  (yyval.node) = (yyvsp[0].node);
		}
#line 11243 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 427: /* enumlist: enumlist_item DOXYGENPOSTSTRING  */
#line 6678 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                  {
		  Setattr((yyvsp[-1].node),"_last",(yyvsp[-1].node));
		  set_comment((yyvsp[-1].node), (yyvsp[0].str));
		  (yyval.node) = (yyvsp[-1].node);
		}
#line 11253 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 428: /* enumlist: enumlist_item DOXYGENSTRING  */
#line 6683 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
		  Setattr((yyvsp[-1].node), "_last", (yyvsp[-1].node));
		  /* Misplaced doxygen string, attach it to previous parameter, like Doxygen does */
		  set_comment((yyvsp[-1].node), (yyvsp[0].str));
		  (yyval.node) = (yyvsp[-1].node);
		}
#line 11264 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 429: /* enumlist: enumlist_item COMMA enumlist  */
#line 6689 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
		  if ((yyvsp[0].node)) {
		    set_nextSibling((yyvsp[-2].node), (yyvsp[0].node));
		    Setattr((yyvsp[-2].node),"_last",Getattr((yyvsp[0].node),"_last"));
		    Setattr((yyvsp[0].node),"_last",NULL);
		  } else {
		    Setattr((yyvsp[-2].node),"_last",(yyvsp[-2].node));
		  }
		  (yyval.node) = (yyvsp[-2].node);
		}
#line 11279 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 430: /* enumlist: enumlist_item DOXYGENPOSTSTRING COMMA enumlist  */
#line 6699 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                     {
		  if ((yyvsp[0].node)) {
		    set_nextSibling((yyvsp[-3].node), (yyvsp[0].node));
		    Setattr((yyvsp[-3].node),"_last",Getattr((yyvsp[0].node),"_last"));
		    Setattr((yyvsp[0].node),"_last",NULL);
		  } else {
		    Setattr((yyvsp[-3].node),"_last",(yyvsp[-3].node));
		  }
		  set_comment((yyvsp[-3].node), (yyvsp[-2].str));
		  (yyval.node) = (yyvsp[-3].node);
		}
#line 11295 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 431: /* enumlist: enumlist_item COMMA DOXYGENPOSTSTRING enumlist  */
#line 6710 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                     {
		  if ((yyvsp[0].node)) {
		    set_nextSibling((yyvsp[-3].node), (yyvsp[0].node));
		    Setattr((yyvsp[-3].node),"_last",Getattr((yyvsp[0].node),"_last"));
		    Setattr((yyvsp[0].node),"_last",NULL);
		  } else {
		    Setattr((yyvsp[-3].node),"_last",(yyvsp[-3].node));
		  }
		  set_comment((yyvsp[-3].node), (yyvsp[-1].str));
		  (yyval.node) = (yyvsp[-3].node);
		}
#line 11311 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 432: /* enumlist: optional_ignored_defines  */
#line 6721 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		  (yyval.node) = 0;
		}
#line 11319 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 433: /* enumlist_item: optional_ignored_defines edecl_with_dox optional_ignored_defines  */
#line 6726 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                   {
		  (yyval.node) = (yyvsp[-1].node);
		}
#line 11327 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 434: /* edecl_with_dox: edecl attribute  */
#line 6731 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
                  (yyval.node) = (yyvsp[-1].node);
                  if ((yyvsp[0].node)) {
                    Setattr((yyval.node), "attribute", (yyvsp[0].node));
                  }
                }
#line 11338 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 435: /* edecl_with_dox: DOXYGENSTRING edecl attribute  */
#line 6737 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                {
		  (yyval.node) = (yyvsp[-1].node);
		  set_comment((yyvsp[-1].node), (yyvsp[-2].str));
                  if ((yyvsp[0].node)) {
                    Setattr((yyval.node), "attribute", (yyvsp[0].node));
                  }
		}
#line 11350 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 436: /* edecl: identifier  */
#line 6746 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		   SwigType *type = NewSwigType(T_INT);
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[0].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Delete(type);
		 }
#line 11363 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 437: /* edecl: identifier EQUAL etype  */
#line 6754 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		   SwigType *type = NewSwigType((yyvsp[0].dtype).type == T_BOOL ? T_BOOL : ((yyvsp[0].dtype).type == T_CHAR ? T_CHAR : T_INT));
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[-2].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Setattr((yyval.node),"enumvalue", (yyvsp[0].dtype).val);
		   if ((yyvsp[0].dtype).stringval) {
		     Setattr((yyval.node), "enumstringval", (yyvsp[0].dtype).stringval);
		   }
		   if ((yyvsp[0].dtype).numval) {
		     Setattr((yyval.node), "enumnumval", (yyvsp[0].dtype).numval);
		   }
		   Setattr((yyval.node),"value",(yyvsp[-2].id));
		   Delete(type);
                 }
#line 11384 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 438: /* etype: expr  */
#line 6772 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
                   (yyval.dtype) = (yyvsp[0].dtype);
		   /* We get T_USER here for a typedef - unfortunately we can't
		    * currently resolve typedefs at this stage of parsing. */
		   if (((yyval.dtype).type != T_INT) && ((yyval.dtype).type != T_UINT) &&
		       ((yyval.dtype).type != T_LONG) && ((yyval.dtype).type != T_ULONG) &&
		       ((yyval.dtype).type != T_LONGLONG) && ((yyval.dtype).type != T_ULONGLONG) &&
		       ((yyval.dtype).type != T_SHORT) && ((yyval.dtype).type != T_USHORT) &&
		       ((yyval.dtype).type != T_SCHAR) && ((yyval.dtype).type != T_UCHAR) &&
		       ((yyval.dtype).type != T_CHAR) && ((yyval.dtype).type != T_BOOL) &&
		       ((yyval.dtype).type != T_UNKNOWN) && ((yyval.dtype).type != T_USER)) {
		     Swig_error(cparse_file,cparse_line,"Type error. Expecting an integral type\n");
		   }
                }
#line 11403 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 440: /* expr: type  */
#line 6791 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                      {
		 Node *n;
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = (yyvsp[0].type);
		 (yyval.dtype).type = T_UNKNOWN;
		 /* Check if value is in scope */
		 n = Swig_symbol_clookup((yyvsp[0].type),0);
		 if (n) {
                   /* A band-aid for enum values used in expressions. */
                   if (Strcmp(nodeType(n),"enumitem") == 0) {
                     String *q = Swig_symbol_qualified(n);
                     if (q) {
                       (yyval.dtype).val = NewStringf("%s::%s", q, Getattr(n,"name"));
		       (yyval.dtype).type = SwigType_type(Getattr(n, "type"));
                       Delete(q);
                     }
		   } else {
		     SwigType *type = Getattr(n, "type");
		     if (type) {
		       (yyval.dtype).type = SwigType_type(type);
		     }
		   }
		 }
               }
#line 11432 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 441: /* exprmem: ID ARROW ID  */
#line 6818 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s->%s", (yyvsp[-2].id), (yyvsp[0].id));
	       }
#line 11441 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 442: /* exprmem: ID ARROW ID LPAREN  */
#line 6822 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
		 if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s->%s", (yyvsp[-3].id), (yyvsp[-1].id));
		 append_expr_from_scanner((yyval.dtype).val);
	       }
#line 11452 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 443: /* exprmem: exprmem ARROW ID  */
#line 6828 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
		 (yyval.dtype) = (yyvsp[-2].dtype);
		 Printf((yyval.dtype).val, "->%s", (yyvsp[0].id));
	       }
#line 11461 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 444: /* exprmem: exprmem ARROW ID LPAREN  */
#line 6832 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		 if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		 (yyval.dtype) = (yyvsp[-3].dtype);
		 Printf((yyval.dtype).val, "->%s", (yyvsp[-1].id));
		 append_expr_from_scanner((yyval.dtype).val);
	       }
#line 11472 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 445: /* exprmem: ID PERIOD ID  */
#line 6838 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s.%s", (yyvsp[-2].id), (yyvsp[0].id));
	       }
#line 11481 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 446: /* exprmem: ID PERIOD ID LPAREN  */
#line 6842 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                               {
		 if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s.%s", (yyvsp[-3].id), (yyvsp[-1].id));
		 append_expr_from_scanner((yyval.dtype).val);
	       }
#line 11492 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 447: /* exprmem: exprmem PERIOD ID  */
#line 6848 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		 (yyval.dtype) = (yyvsp[-2].dtype);
		 Printf((yyval.dtype).val, ".%s", (yyvsp[0].id));
	       }
#line 11501 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 448: /* exprmem: exprmem PERIOD ID LPAREN  */
#line 6852 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                              {
		 if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		 (yyval.dtype) = (yyvsp[-3].dtype);
		 Printf((yyval.dtype).val, ".%s", (yyvsp[-1].id));
		 append_expr_from_scanner((yyval.dtype).val);
	       }
#line 11512 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 451: /* exprsimple: string  */
#line 6863 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).stringval = (yyvsp[0].str);
		  (yyval.dtype).val = NewStringf("\"%(escape)s\"", (yyvsp[0].str));
		  (yyval.dtype).type = T_STRING;
	       }
#line 11523 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 452: /* exprsimple: wstring  */
#line 6869 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).stringval = (yyvsp[0].str);
		  (yyval.dtype).val = NewStringf("L\"%(escape)s\"", (yyvsp[0].str));
		  (yyval.dtype).type = T_WSTRING;
	       }
#line 11534 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 453: /* exprsimple: CHARCONST  */
#line 6875 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).stringval = (yyvsp[0].str);
		  (yyval.dtype).val = NewStringf("'%(escape)s'", (yyvsp[0].str));
		  (yyval.dtype).type = T_CHAR;
	       }
#line 11545 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 454: /* exprsimple: WCHARCONST  */
#line 6881 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).stringval = (yyvsp[0].str);
		  (yyval.dtype).val = NewStringf("L'%(escape)s'", (yyvsp[0].str));
		  (yyval.dtype).type = T_WCHAR;
	       }
#line 11556 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 455: /* exprsimple: SIZEOF LPAREN  */
#line 6893 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
		  if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewString("sizeof");
		  append_expr_from_scanner((yyval.dtype).val);
		  (yyval.dtype).type = T_ULONG;
               }
#line 11568 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 456: /* exprsimple: ALIGNOF LPAREN  */
#line 6901 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
		  if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewString("alignof");
		  append_expr_from_scanner((yyval.dtype).val);
		  (yyval.dtype).type = T_ULONG;
	       }
#line 11580 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 457: /* exprsimple: NOEXCEPT LPAREN  */
#line 6909 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
		  if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewString("noexcept");
		  append_expr_from_scanner((yyval.dtype).val);
		  (yyval.dtype).type = T_BOOL;
	       }
#line 11592 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 458: /* exprsimple: SIZEOF ELLIPSIS LPAREN identifier RPAREN  */
#line 6916 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                          {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewStringf("sizeof...(%s)", (yyvsp[-1].id));
		  (yyval.dtype).type = T_ULONG;
               }
#line 11602 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 459: /* exprsimple: SIZEOF exprsimple  */
#line 6926 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		  (yyval.dtype) = default_dtype;
		  (yyval.dtype).val = NewStringf("sizeof(%s)", (yyvsp[0].dtype).val);
		  (yyval.dtype).type = T_ULONG;
	       }
#line 11612 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 462: /* valexpr: LPAREN expr RPAREN  */
#line 6937 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                {
	            (yyval.dtype) = default_dtype;
		    (yyval.dtype).val = NewStringf("(%s)",(yyvsp[-1].dtype).val);
		    (yyval.dtype).stringval = Copy((yyvsp[-1].dtype).stringval);
		    (yyval.dtype).numval = Copy((yyvsp[-1].dtype).numval);
		    (yyval.dtype).type = (yyvsp[-1].dtype).type;
	       }
#line 11624 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 463: /* valexpr: LPAREN expr RPAREN expr  */
#line 6947 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                              {
		 int cast_type_code = SwigType_type((yyvsp[-2].dtype).val);
		 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).unary_arg_type = 0;
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   switch ((yyvsp[-2].dtype).type) {
		     case T_FLOAT:
		     case T_DOUBLE:
		     case T_LONGDOUBLE:
		     case T_FLTCPLX:
		     case T_DBLCPLX:
		       (yyval.dtype).val = NewStringf("(%s)%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val); /* SwigType_str and decimal points don't mix! */
		       break;
		     default:
		       (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-2].dtype).val,0), (yyvsp[0].dtype).val);
		       break;
		   }
		   (yyval.dtype).stringval = 0;
		   (yyval.dtype).numval = 0;
		 }
		 /* As well as C-style casts, this grammar rule currently also
		  * matches a binary operator with a LHS in parentheses for
		  * binary operators which also have an unary form, e.g.:
		  *
		  * (6)*7
		  * (6)&7
		  * (6)+7
		  * (6)-7
		  */
		 if (cast_type_code != T_USER && cast_type_code != T_UNKNOWN) {
		   /* $lhs is definitely a type so we know this is a cast. */
		   (yyval.dtype).type = cast_type_code;
		 } else if ((yyvsp[0].dtype).type == 0 || (yyvsp[0].dtype).unary_arg_type == 0) {
		   /* Not one of the cases above, so we know this is a cast. */
		   (yyval.dtype).type = cast_type_code;
		 } else {
		   (yyval.dtype).type = promote((yyvsp[-2].dtype).type, (yyvsp[0].dtype).unary_arg_type);
		 }
 	       }
#line 11668 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 464: /* valexpr: LPAREN expr pointer RPAREN expr  */
#line 6986 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                      {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).unary_arg_type = 0;
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-3].dtype).val,(yyvsp[-2].type));
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		   (yyval.dtype).stringval = 0;
		   (yyval.dtype).numval = 0;
		 }
 	       }
#line 11683 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 465: /* valexpr: LPAREN expr AND RPAREN expr  */
#line 6996 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                  {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).unary_arg_type = 0;
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_add_reference((yyvsp[-3].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		   (yyval.dtype).stringval = 0;
		   (yyval.dtype).numval = 0;
		 }
 	       }
#line 11698 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 466: /* valexpr: LPAREN expr LAND RPAREN expr  */
#line 7006 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                   {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).unary_arg_type = 0;
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_add_rvalue_reference((yyvsp[-3].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		   (yyval.dtype).stringval = 0;
		   (yyval.dtype).numval = 0;
		 }
 	       }
#line 11713 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 467: /* valexpr: LPAREN expr pointer AND RPAREN expr  */
#line 7016 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                          {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).unary_arg_type = 0;
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-4].dtype).val,(yyvsp[-3].type));
		   SwigType_add_reference((yyvsp[-4].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-4].dtype).val,0), (yyvsp[0].dtype).val);
		   (yyval.dtype).stringval = 0;
		   (yyval.dtype).numval = 0;
		 }
 	       }
#line 11729 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 468: /* valexpr: LPAREN expr pointer LAND RPAREN expr  */
#line 7027 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                           {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).unary_arg_type = 0;
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-4].dtype).val,(yyvsp[-3].type));
		   SwigType_add_rvalue_reference((yyvsp[-4].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-4].dtype).val,0), (yyvsp[0].dtype).val);
		   (yyval.dtype).stringval = 0;
		   (yyval.dtype).numval = 0;
		 }
 	       }
#line 11745 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 469: /* valexpr: AND expr  */
#line 7038 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          {
		 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).val = NewStringf("&%s", (yyvsp[0].dtype).val);
		 (yyval.dtype).stringval = 0;
		 (yyval.dtype).numval = 0;
		 /* Record the type code for expr so we can properly handle
		  * cases such as (6)&7 which get parsed using this rule then
		  * the rule for a C-style cast.
		  */
		 (yyval.dtype).unary_arg_type = (yyvsp[0].dtype).type;
		 switch ((yyval.dtype).type) {
		   case T_CHAR:
		     (yyval.dtype).type = T_STRING;
		     break;
		   case T_WCHAR:
		     (yyval.dtype).type = T_WSTRING;
		     break;
		   default:
		     (yyval.dtype).type = T_POINTER;
		 }
	       }
#line 11771 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 470: /* valexpr: STAR expr  */
#line 7059 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		 (yyval.dtype) = (yyvsp[0].dtype);
		 (yyval.dtype).val = NewStringf("*%s", (yyvsp[0].dtype).val);
		 (yyval.dtype).stringval = 0;
		 (yyval.dtype).numval = 0;
		 /* Record the type code for expr so we can properly handle
		  * cases such as (6)*7 which get parsed using this rule then
		  * the rule for a C-style cast.
		  */
		 (yyval.dtype).unary_arg_type = (yyvsp[0].dtype).type;
		 switch ((yyval.dtype).type) {
		   case T_STRING:
		     (yyval.dtype).type = T_CHAR;
		     break;
		   case T_WSTRING:
		     (yyval.dtype).type = T_WCHAR;
		     break;
		   default:
		     (yyval.dtype).type = T_UNKNOWN;
		 }
	       }
#line 11797 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 481: /* exprcompound: expr PLUS expr  */
#line 7094 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s+%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11807 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 482: /* exprcompound: expr MINUS expr  */
#line 7099 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s-%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11817 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 483: /* exprcompound: expr STAR expr  */
#line 7104 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s*%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11827 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 484: /* exprcompound: expr SLASH expr  */
#line 7109 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s/%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11837 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 485: /* exprcompound: expr MODULO expr  */
#line 7114 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s%%%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11847 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 486: /* exprcompound: expr AND expr  */
#line 7119 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s&%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11857 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 487: /* exprcompound: expr OR expr  */
#line 7124 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s|%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11867 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 488: /* exprcompound: expr XOR expr  */
#line 7129 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s^%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 11877 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 489: /* exprcompound: expr LSHIFT expr  */
#line 7134 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s << %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 11887 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 490: /* exprcompound: expr RSHIFT expr  */
#line 7139 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s >> %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 11897 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 491: /* exprcompound: expr LAND expr  */
#line 7144 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s&&%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11907 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 492: /* exprcompound: expr LOR expr  */
#line 7149 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s||%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11917 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 493: /* exprcompound: expr EQUALTO expr  */
#line 7154 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s==%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11927 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 494: /* exprcompound: expr NOTEQUALTO expr  */
#line 7159 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s!=%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11937 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 495: /* exprcompound: LPAREN expr GREATERTHAN expr RPAREN  */
#line 7168 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                               {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("(%s > %s)", (yyvsp[-3].dtype).val, (yyvsp[-1].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11947 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 496: /* exprcompound: LPAREN exprsimple LESSTHAN expr RPAREN  */
#line 7179 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                  {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("(%s < %s)", (yyvsp[-3].dtype).val, (yyvsp[-1].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11957 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 497: /* exprcompound: expr GREATERTHANOREQUALTO expr  */
#line 7184 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                          {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s >= %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11967 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 498: /* exprcompound: expr LESSTHANOREQUALTO expr  */
#line 7189 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s <= %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 11977 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 499: /* exprcompound: expr PLUS ELLIPSIS  */
#line 7206 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s+...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 11987 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 500: /* exprcompound: expr MINUS ELLIPSIS  */
#line 7211 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s-...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 11997 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 501: /* exprcompound: expr STAR ELLIPSIS  */
#line 7216 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s*...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12007 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 502: /* exprcompound: expr SLASH ELLIPSIS  */
#line 7221 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s/...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12017 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 503: /* exprcompound: expr MODULO ELLIPSIS  */
#line 7226 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s%%...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12027 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 504: /* exprcompound: expr AND ELLIPSIS  */
#line 7231 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s&...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12037 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 505: /* exprcompound: expr OR ELLIPSIS  */
#line 7236 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s|...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12047 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 506: /* exprcompound: expr XOR ELLIPSIS  */
#line 7241 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s^...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12057 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 507: /* exprcompound: expr LSHIFT ELLIPSIS  */
#line 7246 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s << ...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12067 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 508: /* exprcompound: expr RSHIFT ELLIPSIS  */
#line 7251 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s >> ...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12077 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 509: /* exprcompound: expr LAND ELLIPSIS  */
#line 7256 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s&&...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12087 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 510: /* exprcompound: expr LOR ELLIPSIS  */
#line 7261 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s||...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12097 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 511: /* exprcompound: expr EQUALTO ELLIPSIS  */
#line 7266 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s==...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12107 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 512: /* exprcompound: expr NOTEQUALTO ELLIPSIS  */
#line 7271 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                               {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s!=...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12117 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 513: /* exprcompound: LPAREN expr GREATERTHAN ELLIPSIS RPAREN  */
#line 7280 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                              {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("(%s > ...)", (yyvsp[-3].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12127 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 514: /* exprcompound: LPAREN exprsimple LESSTHAN ELLIPSIS RPAREN  */
#line 7290 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                 {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("(%s < %s)", (yyvsp[-3].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12137 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 515: /* exprcompound: expr GREATERTHANOREQUALTO ELLIPSIS  */
#line 7295 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                         {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s >= ...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12147 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 516: /* exprcompound: expr LESSTHANOREQUALTO ELLIPSIS  */
#line 7300 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s <= ...", (yyvsp[-2].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12157 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 517: /* exprcompound: expr LESSEQUALGREATER expr  */
#line 7306 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s <=> %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 /* `<=>` returns one of `std::strong_ordering`,
		  * `std::partial_ordering` or `std::weak_ordering`.  The main
		  * thing to do with the return value in this context is to
		  * compare it with another ordering of the same type or
		  * with a literal 0.  We set .type = T_USER here which does
		  * what we want for the comparison operators, and also means
		  * that deduce_type() won't deduce a type for this (which is
		  * better than it deducing the wrong type).
		  */
		 (yyval.dtype).type = T_USER;
		 (yyval.dtype).unary_arg_type = 0;
	       }
#line 12177 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 518: /* exprcompound: expr QUESTIONMARK expr COLON expr  */
#line 7321 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                                           {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("%s?%s:%s", (yyvsp[-4].dtype).val, (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 /* This may not be exactly right, but is probably good enough
		  * for the purposes of parsing constant expressions. */
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type, (yyvsp[0].dtype).type);
	       }
#line 12189 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 519: /* exprcompound: MINUS expr  */
#line 7328 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("-%s",(yyvsp[0].dtype).val);
		 if ((yyvsp[0].dtype).numval) {
		   switch ((yyvsp[0].dtype).type) {
		     case T_CHAR: // Unsigned on some architectures.
		     case T_UCHAR:
		     case T_USHORT:
		     case T_UINT:
		     case T_ULONG:
		     case T_ULONGLONG:
		       // Avoid negative numval with an unsigned type.
		       break;
		     default:
		       (yyval.dtype).numval = NewStringf("-%s", (yyvsp[0].dtype).numval);
		       break;
		   }
		   Delete((yyvsp[0].dtype).numval);
		 }
		 (yyval.dtype).type = promote_type((yyvsp[0].dtype).type);
		 /* Record the type code for expr so we can properly handle
		  * cases such as (6)-7 which get parsed using this rule then
		  * the rule for a C-style cast.
		  */
		 (yyval.dtype).unary_arg_type = (yyvsp[0].dtype).type;
	       }
#line 12220 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 520: /* exprcompound: PLUS expr  */
#line 7354 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		 (yyval.dtype) = default_dtype;
                 (yyval.dtype).val = NewStringf("+%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).numval = (yyvsp[0].dtype).numval;
		 (yyval.dtype).type = promote_type((yyvsp[0].dtype).type);
		 /* Record the type code for expr so we can properly handle
		  * cases such as (6)+7 which get parsed using this rule then
		  * the rule for a C-style cast.
		  */
		 (yyval.dtype).unary_arg_type = (yyvsp[0].dtype).type;
	       }
#line 12236 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 521: /* exprcompound: NOT expr  */
#line 7365 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
		 (yyval.dtype) = default_dtype;
		 (yyval.dtype).val = NewStringf("~%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[0].dtype).type);
	       }
#line 12246 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 522: /* exprcompound: LNOT expr  */
#line 7370 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                               {
		 (yyval.dtype) = default_dtype;
                 (yyval.dtype).val = NewStringf("!%s", (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12256 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 523: /* exprcompound: type LPAREN  */
#line 7375 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
		 (yyval.dtype) = default_dtype;
		 if (skip_balanced('(',')') < 0) Exit(EXIT_FAILURE);

		 String *qty = Swig_symbol_type_qualify((yyvsp[-1].type), 0);
		 if (SwigType_istemplate(qty)) {
		   String *nstr = SwigType_namestr(qty);
		   Delete(qty);
		   qty = nstr;
		 }
		 /* Try to deduce the type - this could be a C++ "constructor
		  * cast" such as `double(4)` or a function call such as
		  * `some_func()`.  In the latter case we get T_USER, but that
		  * is wrong so we map it to T_UNKNOWN until we can actually
		  * deduce the return type of a function call (which is
		  * complicated because the return type can vary between
		  * overloaded forms).
		  */
		 (yyval.dtype).type = SwigType_type(qty);
		 if ((yyval.dtype).type == T_USER) (yyval.dtype).type = T_UNKNOWN;
		 (yyval.dtype).unary_arg_type = 0;

		 (yyval.dtype).val = qty;
		 append_expr_from_scanner((yyval.dtype).val);
               }
#line 12286 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 524: /* variadic_opt: ELLIPSIS  */
#line 7402 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		(yyval.intvalue) = 1;
	      }
#line 12294 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 525: /* variadic_opt: %empty  */
#line 7405 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
	        (yyval.intvalue) = 0;
	      }
#line 12302 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 527: /* $@12: %empty  */
#line 7413 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { inherit_list = 1; }
#line 12308 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 528: /* raw_inherit: COLON $@12 base_list  */
#line 7413 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                        { (yyval.bases) = (yyvsp[0].bases); inherit_list = 0; }
#line 12314 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 529: /* raw_inherit: %empty  */
#line 7414 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.bases) = 0; }
#line 12320 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 530: /* base_list: base_specifier  */
#line 7417 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
		   Hash *list = NewHash();
		   Node *base = (yyvsp[0].node);
		   Node *name = Getattr(base,"name");
		   List *lpublic = NewList();
		   List *lprotected = NewList();
		   List *lprivate = NewList();
		   Setattr(list,"public",lpublic);
		   Setattr(list,"protected",lprotected);
		   Setattr(list,"private",lprivate);
		   Delete(lpublic);
		   Delete(lprotected);
		   Delete(lprivate);
		   Append(Getattr(list,Getattr(base,"access")),name);
	           (yyval.bases) = list;
               }
#line 12341 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 531: /* base_list: base_list COMMA base_specifier  */
#line 7434 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
		   Hash *list = (yyvsp[-2].bases);
		   Node *base = (yyvsp[0].node);
		   Node *name = Getattr(base,"name");
		   Append(Getattr(list,Getattr(base,"access")),name);
                   (yyval.bases) = list;
               }
#line 12353 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 532: /* @13: %empty  */
#line 7443 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		 (yyval.intvalue) = cparse_line;
	       }
#line 12361 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 533: /* base_specifier: opt_virtual @13 idcolon variadic_opt  */
#line 7445 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node), cparse_file);
		 Setline((yyval.node), (yyvsp[-2].intvalue));
		 Setattr((yyval.node), "name", (yyvsp[-1].str));
		 Setfile((yyvsp[-1].str), cparse_file);
		 Setline((yyvsp[-1].str), (yyvsp[-2].intvalue));
                 if (last_cpptype && (Strcmp(last_cpptype,"struct") != 0)) {
		   Setattr((yyval.node),"access","private");
		   Swig_warning(WARN_PARSE_NO_ACCESS, Getfile((yyval.node)), Getline((yyval.node)), "No access specifier given for base class '%s' (ignored).\n", SwigType_namestr((yyvsp[-1].str)));
                 } else {
		   Setattr((yyval.node),"access","public");
		 }
		 if ((yyvsp[0].intvalue)) {
		   SwigType_add_variadic(Getattr((yyval.node), "name"));
		 }
               }
#line 12383 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 534: /* @14: %empty  */
#line 7462 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                        {
		 (yyval.intvalue) = cparse_line;
	       }
#line 12391 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 535: /* base_specifier: opt_virtual access_specifier @14 opt_virtual idcolon variadic_opt  */
#line 7464 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                        {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node), cparse_file);
		 Setline((yyval.node), (yyvsp[-3].intvalue));
		 Setattr((yyval.node), "name", (yyvsp[-1].str));
		 Setfile((yyvsp[-1].str), cparse_file);
		 Setline((yyvsp[-1].str), (yyvsp[-3].intvalue));
		 Setattr((yyval.node), "access", (yyvsp[-4].id));
		 if (Strcmp((yyvsp[-4].id), "public") != 0) {
		   Swig_warning(WARN_PARSE_PRIVATE_INHERIT, Getfile((yyval.node)), Getline((yyval.node)), "%s inheritance from base '%s' (ignored).\n", (yyvsp[-4].id), SwigType_namestr((yyvsp[-1].str)));
		 }
		 if ((yyvsp[0].intvalue)) {
		   SwigType_add_variadic(Getattr((yyval.node), "name"));
		 }
               }
#line 12411 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 536: /* access_specifier: PUBLIC  */
#line 7481 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           { (yyval.id) = "public"; }
#line 12417 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 537: /* access_specifier: PRIVATE  */
#line 7482 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.id) = "private"; }
#line 12423 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 538: /* access_specifier: PROTECTED  */
#line 7483 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           { (yyval.id) = "protected"; }
#line 12429 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 539: /* templcpptype: CLASS variadic_opt  */
#line 7486 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
                   (yyval.type) = NewString("class");
		   if (!inherit_list) last_cpptype = (yyval.type);
		   if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.type));
               }
#line 12439 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 540: /* templcpptype: TYPENAME variadic_opt  */
#line 7491 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
                   (yyval.type) = NewString("typename");
		   if (!inherit_list) last_cpptype = (yyval.type);
		   if ((yyvsp[0].intvalue)) SwigType_add_variadic((yyval.type));
               }
#line 12449 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 542: /* cpptype: STRUCT attribute  */
#line 7499 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
                   (yyval.type) = NewString("struct");
		   if (!inherit_list) last_cpptype = (yyval.type);
                   if ((yyvsp[0].node)) {
                     Setattr((yyval.type), "attribute", (yyvsp[0].node));
                   }
               }
#line 12461 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 543: /* cpptype: UNION attribute  */
#line 7506 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
                   (yyval.type) = NewString("union");
		   if (!inherit_list) last_cpptype = (yyval.type);
                   if ((yyvsp[0].node)) {
                     Setattr((yyval.type), "attribute", (yyvsp[0].node));
                   }
               }
#line 12473 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 544: /* classkey: CLASS  */
#line 7515 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
		   if (!inherit_list) last_cpptype = NewString("class");
               }
#line 12481 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 545: /* classkey: STRUCT  */
#line 7518 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		   if (!inherit_list) last_cpptype = NewString("struct");
               }
#line 12489 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 546: /* classkey: UNION  */
#line 7521 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
		   if (!inherit_list) last_cpptype = NewString("union");
               }
#line 12497 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 551: /* virt_specifier_seq: OVERRIDE  */
#line 7534 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                              {
                   (yyval.str) = 0;
	       }
#line 12505 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 552: /* virt_specifier_seq: FINAL  */
#line 7537 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       {
                   (yyval.str) = NewString("1");
	       }
#line 12513 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 553: /* virt_specifier_seq: FINAL OVERRIDE  */
#line 7540 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
                   (yyval.str) = NewString("1");
	       }
#line 12521 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 554: /* virt_specifier_seq: OVERRIDE FINAL  */
#line 7543 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
                   (yyval.str) = NewString("1");
	       }
#line 12529 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 556: /* virt_specifier_seq_opt: %empty  */
#line 7549 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
                   (yyval.str) = 0;
               }
#line 12537 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 557: /* class_virt_specifier_opt: FINAL  */
#line 7554 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
                   (yyval.str) = NewString("1");
               }
#line 12545 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 558: /* class_virt_specifier_opt: %empty  */
#line 7557 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
                   (yyval.str) = 0;
               }
#line 12553 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 559: /* exception_specification: THROW LPAREN parms RPAREN  */
#line 7562 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).throws = (yyvsp[-1].pl);
                    (yyval.dtype).throwf = NewString("1");
	       }
#line 12563 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 560: /* exception_specification: NOEXCEPT  */
#line 7567 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).nexcept = NewString("true");
	       }
#line 12572 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 561: /* exception_specification: virt_specifier_seq  */
#line 7571 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).final = (yyvsp[0].str);
	       }
#line 12581 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 562: /* exception_specification: THROW LPAREN parms RPAREN virt_specifier_seq  */
#line 7575 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                              {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).throws = (yyvsp[-2].pl);
                    (yyval.dtype).throwf = NewString("1");
                    (yyval.dtype).final = (yyvsp[0].str);
	       }
#line 12592 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 563: /* exception_specification: NOEXCEPT virt_specifier_seq  */
#line 7581 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).nexcept = NewString("true");
		    (yyval.dtype).final = (yyvsp[0].str);
	       }
#line 12602 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 564: /* exception_specification: NOEXCEPT LPAREN expr RPAREN  */
#line 7586 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                             {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).nexcept = (yyvsp[-1].dtype).val;
	       }
#line 12611 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 565: /* qualifiers_exception_specification: cv_ref_qualifier  */
#line 7592 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
		    (yyval.dtype) = default_dtype;
                    (yyval.dtype).qualifier = (yyvsp[0].dtype).qualifier;
                    (yyval.dtype).refqualifier = (yyvsp[0].dtype).refqualifier;
               }
#line 12621 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 566: /* qualifiers_exception_specification: exception_specification  */
#line 7597 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		    (yyval.dtype) = (yyvsp[0].dtype);
               }
#line 12629 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 567: /* qualifiers_exception_specification: cv_ref_qualifier exception_specification  */
#line 7600 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                          {
		    (yyval.dtype) = (yyvsp[0].dtype);
                    (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
                    (yyval.dtype).refqualifier = (yyvsp[-1].dtype).refqualifier;
               }
#line 12639 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 569: /* cpp_const: %empty  */
#line 7608 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
                 (yyval.dtype) = default_dtype;
               }
#line 12647 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 570: /* ctor_end: cpp_const ctor_initializer SEMI  */
#line 7613 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 { 
                    Clear(scanner_ccode); 
		    (yyval.decl) = default_decl;
		    (yyval.decl).throws = (yyvsp[-2].dtype).throws;
		    (yyval.decl).throwf = (yyvsp[-2].dtype).throwf;
		    (yyval.decl).nexcept = (yyvsp[-2].dtype).nexcept;
		    (yyval.decl).final = (yyvsp[-2].dtype).final;
                    if ((yyvsp[-2].dtype).qualifier)
                      Swig_error(cparse_file, cparse_line, "Constructor cannot have a qualifier.\n");
               }
#line 12662 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 571: /* ctor_end: cpp_const ctor_initializer LBRACE  */
#line 7623 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   { 
                    if ((yyvsp[-2].dtype).qualifier)
                      Swig_error(cparse_file, cparse_line, "Constructor cannot have a qualifier.\n");
                    if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		    (yyval.decl) = default_decl;
                    (yyval.decl).throws = (yyvsp[-2].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[-2].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[-2].dtype).nexcept;
                    (yyval.decl).final = (yyvsp[-2].dtype).final;
               }
#line 12677 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 572: /* ctor_end: LPAREN parms RPAREN SEMI  */
#line 7633 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          { 
                    Clear(scanner_ccode); 
		    (yyval.decl) = default_decl;
                    (yyval.decl).parms = (yyvsp[-2].pl); 
                    (yyval.decl).have_parms = 1; 
               }
#line 12688 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 573: /* ctor_end: LPAREN parms RPAREN LBRACE  */
#line 7639 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                            {
                    if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		    (yyval.decl) = default_decl;
                    (yyval.decl).parms = (yyvsp[-2].pl); 
                    (yyval.decl).have_parms = 1; 
               }
#line 12699 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 574: /* ctor_end: EQUAL definetype SEMI  */
#line 7645 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       { 
		    (yyval.decl) = default_decl;
                    (yyval.decl).defarg = (yyvsp[-1].dtype).val; 
		    (yyval.decl).stringdefarg = (yyvsp[-1].dtype).stringval;
		    (yyval.decl).numdefarg = (yyvsp[-1].dtype).numval;
               }
#line 12710 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 575: /* ctor_end: exception_specification EQUAL default_delete SEMI  */
#line 7651 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                   {
		    (yyval.decl) = default_decl;
                    (yyval.decl).defarg = (yyvsp[-1].dtype).val;
		    (yyval.decl).stringdefarg = (yyvsp[-1].dtype).stringval;
		    (yyval.decl).numdefarg = (yyvsp[-1].dtype).numval;
                    (yyval.decl).throws = (yyvsp[-3].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[-3].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[-3].dtype).nexcept;
                    (yyval.decl).final = (yyvsp[-3].dtype).final;
                    if ((yyvsp[-3].dtype).qualifier)
                      Swig_error(cparse_file, cparse_line, "Constructor cannot have a qualifier.\n");
               }
#line 12727 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 582: /* mem_initializer: idcolon LPAREN  */
#line 7675 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
		  if (skip_balanced('(',')') < 0) Exit(EXIT_FAILURE);
		  Clear(scanner_ccode);
		}
#line 12736 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 583: /* mem_initializer: idcolon LBRACE  */
#line 7687 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
		  if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		  Clear(scanner_ccode);
		}
#line 12745 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 584: /* less_valparms_greater: LESSTHAN valparms GREATERTHAN  */
#line 7693 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
                     String *s = NewStringEmpty();
                     SwigType_add_template(s,(yyvsp[-1].p));
		     (yyval.str) = s;
		     scanner_last_id(1);
                }
#line 12756 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 586: /* identifier: OVERRIDE  */
#line 7703 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          { (yyval.id) = Swig_copy_string("override"); }
#line 12762 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 587: /* identifier: FINAL  */
#line 7704 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                       { (yyval.id) = Swig_copy_string("final"); }
#line 12768 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 589: /* idstring: default_delete  */
#line 7708 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                { (yyval.id) = Char((yyvsp[0].dtype).val); }
#line 12774 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 590: /* idstring: string  */
#line 7709 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.id) = Char((yyvsp[0].str)); }
#line 12780 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 592: /* idstringopt: %empty  */
#line 7713 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.id) = 0; }
#line 12786 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 593: /* idcolon: idtemplate idcolontail  */
#line 7716 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                        { 
		 (yyval.str) = NewStringf("%s%s", (yyvsp[-1].str), (yyvsp[0].str));
		 Delete((yyvsp[0].str));
               }
#line 12795 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 594: /* idcolon: NONID DCOLON idtemplatetemplate idcolontail  */
#line 7720 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                             {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].str),(yyvsp[0].str));
                 Delete((yyvsp[0].str));
               }
#line 12804 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 595: /* idcolon: idtemplate  */
#line 7724 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
		 (yyval.str) = NewString((yyvsp[0].str));
   	       }
#line 12812 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 596: /* idcolon: NONID DCOLON idtemplatetemplate  */
#line 7727 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 {
		 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 12820 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 597: /* idcolon: OPERATOR  */
#line 7730 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		 (yyval.str) = (yyvsp[0].str);
	       }
#line 12828 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 598: /* idcolon: OPERATOR less_valparms_greater  */
#line 7733 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                {
		 (yyval.str) = (yyvsp[-1].str);
		 Append((yyval.str), (yyvsp[0].str));
		 Delete((yyvsp[0].str));
	       }
#line 12838 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 599: /* idcolon: NONID DCOLON OPERATOR  */
#line 7738 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		 (yyval.str) = (yyvsp[0].str);
		 Insert((yyval.str), 0, "::");
               }
#line 12847 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 600: /* idcolontail: DCOLON idtemplatetemplate idcolontail  */
#line 7744 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                           {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].str),(yyvsp[0].str));
		   Delete((yyvsp[0].str));
               }
#line 12856 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 601: /* idcolontail: DCOLON idtemplatetemplate  */
#line 7748 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 12864 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 602: /* idcolontail: DCOLON OPERATOR  */
#line 7751 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
		   (yyval.str) = (yyvsp[0].str);
		   Insert((yyval.str), 0, "::");
               }
#line 12873 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 603: /* idcolontail: DCNOT idtemplate  */
#line 7755 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[0].str));
               }
#line 12881 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 604: /* idtemplate: identifier  */
#line 7761 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                           {
		(yyval.str) = NewString((yyvsp[0].id));
	      }
#line 12889 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 605: /* idtemplate: identifier less_valparms_greater  */
#line 7764 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                 {
		(yyval.str) = NewString((yyvsp[-1].id));
		Append((yyval.str), (yyvsp[0].str));
		Delete((yyvsp[0].str));
	      }
#line 12899 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 607: /* idtemplatetemplate: TEMPLATE identifier less_valparms_greater  */
#line 7772 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                          {
		(yyval.str) = NewString((yyvsp[-1].id));
		Append((yyval.str), (yyvsp[0].str));
		Delete((yyvsp[0].str));
	      }
#line 12909 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 608: /* idcolonnt: identifier idcolontailnt  */
#line 7780 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.str) = NewStringf("%s%s", (yyvsp[-1].id), (yyvsp[0].str));
		 Delete((yyvsp[0].str));
               }
#line 12918 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 609: /* idcolonnt: NONID DCOLON identifier idcolontailnt  */
#line 7784 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                       {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].id),(yyvsp[0].str));
                 Delete((yyvsp[0].str));
               }
#line 12927 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 610: /* idcolonnt: identifier  */
#line 7788 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                            {
		 (yyval.str) = NewString((yyvsp[0].id));
   	       }
#line 12935 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 611: /* idcolonnt: NONID DCOLON identifier  */
#line 7791 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                         {
		 (yyval.str) = NewStringf("::%s",(yyvsp[0].id));
               }
#line 12943 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 612: /* idcolonnt: OPERATOR  */
#line 7794 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          {
		 (yyval.str) = (yyvsp[0].str);
	       }
#line 12951 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 613: /* idcolonnt: NONID DCOLON OPERATOR  */
#line 7797 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                       {
		 (yyval.str) = (yyvsp[0].str);
		 Insert((yyval.str), 0, "::");
               }
#line 12960 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 614: /* idcolontailnt: DCOLON identifier idcolontailnt  */
#line 7803 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                      {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].id),(yyvsp[0].str));
		   Delete((yyvsp[0].str));
               }
#line 12969 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 615: /* idcolontailnt: DCOLON identifier  */
#line 7807 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].id));
               }
#line 12977 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 616: /* idcolontailnt: DCOLON OPERATOR  */
#line 7810 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                 {
		   (yyval.str) = (yyvsp[0].str);
		   Insert((yyval.str), 0, "::");
               }
#line 12986 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 617: /* idcolontailnt: DCNOT identifier  */
#line 7814 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[0].id));
               }
#line 12994 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 619: /* attribute: %empty  */
#line 7821 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.node) = 0; }
#line 13000 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 621: /* attribute_notopt: attribute_item attribute_notopt  */
#line 7826 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                  {
                  (yyval.node) = (yyvsp[0].node);
                  Iterator i;
                  Append((yyval.node), (yyvsp[-1].node));
                  for (i = First((yyvsp[-1].node)); i.key; i = Next(i)) {
                    Setattr((yyval.node), i.key, i.item);
                  }
                  Delete((yyvsp[-1].node));
                }
#line 13014 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 622: /* attribute_item: LLBRACKET attribute_nspace attribute_list RRBRACKET  */
#line 7838 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                     {
                  (yyval.node) = (yyvsp[-1].node);
                  if ((yyvsp[-2].str)) {
                    /* Transform [[using gnu: visibility("hidden"), always_inline]] into
                     * [[gnu::("hidden"), gnu::always_inline]]
                     */
                    (yyval.node) = NewHash();
                    Setattr((yyval.node), (yyvsp[-2].str), (yyvsp[-1].node));
                  }
                }
#line 13029 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 623: /* attribute_nspace: USING identifier COLON  */
#line 7851 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          { (yyval.str) = NewString((yyvsp[-1].id)); }
#line 13035 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 624: /* attribute_nspace: %empty  */
#line 7852 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         { (yyval.str) = 0; }
#line 13041 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 625: /* attribute_list: attribute_element  */
#line 7855 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   {
                  (yyval.node) = NewHash();
                  Setattr((yyval.node), (yyvsp[0].ptype).type, (yyvsp[0].ptype).us);
                }
#line 13050 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 626: /* attribute_list: attribute_element COMMA attribute_list  */
#line 7859 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                         {
                  (yyval.node) = (yyvsp[0].node);
                  Setattr((yyval.node), (yyvsp[-2].ptype).type, (yyvsp[-2].ptype).us);
                }
#line 13059 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 627: /* attribute_element: idstring  */
#line 7866 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                             {
                  (yyval.ptype).type = NewString((yyvsp[0].id));
                  (yyval.ptype).us = NewString("1");
                }
#line 13068 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 628: /* attribute_element: idstring LPAREN  */
#line 7870 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                  {
                  if (skip_balanced('(', ')') < 0) Exit(EXIT_FAILURE);
                  (yyval.ptype).type = NewString((yyvsp[-1].id));
                  Delitem(scanner_ccode, Len(scanner_ccode) - 1);
                  Delitem(scanner_ccode, 0);
                  (yyval.ptype).us = Copy(scanner_ccode);
                  Clear(scanner_ccode);
                }
#line 13081 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 629: /* attribute_element: idstring DCOLON attribute_element  */
#line 7878 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                    {
                  (yyval.ptype).type = NewString((yyvsp[-2].id));
                  (yyval.ptype).us = NewHash();
                  Setattr((yyval.ptype).us, (yyvsp[0].ptype).type, (yyvsp[0].ptype).us);
                }
#line 13091 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 630: /* string: string STRING  */
#line 7886 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                   { 
		   (yyval.str) = (yyvsp[-1].str);
		   Append((yyval.str), (yyvsp[0].str));
		   Delete((yyvsp[0].str));
	       }
#line 13101 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 632: /* wstring: wstring WSTRING  */
#line 7893 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                     {
		   // Concatenated wide strings: L"str1" L"str2"
		   (yyval.str) = (yyvsp[-1].str);
		   Append((yyval.str), (yyvsp[0].str));
		   Delete((yyvsp[0].str));
	       }
#line 13112 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 633: /* wstring: wstring STRING  */
#line 7899 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		   // Concatenated wide string and normal string literal: L"str1" "str2" (C++11).
		   (yyval.str) = (yyvsp[-1].str);
		   Append((yyval.str), (yyvsp[0].str));
		   Delete((yyvsp[0].str));
	       }
#line 13123 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 634: /* wstring: string WSTRING  */
#line 7905 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                    {
		   // Concatenated normal string and wide string literal: "str1" L"str2" (C++11).
		   (yyval.str) = (yyvsp[-1].str);
		   Append((yyval.str), (yyvsp[0].str));
		   Delete((yyvsp[0].str));
	       }
#line 13134 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 637: /* stringbrace: LBRACE  */
#line 7915 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        {
		  if (skip_balanced('{','}') < 0) Exit(EXIT_FAILURE);
		  (yyval.str) = NewString(scanner_ccode);
               }
#line 13143 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 639: /* options: LPAREN kwargs RPAREN  */
#line 7922 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                      {
                  Hash *n;
                  (yyval.node) = NewHash();
                  n = (yyvsp[-1].node);
                  while(n) {
                     String *name, *value;
                     name = Getattr(n,"name");
                     value = Getattr(n,"value");
		     if (!value) value = (String *) "1";
                     Setattr((yyval.node),name, value);
		     n = nextSibling(n);
		  }
               }
#line 13161 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 640: /* options: %empty  */
#line 7935 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                        { (yyval.node) = 0; }
#line 13167 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 641: /* kwargs: kwargs_builder  */
#line 7939 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                {
		 (yyval.node) = (yyvsp[0].nodebuilder).node;
	       }
#line 13175 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 642: /* kwargs_builder: idstring EQUAL stringnum  */
#line 7944 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                          {
		 Node *n = NewHash();
		 Setattr(n, "name", (yyvsp[-2].id));
		 Setattr(n, "value", (yyvsp[0].str));
		 Delete((yyvsp[0].str));
		 (yyval.nodebuilder).node = (yyval.nodebuilder).last = n;
	       }
#line 13187 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 643: /* kwargs_builder: kwargs_builder COMMA idstring EQUAL stringnum  */
#line 7951 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                   {
		 (yyval.nodebuilder) = (yyvsp[-4].nodebuilder);
		 Node *n = NewHash();
		 Setattr(n, "name", (yyvsp[-2].id));
		 Setattr(n, "value", (yyvsp[0].str));
		 Delete((yyvsp[0].str));
		 set_nextSibling((yyval.nodebuilder).last, n);
		 (yyval.nodebuilder).last = n;
	       }
#line 13201 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 644: /* kwargs_builder: idstring  */
#line 7960 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                          {
		 Node *n = NewHash();
		 Setattr(n, "name", (yyvsp[0].id));
		 (yyval.nodebuilder).node = (yyval.nodebuilder).last = n;
	       }
#line 13211 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 645: /* kwargs_builder: kwargs_builder COMMA idstring  */
#line 7965 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                   {
		 (yyval.nodebuilder) = (yyvsp[-2].nodebuilder);
		 Node *n = NewHash();
		 Setattr(n, "name", (yyvsp[0].id));
		 set_nextSibling((yyval.nodebuilder).last, n);
		 (yyval.nodebuilder).last = n;
	       }
#line 13223 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 646: /* kwargs_builder: idstring EQUAL stringtype  */
#line 7972 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                           {
		 Node *n = (yyvsp[0].node);
		 Setattr(n, "name", (yyvsp[-2].id));
		 (yyval.nodebuilder).node = (yyval.nodebuilder).last = n;
	       }
#line 13233 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 647: /* kwargs_builder: kwargs_builder COMMA idstring EQUAL stringtype  */
#line 7977 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                                                                    {
		 (yyval.nodebuilder) = (yyvsp[-4].nodebuilder);
		 Node *n = (yyvsp[0].node);
		 Setattr(n, "name", (yyvsp[-2].id));
		 set_nextSibling((yyval.nodebuilder).last, n);
		 (yyval.nodebuilder).last = n;
	       }
#line 13245 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;

  case 649: /* stringnum: exprnum  */
#line 7987 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"
                         {
		 (yyval.str) = (yyvsp[0].dtype).val;
               }
#line 13253 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"
    break;


#line 13257 "/home/rkhab/work/swig-jse/build/Source/CParse/parser.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= END)
        {
          /* Return failure if at end of input.  */
          if (yychar == END)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 7992 "/home/rkhab/work/swig-jse/Source/CParse/parser.y"


SwigType *Swig_cparse_type(String *s) {
   String *ns;
   ns = NewString(s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSETYPE);
   if (yyparse() == 2) {
      Swig_error(cparse_file, cparse_line, "Parser exceeded stack depth or ran out of memory\n");
      Exit(EXIT_FAILURE);
   }
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return (SwigType *)top;
}


Parm *Swig_cparse_parm(String *s) {
   String *ns;
   ns = NewString(s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARM);
   if (yyparse() == 2) {
      Swig_error(cparse_file, cparse_line, "Parser exceeded stack depth or ran out of memory\n");
      Exit(EXIT_FAILURE);
   }
   /*   Printf(stdout,"parmparse: '%s' ---> '%s'\n", s, top); */
   Delete(ns);
   return (Parm *)top;
}


ParmList *Swig_cparse_parms(String *s, Node *file_line_node) {
   String *ns;
   char *cs = Char(s);
   if (cs && cs[0] != '(') {
     ns = NewStringf("(%s)",s);
   } else {
     ns = NewString(s);
   }
   Setfile(ns, Getfile(file_line_node));
   Setline(ns, Getline(file_line_node));
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARMS);
   if (yyparse() == 2) {
      Swig_error(cparse_file, cparse_line, "Parser exceeded stack depth or ran out of memory\n");
      Exit(EXIT_FAILURE);
   }
   /*   Printf(stdout,"parmsparse: '%s' ---> '%s'\n", s, top); */
   return (ParmList *)top;
}

