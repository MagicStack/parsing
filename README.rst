.. image:: https://travis-ci.org/sprymix/parsing.svg?branch=master
    :target: https://travis-ci.org/sprymix/parsing


Parsing
=======

The Parsing module implements an LR(1) parser generator, as well as the
runtime support for using a generated parser, via the Lr and Glr parser
drivers.  There is no special parser generator input file format, but the
parser generator still needs to know what classes/methods correspond to
various aspects of the parser.  This information is specified via
docstrings, which the parser generator introspects in order to generate a
parser.  Only one parser specification can be embedded in each module, but
it is possible to share modules between parser specifications so that, for
example, the same token definitions can be used by multiple parser
specifications.

The parsing tables are LR(1), but they are generated using a fast algorithm
that avoids creating duplicate states that result when using the generic
LR(1) algorithm.  Creation time and table size are on par with the LALR(1)
algorithm.  However, LALR(1) can create reduce/reduce conflicts that don't
exist in a true LR(1) parser.  For more information on the algorithm, see::

    A Practical General Method for Constructing LR(k) Parsers
    David Pager
    Acta Informatica 7, 249-268 (1977)

Parsing table generation requires non-trivial amounts of time for large
grammars.  Internal pickling support makes it possible to cache the most
recent version of the parsing table on disk, and use the table if the
current parser specification is still compatible with the one that was used
to generate the pickled parsing table.  Since the compatibility checking is
quite fast, even for large grammars, this removes the need to use the
standard code generation method that is used by most parser generators.

Parser specifications are encapsulated by the Spec class.  Parser instances
use Spec instances, but are themselves based on separate classes.  This
allows multiple parser instances to exist simultaneously, without requiring
multiple copies of the parsing tables.  There are two separate parser driver
classes:

Lr:
    Standard Characteristic Finite State Machine (CFSM) driver, based on
    unambiguous LR(1) parsing tables.  This driver is faster than the Glr
    driver, but it cannot deal with all parsing tables that the Glr
    driver can.

Glr:
    Generalized LR driver, capable of tracking multiple parse trees
    simultaneously, if the %split precedence is used to mark ambiguous
    actions.  This driver is closely based on Elkhound's design, which
    is described in a technical report::

        Elkhound: A Fast, Practical GLR Parser Generator
        Scott McPeak
        Report No. UCB/CSD-2-1214 (December 2002)
        http://www.cs.berkeley.edu/~smcpeak/elkhound/

Parser generator directives are embedded in docstrings, and must begin with
a '%' character, followed immediately by one of several keywords:

    Precedence:
        ``%fail`` ``%nonassoc`` ``%left`` ``%right`` ``%split``

    Token:
        ``%token``

    Non-terminal:
        ``%start`` ``%nonterm``

    Production:
        ``%reduce``

All of these directives are associated with classes except for %reduce.
%reduce is associated with methods within non-terminal classes.  The Parsing
module provides base classes from which precedences, tokens, and
non-terminals must be derived.  This is not as restrictive as it sounds,
since there is nothing preventing, for example, a master Token class that
subclasses Parsing.Token, which all of the actual token types then subclass.
Also, nothing prevents using multiple inheritance.

Folowing are the base classes to be subclassed by parser specifications:

  * Precedence
  * Token
  * Nonterm

The Parsing module implements the following exception classes:

  * SpecError - when there is a problem with the grammar specification
  * ParsingException - any problem that occurs during parsing
  * UnexpectedToken - when the input sequence contains a token that is
    not allowed by the grammar (including end-of-input)

In order to maintain compatibility with legacy code, the Parsing module
defines the following aliases. New code should use the exceptions above
that do not shadow Python's builtin exceptions.

   * Exception - superclass for all exceptions that can be raised
   * SyntaxError - alias for UnexpectedToken

Additionally, trying to set private attributes may raise:
  * AttributeError

Author: Jason Evans jasone@canonware.com

Github repo: http://github.com/sprymix/parsing
