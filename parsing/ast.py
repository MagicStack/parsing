"""
The classes `Token` and `Nonterm` can be subclassed and enriched
with docstrings indicating the intended grammar, and will then be
used in the parsing as part of the abstract syntax tree that is
constructed in the process.
"""

from __future__ import annotations

from mypy_extensions import mypyc_attr


@mypyc_attr(serializable=True, allow_interpreted_subclasses=True)
class Symbol:
    pass


class Nonterm(Symbol):
    """
    Non-terminal symbols have sets of productions associated with them.  The
    productions induce a parse forest on an input token stream.  There is
    one special non-terminal, which is denoted via the %start directive,
    whereas all other non-terminals are denoted via the %nonterm directive.
    In addition to productions (%reduce directives associated with class
    methods), the merge() method may be called during resolution of
    ambiguous parses.  See the merge() documentation for further details.

    Following are examples of how to specify non-terminal classes and their
    associated productions:

    class E(Parsing.Nonterm):
        "%start E"
        def __init__(self):
            Parsing.Nonterm.__init__(self)
            # ...

        # Productions.
        def reduceA(self, E, plus, T):
            "%reduce E plus T [split]"
            print "%r ::= %r %r %r." % (self, E, plus, T)

        def reduceB(self, T):
            "%reduce T"

    class T(Parsing.Nonterm):
            "%nonterm" # Name implicitly same as class name.
        def reduceA(self, T, star, F):
            "%reduce T star F"

        def reduceB(self, F):
            "%reduce F [p1]"

    class F(Parsing.Nonterm):
        "%nonterm F [p2]"
        def reduceA(self, lparen, E, rparen):
            "%reduce lparen E rparen"

        def reduceB(self, id):
            "%reduce id"
    """

    def merge(self, other: Nonterm) -> Nonterm:
        """
        Merging happens when there is an ambiguity in the input that allows
        non-terminals to be part of multiple overlapping series of
        reductions.  If no merge() method is specified, the parser will
        throw a syntax error upon encountering an ambiguity that confounds
        reduction processing.  However, it may be useful to either discard
        one of the possible parses, or to explicitly record the ambiguity in
        the data structures being created during parsing.  In both of these
        cases, the non-terminal-specific merge() is the place to do the
        work; merge() returns an object that is stored by the parser onto
        the parse stack.  In the case where merge() discards one of the
        possible parses, it need only return the parse that is to be
        preserved (self or other).

        If multiple merges are necessary, they cause a series of merge()
        calls.  The first alternative (self) may be the result of a previous
        merge() call, whereas other will not have not been merged yet
        (unless as the result of merging further down in the parse forest).

        The alternative that is discarded is never touched by the parser
        again, so if any immediate cleanup is necessary, it should be done
        in merge().
        """
        raise SyntaxError(
            "No merge() for %r; merging %r <--> %r" % (type(self), self, other)
        )


class Token(Symbol):
    """
    Tokens are terminal symbols.  The parser is fed Token instances, which
    is what drives parsing.  Typically, the user will define a class that
    subclasses Parsing.Token and implement parser-specific machinery there,
    then derive all actual token types from that class.

    class Token(Parsing.Token):
        def __init__(self, parser):
            Parsing.Token.__init__(self, parser)
            # ...

    class Plus(Token):
        "%token plus [p1]"

    class star(Token):
        "%token star [p2]" # Name implicitly same as class name.

    class lparen(Token):
        "%token [split]"

    class rparen(Token):
        "%token [none]" # [none] not necessary, since it's the default.

    class id(Token):
        "%token" """


class Precedence:
    """
    Precedences can be associated with tokens, non-terminals, and
    productions.  Precedence isn't as important for GLR parsers as for LR
    parsers, since GLR parsing allows for parse-time resolution of
    ambiguity.  Still, precedence can be useful for reducing the volume of
    ambiguities that must be dealt with at run-time.

    There are five precedence types: %fail, %nonassoc, %left, %right, and
    %split.  Each precedence can have relationships with other precedences:
    <, >, or =.  These relationships specify a directed acyclic graph (DAG),
    which is used to compute the transitive closures of relationships among
    precedences.  If no path exists between two precedences that are
    compared during conflict resolution, parser generation fails.  < and >
    are reflexive; it does not matter which is used.  Conceptually, the =
    relationship causes precedences to share a node in the DAG.

    During conflict resolution, an error results if no path exists in the
    DAG between the precedences under consideration.  When such a path
    exists, the highest precedence non-terminal or production takes
    precedence.  Associativity only comes into play for shift/reduce
    conflicts, where the terminal and the production have equivalent
    precedences (= relationship).  In this case, the non-terminal's
    associativity determines how the conflict is resolved.

    The %fail and %split associativities are special because they can be
    mixed with other associativities.  During conflict resolution, if
    another action has non-%fail associativity, then the %fail (lack of)
    associativity is overridden.  Similarly, %split associativity overrides
    any other associativity.  In contrast, any mixture of associativity
    between %nonassoc/%left/%right causes an unresolvable conflict.

        %fail : Any conflict is a parser-generation-time error.

                A pre-defined precedence, [none], is provided.  It has
                %fail associativity, and has no pre-defined precedence
                relationships.

    %nonassoc : Resolve shift/reduce conflicts by removing both
                possibilities, thus making conflicts a parse-time error.

        %left : Resolve shift/reduce conflicts by reducing.

        %right : Resolve shift/reduce conflicts by shifting.

        %split : Do not resolve conflicts; the GLR algorithm will split
                the parse stack when necessary.

                A pre-defined precedence, [split], is provided.  It has
                %split associativity, and has no pre-defined precedence
                relationships.

    By default, all symbols have [none] precedence.  Each production
    inherits the precedence of its left-hand-side nonterminal's precedence
    unless a precedence is manually specified for the production.

    Following are some examples of how to specify precedence classes:

    class P1(Parsing.Precedence):
        "%split p1"

    class p2(Parsing.Precedence):
        "%left" # Name implicitly same as class name.

    class P3(Parsing.Precedence):
        "%left p3 >p2" # No whitespace is allowed between > and p2.

    class P4(Parsing.Precedence):
        "%left p4 =p3" # No whitespace is allowed between = and p3.
    """
