"""
The classes `Token` and `Nonterm` can be subclassed and enriched
with docstrings indicating the intended grammar, and will then be
used in the parsing as part of the abstract syntax tree that is
constructed in the process.
"""

from parsing.interfaces import is_parser, is_symspec


class Symbol(object):
    def __init__(self, symSpec, parser):
        assert is_symspec(symSpec)
        assert is_parser(parser)
        self.__symSpec = symSpec
        self.__parser = parser

    def __repr__(self):
        return "%r" % self.symSpec

    def __getSymSpec(self):
        return self.__symSpec

    def __setSymSpec(self):
        raise AttributeError

    symSpec = property(__getSymSpec, __setSymSpec)

    def __getParser(self):
        return self.__parser

    def __setParser(self):
        raise AttributeError

    parser = property(__getParser, __setParser)


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

    def __init__(self, parser):
        assert is_parser(parser)
        Symbol.__init__(self, parser._spec._sym2spec[type(self)], parser)

    def merge(self, other):
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
            "No merge() for %r; merging %r <--> %r" % (
                type(self), self, other))


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
        "%token"
"""

    def __init__(self, parser):
        assert is_parser(parser)
        Symbol.__init__(self, parser._spec._sym2spec[type(self)], parser)
        self.__parser = parser
