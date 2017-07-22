# ============================================================================
# Copyright (c) 2007 Jason Evans <jasone@canonware.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ============================================================================
"""
This module contains classes that are used in the specification of grammars.
"""

import re
import sys
from parsing.ast import Token, Nonterm
from parsing.errors import SpecError
from parsing import introspection


class Precedence(object):
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
    assoc_tok_re = re.compile(r'([<>=])([A-Za-z]\w*)')

    def __init__(self, name, assoc, relationships):
        assert assoc in ["fail", "nonassoc", "left", "right", "split"]
        assert type(relationships) == dict

        self.name = name
        self.assoc = assoc
        self.relationships = relationships  # Raw relationships specification.

        # Precedences that have equivalent precedence.
        self.equiv = set((self, ))
        # Precedences that have higher precedence.
        self.dominators = set()

    def __repr__(self):
        equiv = [prec.name for prec in self.equiv]
        equiv.sort()
        domin = [prec.name for prec in self.dominators]
        domin.sort()
        return "[%%%s %s ={%s} <{%s}]" % (
            self.assoc, self.name, ",".join(equiv), ",".join(domin))


class SymbolSpec(int):
    seq = 0

    def __new__(cls, *args, **kwargs):
        result = int.__new__(cls, SymbolSpec.seq)
        result.seq = SymbolSpec.seq
        SymbolSpec.seq += 1
        return result

    def __init__(self, name, prec):
        assert type(name) == str

        self.name = name
        self.prec = prec
        self.firstSet = []  # Set.
        self.followSet = []  # Set.

    def __repr__(self):
        return "%s" % self.name

    __str__ = __repr__

    def firstSetMerge(self, sym):
        if sym not in self.firstSet:
            self.firstSet.append(sym)
            return False
        else:
            return True

    def followSetMerge(self, set):
        ret = True
        for sym in set:
            if sym != epsilon and sym not in self.followSet:
                self.followSet.append(sym)
                ret = False
        return ret


class NontermSpec(SymbolSpec):
    token_re = re.compile(r'([A-Za-z]\w*)')
    precedence_tok_re = re.compile(r'\[([A-Za-z]\w*)\]')

    def __init__(self, nontermType, name, qualified, prec):
        assert issubclass(nontermType, Nonterm)  # Add forward decl for Lyken.

        SymbolSpec.__init__(self, name, prec)

        self.qualified = qualified
        self.nontermType = nontermType
        self.productions = []  # Set.

    @classmethod
    def from_class(cls, nt_subclass, name=None, module=None):
        if name is None:
            name = nt_subclass.__name__
        if module is None:
            module_name = nt_subclass.__module__
        else:
            module_name = module.__name__
        if nt_subclass.__doc__ is None:
            dirtoks = ['%nonterm', name]
        else:
            dirtoks = introspection.parse_docstring(nt_subclass.__doc__)
        is_start = (dirtoks[0] == '%start')
        # if dirtoks[0] in SHORTHAND:
        #    dirtoks = ['%nonterm', name]
        symbol_name = None
        prec = None
        i = 1
        while i < len(dirtoks):
            tok = dirtoks[i]
            m = NontermSpec.precedence_tok_re.match(tok)
            if m:
                if i < len(dirtoks) - 1:
                    raise SpecError("Precedence must come last in "
                                    "non-terminal specification: %s" %
                                    nt_subclass.__doc__)
                prec = m.group(1)
            else:
                m = NontermSpec.token_re.match(tok)
                if m:
                    symbol_name = m.group(1)
                else:
                    raise SpecError("Invalid non-terminal specification: %s" %
                                    nt_subclass.__doc__)
            i += 1
        if symbol_name is None:
            symbol_name = name
        if prec is None:
            prec = "none"

        # nonterm = NontermSpec(symbol_name, nt_subclass,
        #                       "%s.%s" % (module_name, name), prec)
        nonterm = NontermSpec(nt_subclass, symbol_name,
                              "%s.%s" % (module_name, name), prec)
        return nonterm, is_start


# AKA terminal symbol.
class TokenSpec(SymbolSpec):
    def __init__(self, tokenType, name, prec):
        assert issubclass(tokenType, Token)
        assert type(name) == str
        assert isinstance(prec, Precedence) or type(prec) == str

        SymbolSpec.__init__(self, name, prec)
        self.tokenType = tokenType


class Production(int):
    seq = 0

    def __new__(cls, *args, **kwargs):
        result = int.__new__(cls, Production.seq)
        result.seq = Production.seq
        Production.seq += 1
        return result

    def __init__(self, method, qualified, prec, lhs, rhs):
        assert isinstance(prec, Precedence)
        assert isinstance(lhs, NontermSpec)
        if __debug__:
            for elm in rhs:
                assert isinstance(elm, SymbolSpec)

        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs

    def __getstate__(self):
        return (self.qualified, self.prec, self.lhs, self.rhs, self.seq)

    def __setstate__(self, data):
        # Convert qualified name to a function reference.
        (qualified, prec, lhs, rhs, seq) = data
        elms = qualified.split(".")
        method = sys.modules[elms[0]]
        for elm in elms[1:]:
            method = method.__dict__[elm]

        # Set state.
        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs
        self.seq = seq

    def __repr__(self):
        return (
            "%r ::= %s. [%s]" % (
                self.lhs,
                " ".join(["%r" % elm for elm in self.rhs]),
                self.prec.name
            )
        )

    # Optional callback method.
    #
    # Called when a production is reduced.
    def reduce(self, lhs, *rhs):
        pass


# <$>.
class EndOfInput(Token):
    pass


class EndOfInputSpec(TokenSpec):
    def __init__(self):
        TokenSpec.__init__(self, EndOfInput, "<$>", "none")


eoi = EndOfInputSpec()


# <e>.
class Epsilon(Token):
    pass


class EpsilonSpec(TokenSpec):
    def __init__(self):
        TokenSpec.__init__(self, Epsilon, "<e>", "none")


epsilon = EpsilonSpec()


class NontermStart(Nonterm):
    def reduce(self, userStartSym, eoi):
        pass


class Start(Production):
    def __init__(self, startSym, userStartSym):
        Production.__init__(self, None, startSym, userStartSym)


class Action(object):
    """
Abstract base class, subclassed by {Shift,Reduce}Action.
"""

    def __init__(self):
        pass


class ShiftAction(Action):
    """
Shift action, with assocated nextState.
"""

    def __init__(self, nextState):
        Action.__init__(self)
        self.nextState = nextState

    def __repr__(self):
        return "[shift %r]" % self.nextState

    def __eq__(self, other):
        if not isinstance(other, ShiftAction):
            return False
        if self.nextState != other.nextState:
            return False
        return True


class ReduceAction(Action):
    """
Reduce action, with associated production.
"""

    def __init__(self, production):
        Action.__init__(self)
        self.production = production

    def __repr__(self):
        return "[reduce %r]" % self.production

    def __eq__(self, other):
        if not isinstance(other, ReduceAction):
            return False
        if self.production != other.production:
            return False
        return True
