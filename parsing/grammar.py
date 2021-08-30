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

from __future__ import annotations
from typing import (
    TYPE_CHECKING,
    Any,
    Callable,
    Iterable,
    Mapping,
    List,
    Optional,
    Tuple,
    Type,
)

import re
import sys
from parsing.ast import Token, Nonterm
from parsing.errors import SpecError
from parsing import introspection

if TYPE_CHECKING:
    import types


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

    assoc_tok_re = re.compile(r"([<>=])([A-Za-z]\w*)")

    def __init__(
        self,
        name: str,
        assoc: str,
        relationships: Mapping[str, str],
    ) -> None:
        assert assoc in ["fail", "nonassoc", "left", "right", "split"]

        self.name = name
        self.assoc = assoc
        self.relationships = relationships  # Raw relationships specification.

        # Precedences that have equivalent precedence.
        self.equiv: set[Precedence] = set((self,))
        # Precedences that have higher precedence.
        self.dominators: set[Precedence] = set()

    def __repr__(self) -> str:
        equiv = [prec.name for prec in self.equiv]
        equiv.sort()
        domin = [prec.name for prec in self.dominators]
        domin.sort()
        return "[%%%s %s ={%s} <{%s}]" % (
            self.assoc,
            self.name,
            ",".join(equiv),
            ",".join(domin),
        )


class PrecedenceRef(Precedence):
    def __init__(self, name: str) -> None:
        super().__init__(name, "fail", {})


class SymbolSpec:
    seq = 0

    def __init__(self, name: str, prec: Precedence) -> None:
        self.name = name
        self.prec = prec
        self.firstSet: set[SymbolSpec] = set()
        self.followSet: set[SymbolSpec] = set()
        self.seq = SymbolSpec.seq
        SymbolSpec.seq += 1

    def __hash__(self) -> int:
        return self.seq

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, SymbolSpec):
            return self.seq == other.seq
        else:
            return NotImplemented

    def __lt__(self, other: Any) -> bool:
        if isinstance(other, SymbolSpec):
            return self.seq < other.seq
        else:
            return NotImplemented

    def __repr__(self) -> str:
        return self.name

    __str__ = __repr__

    def firstSetMerge(self, sym: SymbolSpec) -> bool:
        if sym not in self.firstSet:
            self.firstSet.add(sym)
            return False
        else:
            return True

    def followSetMerge(self, set: Iterable[SymbolSpec]) -> bool:
        ret = True
        for sym in set:
            if sym != epsilon and sym not in self.followSet:
                self.followSet.add(sym)
                ret = False
        return ret


class NontermSpec(SymbolSpec):
    token_re = re.compile(r"([A-Za-z]\w*)")
    precedence_tok_re = re.compile(r"\[([A-Za-z]\w*)\]")

    def __init__(
        self,
        nontermType: Type[Nonterm],
        name: str,
        qualified: str,
        prec: Precedence,
    ) -> None:
        super().__init__(name, prec)
        self.qualified = qualified
        self.nontermType = nontermType
        self.productions: set[Production] = set()

    @classmethod
    def from_class(
        cls,
        nt_subclass: type,
        name: Optional[str] = None,
        module: Optional[types.ModuleType] = None,
    ) -> Tuple[NontermSpec, bool]:
        if name is None:
            name = nt_subclass.__name__
        if module is None:
            module_name = nt_subclass.__module__
        else:
            module_name = module.__name__
        if nt_subclass.__doc__ is not None:
            dirtoks = introspection.parse_docstring(nt_subclass.__doc__)
        else:
            dirtoks = ("%nonterm", name)
        is_start = dirtoks[0] == r"%start"
        symbol_name = None
        prec = None
        i = 1
        while i < len(dirtoks):
            tok = dirtoks[i]
            m = NontermSpec.precedence_tok_re.match(tok)
            if m:
                if i < len(dirtoks) - 1:
                    raise SpecError(
                        "Precedence must come last in "
                        "non-terminal specification: %s" % nt_subclass.__doc__
                    )
                prec = PrecedenceRef(m.group(1))
            else:
                m = NontermSpec.token_re.match(tok)
                if m:
                    symbol_name = m.group(1)
                else:
                    raise SpecError(
                        "Invalid non-terminal specification: %s"
                        % nt_subclass.__doc__
                    )
            i += 1
        if symbol_name is None:
            symbol_name = name
        if prec is None:
            prec = PrecedenceRef("none")

        nonterm = NontermSpec(
            nt_subclass, symbol_name, f"{module_name}.{name}", prec
        )
        return nonterm, is_start


# AKA terminal symbol.
class TokenSpec(SymbolSpec):
    def __init__(
        self,
        tokenType: Type[Token],
        name: str,
        prec: Precedence,
    ) -> None:
        super().__init__(name, prec)
        self.tokenType = tokenType


class Production:
    seq = 0

    def __init__(
        self,
        method: Callable[..., Nonterm | None],
        qualified: str,
        prec: Precedence,
        lhs: NontermSpec,
        rhs: List[SymbolSpec],
    ) -> None:
        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs
        self.seq = Production.seq
        Production.seq += 1

    def __hash__(self) -> int:
        return self.seq

    def __eq__(self, other: Any) -> bool:
        if type(other) == Production:
            return self.seq == other.seq
        else:
            return NotImplemented

    def __lt__(self, other: Any) -> bool:
        if type(other) == Production:
            return self.seq < other.seq
        else:
            return NotImplemented

    def __getstate__(
        self,
    ) -> Tuple[str, Precedence, NontermSpec, List[SymbolSpec], int]:
        return (self.qualified, self.prec, self.lhs, self.rhs, self.seq)

    def __setstate__(
        self,
        data: Tuple[str, Precedence, NontermSpec, List[SymbolSpec], int],
    ) -> None:
        # Convert qualified name to a function reference.
        (qualified, prec, lhs, rhs, seq) = data
        elms = qualified.split(".")
        assert len(elms) > 1
        module = sys.modules[elms[0]]
        method = module.__dict__[elms[1]]
        for elm in elms[2:]:
            method = method.__dict__[elm]

        # Set state.
        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs
        self.seq = seq

    def __repr__(self) -> str:
        return "%r ::= %s. [%s]" % (
            self.lhs,
            " ".join(["%r" % elm for elm in self.rhs]),
            self.prec.name,
        )

    # Optional callback method.
    #
    # Called when a production is reduced.
    def reduce(self, lhs: NontermSpec, *rhs: SymbolSpec) -> None:
        pass


# <$>.
class EndOfInput(Token):
    pass


class EndOfInputSpec(TokenSpec):
    def __init__(self) -> None:
        super().__init__(EndOfInput, "<$>", PrecedenceRef("none"))


eoi = EndOfInputSpec()


# <e>.
class Epsilon(Token):
    pass


class EpsilonSpec(TokenSpec):
    def __init__(self) -> None:
        super().__init__(Epsilon, "<e>", PrecedenceRef("none"))


epsilon = EpsilonSpec()


class NontermStart(Nonterm):
    def reduce(self, userStartSym: SymbolSpec, eoi: EndOfInputSpec) -> None:
        pass


class Action:
    """
    Abstract base class, subclassed by {Shift,Reduce}Action."""

    def __init__(self) -> None:
        pass


class ShiftAction(Action):
    """
    Shift action, with assocated nextState."""

    def __init__(self, nextState: int) -> None:
        super().__init__()
        self.nextState = nextState

    def __repr__(self) -> str:
        return "[shift %r]" % self.nextState

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, ShiftAction):
            return False
        if self.nextState != other.nextState:
            return False
        return True


class ReduceAction(Action):
    """
    Reduce action, with associated production."""

    def __init__(self, production: Production) -> None:
        super().__init__()
        self.production = production

    def __repr__(self) -> str:
        return "[reduce %r]" % self.production

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, ReduceAction):
            return False
        if self.production != other.production:
            return False
        return True
