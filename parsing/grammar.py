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
    ClassVar,
    Dict,
    Iterable,
    List,
    Mapping,
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


class PrecedenceSpec:
    assoc_tok_re: ClassVar[re.Pattern[str]] = re.compile(
        r"([<>=])([A-Za-z]\w*)"
    )

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
        self.equiv: set[PrecedenceSpec] = set((self,))
        # Precedences that have higher precedence.
        self.dominators: set[PrecedenceSpec] = set()

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


class PrecedenceRef(PrecedenceSpec):
    def __init__(self, name: str) -> None:
        super().__init__(name, "fail", {})


class SymbolSpec:
    name: str
    prec: PrecedenceSpec
    firstSet: set[SymbolSpec]
    followSet: set[SymbolSpec]

    def __init__(self, name: str, prec: PrecedenceSpec) -> None:
        self.name = name
        self.prec = prec
        self.firstSet = set()
        self.followSet = set()

    def __repr__(self) -> str:
        return self.name

    def __str__(self) -> str:
        return repr(self)

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
    token_re: ClassVar[re.Pattern[str]] = re.compile(r"([A-Za-z]\w*)")
    precedence_tok_re: ClassVar[re.Pattern[str]] = re.compile(
        r"\[([A-Za-z]\w*)\]"
    )

    def __init__(
        self,
        nontermType: Type[Nonterm],
        name: str,
        qualified: str,
        prec: PrecedenceSpec,
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
        prec: PrecedenceSpec,
    ) -> None:
        super().__init__(name, prec)
        self.tokenType = tokenType


class Item:
    production: Production
    dotPos: int
    symbol: Optional[SymbolSpec]

    def __init__(
        self,
        production: Production,
        dotPos: int,
    ) -> None:
        self.production = production
        self.dotPos = dotPos
        rhsLen = len(production.rhs)
        if dotPos == rhsLen:
            self.symbol = None
        elif dotPos < rhsLen:
            self.symbol = production.rhs[dotPos]
        else:
            raise AssertionError("dotPos outside of RHS!")

    def __repr__(self) -> str:
        strs = []
        strs.append("[%r ::=" % self.production.lhs)
        assert self.dotPos <= len(self.production.rhs)
        i = 0
        while i < self.dotPos:
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(" *")
        while i < len(self.production.rhs):
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(".] [%s]" % (self.production.prec.name,))

        return "".join(strs)

    def lr0__repr__(self) -> str:
        strs = []
        strs.append("%r ::=" % self.production.lhs)
        assert self.dotPos <= len(self.production.rhs)
        i = 0
        while i < self.dotPos:
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(" *")
        while i < len(self.production.rhs):
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(". [%s]" % self.production.prec.name)

        return "".join(strs)


_item_cache: dict[tuple[Production, int], Item] = {}


class Production:
    def __init__(
        self,
        method: Callable[..., Nonterm | None],
        qualified: str,
        prec: PrecedenceSpec,
        lhs: NontermSpec,
        rhs: Tuple[SymbolSpec, ...],
    ) -> None:
        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs

    def __getstate__(
        self,
    ) -> Tuple[str, PrecedenceSpec, NontermSpec, Tuple[SymbolSpec, ...]]:
        return (self.qualified, self.prec, self.lhs, self.rhs)

    def __setstate__(
        self,
        data: Tuple[str, PrecedenceSpec, NontermSpec, Tuple[SymbolSpec, ...]],
    ) -> None:
        # Convert qualified name to a function reference.
        (self.qualified, self.prec, self.lhs, self.rhs) = data
        elms = self.qualified.split(".")
        assert len(elms) > 1
        module = sys.modules[elms[0]]
        method = module.__dict__[elms[1]]
        for elm in elms[2:]:
            method = method.__dict__[elm]
        self.method = method

    def __repr__(self) -> str:
        return "%r ::= %s. [%s]" % (
            self.lhs,
            " ".join(["%r" % elm for elm in self.rhs]),
            self.prec.name,
        )

    def item(self, dotPos: int) -> Item:
        key = (self, dotPos)
        item = _item_cache.get(key)
        if item is None:
            item = Item(self, dotPos)
            _item_cache[key] = item
        return item

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


ActionState = Dict[SymbolSpec, List[Action]]
GotoState = Dict[SymbolSpec, int]
