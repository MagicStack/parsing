"""
This module declares several structural ("duck typing") interfaces
that objects or classes can implement to be used in the library
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Protocol
    from parsing.ast import Token
    from parsing.automaton import Spec
    from parsing.grammar import NontermSpec, Precedence, TokenSpec

    class Parser(Protocol):
        _spec: Spec

        def token(self, token: Token) -> None:
            ...

        def eoi(
            self,
        ) -> None:
            ...

    class SymbolSpec(Protocol):
        name: str
        prec: str

    class SpecSource(Protocol):
        def get_precedences(self) -> list[Precedence]:
            ...

        def get_tokens(self) -> list[TokenSpec]:
            ...

        def get_nonterminals(self) -> tuple[list[NontermSpec], NontermSpec]:
            ...
