"""
This module declares several structural ("duck typing") interfaces
that objects or classes can implement to be used in the library
"""

from __future__ import annotations

import abc

from parsing.ast import Symbol, Token
from parsing.grammar import (
    ActionState,
    GotoState,
    SymbolSpec,
    NontermSpec,
    PrecedenceSpec,
    TokenSpec,
)


class Spec(abc.ABC):
    @abc.abstractmethod
    def sym_spec(self, sym: Symbol) -> SymbolSpec:
        raise NotImplementedError

    @abc.abstractmethod
    def actions(self) -> list[ActionState]:
        raise NotImplementedError

    @abc.abstractmethod
    def goto(self) -> list[GotoState]:
        raise NotImplementedError

    @abc.abstractmethod
    def start_sym(self) -> NontermSpec:
        raise NotImplementedError

    @abc.abstractproperty
    def pureLR(self) -> int:
        raise NotImplementedError

    @abc.abstractproperty
    def conflicts(self) -> int:
        raise NotImplementedError


class SpecSource(abc.ABC):
    @abc.abstractmethod
    def get_precedences(self) -> list[PrecedenceSpec]:
        raise NotImplementedError

    @abc.abstractmethod
    def get_tokens(self) -> list[TokenSpec]:
        raise NotImplementedError

    @abc.abstractmethod
    def get_nonterminals(self) -> tuple[list[NontermSpec], NontermSpec]:
        raise NotImplementedError


class Parser(abc.ABC):
    def __init__(self, spec: Spec) -> None:
        raise NotImplementedError

    @abc.abstractmethod
    def token(self, token: Token) -> None:
        raise NotImplementedError

    @abc.abstractmethod
    def eoi(self) -> None:
        raise NotImplementedError
