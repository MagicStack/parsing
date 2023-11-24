from __future__ import annotations

from parsing.ast import Symbol, Nonterm, Token
from parsing.errors import UnexpectedToken
from parsing.grammar import (
    Production,
    TokenSpec,
    EndOfInput,
    Epsilon,
    ShiftAction,
    ReduceAction,
)
from parsing.interfaces import Parser, Spec


class Lr(Parser):
    """
    LR(1) parser.  The Lr class uses a Spec instance in order to parse
    input that is fed to it via the token() method, and terminated via the
    eoi() method.
    """

    _spec: Spec
    _start: list[Symbol] | None
    _stack: list[tuple[Symbol, int]]

    def __init__(self, spec: Spec) -> None:
        if __debug__:
            if type(self) is Lr:
                assert spec.pureLR
        assert spec.conflicts == 0
        self._spec = spec
        self._action = spec.actions()
        self._goto = spec.goto()
        self._start_sym = spec.start_sym()
        self.reset()
        self.verbose = False

    @property
    def spec(self) -> Spec:
        return self._spec

    @property
    def start(self) -> list[Symbol] | None:
        """A list of parsing results.  For LR parsing, there is only ever one
        result, but for compatibility with the Glr interface, start is a
        list."""
        return self._start

    def reset(self) -> None:
        self._start = None
        self._stack = [(Epsilon(), 0)]

    def token(self, token: Token) -> None:
        """Feed a token to the parser."""
        tokenSpec = self._spec.sym_spec(token)
        self._act(token, tokenSpec)  # type: ignore

    def eoi(self) -> None:
        """Signal end-of-input to the parser."""
        token = EndOfInput()
        self.token(token)

        assert self._stack[-1][0] == token  # <$>.
        if self.verbose:
            self._printStack()
            print("   --> accept")
        self._stack.pop()

        self._start = [self._stack[1][0]]
        assert self._spec.sym_spec(self._start[0]) == self._start_sym

    def _act(self, sym: Token, symSpec: TokenSpec) -> None:
        if self.verbose:
            self._printStack()
            print("INPUT: %r" % symSpec)

        while True:
            top = self._stack[-1]
            if symSpec not in self._action[top[1]]:
                raise UnexpectedToken("Unexpected token: %r" % sym)

            actions = self._action[top[1]][symSpec]
            assert len(actions) == 1
            action = actions[0]

            if self.verbose:
                print("   --> %r" % action)
            if type(action) is ShiftAction:
                self._stack.append((sym, action.nextState))
                break
            else:
                assert type(action) is ReduceAction
                self._reduce(action.production)

            if self.verbose:
                self._printStack()

    def _printStack(self) -> None:
        print("STACK:", end=" ")
        for node in self._stack:
            symSpec = self._spec.sym_spec(node[0])
            print("%r" % symSpec, end=" ")
        print()
        print("      ", end=" ")
        for node in self._stack:
            symSpec = self._spec.sym_spec(node[0])
            print(
                "%r%s"
                % (
                    node[1],
                    (" " * (len("%r" % symSpec) - len("%r" % node[1]))),
                ),
                end=" ",
            )
        print()

    def _reduce(self, production: Production) -> None:
        nRhs = len(production.rhs)
        rhs = []
        for i in range(len(self._stack) - nRhs, len(self._stack)):
            rhs.append(self._stack[i][0])

        r = self._production(production, rhs)

        for i in range(nRhs):
            self._stack.pop()

        top = self._stack[-1]
        self._stack.append((r, self._goto[top[1]][production.lhs]))

    def _production(
        self, production: Production, rhs: list[Symbol]
    ) -> Nonterm:
        sym = production.lhs.nontermType()
        nRhs = len(rhs)
        assert nRhs == len(production.rhs)
        r = production.method(sym, *rhs)

        # Python's method definition syntax makes returning self from %reduce
        # methods cumbersome, so translate None here.
        if r is None:
            r = sym

        return r
