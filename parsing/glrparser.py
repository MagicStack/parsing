from __future__ import annotations
from typing import Iterator, List

from parsing.ast import Symbol, Nonterm, Token
from parsing.errors import UnexpectedToken
from parsing.grammar import (
    Production,
    SymbolSpec,
    TokenSpec,
    EndOfInput,
    ShiftAction,
    ReduceAction,
)
from parsing.lrparser import Lr


class GssPathStep:
    pass


class Gssn(GssPathStep):
    """Graph-structured stack node."""

    def __init__(
        self, below: Gssn | None, value: Symbol | None, nextState: int
    ) -> None:
        self._edges: list[Gsse] = []
        if below is not None and value is not None:
            Gsse(below, self, value)
        self.nextState = nextState

    def __repr__(self) -> str:
        return "[%d]" % self.nextState

    @property
    def edge(self) -> Gsse:
        assert len(self._edges) == 1
        return self._edges[0]

    def edges(self) -> Iterator[Gsse]:
        for edge in self._edges:
            yield edge

    def nodes(self) -> Iterator[Gssn]:
        for edge in self._edges:
            yield edge.node

    # Iterate over all paths of length pathLen.  Path length is measured as the
    # number of edges in the path, so a path of length 0 still consists of a
    # single node.
    #
    # Each path is encoded as a list that alternates between nodes and edges,
    # where the first and last elements are always nodes.
    #
    # <e>-grammars can cause cycles, which requires that we avoid infinite
    # recursion.
    def paths(
        self, pathLen: int | None = None
    ) -> Iterator[tuple[GssPathStep, ...]]:
        assert pathLen is None or isinstance(pathLen, int) and pathLen >= 0

        for path in self._pathsRecurse(pathLen, []):
            yield path

    def _pathsRecurse(
        self, pathLen: int | None, path: list[GssPathStep]
    ) -> Iterator[tuple[GssPathStep, ...]]:
        path.insert(0, self)
        if pathLen is None and len(self._edges) == 0:
            yield tuple(path[:])
        elif pathLen is not None and len(path) - 1 == pathLen * 2:
            yield tuple(path[:])
        else:
            for edge in self.edges():
                # Avoid infinite recursion due to <e>-production cycles.
                if len(path) < 3 or edge != path[1]:
                    path.insert(0, edge)
                    for x in edge.node._pathsRecurse(pathLen, path):
                        yield x
                    path.pop(0)
        path.pop(0)


class Gsse(GssPathStep):
    """Graph-structured stack edge."""

    def __init__(self, below: Gssn, above: Gssn, value: Symbol) -> None:
        self.node = below
        above._edges.append(self)
        self.value = value

    def __repr__(self) -> str:
        return "{%r}" % self.value

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Gsse):
            return NotImplemented
        else:
            return self.node == other.node and self.value == other.value


#
# End graph-structured stack (GSS) classes.
# ========================================================================


class Glr(Lr):
    """
    GLR parser.  The Glr class uses a Spec instance in order to parse input
    that is fed to it via the token() method, and terminated via the eoi()
    method."""

    def reset(self) -> None:
        self._start = None

        # Initialize with a stack that is in the start state.
        self._gss: List[Gssn] = []
        top = Gssn(None, None, 0)
        self._gss.append(top)

    def token(self, token: Token) -> None:
        """
        Feed a token to the parser."""
        if self.verbose:
            print("%s" % ("-" * 80))
            print("INPUT: %r" % token)
        tokenSpec = self._spec.sym_spec(token)
        self._act(token, tokenSpec)  # type: ignore
        if len(self._gss) == 0:
            raise UnexpectedToken(f"Unexpected token: {token:r}")

    def eoi(self) -> None:
        """
        Signal end-of-input to the parser."""
        token = EndOfInput()
        self.token(token)

        # Gather the start symbols from the stacks.
        self._start = []
        for top in self._gss:
            for path in top.paths():
                assert len(path) == 5
                if self.verbose:
                    print(f"   --> accept {path:r}")
                edge = path[1]
                assert isinstance(edge, Gsse)
                assert isinstance(edge.value, Nonterm)
                assert (
                    self._spec.sym_spec(edge.value) == self._start_sym
                )
                self._start.append(edge.value)

        if len(self._start) == 0:
            raise UnexpectedToken("Unexpected end of input")

        if self.verbose:
            print("Start: %r" % self._start)
            print("%s" % ("-" * 80))

    def _act(self, sym: Token, symSpec: TokenSpec) -> None:
        self._reductions(sym, symSpec)
        self._shifts(sym, symSpec)

    def _reductions(self, sym: Token, symSpec: TokenSpec) -> None:
        # epsilons is a dictionary that maps production-->[tops].  The purpose
        # is to avoid repeating the same epsilon production on a particular
        # stack top.  Ordinary productions do not require this care because we
        # can notice when a path has already been used for a production.
        epsilons = {}

        if self.verbose:
            nReduces = 0

        # Enqueue work.
        workQ: list[tuple[tuple[GssPathStep, ...], Production]] = []
        i = 0
        while i < len(self._gss):
            top = self._gss[i]
            if symSpec not in self._action[top.nextState]:
                # Unexpected token for this stack.
                self._gss.pop(i)
            else:
                for action in self._action[top.nextState][symSpec]:
                    if type(action) is ReduceAction:
                        if len(action.production.rhs) == 0:
                            if action.production not in epsilons:
                                assert (
                                    len([path for path in top.paths(0)]) == 1
                                )
                                path = [p for p in top.paths(0)][0]
                                epsilons[action.production] = [top]
                                workQ.append((path, action.production))
                                if self.verbose:
                                    print(
                                        "   --> enqueue(a) %r"
                                        % action.production
                                    )
                                    print("                  %r" % path)
                            elif top not in epsilons[action.production]:
                                assert (
                                    len([path for path in top.paths(0)]) == 1
                                )
                                path = [p for p in top.paths(0)][0]
                                epsilons[action.production].append(top)
                                workQ.append((path, action.production))
                                if self.verbose:
                                    print(
                                        "   --> enqueue(b) %r"
                                        % action.production
                                    )
                                    print("                  %r" % path)
                        else:
                            # Iterate over all reduction paths through stack
                            # and enqueue them.
                            for path in top.paths(len(action.production.rhs)):
                                workQ.append((path, action.production))
                                if self.verbose:
                                    print(
                                        "   --> enqueue(c) %r"
                                        % action.production
                                    )
                                    print("                  %r" % path)
                i += 1

        # Process the work queue.
        while len(workQ) > 0:
            (path, production) = workQ.pop(0)

            if self.verbose:
                print("   --> reduce %r" % production)
                print("              %r" % path)
                nReduces += 1

            self._glr_reduce(workQ, epsilons, path, production, symSpec)

        if self.verbose:
            if nReduces > 0:
                self._printStack()

    def _glr_reduce(
        self,
        workQ: list[tuple[tuple[GssPathStep, ...], Production]],
        epsilons: dict[Production, list[Gssn]],
        path: tuple[GssPathStep, ...],
        production: Production,
        symSpec: SymbolSpec,
    ) -> None:
        assert len(path[1::2]) == len(production.rhs)

        # Build the list of RHS semantic values to pass to the reduction
        # action.
        rhs = [edge.value for edge in path[1::2]]  # type: ignore

        # Call the user reduction method.
        r = self._production(production, rhs)

        below = path[0]
        assert isinstance(below, Gssn)
        done = False
        for top in self._gss:
            if top.nextState == self._goto[below.nextState][production.lhs]:
                # top is compatible with the reduction result we want to add to
                # the set of stack tops.
                for edge in top.edges():
                    if edge.node == below:
                        nonterm = edge.value
                        assert isinstance(nonterm, Nonterm)
                        # There is already a below<--top link, so merge
                        # competing interpretations.
                        if self.verbose:
                            print("   --> merge %r <--> %r" % (nonterm, r))
                        value = production.lhs.nontermType.merge(nonterm, r)
                        if self.verbose:
                            if value == edge.value:
                                print(
                                    "             %s"
                                    % ("-" * len("%r" % nonterm))
                                )
                            else:
                                print(
                                    "             %s      %s"
                                    % (
                                        (" " * len("%r" % nonterm)),
                                        "-" * len("%r" % r),
                                    )
                                )
                        edge.value = value
                        done = True
                        break
                if not done:
                    # Create a new below<--top link.
                    edge = Gsse(below, top, r)
                    if self.verbose:
                        print("   --> shift(b) %r" % top)

                    # Enqueue reduction paths that were created as a result of
                    # the new link.
                    self._enqueueLimitedReductions(
                        workQ, epsilons, edge, symSpec
                    )
                    done = True
                break
        if not done:
            # There is no compatible stack top, so create a new one.
            top = Gssn(below, r, self._goto[below.nextState][production.lhs])
            self._gss.append(top)
            if self.verbose:
                print(
                    "   --> shift(c) %r"
                    % self._goto[below.nextState][production.lhs]
                )
            self._enqueueLimitedReductions(workQ, epsilons, top.edge, symSpec)

    # Enqueue paths that incorporate edge.
    def _enqueueLimitedReductions(
        self,
        workQ: list[tuple[tuple[GssPathStep, ...], Production]],
        epsilons: dict[Production, list[Gssn]],
        edge: Gsse,
        symSpec: SymbolSpec,
    ) -> None:
        gotos = self._goto

        for top in self._gss:
            if symSpec in self._action[top.nextState]:
                for action in self._action[top.nextState][symSpec]:
                    if type(action) is ReduceAction:
                        if len(action.production.rhs) == 0:
                            if (
                                gotos[top.nextState][action.production.lhs]
                                == top.nextState
                            ):
                                # Do nothing, since enqueueing a reduction
                                # would result in performing the same reduction
                                # twice.
                                pass
                            elif action.production not in epsilons:
                                p = (top,)
                                epsilons[action.production] = [top]
                                workQ.append((p, action.production))
                                if self.verbose:
                                    print(
                                        "   --> enqueue(d) %r"
                                        % action.production
                                    )
                                    print("                  %r" % p)
                            elif top not in epsilons[action.production]:
                                path = (top,)
                                epsilons[action.production].append(top)
                                workQ.append((path, action.production))
                                if self.verbose:
                                    print(
                                        "   --> enqueue(e) %r"
                                        % action.production
                                    )
                                    print("                  %r" % path)
                        else:
                            # Iterate over all reduction paths through stack
                            # and enqueue them if they incorporate edge.
                            for rp in top.paths(len(action.production.rhs)):
                                if edge in rp[1::2]:
                                    workQ.append((rp, action.production))
                                    if self.verbose:
                                        print(
                                            "   --> enqueue(f) %r"
                                            % action.production
                                        )
                                        print("                  %r" % rp)

    def _shifts(self, sym: Token, symSpec: TokenSpec) -> None:
        prevGss = self._gss
        self._gss = []

        if self.verbose:
            nShifts = 0

        for topA in prevGss:
            if symSpec in self._action[topA.nextState]:
                for action in self._action[topA.nextState][symSpec]:
                    if type(action) is ShiftAction:
                        merged = False
                        for topB in self._gss:
                            if topB.nextState == topA.nextState:
                                Gsse(topA, topB, sym)
                                merged = True
                                break
                        if not merged:
                            top = Gssn(topA, sym, action.nextState)
                            self._gss.append(top)
                            if self.verbose:
                                print("   --> shift(a) %d" % action.nextState)
                                nShifts += 1
        if self.verbose:
            if nShifts > 0:
                self._printStack()

    def _printStack(self) -> None:
        i = 0
        for top in self._gss:
            for path in top.paths():
                if i == 0:
                    print("STK 0:", end=" ")
                else:
                    print("    %d:" % i, end=" ")
                for elm in path:
                    print("%r" % elm, end=" ")
                print()
                i += 1
