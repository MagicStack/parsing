"""
The classes in this module are used to compute the LR(1) automaton
using Pager's (1977) Practical General Method.
"""
from __future__ import annotations
from typing import (
    TYPE_CHECKING,
    Any,
    Callable,
    Dict,
    Iterable,
    Iterator,
    List,
    Optional,
    Tuple,
    Type,
)

import collections
import inspect
import time
import types
import pickle
import sys

from parsing.errors import SpecError
from parsing import interfaces
from parsing import introspection
from parsing import module_spec
from parsing.ast import Symbol
from parsing.grammar import (
    Item,
    PrecedenceSpec,
    PrecedenceRef,
    Production,
    TokenSpec,
    NontermSpec,
    SymbolSpec,
    EndOfInput,
    eoi,
    Epsilon,
    epsilon,
    NontermStart,
    Action,
    ShiftAction,
    ReduceAction,
)

if TYPE_CHECKING:
    from typing_extensions import Literal
    from parsing.interfaces import SpecSource

    SpecCompatibility = Literal[
        "compatible", "incompatible", "itemsets", "repickle"
    ]

    PickleMode = Literal["r", "w", "rw"]

    ConflictResolution = Literal[
        "neither",  # Discard both.
        "old",  # Keep old.
        "both",  # Keep both.
        "new",  # Keep new.
        "err",  # Unresolvable conflict.
    ]

    ActionState = Dict[SymbolSpec, List[Action]]
    GotoState = Dict[SymbolSpec, int]


_firstSetCache: dict[Tuple[SymbolSpec, ...], frozenset[SymbolSpec]] = {}


def computeFirstSet(s: Tuple[SymbolSpec, ...]) -> frozenset[SymbolSpec]:
    firstSet = _firstSetCache.get(s)
    if firstSet is None:
        # Calculate the first set for the string encoded by the s vector.
        result = set()
        mergeEpsilon = True
        for sym in s:
            hasEpsilon = False
            for elm in sym.firstSet:
                if elm == epsilon:
                    hasEpsilon = True
                else:
                    result.add(elm)
            if not hasEpsilon:
                mergeEpsilon = False
                break
        # Merge epsilon if it was in the first set of every symbol.
        if mergeEpsilon:
            result.add(epsilon)

        # Cache the result.
        _firstSetCache[s] = firstSet = frozenset(result)

    return firstSet


class ItemSet:
    def __init__(self, items: Iterable[Tuple[Item, set[SymbolSpec]]]) -> None:
        self._kernel: Dict[Item, set[SymbolSpec]] = {}
        self._added: Dict[Item, set[SymbolSpec]] = {}
        self._symMap: dict[SymbolSpec, set[Item]] = {}
        self._all: collections.ChainMap[
            Item, set[SymbolSpec]
        ] = collections.ChainMap(self._kernel, self._added)

        for item, lookahead in items:
            assert item.production.lhs.name == "<S>" or item.dotPos != 0
            self._kernel[item] = set(lookahead)
            sym = item.symbol
            if sym is not None:
                if sym in self._symMap:
                    self._symMap[sym].add(item)
                else:
                    self._symMap[sym] = {item}

        self._hash = hash(frozenset(self._kernel))

    def __repr__(self) -> str:
        kernel = ", ".join(
            self._item_repr(i, la) for i, la in self._kernel.items()
        )
        added = ", ".join(
            self._item_repr(i, la) for i, la in self._added.items()
        )
        return "ItemSet(kernel: %s, added: %r)" % (kernel, added)

    def _item_repr(self, item: Item, lookahead: set[SymbolSpec]) -> str:
        strs = []
        strs.append("[%r ::=" % item.production.lhs)
        assert item.dotPos <= len(item.production.rhs)
        i = 0
        while i < item.dotPos:
            strs.append(" %r" % item.production.rhs[i])
            i += 1
        strs.append(" *")
        while i < len(item.production.rhs):
            strs.append(" %r" % item.production.rhs[i])
            i += 1
        strs.append(
            "., %s] [%s]"
            % (
                "/".join(["%r" % sym for sym in lookahead]),
                item.production.prec.name,
            )
        )

        return "".join(strs)

    def __len__(self) -> int:
        return len(self._kernel)

    def __hash__(self) -> int:
        return self._hash

    def __eq__(self, other: Any) -> bool:
        if type(other) == ItemSet:
            return self._kernel.keys() == other._kernel.keys()
        else:
            return NotImplemented

    def __iter__(self) -> Iterator[Item]:
        return iter(self._all)

    def items(self) -> Iterator[Tuple[Item, set[SymbolSpec]]]:
        return iter(self._all.items())

    # Merge an added item.
    def addedAppend(self, item: Item, lookahead: Iterable[SymbolSpec]) -> bool:
        assert item.dotPos == 0
        assert item.production.lhs.name != "<S>"

        curLookahead = self._added.get(item)
        if curLookahead is not None:
            oldLen = len(curLookahead)
            curLookahead.update(lookahead)
            return oldLen != len(curLookahead)
        else:
            self._added[item] = set(lookahead)
            sym = item.symbol
            if sym is not None:
                if sym in self._symMap:
                    self._symMap[sym].add(item)
                else:
                    self._symMap[sym] = {item}
            return True

    # Given a list of items, compute their closure and merge the results into
    # the set of added items.
    def _closeItems(
        self, items: Iterable[Tuple[Item, Iterable[SymbolSpec]]]
    ) -> None:
        # Iterate over the items until no more can be added to the closure.
        worklist = list(items)
        i = 0
        while i < len(worklist):
            item, lookahead = worklist[i]
            sym = item.symbol
            if isinstance(sym, NontermSpec):
                prefix = item.production.rhs[item.dotPos + 1 :]
                for lookaheadSym in lookahead:
                    firstSet = computeFirstSet(prefix + (lookaheadSym,))
                    for prod in sym.productions:
                        tItem = prod.item(0)
                        if self.addedAppend(tItem, firstSet):
                            worklist.append((tItem, firstSet))
            i += 1

    # Calculate and merge the kernel's transitive closure.
    def closure(self) -> None:
        self._closeItems(self._kernel.items())

    # Calculate the kernel of the goto set, given a particular symbol.
    def goto(self, sym: SymbolSpec) -> ItemSet | None:
        items = self._symMap.get(sym)
        if items:
            return ItemSet(
                (i.production.item(i.dotPos + 1), self._all[i]) for i in items
            )
        else:
            return None

    # Merge the kernel of other into this ItemSet, then update the closure.
    # It is not sufficient to copy other's added items, since other has not
    # computed its closure.
    def merge(self, other: ItemSet) -> bool:
        items = []
        for item, oLookahead in other._kernel.items():
            sLookahead = self._kernel.get(item)
            # The other ItemSet must be weakly compatible with this one,
            # so it must share the kernel.
            assert sLookahead is not None
            tLookahead = oLookahead - sLookahead
            if tLookahead:
                sLookahead.update(tLookahead)
                items.append((item, tLookahead))

        if items:
            self._closeItems(items)
            return True
        else:
            return False

    # Determine if self and other are weakly compatible, as defined by the
    # Pager(1977) algorithm.
    def weakCompat(self, other: ItemSet) -> bool:
        # Check for identical kernel LR(0) items,
        # and pair items, for later use.
        if len(self) != len(other):
            return False
        pairs = []
        for sItem, sLookahead in self._kernel.items():
            if sItem not in other._kernel:
                return False
            oLookahead = other._kernel[sItem]
            pairs.append((sLookahead, oLookahead))

        # Check for lookahead compatibility.
        for i in range(len(pairs) - 1):
            isLookahead, ioLookahead = pairs[i]
            for j in range(i + 1, len(pairs)):
                jsLookahead, joLookahead = pairs[j]
                if (
                    (
                        not isLookahead.isdisjoint(joLookahead)
                        or not ioLookahead.isdisjoint(jsLookahead)
                    )
                    and isLookahead.isdisjoint(jsLookahead)
                    and ioLookahead.isdisjoint(joLookahead)
                ):
                    return False
        return True


class Spec(interfaces.Spec):
    """
    The Spec class contains the read-only data structures that the Parser
    class needs in order to parse input.  Parser generation results in a
    Spec instance, which can then be shared by multiple Parser instances."""

    _sym2spec: dict[type[Symbol], SymbolSpec]

    def sym_spec(self, sym: Symbol) -> SymbolSpec:
        return self._sym2spec[type(sym)]

    def actions(self) -> list[ActionState]:
        return self._action

    def goto(self) -> list[GotoState]:
        return self._goto

    def start_sym(self) -> NontermSpec:
        return self._userStartSym

    @property
    def pureLR(self) -> int:
        return self._nConflicts + self._nImpure == 0

    @property
    def conflicts(self) -> int:
        return self._nConflicts

    def __init__(
        self,
        source: types.ModuleType | List[types.ModuleType] | SpecSource,
        pickleFile: Optional[str] = None,
        pickleMode: PickleMode = "rw",
        skinny: bool = True,
        logFile: Optional[str] = None,
        graphFile: Optional[str] = None,
        verbose: bool = False,
        unpickleHook: Callable[[Spec, SpecCompatibility], None] | None = None,
    ) -> None:
        """
        modules : Either a single module, or a list of modules, wherein to
                  look for parser generator directives in docstrings.

        pickleFile : The path of a file to use for Spec pickling/unpickling.

        pickleMode :  "r" : Unpickle from pickleFile.
                      "w" : Pickle to pickleFile.
                      "rw" : Unpickle/pickle from/to pickleFile.

        skinny : If true, discard all data that are only strictly necessary
                 while constructing the parsing tables.  This reduces
                 available debugging context, but substantially reduces
                 pickle size.

        logFile : The path of a file to store a human-readable copy of the
                  parsing tables in.

        graphFile : The path of a file to store a graphviz representation
                    (dot format) of the precedence relationship graph.

        verbose : If true, print progress information while generating the
                  parsing tables."""
        self._skinny = skinny
        self._verbose = verbose

        # Default (no) precedence.
        self._none = PrecedenceSpec("none", "fail", {})
        self._split = PrecedenceSpec("split", "split", {})

        # Symbols are maintained as two separate sets so that non-terminals and
        # terminals (tokens) can be operated on separately where needed.
        self._precedences = {
            self._none.name: self._none,
            self._split.name: self._split,
        }
        self._nonterms: Dict[str, NontermSpec] = {}
        self._tokens: Dict[str, TokenSpec] = {
            eoi.name: eoi,
            epsilon.name: epsilon,
        }
        self._sym2spec: Dict[Type[Symbol], SymbolSpec] = {
            EndOfInput: eoi,
            Epsilon: epsilon,
        }
        self._productions: list[Production] = []

        sources: list[types.ModuleType] | SpecSource
        if isinstance(source, types.ModuleType):
            sources = [source]
        else:
            sources = source

        if not isinstance(sources, list):
            spec_source = sources
        else:
            spec_source = module_spec.ModuleSpecSource(sources)

        if self._verbose:
            print(
                (
                    "Parsing.Spec: Introspecting to acquire formal "
                    "grammar specification..."
                )
            )

        # ===========================================================
        # Precedence.
        #
        for prec in spec_source.get_precedences():
            name = prec.name
            if name in self._precedences:
                raise SpecError("Duplicate precedence name: %s" % (name,))
            if name in self._tokens:
                raise SpecError(
                    "Identical token/precedence names: %s" % (name,)
                )
            if name in self._nonterms:
                raise SpecError(
                    "Identical nonterm/precedence names: %s" % (name,)
                )
            self._precedences[name] = prec

        # ===========================================================
        # Token.
        #
        for token in spec_source.get_tokens():
            name = token.name
            tt = token.tokenType
            if name in self._precedences:
                raise SpecError(
                    "Identical precedence/token names: %s" % tt.__doc__
                )
            if name in self._tokens:
                raise SpecError("Duplicate token name: %s" % tt.__doc__)
            if name in self._nonterms:
                raise SpecError(
                    "Identical nonterm/token names: %s" % tt.__doc__
                )
            self._tokens[name] = token
            self._sym2spec[tt] = token

        # ===========================================================
        # Nonterm.
        #
        nonterms, userStart = spec_source.get_nonterminals()
        for nonterm in nonterms:
            name = nonterm.name
            nt = nonterm.nontermType
            if name in self._precedences:
                raise SpecError(
                    "Identical precedence/nonterm names: %s" % nt.__doc__
                )
            if name in self._tokens:
                raise SpecError(
                    "Identical token/nonterm names: %s" % nt.__doc__
                )
            if name in self._nonterms:
                raise SpecError("Duplicate nonterm name: %s" % nt.__doc__)
            self._nonterms[name] = nonterm
            self._sym2spec[nt] = nonterm

        self._userStartSym = userStart
        if not isinstance(self._userStartSym, NontermSpec):
            raise SpecError("No start symbol specified")

        self._startSym = NontermSpec(
            NontermStart, "<S>", f"{__name__}.NontermStart", self._none
        )

        self._startProd = Production(
            NontermStart.reduce,
            f"{__name__}.NontermStart.reduce",
            self._none,
            self._startSym,
            (self._userStartSym, eoi),
        )

        # Augment grammar with a special start symbol and production:
        #
        #   <S> ::= S <$>.
        self._startSym.productions.add(self._startProd)
        self._nonterms["<S>"] = self._startSym
        self._productions.append(self._startProd)

        # Resolve references in the grammar specification.
        self._references(logFile, graphFile)

        # Everything below this point is computed from the above (once
        # introspection is complete).

        # Each element corresponds to an element in _action.
        self._itemSets: list[ItemSet] = []
        self._itemSetsHash: dict[ItemSet, list[int]] | None = None
        # LR parsing tables.  The tables conceptually contain one state per
        # row, where each row contains one element per symbol.  The table is
        # conceptually in row-major form, but each row is actually a
        # dictionary. If no entry for a symbol exists for a particular state,
        # then input of that symbol is an error for that state.
        self._action: list[ActionState] = []
        self._goto: list[GotoState] = []
        self._startState: int | None = None
        self._nActions = 0
        self._nConflicts = 0
        self._nImpure = 0  # Number of LR impurities (does not affect GLR).

        # Generate parse tables.
        self._prepare(pickleFile, pickleMode, logFile, unpickleHook)

    def __repr__(self) -> str:
        if self._skinny:
            # Print a very reduced summary, since most info has been discarded.
            return "Parsing.Spec: %d states, %d actions (%d split)" % (
                len(self._action),
                self._nActions,
                self._nImpure,
            )

        lines = []

        #  =================================================================
        lines.append("Precedences:")
        deco = [(prec.name, prec) for prec in self._precedences.values()]
        deco.sort()
        for elm in deco:
            prec = elm[1]
            lines.append("  %r" % prec)

        lines.append("Tokens:")
        sym: SymbolSpec
        for token in self._tokens.values():
            lines.append("  %r %r" % (token, token.prec))
            lines.append("    First set: %r" % token.firstSet)
            lines.append("    Follow set: %r" % token.followSet)

        lines.append("Non-terminals:")
        for sym in self._nonterms.values():
            lines.append("  %r %r" % (sym, sym.prec))
            lines.append("    First set: %r" % sym.firstSet)
            lines.append("    Follow set: %r" % sym.followSet)
            lines.append("    Productions:")
            for prod in sym.productions:
                lines.append("      %r" % prod)

        lines.append("Item sets:")
        for i in range(len(self._itemSets)):
            lines.append("  %d: %r" % (i, self._itemSets[i]))
        #  =================================================================

        ntokens = len(self._tokens) - 1
        nnonterms = len(self._nonterms) - 1
        nproductions = len(self._productions) - 1
        nstates = len(self._action)
        lines.append(
            "Parsing.Spec: %d token%s, %d non-terminal%s, "
            "%d production%s, %d state%s, %d action%s (%d split):"
            % (
                ntokens,
                ("s", "")[ntokens == 1],
                nnonterms,
                ("s", "")[nnonterms == 1],
                nproductions,
                ("s", "")[nproductions == 1],
                nstates,
                ("s", "")[nstates == 1],
                self._nActions,
                ("s", "")[self._nActions == 1],
                self._nImpure,
            )
        )
        if self.pureLR:
            lines.append("Algorithm compatibility: GLR, LR")
        elif self._nConflicts == 0:
            lines.append("Algorithm compatibility: GLR")
        else:
            lines.append("Algorithm compatibility: None, due to ambiguity")
        lines.append("Parsing tables:")
        for i in range(len(self._action)):
            lines.append("  %s" % ("=" * 78))
            lines.append(
                "  State %d:%s"
                % (i, ("", " (start state)")[self._startState == i])
            )
            for item in self._itemSets[i]:
                lines.append(
                    " %s%s" % (" " * (len("%d" % i) + 9), item.lr0__repr__())
                )
            lines.append("    Goto:")
            for sym in self._goto[i]:
                lines.append("    %15r : %r" % (sym, self._goto[i][sym]))
            lines.append("    Action:")
            for sym in self._action[i]:
                for action in self._action[i][sym]:
                    conflict = "   "
                    for other in self._action[i][sym]:
                        if action != other:
                            resolution = self._resolve(sym, other, action)
                            if resolution == "err":
                                conflict = "XXX"
                                break

                    if type(action) == ShiftAction:
                        lines.append(
                            "%s %15r : %-6s %d [%s]"
                            % (
                                conflict,
                                sym,
                                "shift",
                                action.nextState,
                                sym.prec.name,
                            )
                        )
                    else:
                        assert type(action) == ReduceAction
                        lines.append(
                            "%s %15r : %-6s %r"
                            % (conflict, sym, "reduce", action.production)
                        )

        ret = "\n".join(lines)
        return ret

    def _prepare(
        self,
        pickleFile: Optional[str],
        pickleMode: PickleMode,
        logFile: Optional[str],
        unpickleHook: Callable[[Spec, SpecCompatibility], None] | None = None,
    ) -> None:
        """
        Compile the specification into data structures that can be used by
        the Parser class for parsing."""
        # Check for a compatible pickle.
        compat = self._unpickle(pickleFile, pickleMode)
        if unpickleHook is not None:
            unpickleHook(self, compat)

        if self._verbose and compat in ["itemsets", "incompatible"]:
            start = time.monotonic()

        if compat == "incompatible":
            # Create the collection of sets of LR(1) items.
            self._firstSets()
            self._followSets()
            self._items()

        if compat == "compatible":
            # Just because the pickle was compatible does not mean that it is
            # valid for parsing.
            if self._nConflicts != 0:
                raise SpecError(
                    "Compatible pickle is invalid due to conflicts (%d)"
                    % self._nConflicts
                )
        if compat in ["itemsets", "incompatible"]:
            # Generate LR(1) parsing tables.
            self._lr()

            # Disambiguate actions.
            self._disambiguate()

            # Check for unused or ambiguous definitions, as well as reporting
            # ambiguities.
            try:
                self._validate(logFile)
            finally:
                if self._verbose:
                    print(
                        "Parsing.Spec: LR(1) parser generation took "
                        f"{(time.monotonic() - start) * 1000:.1f} milliseconds"
                    )
                sys.stdout.flush()
                # Pickle the spec, if method parameters so dictate, even if
                # there were validation errors, so that the pickle might be
                # used in part during later runs.
                self._pickle(pickleFile, pickleMode)
        elif compat == "repickle":
            # Pickle the spec, if method parameters so dictate.
            self._pickle(pickleFile, pickleMode)

        if self._skinny:
            # Discard data that are not needed during parsing.  Note that
            # _pickle() also discarded data that don't even need to be pickled.
            del self._precedences
            del self._nonterms
            del self._tokens
            del self._productions

    # Introspect modules and find special parser declarations.  In order to be
    # a special class, the class must both 1) be subclassed from Token or
    # Nonterm, and 2) contain the appropriate %foo docstring.
    def _introspect(self, spec_source: SpecSource) -> None:
        if self._verbose:
            print(
                (
                    "Parsing.Spec: Introspecting to acquire formal "
                    "grammar specification..."
                )
            )

        self._precedences["none"] = self._none
        self._precedences["split"] = self._split

        # ===========================================================
        # Precedence.
        #
        for prec in spec_source.get_precedences():
            name = prec.name
            if name in self._precedences:
                raise SpecError("Duplicate precedence name: %s" % (name,))
            if name in self._tokens:
                raise SpecError(
                    "Identical token/precedence names: %s" % (name,)
                )
            if name in self._nonterms:
                raise SpecError(
                    "Identical nonterm/precedence names: %s" % (name,)
                )
            self._precedences[name] = prec

        # ===========================================================
        # Token.
        #
        for token in spec_source.get_tokens():
            name = token.name
            tt = token.tokenType
            if name in self._precedences:
                raise SpecError(
                    "Identical precedence/token names: %s" % tt.__doc__
                )
            if name in self._tokens:
                raise SpecError("Duplicate token name: %s" % tt.__doc__)
            if name in self._nonterms:
                raise SpecError(
                    "Identical nonterm/token names: %s" % tt.__doc__
                )
            self._tokens[name] = token
            self._sym2spec[tt] = token

        # ===========================================================
        # Nonterm.
        #
        nonterms, userStart = spec_source.get_nonterminals()
        for nonterm in nonterms:
            name = nonterm.name
            nt = nonterm.nontermType
            if name in self._precedences:
                raise SpecError(
                    "Identical precedence/nonterm names: %s" % nt.__doc__
                )
            if name in self._tokens:
                raise SpecError(
                    "Identical token/nonterm names: %s" % nt.__doc__
                )
            if name in self._nonterms:
                raise SpecError("Duplicate nonterm name: %s" % nt.__doc__)
            self._nonterms[name] = nonterm
            self._sym2spec[nt] = nonterm

        self._userStartSym = userStart
        if not isinstance(self._userStartSym, NontermSpec):
            raise SpecError("No start symbol specified")

    # Resolve all symbolic (named) references.
    def _references(
        self, logFile: Optional[str], graphFile: Optional[str]
    ) -> None:
        # Build the graph of Precedence relationships.
        self._resolvePrec(graphFile)

        # Resolve Token-->Precedence references.
        for token in self._tokens.values():
            if isinstance(token.prec, PrecedenceRef):
                token.prec = self._precedences[token.prec.name]

        # Resolve Nonterm-->Precedence references.
        for nonterm in self._nonterms.values():
            if isinstance(nonterm.prec, PrecedenceRef):
                nonterm.prec = self._precedences[nonterm.prec.name]

        # Resolve Nonterm-->{Nonterm,Token,Precedence} references.
        for nonterm in self._nonterms.values():
            methods = inspect.getmembers(
                nonterm.nontermType,
                predicate=lambda f: (
                    inspect.isfunction(f) and isinstance(f.__doc__, str)
                ),
            )
            for k, v in methods:
                dirtoks = introspection.parse_docstring(v.__doc__)
                if dirtoks[0] == "%reduce":
                    rhs: List[SymbolSpec] = []
                    rhs_terms = []
                    prec = None
                    for i in range(1, len(dirtoks)):
                        tok = dirtoks[i]
                        m = NontermSpec.token_re.match(tok)
                        if m:
                            # Symbolic reference.
                            if tok in self._tokens:
                                rhs.append(self._tokens[tok])
                                rhs_terms.append(self._tokens[tok])
                            elif tok in self._nonterms:
                                rhs.append(self._nonterms[tok])
                            else:
                                raise SpecError(
                                    "Unknown symbol '%s' in reduction "
                                    "specification: %s" % (tok, v.__doc__)
                                )
                        else:
                            m = NontermSpec.precedence_tok_re.match(tok)
                            if m:
                                # Precedence.
                                if i < len(dirtoks) - 1:
                                    raise SpecError(
                                        "Precedence must come last in "
                                        "reduction specification: %s"
                                        % v.__doc__
                                    )
                                if m.group(1) not in self._precedences:
                                    raise SpecError(
                                        "Unknown precedence in reduction "
                                        "specification: %s" % v.__doc__
                                    )
                                prec = self._precedences[m.group(1)]

                    if prec is None:
                        # Inherit the non-terminal's precedence.
                        if rhs_terms:
                            # Inherit the precedence of the last terminal
                            # symbol in rhs
                            prec = rhs_terms[-1].prec
                        else:
                            # Inherit the non-terminal's precedence.
                            prec = nonterm.prec

                    prod = Production(
                        v,
                        "%s.%s" % (nonterm.qualified, k),
                        prec,
                        nonterm,
                        tuple(rhs),
                    )
                    assert prod not in nonterm.productions
                    nonterm.productions.add(prod)
                    self._productions.append(prod)

        if self._verbose:
            ntokens = len(self._tokens) - 1
            nnonterms = len(self._nonterms) - 1
            nproductions = len(self._productions) - 1
            print(
                "Parsing.Spec: %d token%s, "
                "%d non-terminal%s, %d production%s"
                % (
                    ntokens,
                    ("s", "")[ntokens == 1],
                    nnonterms,
                    ("s", "")[nnonterms == 1],
                    nproductions,
                    ("s", "")[nproductions == 1],
                )
            )

    # Build the graph of Precedence relationships.
    def _resolvePrec(self, graphFile: Optional[str]) -> None:
        # Resolve symbolic references and populate equiv/dominators.
        for precA in self._precedences.values():
            for precBName in precA.relationships:
                if precBName not in self._precedences:
                    raise SpecError(
                        (
                            "Precedence '%s' specifies a relationship with "
                            "unknown Precedence '%s'"
                        )
                        % (precA, precBName)
                    )
                precB = self._precedences[precBName]
                rel = precA.relationships[precBName]
                if rel == "=":
                    if precB not in precA.equiv:
                        precA.equiv.add(precB)
                        precA.equiv.update(precB.equiv)
                        precA.dominators.update(precB.dominators)
                        for precC in precB.equiv.copy():
                            precC.equiv = precA.equiv
                            precC.dominators = precA.dominators
                        assert precB.equiv is precA.equiv
                        assert precB.dominators is precA.dominators
                elif rel == "<":
                    precA.dominators.add(precB)
                elif rel == ">":
                    precB.dominators.add(precA)
                else:
                    assert False

        equiv_classes = {}
        for prec in self._precedences.values():
            equiv_classes[id(prec.equiv)] = next(iter(prec.equiv))

        # Write graphviz precedence graph to graphFile, if graphFile was
        # specified.
        if graphFile is not None:
            with open(graphFile, "w+") as f:
                if self._verbose:
                    print(
                        "Parsing.Spec: Writing graphviz "
                        "precedence graph to '%s'..." % graphFile
                    )
                print("digraph Precedence {", file=f)
                print('    graph [bgcolor=black, labeljust="l"]', file=f)
                print(
                    "    node [shape=record, style=filled, color=black, "
                    "fillcolor=gray, fontname=Helvetica, fontsize=10.0]",
                    file=f,
                )
                print("    edge [color=gray]", file=f)
                for precA in self._precedences.values():
                    if precA is next(iter(precA.equiv)):
                        print(
                            '    Precedence_%s [label="{%s}"]'
                            % (
                                precA.name,
                                "\\n".join(
                                    [
                                        "%s (%s)" % (p.name, p.assoc)
                                        for p in precA.equiv
                                    ]
                                ),
                            ),
                            file=f,
                        )
                        for precB in precA.dominators:
                            print(
                                "    Precedence_%s -> Precedence_%s"
                                % (
                                    next(iter(precB.equiv)).name,
                                    next(iter(precA.equiv)).name,
                                ),
                                file=f,
                            )
                print("}", file=f)

        # Iteratively build dominator sets until no more work can be done.
        done = False
        while not done:
            done = True
            for precA in equiv_classes.values():
                for precB in precA.dominators.copy():
                    diff = precB.equiv - precA.dominators
                    if diff:
                        precA.dominators.update(diff)
                        done = False
                    for precC in precB.dominators:
                        diff = precC.equiv - precA.dominators
                        if diff:
                            precA.dominators.update(diff)
                            done = False

        # Check for cycles in the graph.
        cycles = []
        for precA in self._precedences.values():
            for precB in set((precA,)) | precA.equiv:
                if precB in precA.dominators:
                    cycles.append(
                        "Precedence relationship cycle involving '%s'"
                        % precA.name
                    )
        if len(cycles) > 0:
            raise SpecError("\n".join(cycles))

    # Store state to a pickle file, if requested.
    def _pickle(self, file: Optional[str], mode: PickleMode) -> None:
        if self._skinny:
            # Discard data that don't need to be pickled.
            del self._startSym
            del self._startProd
            del self._itemSets
            del self._itemSetsHash
            del self._startState

        if file is not None and "w" in mode:
            if self._verbose:
                print(
                    "Parsing.Spec: Creating %s Spec pickle in %s..."
                    % (("fat", "skinny")[self._skinny], file)
                )

            with open(file, "wb") as f:
                pickle.dump(self, f, protocol=pickle.HIGHEST_PROTOCOL)

    # Restore state from a pickle file, if a compatible one is provided.  This
    # method uses the same set of return values as does _compatible().
    def _unpickle(
        self, file: Optional[str], mode: PickleMode
    ) -> SpecCompatibility:
        if file is not None and "r" in mode:
            if self._verbose:
                print(
                    "Parsing.Spec: Attempting to use pickle from "
                    'file "%s"...' % file
                )
            try:
                with open(file, "rb") as f:
                    # Any exception at all in unpickling can be assumed to be
                    # due to an incompatible pickle.
                    try:
                        spec: Spec = pickle.load(f)
                    except Exception:
                        if self._verbose:
                            error = sys.exc_info()
                            print(
                                "Parsing.Spec: Pickle load failed: "
                                "Exception %s: %s" % (error[0], error[1])
                            )
                        return "incompatible"
            except IOError:
                if self._verbose:
                    error = sys.exc_info()
                    print(
                        "Parsing.Spec: Pickle open failed: "
                        "Exception %s: %s" % (error[0], error[1])
                    )
                return "incompatible"

            compat = self._compatible(spec)
            if compat == "incompatible":
                if self._verbose:
                    print(
                        'Parsing.Spec: Pickle in "%s" '
                        "is incompatible." % file
                    )
                return compat

            if self._verbose:
                print(
                    'Parsing.Spec: Using %s pickle in "%s" (%s)...'
                    % (("fat", "skinny")[spec._skinny], file, compat)
                )

            if compat in ["compatible", "repickle"]:
                # Copy spec's data structures.
                self._precedences = spec._precedences
                self._action = spec._action
                self._goto = spec._goto
                if not self._skinny:
                    self._startState = spec._startState
                self._nActions = spec._nActions
                self._nConflicts = spec._nConflicts
                self._nImpure = spec._nImpure
            elif compat == "itemsets":
                # Precedences are incompatible, so great care has to be taken
                # when copying from the pickle.  Overwrite all precedence
                # specifications in spec with the new ones, then copy over all
                # of the new symbols/productions (but not the new precedences,
                # of course).  This still leaves table generation, which is
                # done by the _prepare() method later.

                # Nonterminals.
                for key in self._nonterms:
                    nontermSelf = self._nonterms[key]
                    nontermSpec = spec._nonterms[key]
                    nontermSpec.prec = nontermSelf.prec
                    # Productions.
                    for prodSelf in nontermSelf.productions:
                        for prodSpec in nontermSpec.productions:
                            if prodSelf.qualified == prodSpec.qualified:
                                prodSpec.prec = prodSelf.prec
                                break
                        assert prodSelf.qualified == prodSpec.qualified
                # Tokens.
                for key in self._tokens:
                    tokenSelf = self._tokens[key]
                    tokenSpec = spec._tokens[key]
                    tokenSpec.prec = tokenSelf.prec
            else:
                assert False

            # Copy spec data structures that are usable regardless of whether
            # the parsing tables need to be rebuilt.
            self._nonterms = spec._nonterms
            self._tokens = spec._tokens
            self._sym2spec = spec._sym2spec
            self._productions = spec._productions
            self._userStartSym = spec._userStartSym
            if not self._skinny:
                self._startSym = spec._startSym
                self._startProd = spec._startProd
                self._itemSets = spec._itemSets
                self._itemSetsHash = spec._itemSetsHash

            return compat
        else:
            return "incompatible"

    # Determine whether other is compatible with self.  Note that self is not
    # completely initialized; the idea here is to determine whether other's
    # data structures can be copied *before* doing the work of building parsing
    # tables.
    #
    # Itemsets and precedences are not directly related, other than that
    # symbols have precedences associated with them.  Therefore, we check for
    # the following cases:
    #
    #   "compatible" : Completely compatible.
    #
    #   "repickle" : Compatible, but pickle needs to be regenerated.
    #
    #   "itemsets" : Itemsets are compatible, but precedence specifications are
    #                not.
    #
    #   "incompatible" : No useful compatibility.
    def _compatible(self, other: Spec) -> SpecCompatibility:
        ret: SpecCompatibility = "compatible"

        if (not self._skinny) and other._skinny:
            return "incompatible"
        elif self._skinny != other._skinny:
            ret = "repickle"

        # Precedences.
        if len(self._precedences) != len(other._precedences):
            if self._verbose:
                print(
                    "Parsing.Spec: Unequal number of precedences (%d vs %d)"
                    % (len(self._precedences), len(other._precedences))
                )
            ret = "itemsets"
        for key in self._precedences:
            if key not in other._precedences:
                if self._verbose:
                    print("Parsing.Spec: Missing precedence: %s" % key)
                ret = "itemsets"
                continue
            precA = self._precedences[key]
            precB = other._precedences[key]
            if (
                precA.name != precB.name
                or precA.assoc != precB.assoc
                or len(precA.relationships) != len(precB.relationships)
            ):
                if self._verbose:
                    print(
                        "Parsing.Spec: Incompatible precedences: %r vs. %r"
                        % (precA, precB)
                    )
                ret = "itemsets"
                continue
            for prec in precA.relationships:
                rel = precA.relationships[prec]
                if (
                    prec not in precB.relationships
                    or precB.relationships[prec] != rel
                ):
                    if self._verbose:
                        print(
                            "Parsing.Spec: Incompatible precedences: "
                            "%r vs. %r" % (precA, precB)
                        )
                    ret = "itemsets"
                    break

        # Nonterminals.
        if len(self._nonterms) != len(other._nonterms):
            if self._verbose:
                print(
                    "Parsing.Spec: Unequal number of non-terminals "
                    "(%d vs %d)" % (len(self._nonterms), len(other._nonterms))
                )
            return "incompatible"
        for key in self._nonterms:
            if key not in other._nonterms:
                if self._verbose:
                    print("Parsing.Spec: Missing non-terminal: %s" % key)
                return "incompatible"
            nontermA = self._nonterms[key]
            nontermB = other._nonterms[key]
            if (
                nontermA.name != nontermB.name
                or nontermA.qualified != nontermB.qualified
                or nontermA.nontermType != nontermB.nontermType
            ):
                if self._verbose:
                    print(
                        "Parsing.Spec: Incompatible non-terminals: "
                        "%r vs. %r" % (nontermA, nontermB)
                    )
                return "incompatible"
            if nontermA.prec.name != nontermB.prec.name:
                if self._verbose:
                    print(
                        (
                            "Parsing.Spec: Differing precedences for "
                            + "non-terminal: %r"
                        )
                        % nontermA
                    )
                ret = "itemsets"

            # Productions.
            if len(nontermA.productions) != len(nontermB.productions):
                if self._verbose:
                    print(
                        "Parsing.Spec: Unequal number of productions "
                        "(%d vs %d)"
                        % (
                            len(self._productions) - 1,
                            len(other._productions) - 1,
                        )
                    )
                return "incompatible"
            for prodA in nontermA.productions:
                match = False
                for prodB in nontermB.productions:
                    if (
                        prodA.qualified == prodB.qualified
                        and prodA.lhs.name == prodB.lhs.name
                        and len(prodA.rhs) == len(prodB.rhs)
                    ):
                        match = True
                        for i in range(len(prodA.rhs)):
                            if prodA.rhs[i].name != prodB.rhs[i].name:
                                match = False
                                if self._verbose:
                                    print(
                                        "Parsing.Spec: Incompatible"
                                        " productions: %r vs. %r"
                                        % (prodA, prodB)
                                    )
                                break
                        if prodA.prec.name != prodB.prec.name:
                            if self._verbose:
                                print(
                                    "Parsing.Spec: Differing precedences "
                                    "for production: %r" % prodA
                                )
                            ret = "itemsets"
                if not match:
                    return "incompatible"

        # Tokens.
        if len(self._tokens) != len(other._tokens):
            if self._verbose:
                print(
                    "Parsing.Spec: Unequal number of tokens (%d vs %d)"
                    % (len(self._tokens), len(other._tokens))
                )
            return "incompatible"
        for key in self._tokens:
            if key not in other._tokens:
                if self._verbose:
                    print("Parsing.Spec: Missing token: %s" % key)
                return "incompatible"
            tokenA = self._tokens[key]
            tokenB = other._tokens[key]
            if (
                tokenA.name != tokenB.name
                or tokenA.tokenType != tokenB.tokenType
            ):
                if self._verbose:
                    print(
                        "Parsing.Spec: Incompatible tokens: %r vs. %r"
                        % (tokenA, tokenB)
                    )
                return "incompatible"
            if tokenA.prec.name != tokenB.prec.name:
                if self._verbose:
                    print(
                        "Parsing.Spec: Differing precedences for token: %r"
                        % tokenA
                    )
                ret = "itemsets"

        # User start symbol.
        if self._userStartSym.name != other._userStartSym.name:
            if self._verbose:
                print(
                    "Parsing.Spec: Differing start symbols: %s vs. %s"
                    % (self._userStartSym.name, other._userStartSym.name)
                )
            return "incompatible"

        if other._skinny and ret == "itemsets":
            # The itemsets have to be regenerated, since they weren't pickled.
            ret = "incompatible"
        return ret

    # Check for unused prececence/token/nonterm/reduce specifications, then
    # throw a SpecError if any ambiguities exist in the grammar.
    def _validate(self, logFile: Optional[str]) -> None:
        if self._verbose:
            print("Parsing.Spec: Validating grammar...")

        lines = []
        if self._nConflicts > 0:
            lines.append(
                "Parsing.Spec: %d unresolvable conflict%s"
                % (self._nConflicts, ("s", "")[self._nConflicts == 1])
            )

        # Previous code guarantees that all precedence/token/nonterm names are
        # unique.  Therefore, we can build a single dictionary here that keys
        # on names.
        used: Dict[str, Any] = {}
        productions = []
        for itemSet in self._itemSets:
            for item, lookahead in itemSet.items():
                productions.append(item.production)
                used[item.production.prec.name] = item.production.prec
                lhs: Tuple[SymbolSpec, ...] = (item.production.lhs,)
                for sym in lhs + item.production.rhs:
                    used[sym.name] = sym
                    used[sym.prec.name] = sym.prec

                for token_spec in lookahead:
                    used[token_spec.prec.name] = token_spec.prec

        nUnused = 0

        # Precedences.
        for prec in self._precedences:
            if prec not in [self._none.name, self._split.name]:
                if prec not in used:
                    nUnused += 1
                    lines.append(
                        "Parsing.Spec: Unused precedence: %r"
                        % self._precedences[prec]
                    )

        # Tokens.
        for token in self._tokens:
            if token not in [eoi.name, epsilon.name]:
                if token not in used:
                    nUnused += 1
                    lines.append(
                        "Parsing.Spec: Unused token: %s" % self._tokens[token]
                    )

        # Nonterms.
        for nonterm in self._nonterms:
            if nonterm not in [self._startSym.name]:
                if nonterm not in used:
                    nUnused += 1
                    lines.append(
                        "Parsing.Spec: Unused nonterm: %s"
                        % self._nonterms[nonterm]
                    )

        # Productions.
        for production in self._productions:
            if production not in productions:
                nUnused += 1
                lines.append(
                    "Parsing.Spec: Unused production: %r" % production
                )

        if nUnused > 0:
            lines.insert(
                (1, 0)[self._nConflicts == 0],
                "Parsing.Spec: %d unused definition%s"
                % (nUnused, ("s", "")[nUnused == 1]),
            )

        # Write to logFile, if one was specified.
        if logFile is not None:
            with open(logFile, "w+") as f:
                if self._verbose:
                    print("Parsing.Spec: Writing log to '%s'..." % logFile)
                f.write("%s" % "\n".join(lines + ["%r" % self]))

        # Conflicts are fatal.
        if self._nConflicts > 0:
            raise SpecError("%s" % ("\n".join(lines)))

        # Make sure to let the user know about unused symbols if verbosity is
        # enabled, and there weren't any conflicts to cause notification via an
        # exception.
        if self._verbose:
            ntokens = len(self._tokens) - 1
            nnonterms = len(self._nonterms) - 1
            nproductions = len(self._productions) - 1
            lines.append(
                "Parsing.Spec: %d token%s, %d non-terminal%s, %d production%s"
                % (
                    ntokens,
                    ("s", "")[ntokens == 1],
                    nnonterms,
                    ("s", "")[nnonterms == 1],
                    nproductions,
                    ("s", "")[nproductions == 1],
                )
            )
            sys.stdout.write("%s\n" % "\n".join(lines))

    # Compute the first sets for all symbols.
    def _firstSets(self) -> None:
        # Terminals.
        # first(X) is X for terminals.
        for sym in self._tokens.values():
            sym.firstSetMerge(sym)

        # Non-terminals.
        #
        # Repeat the following loop until no more symbols can be added to any
        # first set.
        done = False
        while not done:
            done = True
            for name in self._nonterms:
                nonterm = self._nonterms[name]
                for prod in nonterm.productions:
                    # Merge epsilon if there is an empty production.
                    if len(prod.rhs) == 0:
                        if not nonterm.firstSetMerge(epsilon):
                            done = False

                    # Iterate through the RHS and merge the first sets into
                    # this symbol's, until a preceding symbol's first set does
                    # not contain epsilon.
                    for elm in prod.rhs:
                        containsEpsilon = False
                        for elmSym in elm.firstSet:
                            if not nonterm.firstSetMerge(elmSym):
                                done = False
                            if elmSym == epsilon:
                                containsEpsilon = True
                        if not containsEpsilon:
                            break

    # Compute the follow sets for all symbols.
    def _followSets(self) -> None:
        self._startSym.followSet = {epsilon}

        # Repeat the following loop until no more symbols can be added to any
        # follow set.
        done = False
        while not done:
            done = True
            for name in self._nonterms:
                sym = self._nonterms[name]
                for prod in sym.productions:
                    # For all A ::= aBb, merge first(b) into follow(B).
                    for i in range(len(prod.rhs) - 1):
                        for j in range(i + 1, len(prod.rhs)):
                            if not prod.rhs[i].followSetMerge(
                                prod.rhs[j].firstSet
                            ):
                                done = False
                            if epsilon not in prod.rhs[j].firstSet:
                                break

                    # For A ::= ab, or A ::= aBb where first(b) contains <e>,
                    # merge follow(A) into follow(B).
                    for i in range(len(prod.rhs) - 1, -1, -1):
                        if not prod.rhs[i].followSetMerge(prod.lhs.followSet):
                            done = False
                        if epsilon not in prod.rhs[i].firstSet:
                            break

    # Compute the collection of sets of LR(1) items.
    def _items(self) -> None:
        # Add {[S' ::= * S $., <e>]} to _itemSets.
        tItem = self._startProd.item(0)
        tLookahead: set[SymbolSpec] = {epsilon}
        tItemSet = ItemSet(((tItem, tLookahead),))
        tItemSet.closure()
        self._itemSets.append(tItemSet)
        itemSetCount = 1

        # List of state numbers that need to be processed.
        worklist = [0]
        if self._verbose:
            nwork = len(worklist)
            print(
                "Parsing.Spec: Generating LR(1) itemset collection... ",
                end=" ",
            )
            sys.stdout.write("+")
            sys.stdout.flush()

        # itemSetsHash uses itemsets as keys.  A value is a list of _itemSets
        # indices; these itemsets are the ones referred to the key itemset.
        itemSetsHash = {tItemSet: [0]}

        syms: List[SymbolSpec] = list(self._tokens.values())
        syms += list(self._nonterms.values())
        while worklist:
            if self._verbose:
                if abs(len(worklist) - nwork) >= 10:
                    nwork = len(worklist)
                    sys.stdout.write("[%d/%d]" % (len(worklist), itemSetCount))
                    sys.stdout.flush()

            i = worklist.pop(0)
            itemSet = self._itemSets[i]
            for sym in syms:
                gotoSet = itemSet.goto(sym)
                if gotoSet is None:
                    continue
                merged = False
                if gotoSet in itemSetsHash:
                    for j in itemSetsHash[gotoSet]:
                        mergeSet = self._itemSets[j]
                        if mergeSet.weakCompat(gotoSet):
                            merged = True
                            if mergeSet.merge(gotoSet):
                                # Process worklist in MRU order.  This
                                # causes a depth-first traversal.
                                if j in worklist:
                                    worklist.remove(j)
                                else:
                                    if self._verbose:
                                        sys.stdout.write(".")
                                        sys.stdout.flush()
                                worklist.insert(0, j)
                            break
                if not merged:
                    gotoSet.closure()
                    worklist.append(itemSetCount)
                    if gotoSet not in itemSetsHash:
                        itemSetsHash[gotoSet] = [itemSetCount]
                    else:
                        itemSetsHash[gotoSet].append(itemSetCount)
                    self._itemSets.append(gotoSet)
                    itemSetCount += 1
                    if self._verbose:
                        sys.stdout.write("+")
                        sys.stdout.flush()

        if self._verbose:
            sys.stdout.write("\n")
            sys.stdout.flush()
        self._itemSetsHash = itemSetsHash

    # Compute LR parsing tables.
    def _lr(self) -> None:
        # The collection of sets of LR(1) items already exists.
        assert len(self._itemSets) > 0
        assert len(self._action) == 0
        assert len(self._goto) == 0
        assert self._startState is None
        assert self._nConflicts == 0

        if self._verbose:
            print(
                "Parsing.Spec: Generating LR(1) parsing "
                "tables (%d state%s)... "
                % (len(self._itemSets), ("s", "")[len(self._itemSets) == 1]),
                end=" ",
            )
            sys.stdout.flush()

        itemSetsHash = self._itemSetsHash
        assert itemSetsHash is not None

        for itemSet in self._itemSets:
            if self._verbose:
                sys.stdout.write(".")
                sys.stdout.flush()
            # ==============================================================
            # _action.
            state: ActionState = {}
            self._action.append(state)
            for item, lookahead in itemSet.items():
                # X ::= a*Ab
                sym = item.symbol
                if sym is not None:
                    if isinstance(sym, TokenSpec):
                        itemSetB = itemSet.goto(sym)
                        assert itemSetB is not None
                        for i in itemSetsHash[itemSetB]:
                            itemSetC = self._itemSets[i]
                            if itemSetC.weakCompat(itemSetB):
                                self._actionAppend(state, sym, ShiftAction(i))
                                break

                    # Check if this is the start state.
                    if (
                        self._startState is None
                        and item.production.lhs == self._startSym
                        and item.dotPos == 0
                    ):
                        assert len(item.production.rhs) == 2
                        self._startState = len(self._action) - 1
                # X ::= a*
                else:
                    for lookaheadSym in lookahead:
                        self._actionAppend(
                            state, lookaheadSym, ReduceAction(item.production)
                        )
            # =============================================================
            # _goto.
            gstate: GotoState = {}
            self._goto.append(gstate)
            for nonterm in self._nonterms.values():
                itemSetB = itemSet.goto(nonterm)
                if itemSetB is not None:
                    refs = itemSetsHash.get(itemSetB)
                    if refs:
                        for i in refs:
                            itemSetC = self._itemSets[i]
                            if itemSetC.weakCompat(itemSetB):
                                assert nonterm not in gstate
                                gstate[nonterm] = i
                                break

        if self._verbose:
            sys.stdout.write("\n")
            sys.stdout.flush()

    # Add a symbol action to state, if the action doesn't already exist.
    def _actionAppend(
        self,
        state: dict[SymbolSpec, list[Action]],
        sym: SymbolSpec,
        action: Action,
    ) -> None:
        if sym not in state:
            state[sym] = [action]
        else:
            actions = state[sym]
            if action not in actions:
                state[sym].append(action)

    # Look for action ambiguities and resolve them if possible.
    def _disambiguate(self) -> None:
        assert self._nActions == 0
        assert self._nConflicts == 0
        assert self._nImpure == 0

        if self._verbose:
            print(
                "Parsing.Spec: Disambiguating LR(1) parsing tables... ",
                end=" ",
            )
            sys.stdout.flush()

        for stateInd in range(len(self._action)):
            state = self._action[stateInd]
            if self._verbose:
                vRes = "."
                vNConflicts = 0
            for sym in list(state):
                nConflicts = 0
                acts = [act for act in state[sym]]
                # Construct a list that corresponds to acts; each element
                # indicates whether to preserve the action.
                actStats = [True] * len(acts)

                # Fill in the cells of actStats.
                for i in range(len(acts)):
                    actI = acts[i]
                    for j in range(i + 1, len(acts)):
                        actJ = acts[j]
                        res = self._resolve(sym, actI, actJ)
                        if res == "neither":
                            actStats[i] = False
                            actStats[j] = False
                        elif res == "old":
                            actStats[j] = False
                        elif res == "both":
                            pass
                        elif res == "new":
                            actStats[i] = False
                        elif res == "err":
                            actStats[i] = False
                            actStats[j] = False
                            nConflicts += 1
                        else:
                            assert False

                # Look for actions that can coexist or dominate all other
                # actions.
                newActs = []
                for j in range(len(acts)):
                    if actStats[j]:
                        newActs.append(acts[j])
                # Replace the action set if there exists a valid resolution
                # among the actions.
                if len(newActs) > 0 or nConflicts == 0:
                    if self._verbose:
                        if len(newActs) != len(acts):
                            vRes = "_"
                    state[sym] = newActs
                    nConflicts = 0
                elif self._verbose:
                    vNConflicts += nConflicts

                nActions = len(state[sym])
                if nActions > 1:
                    nImpure = nActions
                else:
                    nImpure = 0

                # Update summary stats.
                self._nActions += nActions
                self._nConflicts += nConflicts
                self._nImpure += nImpure

                # Lr._act() does not expect an empty action list.
                if not state[sym]:
                    state.pop(sym)

            if self._verbose:
                if vNConflicts == 0:
                    sys.stdout.write("%s" % vRes)
                else:
                    sys.stdout.write("[%d:%d]" % (stateInd, vNConflicts))
                sys.stdout.flush()

        if self._verbose:
            sys.stdout.write("\n")
            sys.stdout.flush()

    # Compute how to resolve an action conflict.
    #
    # ret: "neither" : Discard both.
    #      "old"     : Keep old.
    #      "both"    : Keep both.
    #      "new"     : Keep new.
    #      "err"     : Unresolvable conflict.
    def _resolve(
        self, sym: SymbolSpec, oldAct: Action, newAct: Action
    ) -> ConflictResolution:
        ret: ConflictResolution

        if type(oldAct) == ShiftAction:
            oldPrec = sym.prec
        elif type(oldAct) == ReduceAction:
            oldPrec = oldAct.production.prec
        else:
            assert False

        if type(newAct) == ShiftAction:
            newPrec = sym.prec
        elif type(newAct) == ReduceAction:
            newPrec = newAct.production.prec
        else:
            assert False

        if oldPrec in newPrec.dominators:
            # Discard new action.
            ret = "old"
        elif newPrec in oldPrec.dominators:
            # Discard old action.
            ret = "new"
        elif oldPrec in newPrec.equiv:
            assert newPrec in oldPrec.equiv

            if oldPrec.assoc == "split" or newPrec.assoc == "split":
                ret = "both"
            elif type(newAct) == type(oldAct):
                assert type(newAct) == ReduceAction
                assert type(oldAct) == ReduceAction
                # Fatal reduce/reduce conflict.
                ret = "err"
            else:
                if (
                    oldPrec.assoc != "fail"
                    and newPrec.assoc != "fail"
                    and oldPrec.assoc != newPrec.assoc
                ):
                    # Conflicting associativity.
                    ret = "err"
                else:
                    # Determine associativity.  If only one of the actions has
                    # %fail associativity, it is overridden by the other.
                    if oldPrec.assoc == "fail":
                        assoc = newPrec.assoc
                    else:
                        assoc = oldPrec.assoc
                    assert assoc in ["fail", "nonassoc", "left", "right"]

                    if assoc == "fail":
                        ret = "err"
                    elif assoc == "left":
                        if type(oldAct) == ShiftAction:
                            ret = "new"
                        else:
                            assert type(newAct) == ShiftAction
                            ret = "old"
                    elif assoc == "right":
                        if type(oldAct) == ShiftAction:
                            ret = "old"
                        else:
                            assert type(newAct) == ShiftAction
                            ret = "new"
                    elif assoc == "nonassoc":
                        ret = "neither"
                    else:
                        assert False
        else:
            if newPrec in oldPrec.equiv:
                print("%r <--> %r" % (oldPrec, newPrec))
            assert newPrec not in oldPrec.equiv
            # No specified relationship between precedences.
            ret = "err"

        return ret
