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
#
# Release history:
#
# 1.4 (15 December 2012): Python 3 support.
#
#                         Performance optimizations & bug fixes.
#
#                         Published on PyPI.
#
# 1.3 (8 August 2007): Retroactively number public releases.
#
#                      Back-port to Python 2.4.
#
#                      Remove some magic surrounding epsilon, in order to
#                      generalize/simplify.
#
# 1.2 (6 May 2007): Fix some off-by-one errors in production count reporting.
#
#                   Add some missing code that helps detect which definitions
#                   are used/unused when building the parser.
#
# 1.1 (22 March 2007): Optimize/generalize Lr._production() by using argument
#                      list expansion.
#
# 1.0 (19 March 2007): Initial public release.
#
# ============================================================================
"""
The Parsing module implements an LR(1) parser generator, as well as the
runtime support for using a generated parser, via the Lr and Glr parser
drivers.  There is no special parser generator input file format, but the
parser generator still needs to know what classes/methods correspond to
various aspects of the parser.  This information is specified via
docstrings, which the parser generator introspects in order to generate a
parser.  Only one parser specification can be embedded in each module, but
it is possible to share modules between parser specifications so that, for
example, the same token definitions can be used by multiple parser
specifications.

The parsing tables are LR(1), but they are generated using a fast algorithm
that avoids creating duplicate states that result when using the generic
LR(1) algorithm.  Creation time and table size are on par with the LALR(1)
algorithm.  However, LALR(1) can create reduce/reduce conflicts that don't
exist in a true LR(1) parser.  For more information on the algorithm, see:

    A Practical General Method for Constructing LR(k) Parsers
    David Pager
    Acta Informatica 7, 249-268 (1977)

Parsing table generation requires non-trivial amounts of time for large
grammars.  Internal pickling support makes it possible to cache the most
recent version of the parsing table on disk, and use the table if the
current parser specification is still compatible with the one that was used
to generate the pickled parsing table.  Since the compatibility checking is
quite fast, even for large grammars, this removes the need to use the
standard code generation method that is used by most parser generators.

Parser specifications are encapsulated by the Spec class.  Parser instances
use Spec instances, but are themselves based on separate classes.  This
allows multiple parser instances to exist simultaneously, without requiring
multiple copies of the parsing tables.  There are two separate parser driver
classes:

  Lr : Standard Characteristic Finite State Machine (CFSM) driver, based on
       unambiguous LR(1) parsing tables.  This driver is faster than the Glr
       driver, but it cannot deal with all parsing tables that the Glr
       driver can.

  Glr : Generalized LR driver, capable of tracking multiple parse trees
        simultaneously, if the %split precedence is used to mark ambiguous
        actions.  This driver is closely based on Elkhound's design, which
        is described in a technical report:

            Elkhound: A Fast, Practical GLR Parser Generator
            Scott McPeak
            Report No. UCB/CSD-2-1214 (December 2002)
            http://www.cs.berkeley.edu/~smcpeak/elkhound/

Parser generator directives are embedded in docstrings, and must begin with
a '%' character, followed immediately by one of several keywords:

    Precedence : %fail %nonassoc %left %right %split
         Token : %token
  Non-terminal : %start %nonterm
    Production : %reduce

All of these directives are associated with classes except for %reduce.
%reduce is associated with methods within non-terminal classes.  The Parsing
module provides base classes from which precedences, tokens, and
non-terminals must be derived.  This is not as restrictive as it sounds,
since there is nothing preventing, for example, a master Token class that
subclasses Parsing.Token, which all of the actual token types then subclass.
Also, nothing prevents using multiple inheritance.

Following are the base classes to be subclassed by parser specifications:

  * Precedence
  * Token
  * Nonterm


"""
__all__ = ["SpecError", "UnexpectedToken", "Nonterm",
           "Precedence", "Spec", "Token", "Lr", "Glr",
           "ModuleSpecSource"]

from six import print_
from six.moves import range

from parsing.errors import (ParsingError, SpecError,                   # noqa
                            UnexpectedToken, AnyException)             # noqa
from parsing.grammar import (Precedence, Production, SymbolSpec,       # noqa
                             NontermSpec, TokenSpec, EndOfInput,       # noqa
                             Epsilon, epsilon, NontermStart,           # noqa
                             ShiftAction, ReduceAction)
from parsing.ast import Symbol, Nonterm, Token                         # noqa
from parsing.automaton import Spec
from parsing.module_spec import ModuleSpecSource

# Exception aliases for legacy code that needs the old names that
# shadow builtin exceptions
Exception = AnyException
SyntaxError = UnexpectedToken

try:
    next
except NameError:
    # Python <= 2.5
    def next(obj):
        return obj.next()


class Lr(object):
    """
    LR(1) parser.  The Lr class uses a Spec instance in order to parse
    input that is fed to it via the token() method, and terminated via the
    eoi() method.
    """

    def __init__(self, spec):
        if __debug__:
            if type(self) == Lr:
                assert spec.pureLR
        assert spec._nConflicts == 0
        self._spec = spec
        self.reset()
        self._verbose = False

    def __getSpec(self):
        return self._spec

    def __setSpec(self, spec):
        raise AttributeError

    spec = property(__getSpec, __setSpec)

    def __getStart(self):
        return self._start

    def __setStart(self, start):
        raise AttributeError

    start = property(
        __getStart,
        __setStart,
        doc="""
A list of parsing results.  For LR parsing, there is only ever one
result, but for compatibility with the Glr interface, start is a
list.
""")

    def __getVerbose(self):
        return self._verbose

    def __setVerbose(self, verbose):
        assert type(verbose) == bool
        self._verbose = verbose

    verbose = property(__getVerbose, __setVerbose)

    def reset(self):
        self._start = None
        self._stack = [(Epsilon(self), 0)]

    def token(self, token):
        """Feed a token to the parser."""
        tokenSpec = self._spec._sym2spec[type(token)]
        self._act(token, tokenSpec)

    def eoi(self):
        """Signal end-of-input to the parser."""
        token = EndOfInput(self)
        self.token(token)

        assert self._stack[-1][0] == token  # <$>.
        if self._verbose:
            self._printStack()
            print_("   --> accept")
        self._stack.pop()

        self._start = [self._stack[1][0]]
        assert self._start[0].symSpec == self._spec._userStartSym

    def _act(self, sym, symSpec):
        if self._verbose:
            self._printStack()
            print_("INPUT: %r" % sym)

        while True:
            top = self._stack[-1]
            if symSpec not in self._spec._action[top[1]]:
                raise UnexpectedToken("Unexpected token: %r" % sym)

            actions = self._spec._action[top[1]][symSpec]
            assert len(actions) == 1
            action = actions[0]

            if self._verbose:
                print_("   --> %r" % action)
            if type(action) == ShiftAction:
                self._stack.append((sym, action.nextState))
                break
            else:
                assert type(action) == ReduceAction
                self._reduce(action.production)

            if self._verbose:
                self._printStack()

    def _printStack(self):
        print_("STACK:", end=' ')
        for node in self._stack:
            print_("%r" % node[0], end=' ')
        print_()
        print_("      ", end=' ')
        for node in self._stack:
            print_("%r%s" % (
                node[1], (" " * (len("%r" % node[0]) - len("%r" % node[1])))),
                end=' ')
        print_()

    def _reduce(self, production):
        nRhs = len(production.rhs)
        rhs = []
        for i in range(len(self._stack) - nRhs, len(self._stack)):
            rhs.append(self._stack[i][0])

        r = self._production(production, rhs)

        for i in range(nRhs):
            self._stack.pop()

        top = self._stack[-1]
        self._stack.append((r, self._spec._goto[top[1]][production.lhs]))

    def _production(self, production, rhs):
        sym = production.lhs.nontermType(self)
        nRhs = len(rhs)
        assert nRhs == len(production.rhs)
        r = production.method(sym, *rhs)

        # Python's method definition syntax makes returning self from %reduce
        # methods cumbersome, so translate None here.
        if r is None:
            r = sym

        return r

# ===========================================================================
# Begin graph-structured stack (GSS) classes.
#


class Gss(list):
    """Graph-structured stack."""

    def __init__(self, glr):
        list.__init__(self)

        self._glr = glr


class Gsse(object):
    """Graph-structured stack edge."""

    def __init__(self, below, above, value):
        self.node = below
        above._edges.append(self)
        self.value = value

    def __repr__(self):
        return "{%r}" % self.value

    def __eq__(self, other):
        if self.node != other.node \
          or self.value != other.value:
            return False
        return True


class Gssn(object):
    """Graph-structured stack node."""

    def __init__(self, below, value, nextState):
        assert isinstance(below, Gssn) or below is None

        self._edges = []
        if below is not None:
            Gsse(below, self, value)
        self.nextState = nextState

    def __repr__(self):
        return "[%d]" % self.nextState

    def __getEdge(self):
        assert len(self._edges) == 1
        return self._edges[0]

    def __setEdge(self):
        raise AttributeError

    edge = property(__getEdge, __setEdge)

    def edges(self):
        for edge in self._edges:
            yield edge

    def nodes(self):
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
    def paths(self, pathLen=None):
        assert pathLen is None or isinstance(pathLen, int) and pathLen >= 0

        for path in self._pathsRecurse(pathLen, []):
            yield path

    def _pathsRecurse(self, pathLen, path):
        path.insert(0, self)
        if pathLen is None and len(self._edges) == 0:
            yield path[:]
        elif pathLen is not None and len(path) - 1 == pathLen * 2:
            yield path[:]
        else:
            for edge in self.edges():
                # Avoid infinite recursion due to <e>-production cycles.
                if len(path) < 3 or edge != path[1]:
                    path.insert(0, edge)
                    for x in edge.node._pathsRecurse(pathLen, path):
                        yield x
                    path.pop(0)
        path.pop(0)

#
# End graph-structured stack (GSS) classes.
# ========================================================================


class Glr(Lr):
    """
GLR parser.  The Glr class uses a Spec instance in order to parse input
that is fed to it via the token() method, and terminated via the eoi()
method.
"""

    def __init__(self, spec):
        Lr.__init__(self, spec)

    def reset(self):
        self._start = None

        # Initialize with a stack that is in the start state.
        self._gss = Gss(self)
        top = Gssn(None, None, 0)
        self._gss.append(top)

        self._paths = []

    def token(self, token):
        """
Feed a token to the parser.
"""
        if self._verbose:
            print_("%s" % ("-" * 80))
            print_("INPUT: %r" % token)
        tokenSpec = self._spec._sym2spec[type(token)]
        self._act(token, tokenSpec)
        if len(self._gss) == 0:
            raise UnexpectedToken("Unexpected token: %r" % token)

    def eoi(self):
        """
Signal end-of-input to the parser.
"""
        token = EndOfInput(self)
        self.token(token)

        # Gather the start symbols from the stacks.
        self._start = []
        for top in self._gss:
            for path in top.paths():
                assert len(path) == 5
                if self._verbose:
                    print_("   --> accept %r" % path)
                edge = path[1]
                assert isinstance(edge.value, Nonterm)
                assert edge.value.symSpec == self._spec._userStartSym
                self._start.append(edge.value)

        if len(self._start) == 0:
            raise UnexpectedToken("Unexpected end of input")

        if self._verbose:
            print_("Start: %r" % self._start)
            print_("%s" % ("-" * 80))

    def _act(self, sym, symSpec):
        self._reductions(sym, symSpec)
        self._shifts(sym, symSpec)

    def _reductions(self, sym, symSpec):
        # epsilons is a dictionary that maps production-->[tops].  The purpose
        # is to avoid repeating the same epsilon production on a particular
        # stack top.  Ordinary productions do not require this care because we
        # can notice when a path has already been used for a production.
        epsilons = {}

        if self._verbose:
            nReduces = 0

        # Enqueue work.
        workQ = []
        i = 0
        while i < len(self._gss):
            top = self._gss[i]
            if symSpec not in self._spec._action[top.nextState]:
                # Unexpected token for this stack.
                self._gss.pop(i)
            else:
                for action in self._spec._action[top.nextState][symSpec]:
                    if type(action) == ReduceAction:
                        if len(action.production.rhs) == 0:
                            if action.production not in epsilons:
                                assert len(
                                    [path for path in top.paths(0)]) == 1
                                path = [p for p in top.paths(0)][0]
                                epsilons[action.production] = [top]
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print_("   --> enqueue(a) %r" %
                                           action.production)
                                    print_("                  %r" % path)
                            elif top not in epsilons[action.production]:
                                assert len(
                                    [path for path in top.paths(0)]) == 1
                                path = [p for p in top.paths(0)][0]
                                epsilons[action.production].append(top)
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print_("   --> enqueue(b) %r" %
                                           action.production)
                                    print_("                  %r" % path)
                        else:
                            # Iterate over all reduction paths through stack
                            # and enqueue them.
                            for path in top.paths(len(action.production.rhs)):
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print_("   --> enqueue(c) %r" %
                                           action.production)
                                    print_("                  %r" % path)
                i += 1

        # Process the work queue.
        while len(workQ) > 0:
            (path, production) = workQ.pop(0)

            if self._verbose:
                print_("   --> reduce %r" % production)
                print_("              %r" % path)
                nReduces += 1

            self._reduce(workQ, epsilons, path, production, symSpec)

        if self._verbose:
            if nReduces > 0:
                self._printStack()

    def _reduce(self, workQ, epsilons, path, production, symSpec):
        assert len(path[1::2]) == len(production.rhs)

        # Build the list of RHS semantic values to pass to the reduction
        # action.
        rhs = [edge.value for edge in path[1::2]]

        # Call the user reduction method.
        r = self._production(production, rhs)

        below = path[0]
        done = False
        for top in self._gss:
            if top.nextState == \
              self._spec._goto[below.nextState][production.lhs]:
                # top is compatible with the reduction result we want to add to
                # the set of stack tops.
                for edge in top.edges():
                    if edge.node == below:
                        # There is already a below<--top link, so merge
                        # competing interpretations.
                        if self._verbose:
                            print_("   --> merge %r <--> %r" % (edge.value, r))
                        value = production.lhs.nontermType.merge(edge.value, r)
                        if self._verbose:
                            if value == edge.value:
                                print_("             %s" %
                                       ("-" * len("%r" % edge.value)))
                            else:
                                print_("             %s      %s" %
                                       ((" " * len("%r" % edge.value)),
                                        "-" * len("%r" % r)))
                        edge.value = value
                        done = True
                        break
                if not done:
                    # Create a new below<--top link.
                    edge = Gsse(below, top, r)
                    if self._verbose:
                        print_("   --> shift(b) %r" % top)

                    # Enqueue reduction paths that were created as a result of
                    # the new link.
                    self._enqueueLimitedReductions(
                        workQ, epsilons, edge, symSpec)
                    done = True
                break
        if not done:
            # There is no compatible stack top, so create a new one.
            top = Gssn(
                below, r, self._spec._goto[below.nextState][production.lhs])
            self._gss.append(top)
            if self._verbose:
                print_("   --> shift(c) %r" %
                       self._spec._goto[below.nextState][production.lhs])
            self._enqueueLimitedReductions(workQ, epsilons, top.edge, symSpec)

    # Enqueue paths that incorporate edge.
    def _enqueueLimitedReductions(self, workQ, epsilons, edge, symSpec):
        gotos = self._spec._goto

        for top in self._gss:
            if symSpec in self._spec._action[top.nextState]:
                for action in self._spec._action[top.nextState][symSpec]:
                    if type(action) == ReduceAction:
                        if len(action.production.rhs) == 0:
                            if (gotos[top.nextState][action.production.lhs] ==
                                    top.nextState):
                                # Do nothing, since enqueueing a reduction
                                # would result in performing the same reduction
                                # twice.
                                pass
                            elif action.production not in epsilons:
                                path = [top]
                                epsilons[action.production] = [top]
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print_("   --> enqueue(d) %r" %
                                           action.production)
                                    print_("                  %r" % path)
                            elif top not in epsilons[action.production]:
                                path = [top]
                                epsilons[action.production].append(top)
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print_("   --> enqueue(e) %r" %
                                           action.production)
                                    print_("                  %r" % path)
                        else:
                            # Iterate over all reduction paths through stack
                            # and enqueue them if they incorporate edge.
                            for path in top.paths(len(action.production.rhs)):
                                if edge in path[1::2]:
                                    workQ.append((path, action.production))
                                    if self._verbose:
                                        print_("   --> enqueue(f) %r" %
                                               action.production)
                                        print_("                  %r" % path)

    def _shifts(self, sym, symSpec):
        prevGss = self._gss
        self._gss = Gss(self)

        if self._verbose:
            nShifts = 0

        for topA in prevGss:
            if symSpec in self._spec._action[topA.nextState]:
                for action in self._spec._action[topA.nextState][symSpec]:
                    if type(action) == ShiftAction:
                        merged = False
                        for topB in self._gss:
                            if topB.nextState == topA.nextState:
                                Gsse(topA, topB, sym)
                                merged = True
                                break
                        if not merged:
                            top = Gssn(topA, sym, action.nextState)
                            self._gss.append(top)
                            if self._verbose:
                                print_("   --> shift(a) %d" % action.nextState)
                                nShifts += 1
        if self._verbose:
            if nShifts > 0:
                self._printStack()

    def _printStack(self):
        i = 0
        for top in self._gss:
            for path in top.paths():
                if i == 0:
                    print_("STK 0:", end=' ')
                else:
                    print_("    %d:" % i, end=' ')
                for elm in path:
                    print_("%r" % elm, end=' ')
                print_()
                i += 1
