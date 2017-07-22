"""
This module contains functionality for extracting a grammar from
classes in a module.
"""
import types
from parsing.grammar import Precedence, TokenSpec, NontermSpec, SpecError
from parsing.ast import Token, Nonterm
from parsing import introspection


class ModuleSpecSource(object):
    """
    ModuleSpecSource scans one or several modules for subclasses of relevant
    classes (Precedence, Token, Nonterm) with specific docstrings.
    """

    def __init__(self, modules):
        if isinstance(modules, types.ModuleType):
            # Wrap single module in a list.
            modules = [modules]
        self.modules = modules
        items = []
        for module in self.modules:
            for k, v in module.__dict__.items():
                if isinstance(v, type) and isinstance(v.__doc__, str):
                    dirtoks = introspection.parse_docstring(v.__doc__)
                    items.append((module, k, v, dirtoks))
        self.named_objs = items
        self._cache_precedences = None
        self._cache_tokens = None
        self._cache_nonterminals = None

    def get_precedences(self):
        if self._cache_precedences is not None:
            return self._cache_precedences
        result = []
        for module, k, v, dirtoks in self.named_objs:
            if issubclass(v, Precedence) and dirtoks[0] in \
                    ["%fail", "%nonassoc", "%left", "%right", "%split"]:
                name = k
                relationships = {}
                i = 1
                while i < len(dirtoks):
                    tok = dirtoks[i]
                    m = Precedence.assoc_tok_re.match(tok)
                    if m:
                        # Precedence relationship.
                        if m.group(2) in relationships:
                            raise SpecError("Duplicate precedence "
                                            "relationship: %s" % v.__doc__)
                        relationships[m.group(2)] = m.group(1)
                    else:
                        m = NontermSpec.token_re.match(tok)
                        if m:
                            if i != 1:
                                raise SpecError(
                                    "Precedence name must come before "
                                    "relationships: %s" % v.__doc__)
                            name = m.group(1)
                        else:
                            raise SpecError(
                                "Invalid precedence specification: %s" %
                                v.__doc__)
                    i += 1

                prec = Precedence(name, dirtoks[0][1:], relationships)
                result.append(prec)
        self._cache_precedences = result
        return result

    def get_tokens(self):
        if self._cache_tokens is not None:
            return self._cache_tokens
        result = []
        for module, k, v, dirtoks in self.named_objs:
            if issubclass(v, Token) and dirtoks[0] in ["%token"]:
                name = k
                prec = None
                i = 1
                while i < len(dirtoks):
                    tok = dirtoks[i]
                    m = NontermSpec.precedence_tok_re.match(tok)
                    if m:
                        if i < len(dirtoks) - 1:
                            raise SpecError(
                                "Precedence must come last in token "
                                "specification: %s" % v.__doc__)
                        prec = m.group(1)
                    else:
                        m = NontermSpec.token_re.match(tok)
                        if m:
                            name = m.group(1)
                        else:
                            raise SpecError(
                                "Invalid token specification: %s" % v.__doc__)
                    i += 1
                if prec is None:
                    prec = "none"
                # token = TokenSpec(name, v, prec)
                token = TokenSpec(v, name, prec)
                result.append(token)
        self._cache_tokens = result
        return result

    def get_nonterminals(self):
        if self._cache_nonterminals is not None:
            return self._cache_nonterminals
        result = []
        startSym = None
        for module, k, v, dirtoks in self.named_objs:
            if issubclass(v, Nonterm) and \
                            dirtoks[0] in ["%start", "%nonterm"]:
                nonterm, is_start = NontermSpec.from_class(v)
                result.append(nonterm)

                if is_start:
                    # Start symbol.
                    if startSym is not None:
                        raise SpecError(
                            "Only one start non-terminal allowed: %s" %
                            v.__doc__)
                    startSym = nonterm
        self._cache_nonterminals = (result, startSym)
        return result, startSym
