"""
This module declares several structural ("duck typing") interfaces
that objects or classes can implement to be used in the library
"""

__all__ = ['is_parser', 'is_symspec', 'is_spec_source']


def has_methods(obj, methods):
    for method in methods:
        if not hasattr(obj, method):
            return False
        if not callable(getattr(obj, method)):
            return False
    return True


def is_parser(parser):
    """
    returns True if `parser` fits the structural interface for a parser.
    """
    if not has_methods(parser, ['token', 'eoi']):
        return False
    if not hasattr(parser, '_spec'):
        return False
    return True


def is_symspec(symspec):
    """
    returns True if `symspec` can be used as a symbol specification.
    """
    if not hasattr(symspec, 'name'):
        return False
    if not hasattr(symspec, 'prec'):
        return False
    return True


def is_spec_source(source):
    """
    returns True if `source` can be used to define a grammar.
    """
    if not has_methods(source, [
            'get_precedences', 'get_tokens', 'get_nonterminals'
    ]):
        return False
