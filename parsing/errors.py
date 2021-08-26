"""
The Parsing module implements the following exception classes:

  * Exception
  * SpecError
  * UnexpectedToken
"""


# ============================================================================
# Begin exceptions.
#
class AnyException(Exception):
    """
    Top-level class for all exceptions thrown within the Parsing module.
    Needed for compatibility with old code - do not use.
    """


class ParsingError(AnyException):
    """
    Top level Parsing exception class, from which we derive all exceptions that
    occur during the parsing of an input string.
    """


class SpecError(AnyException):
    """
    Specification error exception.  SpecError arises when the Spec
    introspection machinery detects an error either during docstring parsing
    or parser specification generation.
    """


class UnexpectedToken(ParsingError):
    """
    Parser syntax error.  UnexpectedToken arises when a Parser instance detects
    a syntax error according to the Spec it is using, for the input being
    fed to it.
    """


#
# End exceptions.
# ============================================================================
