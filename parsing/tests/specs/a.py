import parsing


class P1(parsing.Precedence):
    "%left p1"


class p2(parsing.Precedence):
    """
    %left
        >p1
    """


# Tokens.
class TokenPlus(parsing.Token):
    "%token plus"


class TokenStar(parsing.Token):
    "%token star"


class TokenLparen(parsing.Token):
    "%token lparen"


class TokenRparen(parsing.Token):
    "%token rparen [p2]"


class TokenId(parsing.Token):
    "%token id"


# Non-terminal definitions.
class NontermE(parsing.Nonterm):
    "%start E"

    def reduceA(self, E, plus, T):
        "%reduce E plus T [p1]"
        self.val = "[%s + %s]" % (E.val, T.val)

    def reduceB(self, T):
        "%reduce T"
        self.val = T.val


class T(parsing.Nonterm):
    "%nonterm"

    def reduceA(self, T, star, F):
        "%reduce T star F"
        self.val = "[%s * %s]" % (T.val, F.val)

    def reduceB(self, F):
        "%reduce F"
        self.val = F.val


class NontermF(parsing.Nonterm):
    """
    %nonterm
    F    [p2]
    """

    def reduceA(self, lparen, E, rparen):
        """%reduce
        lparen E rparen
        """
        self.val = "(%s)" % (E.val,)

    def reduceB(self, id):
        "%reduce id [split]"
        self.val = "ID"
