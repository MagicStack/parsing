import parsing


class foo(parsing.Precedence):
    "%left"


# Tokens.
class plus(parsing.Token):
    "%token"


class star(parsing.Token):
    "%token"


class lparen(parsing.Token):
    "%token"


class rparen(parsing.Token):
    "%token"


class id(parsing.Token):
    "%token"


class bar(parsing.Token):
    "%token"


# Non-terminal definitions.
class E(parsing.Nonterm):
    "%start"

    def reduceA(self, E, plus, T):
        "%reduce E plus T"
        self.val = "[%s + %s]" % (E.val, T.val)

    def reduceB(self, T):
        "%reduce T"
        self.val = T.val

    def reduceC(self, T):
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


class F(parsing.Nonterm):
    "%nonterm"

    def reduceA(self, lparen, E, rparen):
        "%reduce lparen E rparen"
        self.val = "( %s )" % (E.val,)

    def reduceB(self, id):
        "%reduce id"
        self.val = "ID"


class Biz(parsing.Nonterm):
    "%nonterm"

    def reduceA(self):
        "%reduce [foo]"

    def reduceB(self):
        "%reduce bar"
