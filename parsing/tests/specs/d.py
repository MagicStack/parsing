import parsing


# Precedences.
class p1(parsing.Precedence):
    "%left"


class p2(parsing.Precedence):
    "%left >p1"


# Tokens.
class plus(parsing.Token):
    "%token [p1]"


class star(parsing.Token):
    "%token [p2]"


class id(parsing.Token):
    "%token"


# Non-terminal definitions.
class S(parsing.Nonterm):
    "%start"

    def reduce(self, E):
        "%reduce E"
        self.val = E.val


class E(parsing.Nonterm):
    "%nonterm"

    def reduceId(self, id):
        "%reduce id"
        self.val = "ID"

    def reducePlus(self, EA, plus, EB):
        "%reduce E plus E [p1]"
        self.val = "[%s + %s]" % (EA.val, EB.val)

    def reduceStar(self, EA, star, EB):
        "%reduce E star E [p2]"
        self.val = "[%s * %s]" % (EA.val, EB.val)
