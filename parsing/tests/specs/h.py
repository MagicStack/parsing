import parsing


class TokenI(parsing.Token):
    "%token i"


class TokenPlus(parsing.Token):
    "%token plus [split]"


class TokenStar(parsing.Token):
    "%token star [split]"


class S(parsing.Nonterm):
    "%start"

    def __repr__(self):
        return "%r" % self.E

    def reduce(self, E):
        "%reduce E"
        self.E = E


class E(parsing.Nonterm):
    "%nonterm"

    def __repr__(self):
        if self.variant == "i":
            return "i"
        elif self.variant == "plus":
            return "(%r + %r)" % (self.EA, self.EB)
        elif self.variant == "star":
            return "(%r * %r)" % (self.EA, self.EB)
        else:
            assert False

    def merge(self, other):
        if self.variant == "plus":
            return self
        elif other.variant == "plus":
            return other
        else:
            return self

    def reduceI(self, i):
        "%reduce i"
        self.variant = "i"
        self.i = i

    def reducePlus(self, EA, plus, EB):
        "%reduce E plus E [split]"
        self.variant = "plus"
        self.EA = EA
        self.plus = plus
        self.EB = EB

    def reduceStar(self, EA, star, EB):
        "%reduce E star E [split]"
        self.variant = "star"
        self.EA = EA
        self.star = star
        self.EB = EB
