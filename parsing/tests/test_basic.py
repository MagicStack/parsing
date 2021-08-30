import unittest
import parsing


class TestParsing(unittest.TestCase):
    def test_basic_a(self):
        class TestParser(parsing.Lr):
            def __init__(self, spec):
                parsing.Lr.__init__(self, spec)

        from parsing.tests.specs import a

        spec = parsing.Spec(a)

        parser = TestParser(spec)
        parser.token(a.TokenId())
        parser.token(a.TokenStar())
        parser.token(a.TokenId())
        parser.token(a.TokenPlus())
        parser.token(a.TokenId())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[[ID * ID] + ID]")

        parser = TestParser(spec)
        parser.token(a.TokenId())
        parser.token(a.TokenPlus())
        parser.token(a.TokenId())
        parser.token(a.TokenStar())
        parser.token(a.TokenId())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[ID + [ID * ID]]")

        parser = TestParser(spec)
        parser.token(a.TokenId())
        parser.token(a.TokenStar())
        parser.token(a.TokenLparen())
        parser.token(a.TokenId())
        parser.token(a.TokenPlus())
        parser.token(a.TokenId())
        parser.token(a.TokenRparen())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[ID * ([ID + ID])]")

    def test_basic_b(self):
        class TestParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import b

        spec = parsing.Spec(b, skinny=False)

        parser = TestParser(spec)
        parser.token(b.id())
        parser.token(b.star())
        parser.token(b.id())
        parser.token(b.plus())
        parser.token(b.id())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[[ID * ID] + ID]")

        parser = TestParser(spec)
        parser.token(b.id())
        parser.token(b.plus())
        parser.token(b.id())
        parser.token(b.star())
        parser.token(b.id())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[ID + [ID * ID]]")

        parser = TestParser(spec)
        parser.token(b.id())
        parser.token(b.star())
        parser.token(b.lparen())
        parser.token(b.id())
        parser.token(b.plus())
        parser.token(b.id())
        parser.token(b.rparen())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[ID * ([ID + ID])]")

    def test_basic_d(self):
        class TestParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import d

        spec = parsing.Spec(d, skinny=False)

        parser = TestParser(spec)
        parser.token(d.id())
        parser.token(d.star())
        parser.token(d.id())
        parser.token(d.plus())
        parser.token(d.id())
        parser.token(d.star())
        parser.token(d.id())
        parser.eoi()

        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[[ID * ID] + [ID * ID]]")

    def test_basic_h(self):
        class TestGlrParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import h

        spec = parsing.Spec(h, skinny=False)

        parser = TestGlrParser(spec)
        parser.token(h.TokenI())
        parser.token(h.TokenPlus())
        parser.token(h.TokenI())
        parser.token(h.TokenStar())
        parser.token(h.TokenI())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(repr(parser.start[0]), "(i + (i * i))")

    def test_basic_i(self):
        class TestGlrParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import i

        self.assertRaises(parsing.SpecError, parsing.Spec, i)

    def test_basic_pickle(self):
        class TestGlrParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import b

        spec = parsing.Spec(b, skinny=False)
        import pickle

        specPickle = pickle.dumps(spec)
        spec2 = pickle.loads(specPickle)

        parser = TestGlrParser(spec2)
        parser.token(b.id())
        parser.token(b.star())
        parser.token(b.id())
        parser.token(b.plus())
        parser.token(b.id())
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, "[[ID * ID] + ID]")


if __name__ == "__main__":
    unittest.main()
