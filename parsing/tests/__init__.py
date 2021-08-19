import os.path
import unittest


def suite() -> unittest.TestSuite:
    start = os.path.dirname(os.path.dirname(__file__))
    return unittest.defaultTestLoader.discover(start)
