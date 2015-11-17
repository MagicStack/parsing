import os.path
import unittest


if __name__ == '__main__':
    start = os.path.dirname(os.path.dirname(__file__))
    suite = unittest.defaultTestLoader.discover(start)
    runner = unittest.TextTestRunner()
    runner.run(suite)
