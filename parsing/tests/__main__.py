import unittest


if __name__ == "__main__":
    from parsing import tests

    runner = unittest.TextTestRunner()
    runner.run(tests.suite())
