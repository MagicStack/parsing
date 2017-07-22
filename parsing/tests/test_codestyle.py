import os.path
import subprocess
import sys
import unittest


def find_root():
    return os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))


class TestFlake8(unittest.TestCase):

    def test_flake8(self):
        rootpath = find_root()

        try:
            import flake8  # NoQA
        except ImportError:
            raise unittest.SkipTest('flake8 moudule is missing')

        for subdir in ['parsing']:
            try:
                subprocess.check_output(
                    [sys.executable, '-m', 'flake8', subdir],
                    cwd=rootpath)
            except subprocess.CalledProcessError as ex:
                output = ex.output.decode()
                raise AssertionError(
                    'flake8 validation failed:\n{}'.format(output))


if __name__ == '__main__':
    unittest.main()
