import os.path
import subprocess
import sys
import unittest


def find_root():
    return os.path.dirname(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    )


class TestCodeQuality(unittest.TestCase):
    def test_flake8(self):
        rootpath = find_root()

        try:
            import flake8  # NoQA
        except ImportError:
            raise unittest.SkipTest("flake8 moudule is missing")

        for subdir in ["parsing"]:
            try:
                subprocess.check_output(
                    [sys.executable, "-m", "flake8", subdir], cwd=rootpath
                )
            except subprocess.CalledProcessError as ex:
                output = ex.output.decode()
                raise AssertionError(
                    "flake8 validation failed:\n{}".format(output)
                )

    def test_mypy(self):
        rootpath = find_root()
        config_path = os.path.join(rootpath, "pyproject.toml")
        if not os.path.exists(config_path):
            raise RuntimeError("could not locate pyproject.toml file")

        try:
            import mypy  # NoQA
        except ImportError:
            raise unittest.SkipTest("mypy module is missing")

        try:
            subprocess.run(
                [
                    sys.executable,
                    "-m",
                    "mypy",
                    "--config-file",
                    config_path,
                    "parsing",
                ],
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                cwd=rootpath,
            )
        except subprocess.CalledProcessError as ex:
            output = ex.stdout.decode()
            if ex.stderr:
                output += "\n\n" + ex.stderr.decode()
            raise AssertionError(
                f"mypy validation failed:\n{output}"
            ) from None


if __name__ == "__main__":
    unittest.main()
