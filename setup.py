import os
import pathlib
import sys

from setuptools import extension as setuptools_ext
from setuptools import setup
from setuptools.command import build_ext as setuptools_build_ext


_ROOT = pathlib.Path(__file__).parent


with open(str(_ROOT / "README.rst")) as f:
    readme = f.read()


with open(str(_ROOT / "parsing" / "_version.py")) as f:
    for line in f:
        if line.startswith("__version__ ="):
            _, _, version = line.partition("=")
            VERSION = version.strip(" \n'\"")
            break
    else:
        raise RuntimeError(
            "unable to read the version from parsing/_version.py"
        )


USE_MYPYC = False
MYPY_DEPENDENCY = "mypy>=0.910"
setup_requires = []
ext_modules = []

if (
    os.environ.get("PARSING_USE_MYPYC", None) in {"true", "1", "on"}
    or "--use-mypyc" in sys.argv
):
    setup_requires.append(MYPY_DEPENDENCY)
    # Fool setuptools into calling build_ext.  The actual list of
    # extensions would get replaced by mypycify.
    ext_modules.append(
        setuptools_ext.Extension("parsing.foo", ["parsing/foo.c"])
    )
    USE_MYPYC = True


class build_ext(setuptools_build_ext.build_ext):  # type: ignore
    def finalize_options(self) -> None:
        # finalize_options() may be called multiple times on the
        # same command object, so make sure not to override previously
        # set options.
        if getattr(self, "_initialized", False):
            return

        if USE_MYPYC:
            import pkg_resources

            # Double check Cython presence in case setup_requires
            # didn't go into effect (most likely because someone
            # imported Cython before setup_requires injected the
            # correct egg into sys.path.
            try:
                import mypy.version
                from mypyc.build import mypycify
            except ImportError:
                raise RuntimeError(
                    "please install {} to compile parsing from source".format(
                        MYPY_DEPENDENCY
                    )
                )

            mypy_dep = pkg_resources.Requirement.parse(MYPY_DEPENDENCY)
            if mypy.version.__version__ not in mypy_dep:
                raise RuntimeError(
                    "parsing requires {}, got mypy=={}".format(
                        MYPY_DEPENDENCY, mypy.version.__version__
                    )
                )

            self.distribution.ext_modules = mypycify(
                [
                    "parsing/automaton.py",
                    "parsing/grammar.py",
                ],
            )

        super(build_ext, self).finalize_options()


setup(
    name="parsing",
    version=VERSION,
    python_requires=">=3.7.0",
    url="http://www.canonware.com/Parsing/",
    license="MIT",
    author="Jason Evans",
    author_email="jasone@canonware.com",
    description="A pure-Python module that implements an LR(1) "
    "parser generator, as well as CFSM and GLR parser drivers.",
    long_description=readme,
    long_description_content_type="text/x-rst",
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Topic :: Software Development :: Compilers",
        "Topic :: Text Processing :: General",
    ],
    packages=["parsing", "parsing.tests", "parsing.tests.specs"],
    package_data={"parsing": ["py.typed"]},
    setup_requires=setup_requires,
    ext_modules=ext_modules,
    extras_require={
        "test": [
            "flake8",
            MYPY_DEPENDENCY,
        ]
    },
    cmdclass={"build_ext": build_ext},
)
