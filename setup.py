from setuptools import setup


extra = {}

f = open("README.rst", "r")
try:
    extra["long_description"] = f.read()
finally:
    f.close()


setup(
    name="parsing",
    version="2.0.0.dev0",
    python_requires=">=3.7.0",
    url="http://www.canonware.com/Parsing/",
    license="MIT",
    author="Jason Evans",
    author_email="jasone@canonware.com",
    description="A pure-Python module that implements an LR(1) "
    "parser generator, as well as CFSM and GLR parser drivers.",
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
    extras_require={
        "test": [
            "flake8",
            MYPY_DEPENDENCY,
        ]
    },
    **extra,
)
