import sys
from setuptools import setup


extra = {}
if sys.version_info >= (3, 0):
    extra.update(use_2to3=True)


setup(
    name='parsing',
    version='1.4',
    url='http://www.canonware.com/Parsing/',
    license='MIT',
    author='Jason Evans',
    author_email='jasone@canonware.com',
    description='A pure-Python module that implements an LR(1) parser generator, '
                'as well as CFSM and GLR parser drivers.',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3',
        'Topic :: Software Development :: Compilers',
        'Topic :: Text Processing :: General'
    ],
    packages=['parsing'],
    test_suite='parsing.tests.test_basic',
    **extra
)
