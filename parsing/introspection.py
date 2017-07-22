import re


def parse_docstring(s):
    return list(filter(None, re.split(r'\s+', s.replace('\n', ' '))))
