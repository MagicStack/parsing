from __future__ import annotations

import re


def parse_docstring(s: str) -> tuple[str, ...]:
    return tuple(filter(None, re.split(r"\s+", s.replace("\n", " "))))
