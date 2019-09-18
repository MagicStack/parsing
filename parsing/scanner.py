from re import compile as re_compile

class ScannerError(SyntaxError):
    def __init__(self, message, position):
        SyntaxError.__init__(self, message)
        self.offset = position

class Scanner(object):
    def __init__(self, token_classes, re_whitespace):
        self.regexes = [
            (re_compile(tok.tokenType._re), tok.tokenType, tok)
            for tok in token_classes
            if tok.tokenType._re is not None]
        self.keywords = dict([
                                 (tok.tokenType.keyword, (tok.tokenType, tok))
                                 for tok in token_classes
                                 if tok.tokenType.keyword is not None
                                 ])
        self.whitespace = re_compile(re_whitespace)
        self.classes = dict([(tok.name, tok.tokenType) for tok in token_classes])

    def scan(self, string, parser, callback=None):
        whitespace = self.whitespace
        regexes = self.regexes

        idx = 0
        while idx < len(string):
            m = whitespace.match(string, idx)
            if m:
                idx = m.end()
                continue
            max_idx = 0
            max_cls = None
            max_spec = None
            max_str = None
            for (rgx, cls, tspec) in regexes:
                m = rgx.match(string, idx)
                if m and m.end() > max_idx:
                    # print "match %s [%s]"%(m.group(), tspec.name)
                    max_cls = cls
                    max_str = m.group()
                    max_idx = m.end()
                    max_spec = tspec
                    if max_str in self.keywords:
                        max_cls, max_spec = self.keywords[max_str]
            if max_idx == 0:
                raise ScannerError(
                    'Scanning failed at position %s "%s"' % (idx,string[idx]), idx)
            token = max_cls(parser, max_spec, max_str, range=(idx, max_idx))
            if callback is None:
                parser.token(token, max_spec)
            else:
                callback(token, max_spec)
            idx = max_idx
