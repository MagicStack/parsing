"""
The rule DSL provides a number of shortcuts that
allow the user to define grammar translation rules
more succinctly.
"""

from __future__ import print_function
import re
from parsing.grammar import NontermSpec, SpecError

special_re = re.compile('%([a-z]+)($|:)')
first_cap_re = re.compile('(.)([A-Z][a-z]+)')
all_cap_re = re.compile('([a-z0-9])([A-Z])')

snaked_re = re.compile('_[a-z]')

rhs_assign_re = re.compile("([A-Za-z]\\w*)([+?]?=)([A-Za-z]\\w*[?+*]?|'[^']+')")
identifier_re = re.compile('[a-zA-Z][0-9a-zA-Z]*$')
symbol_names = {
    ';': 'semicolon',
    ':': 'colon',
    ',': 'comma',
    '@': 'atsign'
}


def snake_case(name):
    """
    converts a name to snake_case
    """
    s1 = first_cap_re.sub(r'\1_\2', name)
    return all_cap_re.sub(r'\1_\2', s1).lower()

def camel_case(name):
    """
    converts a name to CamelCase
    """
    s1 = snaked_re.sub(lambda x: x[1].upper(), name)
    return s1[0].upper() + s1[1:]

postfix = {'?': '_opt', '*': '_opt_list', '+': '_list'}


def is_reduce_instr(s):
    if s == '%reduce':
        return True
    elif s.startswith('%reduce:'):
        return True


def reduce_instr_type(s):
    if s == '%reduce':
        return None
    elif s.startswith('%reduce:'):
        return s[8:]


class Generator(object):

    def __init__(self, clsdict, name):
        self.clsdict = clsdict
        self.name = name
        self.ruleno = 1

    def numbered_method(self, prefix='reduce'):
        """
        generates a (hopefully) fresh symbol with
        the given prefix.
        """
        val = self.ruleno
        self.ruleno = val + 1
        return '%s_%d' % (prefix, val)

    def add_method(self, lines):
        """
        compiles the method given in `lines` to a method
        for the class dictionary.
        """
        text = '\n'.join(lines)
        #print(self.name, "COMPILED")
        #print(text)
        exec(text, globals(), self.clsdict)

    def compile_reduce(self, lst):
        """
        A reduce rule is the equivalent of a normal
        reduce rule, but with the added functionality
        that AST attributes are derived semi-intelligently
        from the RHS
        """
        assert is_reduce_instr(lst[0])
        fn_type = reduce_instr_type(lst[0])
        fn_name = self.numbered_method()
        rhs_parts = []
        arg_names = []
        for rhs in lst[1:]:
            m = rhs_assign_re.match(rhs)
            if m:
                assigned_name = m.group(1)
                assign_op = m.group(2)
                rhs = m.group(3)
            else:
                assigned_name = None
            suffix = ''
            rhs_parts.append(rhs)
            last_char = rhs[-1]
            while last_char in '+*?':
                suffix += 's'
                rhs = rhs[:-1]
                last_char = rhs[-1]
            if last_char == "'":
                arg_name = '_'
            else:
                # CheeseDeclaration+ => cheese_declarations
                arg_name = snake_case(rhs) + suffix
            if assigned_name is not None:
                arg_names.append(assigned_name)
            else:
                arg_suffix = 1
                orig_arg_name = arg_name
                if arg_name in ['type', 'range']:
                    arg_name += '_'
                while arg_name in arg_names:
                    arg_suffix += 1
                    arg_name = '%s%d' % (orig_arg_name, arg_suffix)
                arg_names.append(arg_name)
        lst_rhs = ' '.join(['%reduce'] + rhs_parts)
        argspec = ', '.join(arg_names)
        fillers = {
            'fn_name': fn_name, 'argspec': argspec,
            'lst_rhs': lst_rhs
        }
        fn_src = ['def %(fn_name)s(self, %(argspec)s):' % fillers,
                  '  "%(lst_rhs)s"' % fillers]
        if fn_type is not None:
            fn_src.append('  self.type = "%s"' % (fn_type,))
        for arg_name in arg_names:
            if arg_name[0] != '_':
                fn_src.append('  self.%(arg_name)s = %(arg_name)s' % {
                    'arg_name': arg_name})
        self.add_method(fn_src)

    def compile_choice(self, lst):
        """
        The %choice shorthand creates rules with a single symbol
        on the right hand side, which gets re-used as the AST for
        this node.
        """
        for name in lst[1:]:
            last_char = name[-1]
            if last_char not in "?*+'":
                arg_name = snake_case(name)
                fn_name = 'r_' + arg_name
            elif last_char in '?*+':
                arg_name = snake_case(name[:-1])
                fn_name = 'r_' + arg_name + postfix[name[-1]]
            elif last_char == "'":
                arg_name = '_'
                fn_name = 'r_const_' + hex(hash(name))
            if not NontermSpec.token_re.match(name):
                raise SpecError("%s is not a valid RHS symbol" % (name,))
            fillers = {
                'fn_name': fn_name, 'arg_name': arg_name,
                'name': name
            }
            fn_src = [
                x % fillers for x in [
                    'def %(fn_name)s(self, %(arg_name)s):',
                    '  "%%reduce %(name)s"',
                    '  return %(arg_name)s']]
            self.add_method(fn_src)

    def compile_enum(self, lst):
        """
        compiles a %enum directive, which simply sets
        self.type to the name of the matched keyword (optionally with a suffix).
        As an example, `%enum:Foo 'bar' 'baz'` will recognize `bar` and `baz`
        keywords and set the `type` attribute to `BarFoo` and `BazFoo`,
        respectively.
        """
        if ':' in lst[0]:
            suffix = lst[0].split(':')[1]
        else:
            suffix = camel_case(self.name)
        for name in lst[1:]:
            if name[0] != "'" or name[-1] != "'":
                raise SpecError("%s must be a literal (enclosed in '')" %
                                (name,))
            kwd_name = name[1:-1]
            if "'" in kwd_name or "\\" in kwd_name:
                raise SpecError("%s has disallowed characters"%(name,))
            if identifier_re.match(kwd_name):
                val_name = kwd_name
            elif kwd_name in symbol_names:
                val_name = symbol_names[kwd_name]
                kwd_name = val_name
            else:
                val_name = hex(hash(kwd_name))
            fillers = {
                'val_name': val_name,
                'suffix': suffix,
                'kwd_name': camel_case(kwd_name),
                'escaped_name': name
            }
            fn_src = [
                x % fillers for x in [
                    'def reduce_%(val_name)s(self, _x):',
                    '  "%%reduce %(escaped_name)s"',
                    "  self.type = '%(kwd_name)s%(suffix)s'"
                ]
            ]
            self.add_method(fn_src)

    def compile_list(self, lst):
        """
        The `%list item sep` instruction creates rules for
        a list of `item`s separated by `sep`s. If `sep`
        is not a keyword or literal symbol, the separators
        get included in the resulting list.
        """
        if len(lst) != 3:
            raise SpecError(
                "%list needs item and sep arguments, got {}".format(lst[1:]))
        if lst[2].startswith("'"):
            # simple list with ignorable separator
            fn_src = [
                'def reduce_single(self, item):',
                '  "%%reduce %s"' % (lst[1],),
                '  return [item]',
                'def reduce_multiple(self, lst, sep, item):',
                '  "%%reduce %s %s %s"' % (
                    self.name,
                    lst[2], lst[1]),
                '  return lst + [item]']
        else:
            # list with non-ignorable separator
            fn_src = [
                'def reduce_single(self, item):',
                '  "%%reduce %s"' % (lst[1],),
                '  return [item]',
                'def reduce_multiple(self, lst, sep, item):',
                '  "%%reduce %s %s %s"' % (
                    self.name,
                    lst[2], lst[1]),
                '  return lst + [sep, item]']
        self.add_method(fn_src)

    def compile_start(self, lst):
        """
        %start is used for the start symbol (mandatory).
        """
        if len(lst) > 2:
            raise SpecError(
                "%start directive with extra stuff: {}".format(lst))
        if len(lst) == 2 and lst[1] != self.name:
            raise SpecError(
                "%start directive uses symbol {}, should be {}".format(
                    lst[1], self.name))

    def compile_nonterm(self, lst):
        """
        %nonterm is used for normal nonterminals (leftover from
        module-based declarations)
        """
        if len(lst) >= 2 and lst[1] != self.name:
            raise SpecError(
                "%nonterm directive uses symbol {}, should be {}".format(
                    lst[1], self.name))

    def compile(self, lst):
        """
        compiles a sequence of symbols (instruction plus parameters)
        into one or multiple methods.
        """
        m = special_re.match(lst[0])
        instr = m.group(1)
        method = getattr(self, 'compile_' + instr)
        method(lst)


def interpret_docstring(docstring, clsdict, name):
    """
    This function is called with the docstrings of Nonterm classes in order
    to allow shorthand notations for a few common patterns in AST construction.

    :param docstring: the docstring to be interpreted
    :param clsdict: the class namespace
    :param name: the name of the class (for recursive rules such as list
    """
    if '%' not in docstring:
        return
    generator = Generator(clsdict, name)
    tokens = docstring.split()
    cur_group = []
    for token in tokens:
        if token[0] == '%':
            if cur_group:
                generator.compile(cur_group)
            cur_group = [token]
        else:
            cur_group.append(token)
    if cur_group:
        generator.compile(cur_group)
