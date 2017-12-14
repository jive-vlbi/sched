## A python parser for SCHED's "keyin" files.
## 
## Syntax:
## !.*$ defines comments.
## KEY "=" VAL
## KEY VAL "the equals sign is optional so long as its absence doesn't lead to ambiguity"

## (1) Keys no longer than 8 chars(!)
## (1a) Except when they are ("overwrite" in first line of SCHED key files).
## (2) Keys are _not_ case sensitive(!) (normalise to GREAT RUNES in honour of FORTRAN
##     Alphabet for keywords?  [A-Za-z]*
## (3) Values are not case sensitive, except when they are. (E.g., unix paths.)
## (4) Values may be comma-separated lists ("arrays")
## (5) Values may be numbers or "character string".
## (6) Values are doubles.
## (7) Values may be expressions. (E.g., "5.5*4")
## (8) hh:mm:ss.ss dd:mm:ss.ss (no embedded blanks or signs; "2:40" is allowed and equal to "160")
## (9) Value may be blank ("").
## Entries terminated by "/"

import re, sys, logging, functools
import ast
import operator as op
import itertools
import copy
import collections

class TokeniseError(RuntimeError): pass
class ParseError(RuntimeError): pass
class InputError(RuntimeError): pass

Token = collections.namedtuple("Token", ["type_", "value", "file_", "line"])

# code to support arithmatic expressions, copied from:
# https://stackoverflow.com/questions/2371436/evaluating-a-mathematical-expression-in-a-string 
      
# supported operators
operators = {ast.Add: op.add, 
             ast.Sub: op.sub, 
             ast.Mult: op.mul,
             ast.Div: op.truediv, 
             ast.Pow: op.pow, 
             ast.USub: op.neg}

def eval_expr(expr):
    return eval_(ast.parse(expr, mode='eval').body)

def eval_(node):
    if isinstance(node, ast.Num): # <number>
        return node.n
    elif isinstance(node, ast.BinOp): # <left> <operator> <right>
        return operators[type(node.op)](eval_(node.left), eval_(node.right))
    elif isinstance(node, ast.UnaryOp): # <operator> <operand> e.g., -1
        return operators[type(node.op)](eval_(node.operand))
    else:
        raise TypeError(node)

def s_expression(scanner, token):
    try:
        return ('number', float(eval_expr(token[1:-1])))
    except:
        return ('value', token)

def s_keyword(scanner, token):
    if len(token) > 9 or '.' in token:
        res = ('value', token)
    else:
        res = ('key', token)
    return res

def s_quote(scanner, token):
    return ('quote', token[1:-1])

def s_number(scanner, token):
    return ('number', float(token))

def s_angle(scanner, token): # also time
    if token.startswith("-"):
        multiplier = -1
        l = token[1:].split(":")
    else:
        multiplier = 1
        l = token.split(":")
    ## It's not python to use reduce.
    ## But I neither remember nor care what is.
    val = functools.reduce(lambda acc, x: 60*acc+float(x), l, 0.0) 
    return ('number', multiplier * val)

def s_value(scanner, token):
    return ('value', token)

def s_string(scanner, token):
    return ('value', token)

def s_misc(scanner, token):
    return ('misc', token)

class Parser:
    """
    Parses a keyin file like object, 
    iterate_keyfile method returns the iterator
    """
    def __init__(self, input_, record_defaults={}, state_defaults={}):
        # (?=...): look-ahead, but doesn't consume
        token_separator = "(?=[\s,/]|$)" 
        self.scanner = re.Scanner([
            ("\!.*\n", None),
            ("[ \t]+", None),
            ("\n", None),
            ("\r\n", None),
            ("=", lambda s, t: ('equal', None)),
            ("'[^'\n]*'", s_quote), 
            ("\"[^\"\n]*\"", s_quote), # Sigh.  Double quotes used in freq.dat
            ("/", lambda s, t: ('end_chunk', None)),
            (",", lambda s, t: ('comma', None)),
            ("[+-]?[0-9]+:[0-9]+:[0-9]+(\.[0-9]*)?" + token_separator, s_angle),
            ("[+-]?[0-9]+:[0-9]+(\.[0-9]*)?" + token_separator, s_angle),
            ("[+-]?[0-9]*\.[0-9]+(E[+-][0-9]{1,3})?(?![A-Za-z_0-9()])" + 
             token_separator, s_number),
            ("[+-]?[0-9]+\.?(?![A-Za-z_0-9()])" + token_separator, s_number),
            # '@' may appear in value (email address)
            ("[@\.$:()/A-Za-z_0-9\_+-]*@[@\.$:()/A-Za-z_0-9\_+-]*", s_value),
            # apparently parens and unquoted minus signs are allowed 
            # in keywords?
            ("[A-Za-z0-9][:()A-Za-z_0-9._+-]*(?=[ \t]*(=|/|\n|\r\n))?", 
             s_keyword),
            ("\([\d*+\-/.()E\s]+\)", s_expression),
            ("[\.$:()/A-Za-z_0-9\_+-]+", s_value),
            (".*", s_misc)
        ])
        self.set_defaults(record_defaults, state_defaults)
        self.input_stack = collections.OrderedDict()
        self.line_number = 0
        self.input_ = input_

    def _push_input(self, filename):
        self.input_stack[self.input_] = self.line_number
        self.input_ = open(filename, "r")

    def _pop_input(self):
        if len(self.input_stack) > 0:
            self.input_.close()
            self.input_, self.line_numer = self.input_stack.popitem()
        else:
            self.input_ = None

    def readline(self):
        self.line_number += 1
        return self.input_.readline()

    def set_defaults(self, record, state):
        self.record_defaults = record
        self.state_defaults = state

    def p_chunk(self):
        entries = []
        while self.tok.type_ != 'end_chunk':
            entries.append(self.p_item())
        logging.debug("p_chunk %s", str(self.tok))
        return dict(entries)

    def p_item(self):
        lhs = self.p_key()
        if self.tok.type_ == 'equal':
            logging.debug("p_item %s", str(self.tok))
            self.tok = next(self.tokIt)
            rhs = self.p_rhs()
        elif self.tok.type_ in ['value', 'quote', 'number']:
            rhs = self.p_rhs()
        else:
            rhs = True # for unitary expressions.
        return (lhs, rhs)

    def p_key(self):
        logging.debug("p_key: %s", str(self.tok))
        if self.tok.type_ != 'key':
            raise ParseError("Expected key token, got %s" % str(self.tok))

        keyword = self.tok.value.upper()
        
        self.tok = next(self.tokIt)
        return keyword

    def p_rhs(self):
        val = self.p_value()
        rhs = [val]
        while self.tok.type_ == 'comma':
            logging.debug("p_rhs: %s", str(self.tok))
            self.tok = next(self.tokIt)
            rhs.append(self.p_value()) # p_value advances tok beyond the value.
        if len(rhs)==1:
            rhs = rhs[0]
        return rhs

    def p_value(self):
        if self.tok.type_ not in ['value', 'quote', 'number', 'key']:
            raise ParseError("Unexpected RHS token %s" % str(self.tok))
        val = self.tok
        logging.debug("p_value: %s", str(val))
        self.tok = next(self.tokIt)
        return val[1]

    def _write_defaults(self, parameters, destination):
        combined = copy.copy(self.record_defaults)
        combined.update(self.state_defaults)
        print("\n".join("{} = {}".format(
            key.upper(), 
            ", ".join(repr(e) for e in combined[key][0])
            if type(combined[key][0]) == list else
            repr(combined[key][0]))
                        for key in sorted(combined.keys())
                        if key in parameters),
              file=destination)

    # SCHED keyin backwards compatibility:
    # control keywords, they mainly support interactive usage,
    # so need to be responsive (handle it when enter is pressed, 
    # instead of waiting for a '/' end marking)
    control_re = re.compile("^\s*(" +
                            "|".join(("SAVE\s(?P<save>.*)", 
                                      "HELP\s(?P<help>.*)", 
                                      "SHOW\s(?P<show>.*)", 
                                      "@(?P<include>.*)")) +
                            ")", 
                            flags=re.IGNORECASE)
    default_file = "sched.par"
    def _handle_control(self, match):
        groups = match.groupdict()
        if groups["save"] is not None:
            destination = groups["save"].strip()
            if destination == "":
                destination = default_file
            to_save = set(self.record_defaults.keys())
            to_save.update(self.state_defaults.keys())
            with open(destination, "w") as f:
                self._write_defaults(to_save, f)
        elif groups["help"] is not None:
            print(", ".join(key.upper() for key in sorted(itertools.chain(
                self.record_defaults.keys(), self.state_defaults.keys()))))
        elif groups["show"] is not None:
            to_show = {word.lower() 
                       for word in groups["show"].split()}
            if len(to_show) == 0:
                to_show = set(self.record_defaults.keys())
                to_show.update(self.state_defaults.keys())
            self._write_defaults(to_show, sys.stdout)
        elif groups["include"] is not None:
            self._push_input(groups["include"].strip())

    def iterate_keyfile(self):
        tokens = []
        while True:
            # try to parse records from current tokens
            self.tokIt = iter(tokens)
            while True:
                try:
                    self.tok = next(self.tokIt)
                    yield self.p_chunk()
                except ParseError as txt:
                    raise ParseError("{} line {}: {}".format(
                        self.tok.file_.name, self.tok.line, txt))
                except StopIteration:
                    break
                # remove used tokens from the collection
                tokens = list(self.tokIt)

            # read more input
            if self.input_ is None:
                if len(tokens) > 0:
                    raise ParseError("Unexpected end of input")
                raise StopIteration()
            if self.input_.isatty():
                print("* ", end="", flush=True)
            # read line by line to allow special control sequences to be 
            # interactive, this only works if tokens are on a single line
            # (assumed to be the case)
            text = self.readline()
            if len(text) == 0:
                self._pop_input()
                if self.input_ is None:
                    raise StopIteration()
                continue
            
            match = self.control_re.match(text)
            if match:
                self._handle_match(match)
            else:
                res = self.scanner.scan(text)
                if res[1] != "":
                    raise ParseError("{} line {}: Unparsed text: {}.".format(
                        self.input_.name, self.line_number, res[1][:20]))
                for token in res[0]:
                    # the scanner returns the type and text
                    # add the file and line number
                    tokens.append(Token(token[0], token[1], 
                                        self.input_, self.line_number))

def print_tree(res):
    logging.debug("Result: %s", str(res))
    all_chunks_text = []
    for chunk in res:
        chunk_text = []
        for (k, v) in chunk:
            logging.debug("%s %s", str(k), str(v))
            e_text = "\t%s : %s" % (repr(k), repr(v))
            j_text = re.sub("'", '"', e_text)
            chunk_text.append(j_text)
        logging.debug("Rendered: %s", chunk_text)
        all_chunks_text.append("\t{\n%s\n\t}" % (",\n".join(chunk_text)))
    logging.debug("All: %s", all_chunks_text)
    contents = ",\n".join(all_chunks_text)
    logging.debug("Contents: %s", contents)
    print("[\n%s\n]\n" % contents)

class KeyfileIterator:
    def __init__(self, input_, record_defaults={}, state_defaults={}):
        # make parser available as an ugly hack to be able to access the input
        # as reading the coverletter text is done outside of the keyin parser
        # as it is not in keyin form
        self.input_ = input_
        self.parser = Parser(input_, record_defaults, state_defaults)
        self.iterator = self.parser.iterate_keyfile()

    def __iter__(self):
        return self.iterator

    def __next__(self):
        return next(self.iterator)

    def set_defaults(self, record, state):
        """
        This is used to show interactive help
        """
        self.parser.set_defaults(record, state)

def iterate_keyfile(f):
    return Parser(f).iterate_keyfile()

def read_keyfile(f):
    return list(iterate_keyfile(f))

def process_file(f):
    res = read_keyfile(f)
    print_tree(res)

import json
    
def convert(fnin, fnout):
    with open(fnin, "r") as i:
        j = read_keyfile(i)
    with open(fnout, "w") as o:
        json.dump(j, o)
    
if __name__=="__main__":
    import sys, logging
    logging.basicConfig(filename='example.log', level=logging.DEBUG)
    logging.debug("Srsly you guys")
    convert(sys.argv[1], sys.argv[2])
    