import schedlib as s

import numpy as np

import pkg_resources

import pickle
import collections
import copy
import re
import itertools
import os.path
import pkg_resources

class KeyinError(RuntimeError):
    pass

subscript_re = re.compile(r"(?P<key>[A-Za-z0-9]+)\((?P<index>\d+)\)")
def merge_record_with_defaults(record, record_defaults, state_defaults):
    """
    Merge record values from keyin reader with defaults,
    state_defaults are updated if new values are in record,
    record_defaults and state_defaults are dicts:
     {key: [default value, operator]}
    key is assumed to be lower case
    returns a (dict for the record merged with default, 
               set of expanded keys present in record) tuple
    where the operator is applied to the supplied value for the key in record
    """

    # There is an opportunity to reduce runtime here:
    # By preprocess defaults with the functions that process keyin values.
    # Right now that is executed on both given values and defaults.
    # Usually there are (way) more defaults than given values.
    # So far, this doesn't seem to be a significant contribution to runtime,
    # so I have chosen to keep things simple for now.

    assert(not (set(record_defaults.keys()) & set(state_defaults.keys())))
    record = {key.lower(): value for key, value in record.items()}
    # keys in record might be starting substring of keys in defaults
    # as long as they are unique, update the keys to their full name
    all_keys = sorted(itertools.chain(record_defaults.keys(),
                                      state_defaults.keys()))
    # keyin allows syntax like '<key>(<n>) = value'
    # store the key part in record keys
    record_keys = []
    # and the n part in start indices
    start_indices = []
    original_key = []
    for key in record.keys():
        match = subscript_re.match(key)
        if match:
            new_key = match.group("key")
            record_keys.append(new_key)
            start_indices.append(int(match.group("index")) - 1)
        else:
            record_keys.append(key)
            start_indices.append(0)
        
    key_indices = np.searchsorted(all_keys, record_keys, side="left")
    values = {key: copy.deepcopy(value[0]) 
              for key, value in record_defaults.items()}
    values.update({key: copy.deepcopy(value[0]) 
                   for key, value in state_defaults.items()})
    try:
        for key, value, key_index, start_index in zip(
                record_keys, record.values(), key_indices, start_indices):
            if (key_index >= len(all_keys)) or \
               (not all_keys[key_index].startswith(key)):
                raise KeyinError("Unknown parameter: {}".format(key))
            if (all_keys[key_index] != key) and \
               (key_index + 1 < len(all_keys)) and \
               (all_keys[key_index+1].startswith(key)):
                raise KeyinError("'Ambiguous keyword: {}".format(key))
            full_key = all_keys[key_index]
            
            # if the default is a list, slice the given value into the default
            if isinstance(values[full_key], list):
                if isinstance(value, str):
                    # don't want to slice strings
                    values[full_key][start_index:start_index+1] = [value]
                else:
                    # assume we do want to slice any other type that supports it
                    try:
                        values[full_key][start_index:start_index+len(value)] = \
                            value
                    except TypeError:
                        values[full_key][start_index:start_index+1] = [value]
            else:
                values[full_key] = value

        # now apply the operator and update the state defaults
        for key, (dummy, operator) in record_defaults.items():
            values[key] = operator(values[key])
        for key, (dummy, operator) in state_defaults.items():
            state_defaults[key][0] = values[key]
            values[key] = operator(values[key])
    except KeyinError:
        raise
    except Exception as e:
        raise KeyinError("Error at key '{}' value '{}'".format(
            key, values.get(key)))

    return (values, set(all_keys[index] for index in key_indices))

# keyin value processing functions
noop = lambda x: x
upper = lambda x: x.upper()
lower = lambda x: x.lower()
foreach = lambda f: (lambda x: [f(e) for e in x])
strip_upper = lambda x: x.upper().strip()
multiply_by = lambda m: (lambda x: x * m)
extend_to = lambda l: (lambda x: x + [x[0]] * (l - len(x)))
# map int to bool (as KEYIN doesn't do bool), 
# the unconventional True if 0 else False comes from SCHED
to_bool = lambda x: x == 0 
def chain(*functions):
    def ret(x):
        for f in functions:
            x = f(x)
        return x
    return ret

def f2str(f_str):
    return bytes(f_str).decode().rstrip()

def f2str_array(a):
    # arrays of string are actually returned as matrices of numpy.bytes_
    # with a fotran/C mix-up in memory layout
    # eg: ["foo", "bar", "", ""] is returned as
    #     [["f", "a", ""], ["o", "r", ""], ["o", "", ""], ["b", "", ""]]
    return np.array(["".join(c.decode() for c in row).rstrip()
                     for row in a.flatten(order="F").reshape(a.shape)], 
                    dtype=object)

def expand_file_name(shell_file_name):
    return os.path.expanduser(os.path.expandvars(shell_file_name))

def get_catalog_dir():
    if "SCHED" in os.environ:
        return os.path.join(os.environ["SCHED"], "catalogs")
    else:
        return os.path.join(pkg_resources.resource_filename("pysched", ".."), 
                            "catalogs")

bool2str = lambda b: "T" if b else "F"

def resize_string(text, size, attr):
    ret = text.ljust(size)
    if len(ret) > size:
        ret = ret[:size]
        s.wlog(1, ("Warning, maximum string length exceeded for {attr}, "
                   "truncating '{text}' to '{ret}'").format(
                       attr=attr, text=text, ret=ret))
    return ret
