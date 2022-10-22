#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Module to manage formulas."""
from math import *
import string
import logging

def real(var):
    """Return real attribute.""" 
    return var.real

def imag(var):
    """Return imag attribute."""
    return var.imag

def create_lambda(formula, variables):
    """Create a lambda function."""
    # List of mathematical functions that can be used in lambda function
    safe_list = ['math','acos', 'asin', 'atan', 'atan2', 'ceil', 'cos', 'cosh', 'degrees',       \
                 'e', 'exp', 'fabs', 'floor', 'fmod', 'frexp', 'hypot', 'ldexp', 'log', 'log10', \
                 'modf', 'pi', 'pow', 'radians', 'sin', 'sinh', 'sqrt', 'tan', 'tanh']
    # safe_dict[f] = "current definition of f in globals()" or None if f is not in globals().
    safe_dict = dict([(k, globals().get(k, None)) for k in safe_list])
    # Manually add the following builtin functions
    safe_dict['abs']     = abs
    safe_dict['int']     = int
    safe_dict['float']   = float
    safe_dict['complex'] = complex
    safe_dict['imag']    = imag
    safe_dict['real']    = real
    func = 'lambda ' + ','.join(variables) + ': ' + formula
    globales = safe_dict
    globales["__builtins__"] = None
    try:
        lam = eval(func, globales)
    except Exception as e:
        return str(e)
    return lam

def exec_lambda(function, arguments):
    """Execute a lambda function."""
    try:
        res = function(*arguments)
    except Exception as e:
        return str(e)
    return res

def split_b(text, limit):
    """Split text into parts.
    
    Note:
        # Catch "\;" and translate it as ";".
    """
    parts = []
    i = 0
    temp = ''
    while i < len(text):
        c = text[i]
        if c == '\\':
            if i + 1 < len(text):
                if text[i+1] == limit:
                    temp += text[i+1]
                else:
                    temp += text[i] + text[i+1]
                i += 2
                continue
            else:
                logging.error('missing char after escape char \'\\\'')
                i += 1
                continue
        elif c == limit:
            parts.append(temp)
            temp = ''
        else:
            temp += c
        i = i + 1
    parts.append(temp)
    return parts

def check_b(varname):
    """Check validity of variable names."""
    if len(varname) < 1:
        return False
    if varname[0] not in string.ascii_letters and varname[0] != '_':
        return False
    for l in varname[1:]:
        if l not in string.ascii_letters and l not in string.digits and l != '_':
            return False
    reserved = ['and','del','from','not','while', 'as','elif','global','or','with','assert','else','if','pass','yield','break',
        'except', 'import','print','class','exec','in','raise','continue','finally','is','return', 'def','for','lambda','try']
    if varname in reserved:
        return False
    return True

def extract_parts(text):
    """Extract formula parts.

    Note:
        Use the following rules:
        1) formula="a+b*c+d;a=../one/two;b=.;c=10.0;d=/other/stop".
        2) ; is the part separator.
        3) \; is interpreted as ;.
        
    TODO: 
        Check whether or not c=10.0 is permitted. For example, 
        - node with element number and field, which to choose?
        - Are "c=field:../one" "c=value:../one" "c=data:10.0" permitted?
    """
    proc1 = split_b(text, ';')
    if len(proc1) < 1:
        return "Missing formula"
    if len(proc1[0]) < 1:
        return "Empty formula"
    formula = proc1[0]
    variables = []
    for i in range(1,len(proc1)):
        proc2 = split_b(proc1[i], '=')
        if len(proc2) != 2:
            return "Variable number " + str(i) + " invalid. Only one '=' symbol allowed"
        name = proc2[0]
        typepath = split_b(proc2[1],':')
        if len(typepath) != 2:
            return "Variable number " + str(i) + " (" + name + ") invalid. Must include 'menu:'"
        type_ = typepath[0]
        if type_ != 'menu': 
            return "Variable number " + str(i) + " (" + name + ") invalid. Must include 'menu:'"
        path = typepath[1]
        if not check_b(name):
            return "'" + name + "' is not a valid variable name"
        variables.append([name,type_,path])
    return (formula, variables)
