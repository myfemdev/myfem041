#!/usr/bin/env python
# -*- coding: utf-8 -*-



def indent(element, level=0, last=False):

    #children = element.getchildren() v < 3.9
    children = list(element)

    l = level
    if (last):
        l = l - 1
    if (l<0):
        l = 0

    element.tail = '\n' + ('\t' * l)

    if len(children) == 0:
        string2 = '\n' + ('\t' * level)
    else:
        string2 = '\n' + ('\t' * (level+1))

    if not isinstance(element.text,str):
        element.text = string2

    i = 0
    for child in children:
        is_last = i + 1 == len(children)
        indent(child, level+1, is_last)
        i = i + 1
