#!/usr/bin/env python
# -*- coding: utf-8 -*-



import os
import sys



#avoid crashes when paths have other characters
def relativefs(path, start=None):
    pathu = str(path, sys.getfilesystemencoding())
    if start is None:
        startu = start
    else:
        startu = str(start, sys.getfilesystemencoding())
    return relative(pathu, startu)



def relative(path, start=None):
    """Gets the path to 'dst' relative to 'src'.
    >>> relative('/usr/', '/usr/bin/')
    '..'
    >>> relative('/usr/local/bin/', '/usr/bin/')
    '../local/bin'
    >>> relative('/home/jeremy/bin/', '/home/jeremy/')
    'bin'
    >>> relative('/home/jeremy/lib/python/', '/home/jeremy/')
    'lib/python'
    >>> relative('/', '/')
    ''
    >>> relative('/', '/usr/bin/')
    '../../'
    """
    if not path: return ''
    if not start: start = str(os.getcwd()) # In Python 2:, sys.getfilesystemencoding())
    if start == path: return ''
    start_list = os.path.abspath(start).split(os.path.sep)
    path_list = os.path.abspath(path).split(os.path.sep)
    # windows: fix different drive letter
    if os.name == 'nt':
        if len(path_list)>0 and len(start_list)>0 and path_list[0]!=start_list[0]:
            return path
    i = len(os.path.commonprefix([start_list, path_list]))
    rel_list = [os.path.pardir] * (len(start_list)-i) + path_list[i:]
    if len(rel_list) == 0: return ''
    return os.path.join(*rel_list)

