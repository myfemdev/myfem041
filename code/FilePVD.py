#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Read PVD files."""
import sys
import trees
import logging

def read(filename):
    """Read a PVD file.
    
    Returns:
        List of times and filenames if ok, error string if not.
    """
    times = []
    try:
        tree = trees.ET.parse(filename)
    except Exception as x:
        return f"Error loading PVD file: {filename}: {repr(x)}"
    root = tree.getroot()
    if root.tag != 'VTKFile':
        return f"Error loading PVD file: root tag != 'VTKFile'"
    #items = root.getchildren() v < 3.9
    items = list(root)
    collections = 0
    for item in items:
        if item.tag == 'Collection':
            collections += 1
            #items2 = item.getchildren() v < 3.9
            items2 = list(item)
            for item2 in items2:
                if item2.tag != 'DataSet':
                    return "Error loading PVD file: dataset tag != 'DataSet'"
                time  = item2.attrib.get('timestep')
                group = item2.attrib.get('group')
                part  = item2.attrib.get('part')
                file  = item2.attrib.get('file')
                # Only load input that: A) are first parts, B) have defined times that are convertible to float and
                #   C) have a defined file.
                if (time is not None and time != '') and \
                   (part is     None or  part == '0' or part == '') and \
                   (file is not None and file != ''):
                    timedouble = None
                    try:
                        timedouble = float(time)
                    except ValueError as e:
                        pass
                    if timedouble is not None:
                        data = {'time':timedouble, 'file':file}
                        times.append(data)
                    else:
                        logging.warning(f"PVD file '{filename}': line skipped because it cannot convert '{time}' to float.")
                else:
                    logging.debug(f"PVD file '{filename}': line skipped.")
    if collections != 1:
        return f"Error loading PVD file: incorrect number of 'Collection' tags {str(collections)}"
    return times

#if __name__ == '__main__':
#    args = len(sys.argv)
#    if args == 2:
#        res = read(sys.argv[1])
#        if isinstance(res,list):
#            for r in res:
#                print r
#        else:
#            print 'result:', res
