#!/usr/bin/env python
# -*- coding: utf-8 -*-



# list of aliases (temporary translate these strings)

alias = {
#    'references_interactive': 'references',
#    'labels_interactive': 'numbering',
#    'scalar_field': 'filled',
#    'scalar_field_range': 'threshold',
#    'scalar_field_contour': 'contour',
#    'scalar_field_deform': 'deformed',
#    'scalar_field_line_probe_interactive': 'plot_over_line',
#    'cut_interactive': 'slice',
#    'clip_interactive': 'cut',
#    'clip_raw_interactive': 'rough_cut',
#    'deformed': 'scalar_deformed'
    }



# does plot need '<name>.plot.xml' ?

plotfile = {
    'mesh': False,
    'references': False,
    'numbering': False,
    'materials': False,
    'filled': False,
    'threshold': True,
    'contour': True,
    'scalar_deformed': True,
    'vector_deformed': True,
    'plot_over_line': True,
    'slice': True,
    'cut': True,
    'rough_cut': True,
    'vector_field': True,
    '2d_graph': True,
    'vector_components': True,
    'pathline': True,
    'streamline': True,
    'image': False
    }



def get_alias(name):
    res = alias.get(name)
    if res is None:
        return name
    else:
        return res



def get_needs_plot(name):
    res = plotfile.get(name)
    if res is True:
        return True
    if res is False:
        return False
    if res is None:
        return None
