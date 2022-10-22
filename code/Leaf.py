#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Leaf class."""
import os.path
import logging
import trees
import config
import Node
import Element

class Leaf(Element.Element):
    """Leaf class."""
    def __init__(self):
        Element.Element.__init__(self)

    def set_elements(self, elements):
        """Set elements (leaf)."""
        self.del_children()
        for element in elements:
            n = Node.Node()
            n.tag = 'element'
            n.set_name(element)
            n.attribs['selected'] = 'true'
            self.add_child(n)

    def set_elements_nosel(self, elements):
        """Set elements but remain unselected (leaf)."""
        self.del_children()
        for element in elements:
            n = Node.Node()
            n.tag = 'element'
            n.set_name(element)
            self.add_child(n)

    def set_elements_with_source(self, menus, elements, sources=None):
        """Set elements with source (leaf)."""
        if sources is None:
            sources = self.get_remote_source(menus)
        if (isinstance(sources, list)):
            self.del_children()
            elementsset = set(elements)
            for source in sources:
                if source in elementsset:
                    n = Node.Node()
                    n.tag = 'element'
                    n.set_name(source)
                    n.attribs['selected'] = 'true'
                    self.add_child(n)
            return True
        else:
            return sources

    def get_elements_with_source(self, menus, sources=None):
        """Get elements with source (leaf)."""
        if sources is None:
            sources = self.get_remote_source(menus)
        self.clear_elements_not_in_source(menus, sources)
        if (isinstance(sources, list)):
            result = []
            names = set()
            for child in self.children:
                name = child.get_name()
                names.add(name)
            for source in sources:
                if source in names:
                    result.append((source,True))
                else:
                    result.append((source,False))
            return result
        else:
            return sources

    def load(self, submenu):
        """Load (leaf)."""
        Element.Element.load(self,submenu)
        #items = submenu.getchildren() v < 3.9
        items = list(submenu)
        for item in items:
            if (item.tag=='element'):
                n = Node.Node()
                n.load(item)
                if item.attrib.get('selected') == config.VALUE_TRUE:
                    n.attribs['selected'] = config.VALUE_TRUE
                name = item.text
                if name is None:
                    name = ''
                n.set_name(name)
                self.add_child(n)

    def preload(self):
        """Pre-load leaf."""
        return []

    def pretest(self, options):
        """Pre-test leaf."""
        the_type = self.attribs.get('type')
        the_subtype = self.attribs.get('subtype')
        if not ( the_type == 'file' and (the_subtype == 'mesh' or the_subtype == 'field')):
            return []
        menus = self.get_top()
        if menus is None:
            return ['Error: menus is None in pretest\n']
        window = menus.get_window()
        dim = None
        dimstr = self.attribs.get('dim')
        if dimstr is not None:
            try:
                dim = int(dimstr)
            except ValueError:
                return ["Error: dim: "+str(dimstr)+" in pretest\n"]
        filename = self.get_first_name()
        # When 'options' is not True, do not test blank-name files
        if options is not True and ( not isinstance(filename,str) or filename == '' ) :
            return []
        # TODO: it can be substituted by FileTrack2.py
        fm = window.filemanager
        tracker = None
        if the_subtype == 'mesh':
            tracker = fm.get_tracker_mesh_file(filename, dim)
        if the_subtype == 'field':
            tracker = fm.get_tracker_file(filename)
        if tracker is not None:
            changed = tracker.is_changed()
        else:
            changed = None
        if changed is None:
            return [f"Warning: {str(the_type)} '{filename}' does not exist"]
        return []

    def get_nodes(self, which):
        """Get nodes (leaf)."""
        result = []
        if which == 'leaf':
            result.append(self)
        elif which == 'leaf-file':
            if self.attribs.get('type') == 'file':
                result.append(self)
        elif which == 'leaf-file-mesh':
            if self.attribs.get('type') == 'file' and self.attribs.get('subtype') == 'mesh':
                result.append(self)
        elif which == 'leaf-file-field':
            if self.attribs.get('type') == 'file' and self.attribs.get('subtype') == 'field':
                result.append(self)
        return result

    def float_range(self,string):
        """Search ranges in a string (leaf type="float").

        Returns:
            [new_string, number_of_elements]. If number_of_elements == -1, it not a Matlab-like range.

        Notes:
            The ranges can be written with different formats:
             - init:step:end
             - init:end (step=1)
             - [init:step:end]
        TODO:
            Admit different initial characters like (), [], ...
        """
        string2=' '
        num = 0
        try:
            array_range = [float(s) for s in string.split(':')]
            if len(array_range)==3:
                if (array_range[0]>array_range[2] and array_range[1]>0) or\
                   (array_range[2]>array_range[0] and array_range[1]<0):
                    logging.warning(f"ERROR: wrong step in Range({string})")
                    return[string2, num]
                low = array_range[0]
                step = array_range[1]
                high = array_range[2]
                while abs(high)-abs(low)>=0:
                    string2 += str(low)+ '  '
                    low+=step
                    num = num + 1
                logging.warning(f"Range({string})")
            elif len(array_range)==2:
                if (array_range[0]>array_range[1]):
                    logging.warning(f"ERROR: wrong step in Range({string})")
                    return[string2, num]
                low = array_range[0]
                step = 1
                high = array_range[1]
                while high-low>=0:
                    string2 += str(low)+'  '
                    low+=step
                    num = num + 1
                logging.warning(f"Range({string})")
            elif len(array_range)==1:
                string2 = string
                num = -1
        except:
            string2 = string
            num = -1
        return [string2,num]

    def save_data(self, parent, parameters=None, transform=None):
        """Save data (leaf)."""
        e = trees.ET.Element("leaf")
        e.attrib = Node.Node.filter_attribs(self.attribs)
        if e.attrib.get('type') == 'float':
            glue = ' '
        else:
            glue = '\n'        
        trfunction = None
        if transform is not None:
            if e.attrib.get('type') == 'file':
                trfunction = transform.add_get_file
            if e.attrib.get('type') == 'folder':
                trfunction = transform.add_get_dir
        if parameters is None or self not in parameters:
            if self.get_attribs().get('type') == 'charlist':
                elements = self.get_elements_selected()
            else:
                elements = self.get_elements()
        else:
            elements = [parameters[self]]
        e.attrib['totalnum'] = str(len(elements))
        v = trees.ET.Element("elements")
        text = ''
        i = 0
        for elem in elements:
            if trfunction is not None:
                text += trfunction(elem)
            else:
                if e.attrib.get('type') == 'float':
                    float_range = self.float_range(elem)
                    if float_range[1] != -1: #MOdify totalnum when it is a range
                        e.attrib['totalnum'] = str(float_range[1])
                    text += float_range[0]
                else:
                    text += elem
            text += glue
            i = i + 1
        v.text = text
        e.append(v)
        parent.append(e)

    def add_parameters(self, data):
        """Add parameters (leaf)."""
        if self.get_attribs().get(config.AT_PARAM) == config.VALUE_TRUE:
            if self.has_source():
                elements = self.get_elements()
            else:
                elements = self.get_elements_selected()
            data[self] = elements

    def save_menu(self, parent):
        """Save menu (leaf)."""
        e = trees.ET.Element(self.get_tag())
        e.attrib = self.get_attribs().copy()
        for child in self.get_children():
            element = trees.ET.Element(child.get_tag())
            element.text = child.get_name()
            element.attrib = child.get_attribs().copy()
            if 'name' in element.attrib:
                del element.attrib['name']
            e.append(element)                
        parent.append(e)

    def dump(self, index=0):
        """Dump (leaf)."""
        logging.debug(3*index*" "+"leaf: name:"+self.get_name()+";")
        for key in self.attribs:
            logging.debug(str(key)+":"+str(self.attribs[key]))
        for child in self.get_elements():
            logging.debug(3*(index+1)*" "+"element:"+str(child))
