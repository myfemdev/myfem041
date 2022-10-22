#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Element class."""
import os.path
import logging
import trees
import config
import Node
import SubMenu
from v_vtk import configPlot

class Element(Node.Node):
    """Element class."""
    def __init__(self):
        Node.Node.__init__(self)

    def combine(self, element):
        """Merges two Element objects.

        Note:
            It also replaces children.
        """
        Node.Node.combine(self, element)

    def plot_do(self, path_exe):
        """Plot do."""
        plot_type = self.get_attribs().get(config.PLOT)
        if plot_type is not None:
            alias = configPlot.get_alias(plot_type)
            needs = configPlot.get_needs_plot(alias)
            if needs is True:
                filename = alias + '.plot.xml'
                logging.debug('loading'+filename)
                path = os.path.join(path_exe, os.pardir, 'plots', filename)
                if isinstance(self, Leaf.Leaf):
                    newstruct = Leaf.Leaf()
                else:
                    newstruct = Struct_.Struct()
                result = newstruct.load_file(path)
                if result:
                    self.combine(newstruct)
                    self.get_attribs()[config.PLOTTED] = config.VALUE_TRUE
                    self.set_children_parents()
                    return True
                else:
                    return f"Error: can not load plot XML configuration file: '{filename}'"
            elif needs is False:
                self.get_attribs()[config.PLOTTED] = config.VALUE_TRUE
                return True
            elif needs is None: # Plot needs a configuration XML file
                return 'Error: unknown plot type'
            else:
                return 'Error: unknown return value of get_needs_plot'
        else:
            return 'Error: unknown plot type'

    def load(self,item):
        """Load element."""
        Node.Node.load(self,item)

    def load_file(self, filename):
        """Load file (element)."""
        try:
            tree = trees.ET.parse(filename)
        except Exception as x:
            logging.warning(repr(x))
            return False
        root = tree.getroot()
        self.load(root)
        return True

    def save_data(self, parent, parameters=None, transform=None):
        """Save data (element)."""
        e = trees.ET.Element(self.get_tag())
        e.attrib = Node.Node.filter_attribs(self.attribs)
        for child in self.get_children():
            child.save_data(e, parameters, transform)
        parent.append(e)

    def prepare(self, menus):
        """Prepare element."""
        sources = self.get_remote_source(menus)
        self.clear_elements_not_in_source(menus, sources)
        self.create_elements_from_source( menus, sources)
        if not isinstance(self,Leaf.Leaf):
            for child in self.get_children():
                child.prepare(menus)

    def preload(self):
        """Pre-load element."""
        errors = []
        for child in self.get_children():
            r = child.preload()
            errors.extend(r)
        return errors

    def pretest(self, options):
        """Pre-test element."""
        errors = []
        for child in self.get_children():
            r = child.pretest(options)
            errors.extend(r)
        return errors

    def get_nodes(self, which):
        """Get nodes (element)."""
        result = []
        for child in self.get_children():
            r = child.get_nodes(which)
            result.extend(r)
        return result

    def save_menu(self, parent):
        """Save menu (element)."""
        e = trees.ET.Element(self.get_tag())
        e.attrib = self.get_attribs().copy()
        for child in self.get_children():
            child.save_menu(e)
        parent.append(e)

    @staticmethod
    def build(item):
        """Build element.

        Note:
            Static method.
        """
        if (item.tag=='leaf'):
            e = Leaf.Leaf()
            e.load(item)
            return e
        if (item.tag=='struct'):
            e = Struct_.Struct()
            e.load(item)
            return e
        # Detect (2nd level) submenu tag and add it to hierarchy 
        if (item.tag=='submenu'):
            e = SubMenu.SubMenu()
            e.load(item)
            return e        
        return None

    def dump(self, index=0):
        """Dump element."""
        logging.debug(3*index*' '+" element("+str(self.tag)+"): name: "+self.get_name()+";")
        for key in self.attribs:
            logging.debug(str(key)+":"+str(self.attribs[key])+" ")
        print
        for child in self.get_children():
            child.dump(index + 1)



    def get_elements_selected(self):
        """Get selected elements.
        
        Note:
            For element ~ child / child with (name, selected).
            Leafs with source have not selected elements missing.
        """
        elements = []
        for element in self.children:
            if element.attribs.get('selected') == 'true':
                elements.append(element.get_name())
        return elements

    def get_elements(self):
        """Get elements.
        
        Note:
            Leafs with source have not selected elements missing.
        """
        elements = []
        for element in self.children:
            elements.append(element.get_name())
        return elements

    def set_elements(self, elements):
        """Set element."""
        pass

    def set_elements_nosel(self, elements):
        """Set elements but remain unselected."""
        pass

    def set_elements_with_source(self, menus, elements):
        """Set elements with source."""
        pass

    def get_elements_with_source(self, menus):
        """Get elements with source."""
        pass

    def clear_elements_not_in_source(self, menus, sources=None):
        """Get elements not in source."""
        if sources is None:
            sources = self.get_remote_source(menus)
        if not isinstance(sources,list):
            return sources
        sourcesset = set(sources)
        integer = 0
        for integer in range(len(self.children)-1,-1,-1):
            child = self.children[integer]
            name = child.get_name()
            if name not in sourcesset:
                self.del_child(integer)
        return True

    def create_elements_from_source(self, menus, sources=None):
        """Create elements from source."""
        pass

import Struct_
import Leaf