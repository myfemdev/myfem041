#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Menu class."""
import logging
import trees
import Node
import SubMenu

class Menu(Node.Node):
    """Menu class."""
    def __init__(self):
        Node.Node.__init__(self)

    def load(self,menu):
        """Load menu."""
        Node.Node.load(self,menu)
        #items = menu.getchildren() v < 3.9
        items = list(menu)
        for item in items:
            if (item.tag=='submenu'):
                submenu = SubMenu.SubMenu()
                submenu.load(item)
                self.add_child(submenu)
            
    def save_data(self, parent, parameters=None, transform=None):
        """Save menu data."""
        e = trees.ET.Element(self.get_tag())
        e.set("name",self.get_name())
        for child in self.get_children():
            child.save_data(e, parameters, transform)
        parent.append(e)

    def prepare(self, menus):
        """Prepare menu."""
        for child in self.get_children():
            child.prepare(menus)

    def preload(self):
        """Preload menu."""
        errors = []
        for child in self.get_children():
            r = child.preload()
            errors.extend(r)
        return errors

    def pretest(self, options):
        """Pre-test menu."""
        errors = []
        for child in self.get_children():
            r = child.pretest(options)
            errors.extend(r)
        return errors

    def get_nodes(self, which):
        """Get nodes."""
        result = []
        for child in self.get_children():
            r = child.get_nodes(which)
            result.extend(r)
        return result

    def save_menu(self, parent):
        """Save menu."""
        e = trees.ET.Element(self.get_tag())
        e.attrib = self.get_attribs().copy()
        for child in self.get_children():
            child.save_menu(e)
        parent.append(e)

    def dump(self, index=0):
        """Dump menu."""
        logging.debug(3*index*' '+'menu: name, '+self.get_name())
        for child in self.get_children():
            child.dump(index + 1)
