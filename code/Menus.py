#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Menus class."""
import shutil
import trees
import logging
import config
import NoDeep
import Node
import Menu
import SubMenu
import Leaf

class Menus(Node.Node):
    """Menus class."""
    def __init__(self, window):
        Node.Node.__init__(self)
        self.tag     = None
        self.indexed = {} # num: submenu
        self.window  = NoDeep.NoDeep(window)

    def get_window(self):
        return self.window.geti()

    def is_loaded(self):
        return self.get_tag() is not None

    def to_save(self):
        return self.get_tag() is not None and self.get_attribs().get("save") != "false"

    def load_file(self, filename=None):
        """Load Menu from filename.
        
        Args: 
            filename: (str) XML file to load menu from.
            
        Returns:
            (bool) Whether or not the load was successful.
        """ 
        logging.debug(filename)
        if filename is None:
            filename = config.FILE_MENULOCAL
        try:
            tree = trees.ET.parse(filename)
        except Exception as x:
            if filename == config.FILE_MENULOCAL:
                self.get_window().errormsg(f"Unable to load menu file {filename}: {repr(x)}.")
            return False
        Node.Node.__init__(self)
        self.indexed = {}
        root         = tree.getroot()
        Node.Node.load(self, root)
        #items = root.getchildren() v < 3.9
        items = list(root)
        num = 0
        for item in items:
            if item.tag == "menu":
                menu = Menu.Menu()
                menu.load(item)
                self.add_child(menu)
                # Build dictionary from indices
                for submenu in menu.get_children():
                    self.indexed[num] = submenu
                    submenu.set_index(num)
                    num = num + 1
                    # Assign index to submenus in order to identify events
                    for subsubmenu in submenu.get_children():
                        if isinstance(subsubmenu, SubMenu.SubMenu):
                            self.indexed[num] = subsubmenu
                            subsubmenu.set_index(num)
                            num = num + 1
        return True

    def reindex(self, n=None):
        """Calculate unique numbers for submenu calls.
       
        Note:
            The latter can be rewritten to get a starting index.
        """
        num = 0 if n is None else n
        self.indexed.clear()
        for menu in self.get_children():
            for submenu in menu.get_children():
                self.indexed[num] = submenu
                submenu.set_index(num)
                num = num + 1
                #Reassign an index to subsubmenus in order to identify their events
                for subsubmenu in submenu.get_children():
                    if isinstance(subsubmenu, SubMenu.SubMenu):
                        self.indexed[num] = subsubmenu
                        subsubmenu.set_index(num)
                        num = num + 1
        return num

    def save_data(self, filename=None, parameters=None, extras=[], force=False, transform=None, transformextra=None):
        """Save data."""
        # Temporary
        if not self.is_loaded():
            return
        e = trees.ET.Element("data")
        tree = trees.ET.ElementTree(e)
        root = tree.getroot()
        for extra in extras: # Allow filename and other data
            extra.save_data(root, parameters, transformextra)
        for menu in self.get_children():
            if force or menu.get_attribs().get(config.AT_SAVETHIS) != config.VALUE_FALSE:
                menu.save_data(root, parameters, transform)
        Menus.indent_data(root)
        if filename is None:
            filename = self.get_datafile()
        if filename is not None:
            logging.debug(f"Save_data {filename}")
            tree.write(filename, "iso-8859-15") # Latin-9, (similar to ISO 8859-1) #TODO: evaluate to use utf-8

    def get_parameters(self):
        """Get parameters."""
        data = {}
        for child in self.get_children():
            child.add_parameters(data)
        return data

    def prepare(self):
        """Prepare all children."""
        for child in self.get_children():
            child.prepare(self)

    def preload(self):
        """Preload all children."""
        errors = []
        for child in self.get_children():
            r = child.preload()
            errors.extend(r)
        return errors

    def pretest(self, options=False):
        """Pre-test files.

        Args:
            options: (bool) If True, test all files; if False, test only files != ''
        """
        errors = []
        for child in self.get_children():
            r = child.pretest(options)
            errors.extend(r)
        return errors

    def get_nodes(self, which):
        """Get children nodes."""
        result = []
        for child in self.get_children():
            r = child.get_nodes(which)
            result.extend(r)
        return result

    def save_menu(self, filename=None, force=False):
        """Save menu."""
        if not self.is_loaded() or not self.to_save(): # Do not save
            return
        e = trees.ET.Element(self.get_tag())
        tree = trees.ET.ElementTree(e)
        root = tree.getroot()
        root.attrib = self.get_attribs().copy()
        for child in self.get_children():
            if force or child.get_attribs().get(config.AT_SAVETHIS) != config.VALUE_FALSE:
                child.save_menu(root)
        Menus.indent_menu(root)
        if filename is None:
            filename = config.FILE_MENULOCAL
        logging.debug(f"save_menu {filename}")
        try:
            shutil.copy2( filename , filename+'~' )
        except IOError:
            pass
        tree.write(filename, "utf-8")

    def get_index(self, num):
        """Get index."""
        if (num in self.indexed):
            return self.indexed[num]
        else:
            return None

    def get_datafile(self):
        """Get string 'local.dat.xml'.
        
        TODO:
            Evaluate whether or not this method can be removed.
        """
        return config.FILE_MENULOCALSOLVER

    def dump1(self, obj=None, index=0):
        """Dump an (optional) object."""
        obj2 = obj
        if (obj is None):
            obj2 = self
            print(2*index*' '+'menus: datafile:'+str(obj2.get_datafile()))
        else:
            print(2*index*' '+str(obj2.get_name()))
        for child in obj2.get_children():
            self.dump(child, index + 1)

    def dump(self, index=0):
        """Dump self."""
        print(3*index*' '+'menus: datafile:'+str(self.get_datafile()))
        for child in self.get_children():
            child.dump(index + 1)

    @staticmethod
    def indent_data(element, level=0, last=False):
        """Indent data.

        Note:
             Static method.
        """
        #children = element.getchildren() v < 3.9
        children = list(element)
        l = level
        if (last):
            l = l - 1
        if (l<0):
            l = 0
        element.tail = '\n' + l*'\t'
        string1 = ''
        string2 = ''
        string3 = ''
        if (element.tag == 'elements'):
            string1 = '\n'+level*'\t'
        if (True):
            if len(children)==0:
                string2 = '\n' + level*'\t'
                string3 = ''
            else:
                string2 = '\n' + (level+1)*'\t'
                string3 = '\t'
        if (isinstance(element.text,str)):
            if (len(element.text)==0):
                element.text = string2
            else:
                text = element.text.replace('\n', '\n'+level*'\t')
                if '\n' in element.text:
                    element.text = string1 + text + string3
                else:
                    element.text = string1 + text + string2
        else:
            element.text = string2
        i = 0
        for child in children:
            is_last = i + 1 == len(children)
            Menus.indent_data(child, level+1, is_last)
            i = i + 1

    @staticmethod
    def indent_menu(element, level=0, last=False):
        """Indent menu.

        Note:
             Static method.
        """
        #children = element.getchildren() v < 3.9
        children = list(element)
        l = level
        if (last):
            l = l - 1
        if (l<0):
            l = 0
        element.tail = '\n' + l*'\t'
        string1 = ''
        string2 = ''
        if (True):
            if len(children)==0:
                string2 = '\n' + level*'\t'
            else:
                string2 = '\n' + (level+1)*'\t'
        tags = set()
        tags.add('menus')
        tags.add('menu')
        tags.add('submenu')
        tags.add('action')
        tags.add('struct')
        tags.add('leaf')
        if element.tag in tags:
            if (isinstance(element.text,str)):
                if (len(element.text)==0):
                    element.text = string2
                else:
                    element.text = string1 + text + string2
            else:
                element.text = string2
        i = 0
        for child in children:
            is_last = i + 1 == len(children)
            Menus.indent_menu(child, level+1, is_last)
            i = i + 1
