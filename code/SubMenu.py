#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Submenu class."""
import logging
import trees
import Node
import Element

class SubMenu(Node.Node):
    """SubMenu class."""
    def __init__(self):
        Node.Node.__init__(self)
        self.indexed = {}
        self.actions = []
        self.num     = -1

    def get_index(self, num=None):
        """Get submenu index.

        Note:
            It also works for Windows event control.
        """
        if num is None:
            return self.num
        if (num in self.indexed):
            return self.indexed[num]
        else:
            return None

    def set_index(self, num):
        """Set submenu index."""
        self.num = num

    def load(self,submenu):
        """Load submenu."""
        Node.Node.load(self,submenu)
        #items = submenu.getchildren() v < 3.9
        items = list(submenu)
        for item in items:
            if (item.tag=='action'):
                # item is an elementtree class
                action = {}
                action['source'] = item.attrib.get('source') # unused
                action['name']   = item.get('name')
                action['title']  = item.get('title')
                action['data']   = item.get('data')
                action['reload'] = item.get('reload')
                params = [] # one action: [param0, param1, ...]
                #subitems = item.getchildren() v < 3.9
                subitems = list(item)
                for subitem in subitems:
                    if (subitem.tag=='param'):
                        text = subitem.text
                        if text is None:
                            text = ''
                        param={'text':text, 'attrib':subitem.attrib.copy()}
                        params.append(param)
                action['params'] = params
                self.actions.append(action)
            else:
                elem = Element.Element.build(item)
                if (elem is not None):
                    self.add_child(elem)
                else:
                    logging.warning('Unknown element')

    def get_actions(self):
        """Get submenu actions."""
        return self.actions

    def save_data(self, parent, parameters=None, transform=None):
        """Save submenu data."""
        e = trees.ET.Element(self.get_tag())
        e.set("name",self.get_name())
        for child in self.get_children():
            child.save_data(e, parameters, transform)
        parent.append(e)

    def prepare(self, menus):
        """Prepara submenu."""
        for child in self.get_children():
            child.prepare(menus)

    def preload(self):
        """Pre-load submenu."""
        errors = []
        for child in self.get_children():
            r = child.preload()
            errors.extend(r)
        return errors

    def pretest(self, options):
        """Pre-test submenu."""
        errors = []
        for child in self.get_children():
            r = child.pretest(options)
            errors.extend(r)
        return errors

    def get_nodes(self, which):
        """Get nodes (submenu)."""
        result = []
        for child in self.get_children():
            r = child.get_nodes(which)
            result.extend(r)
        return result

    def save_menu(self, parent):
        """Save menu (submenu)."""
        e = trees.ET.Element(self.get_tag())
        e.attrib = self.get_attribs().copy()
        for action in self.actions:
            a = trees.ET.Element('action')
            name = action.get('name')
            if name is not None:
                a.set('name',name)
            # Custom execution attributes: title (window title), data (command)
            title = action.get('title')
            if title is not None:
                a.set('title',title)
            data = action.get('data')
            if data is not None:
                a.set('data',data)
            reload_menu = action.get('reload')
            if reload_menu is not None:
                a.set('reload',reload_menu)
            source = action.get('source') # unused
            if source is not None:
                a.set('source',source)
            for param in action.get('params'):
                p = trees.ET.Element('param')
                p.text = param.get('text')
                p.attrib = param.get('attrib').copy()
                a.append(p)
            e.append(a)
        for child in self.get_children():
            child.save_menu(e)
        parent.append(e)

    def dump(self, index=0):
        """Dump submenu."""
        logging.debug(3*index*' '+"submenu: name, "+self.get_name()+" #actions: "+str(len(self.actions)))
        for child in self.get_children():
            child.dump(index + 1)

    def reindex(self, n=None):
        """Re-index submenu."""
        if n is None:
            num = 0
        else:
            num = n
        self.indexed.clear()
        if True:
            for submenu in self.get_children():
                self.indexed[num] = submenu
                submenu.set_index(num)
                num = num + 1
                #Reassign index to submenus in order to identity their events
                for subsubmenu in submenu.get_children():
                    if isinstance(subsubmenu, SubMenu):
                        self.indexed[num] = subsubmenu
                        subsubmenu.set_index(num)
                        num = num + 1
        return num
