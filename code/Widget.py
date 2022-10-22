#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Widget class."""
import wx
import wx.lib.newevent
import config
import dialogs
import logging
import copy

PIXELS_MARGIN              = 4
TXT_VALUE                  = 'Value of '
TXT_CHOOSE                 = 'Choose '
TXT_CHOOSE_CUSTOMIZE       = 'Choose/Add/Del '
TXT_BUTTON_ADD             = 'Add'
TXT_BUTTON_DEL             = 'Del'
TXT_BUTTON_DOTS            = '...'
TXT_END_LINE               = ':'
TXT_MULTIPLE_SEL           = ''
TXT_TOOLTIP_BUTTON_ADD     = 'Adds an element with the given name'
TXT_TOOLTIP_BUTTON_DEL     = 'Deletes selected element'
TXT_BUTTON_TABULAR         = 'Show'
TXT_TOOLTIP_BUTTON_TABULAR = 'Open a window with tabular data for these items'
TXT_BUTTON_HELP            = 'Help'
TXT_TOOLTIP_BUTTON_HELP    = 'Open a window with help for this item'
EventStructChange, EVT_STRUCT_CHANGE = wx.lib.newevent.NewCommandEvent()

class Widget(wx.Panel):
    """Widget class."""
    def __init__(self, parent, window, struct, index):
        wx.Panel.__init__(self, parent)
        self.parent = parent
        self.window = window
        self.menus = window.menus
        self.struct = struct
        self.index = index
        self.SetBackgroundColour(wx.Colour(200,200,200))
        self.errors = []

    def get_struct(self):
        """Get Struct (Widget)."""
        return self.struct

    def update_from_struct(self):
        pass

    def update_from_param(self, param):
        pass

    def log(self, txt):
        """Log widget."""
        self.window.add_text(txt+' '+self.struct.get_path()+'\n')

    def end(self):
        """End widget."""
        self.save_mem()
        logging.debug(f"Saving {str(self.struct.get_path())}")

    def save_mem(self):
        pass

    def save_log(self):
        pass

    def has_errors(self):
        """Return whether it has errors (Widget)."""
        return len(self.errors)>0

    def get_errors(self):
        """Return error (Widget)."""
        return self.errors

    def check_error(self, result):
        """Check error (Widget)."""
        if isinstance(result,list):
            return result
        else:
            error = str(result)
            self.errors.append(error)
            self.window.errormsg(error)
            return []
    
    def check_repeated(self, name, struct):
        """Check repeated (Widget)."""
        for child in struct.get_children():
            if child.get_name() == name:
                self.window.errormsg(f"Error: name '{name}' already exists.")
                return True
        return False

    @staticmethod
    def build(struct, parent, window, index):
        """Build widget.

        Note:
            It is a static method.
        """
        tag = struct.get_tag()
        attribs = struct.get_attribs()
        widget = None
        # See Node.get_for_source_if_sel_if() for a similar classification.
        if tag=='struct': 
            # Allow us to show a matrix from file with WindowTabular.
            if attribs.get(config.AT_SHOW) == config.AT_MATRIX and \
                attribs.get('data') is not None:
                import WindowTabular
                table = WindowTabular.WindowTabular(window, window.tabular_onclose)
                table.display(struct,True)
            # Check if struct has defaults (with or without name).
            default_list = struct.get_defaults()
            default_names = []
            for default in default_list:
                namede = default.get_attribs().get('name')
                if namede is not None:
                    default_names.append(namede)
            has_default = len(default_list) > len(default_names)
            has_default_names = len(default_names) > 0
            # There can be hidden elements.
            if len(struct.get_children()) == 0 and \
                attribs.get(config.AT_SOURCE) is None and \
                attribs.get(config.AT_CUSTOMIZE)  != config.VALUE_TRUE and \
                attribs.get(config.AT_SHOWVALUES) != config.VALUE_TRUE and \
                not has_default_names:
                return widget
            selection = attribs.get('selection')
            if (selection is None or selection == "none"):
                widget = WidgetList.WidgetList(0, parent, window, struct, index)
            elif (selection == "single"):
                widget = WidgetCombo.WidgetCombo(parent, window, struct, index)
            else:
                window.errormsg("XML menu: struct with selection present, but not one of ['none','single']")
        elif tag=='leaf':
            selection = attribs.get('selection')
            type_     = attribs.get('type')
            showfile  = attribs.get('showfile')
            if   (type_ == 'folder'):
                widget = WidgetFolderFile.WidgetFolderFile(WidgetFolderFile.FOLDER, parent, window, struct, index)
            elif (type_ == 'file'):
                widget = WidgetFolderFile.WidgetFolderFile(WidgetFolderFile.FILE, parent, window, struct, index)
            elif (type_ == 'float'):
                widget = WidgetEntry.WidgetEntry(0, parent, window, struct, index)
            elif (type_ == 'char'):
                widget = WidgetEntry.WidgetEntry(1, parent, window, struct, index)
            elif (type_ == 'complex'):
                widget = WidgetEntry.WidgetEntry(2, parent, window, struct, index)
            elif (type_ == 'charlist' and showfile is None):
                if (selection is None or selection == "single"):
                    widget = WidgetCombo.WidgetCombo(parent, window, struct, index)
                elif (selection == "multiple"):
                    widget = WidgetList.WidgetList(2, parent, window, struct, index)
                else:
                    window.errormsg("XML menu: leaf of type charlist with selection present, but not one of ['single','multiple']")
            elif (type_ == 'charlist' and showfile is not None):
                widget = WidgetText.WidgetText(parent, window, struct, index)
            else:
                window.errormsg(f"XML menu: leaf of unknown type, {str(type_)}")
        return widget

    def SetFocus(self):
        pass

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
                if (array_range[0]>array_range[2] and array_range[1]>0) or \
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

    def check_candel(self, name, struct):
        """Check if name can be deleted (Widget)."""
        struct_copy = copy.deepcopy(struct)
        attribs = struct_copy.get_attribs()
        # Discard if struct is customizable.
        if attribs.get(config.AT_CUSTOMIZE) != config.VALUE_TRUE:
            return False
        # Discard if showvalues is present.
        if attribs.get(config.AT_SHOWVALUES) == config.VALUE_TRUE:
            return False
        # Discard if struct has source and if name is in source.
        if attribs.get(config.AT_SOURCE) is not None:
            elements = self.check_error(struct_copy.get_elements_with_source(self.menus))
            for element in elements:
                if element[0] == name:
                    return False
        # Discard if name was in defaults (with name).
        default_list = struct_copy.get_defaults()
        for default in default_list:
            namede = default.get_attribs().get('name')
            if namede == name:
                return False
        # Otherwise, name can be deleted.
        return True

import WidgetList
import WidgetChoice
import WidgetCombo
import WidgetEntry
import WidgetFolderFile
import WidgetText