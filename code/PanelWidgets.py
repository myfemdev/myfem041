#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""PanelWidgets class."""
import wx
import logging
import config
import Widget

PIXELS_MARGIN = 6

class PanelWidgets(wx.ScrolledWindow):
    """PanelWidgets class."""
    def __init__(self, parent, window):
        wx.ScrolledWindow.__init__(self, parent)
        self.window = window
        self.box = wx.BoxSizer(wx.VERTICAL)
        self.SetSizer(self.box)
        self.SetScrollRate(1,1)
        self.widgetcounter = 0
        self.widgets = []
        self.Bind(Widget.EVT_STRUCT_CHANGE, self.event_struct_change)

    def display_set(self, struct):
        """Set display set (PanelWidgets)."""
        for a in range(len(self.widgets)-1,-1,-1):
            self.widgets[a].end()
        del self.widgets[:]
        self.box.Clear(True)
        self.widgetcounter = 0
        self.Scroll(0,0)
        self.add_widget(struct)

    def display_add(self, struct, index):
        """Add display (PanelWidgets)."""
        for a in range(len(self.widgets)-1,index-1,-1): # Remove widgets with index bigger than the given one
            self.widgets[a].end()
            self.box.Detach(a)
            self.widgets[a].Destroy()
            self.widgetcounter = self.widgetcounter - 1
        del self.widgets[index:]
        self.add_widget(struct)
        sz = self.GetVirtualSize()
        w,h = sz.Get() # Old wx: w,h = self.GetVirtualSizeTuple()
        self.Scroll(0,h) # Automatic scroll for every added widget

    def add_widget(self, struct):
        """Add widget (PanelWidgets)."""
        if struct is not None:
            continuar = True
            copyvalue = struct.get_attribs().get(config.AT_COPY)
            if copyvalue is not None:
                self.window.menu_copy_load(copyvalue)
                continuar = False
            if continuar:
                if struct.plot_able():
                    ok = struct.plot_do(self.window.path_exe)
                    if ok is not True:
                        self.window.errormsg(ok)
                # TODO: analyse whether the following block and the previous one could follow the widget creation.
                if struct.plot_has():
                    resultado = self.window.panelB.add_only_changed(struct)
                struct.apply_to_parent_plots(self.window.panelB.update)            
            if continuar:
                widget = Widget.Widget.build(struct, self, self.window, len(self.widgets))
                if widget is not None and widget.has_errors():
                    widget.Destroy()
                    widget = None
                if widget is not None:
                    self.SetFocus()
                    self.box.Add(widget , 0 , wx.EXPAND | wx.TOP | wx.BOTTOM | wx.LEFT | wx.RIGHT , PIXELS_MARGIN)
                    self.widgets.append(widget)
                    self.widgetcounter = self.widgetcounter + 1
                if self.widgetcounter == 1 and widget is not None:
                    widget.SetFocus()
        self.FitInside()

    def update_widget_struct(self, struct):
        """Update widget with struct contents."""
        has_structs = False
        for w in self.widgets:
            if w.get_struct() is struct:
                has_structs = True
                w.update_from_struct()
        if not has_structs:
            # Nothing to do; the struct stays modified for the next time being shown
            pass

    def update_widget_param(self, struct, param):
        """Update widget with param contents.
        
        Note: 
            It is called from edges/faces/subsel interactive. 
            It is sent to widget list.

        TODO:
            Analyze whether or not it can be remove and send everything to widget struct.
        """
        has_structs = False
        for w in self.widgets:
            if w.get_struct() is struct:
                has_structs = True
                w.update_from_param(param)
        if not has_structs:
            logging.warning('missing param update')
            has_source = struct.has_source()
            if has_source:
                elements = struct.get_elements_with_source(self.window.menus)
                if not isinstance(elements,list):
                    logging.warning('get_elements_with_source PanelWidgets error: '+str(elements))
                else:
                    names = []
                    for element in elements:
                        if element[1]:
                            if param != element[0]:
                                names.append(element[0])
                        else:
                            if param == element[0]:
                                names.append(element[0])
                    struct.set_elements(names)
            else:
                children = struct.get_children()
                for child in children:
                    if child.get_name() == param:
                        attribs = child.get_attribs()
                        if "selected" in attribs:
                            del attribs["selected"]
                        else:
                            attribs["selected"] = "true"
            struct.apply_to_all_plots(self.window.panelB.update)

    def event_struct_change(self, event):
        """Event struct change.

        Note:
            It is called by the widgets (through signals).
        """
        self.display_add(event.struct, event.index)
        self.window.apply_config()