#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Dialog module."""
import wx
import os
import config

def get_folder(parent, path_initial):
    """Get folder."""
    dialog = wx.DirDialog(parent, "Select a folder", path_initial, wx.RESIZE_BORDER)
    result = dialog.ShowModal()
    path = None
    if (result == wx.ID_OK):
        path = dialog.GetPath()
    dialog.Destroy()
    dialog = None
    return path

def get_file(parent, path_initial, file_extensions=None):
    """Get file."""
    (dir_name, file_name) = os.path.split(path_initial)
    if dir_name == "": # to stay in current directory
        dir_name = "."
    if file_extensions is None:
        dialog = wx.FileDialog(parent, "Select a file", dir_name, file_name, style=wx.FD_OPEN)
    else:
        dialog = wx.FileDialog(parent, "Select a file", dir_name, file_name, file_extensions, style=wx.FD_OPEN)
    result = dialog.ShowModal()
    file = None
    if (result == wx.ID_OK):
        file = dialog.GetPath()
    dialog.Destroy()
    dialog = None
    return file

def get_file_save(parent, dir_name, file_name, file_extensions=None, title=None):
    """Get file and save."""
    if title == None:
        title = "Select a file to save"
    if file_extensions is None:
        dialog = wx.FileDialog(parent, title, dir_name, file_name, style=wx.SAVE|wx.FD_OVERWRITE_PROMPT)
    else:
        dialog = wx.FileDialog(parent, title, dir_name, file_name, file_extensions, style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)
    result = dialog.ShowModal()
    file = None
    ext_selected = None
    if (result == wx.ID_OK):
        file = dialog.GetPath()
        ext_selected = dialog.GetFilterIndex()
    dialog.Destroy()
    dialog = None
    return [file,ext_selected]

def show_info(parent, txt):
    """Show info."""
    dialog = wx.MessageDialog(parent, txt, 'Info', wx.OK | wx.ICON_INFORMATION)
    dialog.ShowModal()
    dialog.Destroy()
    dialog = None

def show_error(parent, txt):
    """Show error."""
    dialog = wx.MessageDialog(parent, txt, 'Error', wx.OK | wx.ICON_ERROR)
    dialog.ShowModal()
    dialog.Destroy()
    dialog = None

def ask_yes_no(parent, txt):
    """Ask yes/no."""
    dialog = wx.MessageDialog(parent, txt, 'Question', wx.YES_NO | wx.NO_DEFAULT)
    result = dialog.ShowModal()
    dialog.Destroy()
    dialog = None
    return result == wx.ID_YES

def ask_ok_cancel(parent, txt):
    """Dialog with ok/cancel."""
    dialog = wx.MessageDialog(parent, txt, 'Warning', wx.OK | wx.CANCEL)
    result = dialog.ShowModal()
    dialog.Destroy()
    dialog = None
    return result == wx.ID_OK

def about(name, path):
    """About dialog."""
    if   name.lower() == 'menum' or name.lower() == 'opennum':
        description = name + """ is software to reconfigurable interfaces."""
        license = " " #include here license text if you want to show it in the About dialog.
    else:
        description = name + """ is a software distributed together with OpenNum.""" 
        licence = name + """ is free software; you can redistribute it and/or modify it 
under the terms of the GNU General Public License as published by the Free Software Foundation; 
either version 2 of the License, or (at your option) any later version.

""" + name + """ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
See the GNU General Public License for more details. You should have received a copy of 
the GNU General Public License along with File Hunter; if not, write to 
the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"""
    info = wx.AboutDialogInfo()
    info.SetName(name)
    info.SetVersion('0.5.9')
    info.SetDescription(description)
    info.SetCopyright('(C) 2009,2010 dma')
    info.SetWebSite('http://www.example.com')
    info.SetLicence(licence)
    info.AddDeveloper('someone')
    info.AddDocWriter('someone')
    info.AddArtist('another one')
    info.AddTranslator('another one')
    wx.AboutBox(info)