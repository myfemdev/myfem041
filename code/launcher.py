#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Create App object.

Notes:
    The application should not be run from launcher.py; just create a Python file, named after the Application, with the code::
     
        import launcher
        if __name__ == '__main__':
            launcher.run()
"""
import os
import sys
import logging
import wx
import errno
import Window

def create_dir(dirname):
    """Create directory.

    Args:
        dirname (str): Directory name.

    Note:
        Sends a warning if directory exists.
    """    
    try:
        os.makedirs(dirname) 
    except OSError as e: # Python 2: except OSError, e
        if e.errno != errno.EEXIST:
            logging.warning(repr(e))
    return os.path.isdir(dirname)

def run():
    """Run wxPython App.

    Local variables:
        appname (str): App name.
        exe_path_abs (str): Absolute path of executable directory.
        configdir (str): Directory for the user-dependent application data files (see `GetUserDataDir`_).

    .. _GetUserDataDir:
        https://docs.wxpython.org/wx.StandardPaths.html?highlight=getuserdatadir#wx.StandardPaths.GetUserDataDir
    """    
    # Logging configuration 
    #   The root logging level is WARNING by default.
    #   See config.CONFIG_VERBOSE_OPTION to modify level through GUI. 
    logging.basicConfig(format="%(asctime)s %(levelname)s: %(message)s", datefmt='%m/%d/%Y %H:%M:%S')
    # App path
    exe_path      = sys.argv[0]
    exe_path_real = os.path.realpath(exe_path)
    exe_path_dir  = os.path.dirname( exe_path_real)
    exe_path_name = os.path.basename(exe_path_real)
    exe_path_abs  = os.path.abspath( exe_path_dir)
    # App name
    point = exe_path_name.find('.')
    if point > 0:
        appname = exe_path_name[:point]
    else:
        appname = exe_path_name
    # App object
    app = wx.App(False) 
    app.SetAppName(appname)
    # App configuration directory
    configdir = wx.StandardPaths.Get().GetUserDataDir()
    result    = create_dir(configdir)
    if not result:
        configdir = None
    #Show App
    frame = Window.Window(appname, exe_path_abs, configdir)
    frame.Centre()
    frame.Show()
    app.MainLoop()

# Commented to avoid run launcher.py
#if __name__ == '__main__':
#    run()

