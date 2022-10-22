#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Window class."""
import os
import shutil
import config
import logging
import wx
import Menus
import dialogs
import PanelWidgets
import PanelVisual
import PanelExec
import Log
import Menu    # for materialsDB name
import SubMenu # for materialsDB name
import Leaf    # for materialsDB name
import Applications2
import Process
import FileManager2
import Parametrize
import WindowTabular
import ThreadRemote
import WindowRemote
import WindowCustom
import WindowHtml
import WindowStatus
import ThreadRemoteQsub
import subprocess

class Window(wx.Frame):
    """Window class."""
    # Declaration of Windows IDs
    ID_OPEN    =  3*config.ID_SPACES+config.SPACE_MENU
    ID_EXIT    =  4*config.ID_SPACES+config.SPACE_MENU
    ID_HELP    =  5*config.ID_SPACES+config.SPACE_MENU
    ID_DUMP    =  6*config.ID_SPACES+config.SPACE_MENU
    ID_SAVE    =  7*config.ID_SPACES+config.SPACE_MENU
    ID_ABOUT   =  8*config.ID_SPACES+config.SPACE_MENU
    ID_HELP1   =  9*config.ID_SPACES+config.SPACE_MENU
    ID_HELP2   = 10*config.ID_SPACES+config.SPACE_MENU
    ID_HELP3   = 11*config.ID_SPACES+config.SPACE_MENU
    ID_HELP4   = 12*config.ID_SPACES+config.SPACE_MENU
    ID_HELP5   = 13*config.ID_SPACES+config.SPACE_MENU
    ID_NULL    = 14*config.ID_SPACES+config.SPACE_MENU
    ID_VADD    = 21*config.ID_SPACES+config.SPACE_MENU
    ID_MADD    = 22*config.ID_SPACES+config.SPACE_MENU
    ID_TDEL    = 23*config.ID_SPACES+config.SPACE_MENU
    ID_MESHES  = 24*config.ID_SPACES+config.SPACE_MENU
    ID_MACTION = 25*config.ID_SPACES+config.SPACE_MENU
    MIN_PROTECTED_ID = 26

    def __init__(self, appname, path, configdir):
        self.title = appname
        wx.Frame.__init__(self, parent=None, id=wx.ID_ANY, title=self.title)
        self.apps_index         = 0 #: (int) Application menu index.
        self.menuapp            = None
        self.menusampledata     = None
        self.application        = None
        self.sample_data        = None
        self.has_folder         = False
        self.has_app            = False
        self.only_one_app       = False
        self.only_one_example   = False
        self.path_local         = None
        self.path_exe           = path      # Python 2: str(path,sys.getfilesystemencoding())
        self.MaterialsDB_exists = False     #: (bool) Flag to check existence of Materials DB.
        self.ConfigFile_exists  = False     #: (bool) Flag to check existence of config.
        self.apply_config_flag  = True      #: (bool) Flag to apply config changes (only in the configuration menu).
        self.reload_menu        = False     #: (bool) Flag to reaload menu after a solver execution.
        self.configdir          = configdir #: (str) Path to user-dependent application data files.
        if self.configdir is None:
            self.errormsg("Error: Configuration directory is None.")
        self.appsdir            = self.calculate_appsdir()
        self.menus              = Menus.Menus(self)
        self.apps2              = None
        self.materials          = None
        self.config             = None
        self.tabular            = None      #: (bool) Flag for the tabular data window.
        self.threads            = []
        self.process            = Process.Process(self)
        self.timer              = None
        self.menulast           = []        #: (list) Special menus [Materials Database]
        self.path_local_save_filename = None
        self.path_local_dir_materials = None
        self.path_local_materials     = None
        self.path_local_remotedata    = None
        self.init_config()                  # Load initial configuration
        if self.configdir is not None:
            self.path_local_save_filename = os.path.join(self.configdir, config.FILE_LASTFOLDER)
            self.path_local_materials_dir = os.path.join(self.configdir, config.DIR_MATERIALS)
            self.path_local_materials     = os.path.join(self.configdir, config.DIR_MATERIALS, config.FILE_MATERIALS)
            self.path_local_config_dir    = os.path.join(self.configdir, config.DIR_CONFIG)
            self.path_local_config        = os.path.join(self.configdir, config.DIR_CONFIG,    config.FILE_CONFIG)
            self.path_local_remotedata    = os.path.join(self.configdir, config.FILE_REMOTEDATA)
            self.copy_materials()
            self.load_materials()
            self.copy_config()
            self.load_config()
            self.apply_config()
            if self.maximize_window:
                self.Maximize()
        self.path_local_saved = self.path_local_read()
        self.taskssave                    = [] #: (list) Tasks saved.
        self.taskscurrent                 = -1 #: (int) -1: ready to run, -2: disabled, 0: running first task, ...
        # Images
        if os.path.isfile(os.path.join(self.path_exe,os.pardir,config.DIR_IMAGES,'icon.'+self.title+'.xpm')):
            ico = wx.Icon(os.path.join(self.path_exe,os.pardir,config.DIR_IMAGES,'icon.'+self.title+'.xpm'), wx.BITMAP_TYPE_XPM)
        else:
            ico = wx.Icon(os.path.join(self.path_exe,os.pardir,config.DIR_IMAGES,'icon.opennum.xpm'), wx.BITMAP_TYPE_XPM)
        self.SetIcon(ico)
        # Window parts 
        self.splitterB = wx.SplitterWindow(self,           -1)
        self.splitterA = wx.SplitterWindow(self.splitterB, -1)
        self.panelA    = PanelWidgets.PanelWidgets(self.splitterA, self)
        self.panelB    = PanelVisual.PanelVisual(  self.splitterA, self, self.path_exe)
        self.panelC    = PanelExec.PanelExec(      self.splitterB)
        self.splitterB.SetMinimumPaneSize(80)
        self.splitterB.SetSashGravity(1.0)
        self.splitterB.SplitHorizontally(self.splitterA, self.panelC, self.window_height)
        self.splitterA.SetMinimumPaneSize(100)
        self.splitterA.SplitVertically(self.panelA, self.panelB, self.splitterA_size)
        self.splitterA.Layout()
        self.splitterB.Layout()
        self.Layout()
        self.CreateStatusBar()
        self.menubar = wx.MenuBar()
        self.SetMenuBar(self.menubar)
        self.menu_postload()
        # Logger
        self.logger = Log.Log()
        self.logger.add_logger(self.panelC)
        #File manager
        self.filemanager = FileManager2.FileManager2()
        self.filemanager.set_callback(self.logger.add_text)
        # Box
        box = wx.BoxSizer(wx.HORIZONTAL)
        box.Add(self.splitterB, 1, wx.EXPAND)
        self.SetSizer(box)
        self.SetMinSize((400,300))
        self.SetSize((self.window_width,self.window_height))
        # Timer
        self.timer = wx.Timer(self)
        #Bind
        self.Bind(wx.EVT_MENU,        self.event_menu)
        self.Bind(wx.EVT_CLOSE,       self.event_close)
        self.Bind(wx.EVT_END_PROCESS, self.event_end_process)
        self.Bind(wx.EVT_TIMER,       self.event_timer)
        # Load last folder
        self.load_lastfolder()
        # Remote data
        self.remotedata = {}
        self.copy_remotedata() # Copy if remote data do not exist.
        self.load_remotedata()
        self.qsubdata = {}

    def get_appname(self):
        """ Return Windows title."""
        return self.title

    def errormsg(self, txt):
        """Send an error message to logging and show it in a dialog box."""
        logging.error(txt)
        dialogs.show_error(self, txt)

    def askmsg(self, txt, mode):
        """Ask message."""
        if mode == 'yesno':
            res = dialogs.ask_yes_no(self, txt)
        elif mode == 'okcancel':
            res = dialogs.ask_ok_cancel(self, txt)
        else:
            res = False
        logging.warning(f"askmsg:{txt}: {str(res)}")
        return res

    def calculate_appsdir(self, apps_dir=None):
        """Sets the apps directory.
        
        Args:
            apps_dir (str): Apps directory. By default, config.DIR_APPS (apps/).
           
        Returns:
            str: If exists, path to apps_dir in the user-dependent application data directory.
            Otherwise, path to apps_dir in the installation directory.
        """    
        if apps_dir is None:
            apps_dir = config.DIR_APPS
        dirappshome = os.path.join(self.configdir, apps_dir)
        if os.path.isdir(dirappshome):
            return dirappshome
        else:
            return os.path.join(self.path_exe, os.pardir, apps_dir)

    def interface_separa(self):
        """Interface separator."""
        pos = self.menubar.GetMenuCount()
        menu_sep = wx.Menu()
        self.menubar.Append(menu_sep, ' | ')
        self.menubar.EnableTop(pos, False)

    def interface_all(self):
        """All interfaces."""
        self.interface_clear()
        self.interface_pre1()
        self.interface_pre2()
        self.interface_in()
        self.interface_post1()

    def interface_clear(self):
        """Clear interface."""
        while self.menubar.GetMenuCount()>0:
            self.menubar.Remove(0)

    def interface_pre1(self):
        """Infercafe pre, version 1."""
        self.menu_folder = wx.Menu()
        self.item_o = wx.MenuItem(self.menu_folder, self.ID_OPEN, self.folder_name+'\tCtrl+F', 'Select a working folder')
        self.menu_folder.Append(self.item_o) # Old wx: self.menu_folder.AppendItem(self.item_o)
        self.menuapp = wx.MenuItem(self.menu_folder, self.ID_NULL, self.applications_name)
        self.menu_folder.Append(self.menuapp) #Old wx: self.menu_folder.AppendItem(self.menuapp)
        self.menuapp.Enable(False)
        self.menusampledata = wx.MenuItem(self.menu_folder, self.ID_NULL, self.sample_data_name)
        self.menu_folder.Append(self.menusampledata) #Old wx: self.menu_folder.AppendItem(self.menusampledata)
        self.menusampledata.Enable(False)
        self.item_q = wx.MenuItem(self.menu_folder, self.ID_EXIT, self.quit_name+'\tCtrl+Q', 'Exit program')
        self.menu_folder.Append(self.item_q) # Old wx: self.menu_folder.AppendItem(self.item_q)
        self.menubar.Append(self.menu_folder, self.project_name)

    def load_menuapp(self):
        """Application menu."""
        if self.menuapp is not None and not (self.only_one_app and self.only_one_example):
            self.menu_folder.Delete(self.menusampledata) # Old wx: DeleteItem
            self.menu_folder.Delete(self.item_q) # Old wx: DeleteItem
            if isinstance(self.menuapp, wx.MenuItem):
                self.menu_folder.Delete(self.menuapp) # Old wx: DeleteItem
                appsdir = self.calculate_appsdir()
                apps = Applications2.Applications2()
                ok = apps.read(appsdir)
                if not ok:
                    self.errormsg('Error reading application list')
                if len(apps.get()) == 1: # If there is only one application, load the 1st sample data
                    self.menuapp = wx.MenuItem(self.menu_folder, self.ID_NULL, self.applications_name)
                    self.menu_folder.Append(self.menuapp) # Old wx: AppendItem
                    self.menuapp.Enable(False)
                else:
                    self.apps_index = 0
                    self.application = apps.build_application()
                    self.apps_index = self.application.reindex(self.apps_index)
                    self.menuapp = self.build_menu(self.application)
                    self.menu_folder.AppendSubMenu(self.menuapp, self.applications_name)
            self.menusampledata = wx.MenuItem(self.menu_folder, self.ID_NULL, self.sample_data_name)
            self.menu_folder.Append(self.menusampledata) # Old wx: AppendItem
            self.menusampledata.Enable(False)
            if not (self.has_app or os.path.exists(config.FILE_MENULOCAL)):
                self.menu_folder.AppendSeparator()
            self.item_q = wx.MenuItem(self.menu_folder, self.ID_EXIT, self.quit_name+'\tCtrl+Q', 'Exit program')
            self.menu_folder.Append(self.item_q) # Old wx: AppendItem
            #self.menus.reindex(self.apps_index) #required to reindex materials DB when lastfolder does not exist or in a new working folder

    def load_menusampledata(self):
        """Menu Sample data."""
        self.menu_folder.Delete(self.menusampledata) # Old wx: DeleteItem
        self.menu_folder.Delete(self.item_q) # Old wx: DeleteItem
        appsdir = self.calculate_appsdir()
        apps = Applications2.Applications2()
        ok = apps.read(appsdir)
        if not ok:
            self.errormsg('Error reading application list')
        self.sample_data = apps.build_sampledata(self.menus.get_name())
        if len(self.sample_data.get_children()) == 1:
            self.menusampledata = wx.MenuItem(self.menu_folder, self.ID_NULL, self.sample_data_name)
            self.menu_folder.Append(self.menusampledata) # Old wx: AppendItem
            self.menusampledata.Enable(False)
        else:
            self.apps_index = self.sample_data.reindex(self.apps_index)
            self.menusampledata = self.build_menu(self.sample_data)
            if apps.has_app(self.menus.get_name()):
                self.menu_folder.AppendSubMenu(self.menusampledata, self.sample_data_name + ' ('+self.menus.get_name()+')')
            else:
                self.menu_folder.AppendSubMenu(self.menusampledata, self.sample_data_name)
        self.menu_folder.AppendSeparator()
        self.item_q = wx.MenuItem(self.menu_folder, self.ID_EXIT, self.quit_name+'\tCtrl+Q', 'Exit program')
        self.menu_folder.Append(self.item_q) # Old wx: AppendItem
        #self.menus.reindex(self.apps_index) #required to reindex materials DB when lastfolder does not exist or in a new working folder

    def load_menuconfig(self):
        """Menu config."""
        self.menu_folder.Delete(self.item_q) # Old wx: DeleteItem
        self.apps_index = self.config.reindex(self.apps_index)
        if self.config is not None:
            if len(self.config.get_children()) > 0:
                configmenus = self.config.get_children()
                for ch in configmenus:
                    if ch.get_name() == config.CONFIG_MENU_NAME:
                        menuconfig = self.build_menu(ch)
                        self.menu_folder.AppendSubMenu(menuconfig, ch.get_name())
                        self.item_q = wx.MenuItem(self.menu_folder, self.ID_EXIT, self.quit_name+'\tCtrl+Q', 'Exit program')
                        self.menu_folder.Append(self.item_q) # Old wx: AppendItem
         #self.menus.reindex(self.apps_index) #required to reindex materials DB when lastfolder does not exist or in a new working folder

    def interface_pre2(self):
        """Interface pre, version 2."""
        if self.has_folder:
            self.load_menuapp()
            if self.has_app or os.path.exists(config.FILE_MENULOCAL):
                self.load_menusampledata()
            if self.config is not None:
                self.load_menuconfig()
        self.menus.reindex(self.apps_index)

    def build_menu(self,submenus,help=False):
        """ Build menu Application, Sample data and Help, new version.
        
        Note:
             Flag help is added to create menu Help.
        """
        menus = submenus.get_children()
        if True:
            menu_temp = wx.Menu()
            for submenu in submenus.get_children():
                if submenu.is_hidden():
                    continue
                (selected,kind) = False,wx.ITEM_NORMAL
                attribs = submenu.get_attribs()
                if (attribs.get('selected') == 'true'):
                    (selected,kind) = True,wx.ITEM_CHECK
                if (attribs.get('separator') == 'true'):
                    menu_temp.AppendSeparator()
                    continue
                if (attribs.get('title') == None): # Allow submenus to show tag title as text
                    item_name = attribs.get('name')
                else:
                    item_name = attribs.get('title')
                item_sample_temp = wx.Menu()
                if help: #Work with other IDs in Help
                    submenuid = config.SPACE_MENU
                else:
                    submenuid = config.SPACE_MENU_DYN
                item_temp = wx.MenuItem(menu_temp, submenu.get_index() * config.ID_SPACES \
                        + submenuid , item_name, 'select ' + submenu.get_name(), kind)
                subsubmenuAdded = False
                for subsubmenu in submenu.get_children():
                    if subsubmenu.is_hidden():
                        continue
                    if not (isinstance(subsubmenu, SubMenu.SubMenu)):
                        continue
                    (selected,kind) = False,wx.ITEM_NORMAL
                    subattribs = subsubmenu.get_attribs()
                    if (subattribs.get('selected')  == 'true'):
                        (selected,kind) = True,wx.ITEM_CHECK
                    if (subattribs.get('separator') == 'true'):
                        item_sample_temp.AppendSeparator()
                        continue
                    if (subattribs.get('title') == None): # Allow submenus to show tag title as text
                        subitem_name = subattribs.get('name')
                    else:
                        subitem_name = subattribs.get('title')
                    if help: #Work with other IDs in Help
                        subsubmenuid = config.SPACE_MENU
                    else:
                        subsubmenuid = config.SPACE_MENU_DYN
                    subitem_temp = wx.MenuItem(item_sample_temp, subsubmenu.get_index() * config.ID_SPACES \
                        + subsubmenuid, subitem_name, 'select ' + subsubmenu.get_name(), kind)
                    item_sample_temp.Append(subitem_temp) #old wx: AppendItem
                    subsubmenuAdded = True
                if subsubmenuAdded:
                    menu_temp.AppendSubMenu(item_sample_temp, item_name)
                else:
                    menu_temp.Append(item_temp) #Old wx: AppendItem
        return menu_temp

    def interface_in(self):
        """Interface in."""
        self.menulast = [] 
        menus = self.menus.get_children()
        for menu in menus:
            if menu.is_hidden():
                continue
            menuattribs = menu.get_attribs()
            if (menuattribs.get('separator') == 'true'):
                self.interface_separa()
                continue
            menu_temp = wx.Menu()
            for submenu in menu.get_children():
                if submenu.is_hidden():
                    continue
                (selected,kind) = False,wx.ITEM_NORMAL
                attribs = submenu.get_attribs()
                if (attribs.get('separator') == 'true'):
                    menu_temp.AppendSeparator()
                    continue
                if (attribs.get('selected')  == 'true'):
                    (selected,kind) = True,wx.ITEM_CHECK
                if (attribs.get('title') is None): # Allow submenus to show tag title as text
                    item_name = attribs.get('name')
                else:
                    item_name = attribs.get('title')
                item_temp = wx.MenuItem(menu_temp, submenu.get_index() * config.ID_SPACES + \
                        config.SPACE_MENU_DYN, item_name, 'select ' + submenu.get_name(), kind)
                subitem_temp = wx.Menu()
                subitemAdded = False
                for subsubmenu in submenu.get_children():
                    if subsubmenu.is_hidden():
                        continue
                    if not (isinstance(subsubmenu, SubMenu.SubMenu)):
                        continue
                    (selected,kind) = False,wx.ITEM_NORMAL
                    subattribs = subsubmenu.get_attribs()
                    if (subattribs.get('separator') == 'true'):
                        subitem_temp.AppendSeparator()
                        continue
                    if (subattribs.get('selected') == 'true'):
                        (selected,kind) = True,wx.ITEM_CHECK
                    if (subattribs.get('title') == None): # Allow submenus to show tag title as text
                        subitem_name = subattribs.get('name')
                    else:
                        subitem_name = subattribs.get('title')
                    subitem_temp.Append(subsubmenu.get_index()*config.ID_SPACES + config.SPACE_MENU_DYN, subitem_name)
                    subitemAdded = True
                if subitemAdded:
                    item_temp.SetSubMenu(subitem_temp)
                menu_temp.Append(item_temp) # Old wx: menu_temp.AppendItem(item_temp)
                if (selected):
                    item_temp.Check()
            if ((menu.get_name() == self.materialsdb_name) or (menu.get_name() == self.materialsdb_name[1:])): 
                self.menulast.append( (menu_temp, '&'+menu.get_name()) )
                self.MaterialsDB_exists = True
            elif menu.get_name() == config.NAME_CONFIG_FILE:
                self.menulast.append( (menu_temp, '&'+menu.get_name()) )
                self.Configfile_exists = True
            else:
                self.menubar.Append(menu_temp, '&'+menu.get_name())

    def load_menuhelp(self):
        """Build menu Help with the same style than Apps y Sample data."""
        helpdir = self.calculate_appsdir(config.DIR_DOCS)
        apps = Applications2.Applications2()
        ok = apps.read(helpdir)
        if not ok:
            self.errormsg('Error reading application list')
        self.menu_help = apps.build_help()
        if len(self.menu_help.get_children()) == 0:
            menuhelpwx = None
        else:
            if self.apps_index < self.MIN_PROTECTED_ID:
                index = self.MIN_PROTECTED_ID
            else:
                index = self.apps_index
            self.menu_help.reindex(index)
            menuhelpwx = self.build_menu(self.menu_help,help=True)
        return menuhelpwx

    def interface_post1(self):
        """Interface post, version 1."""
        menuhelpwx = self.load_menuhelp()
        if menuhelpwx is not None:
            self.menubar.Append(menuhelpwx, self.help_name)
        if len(self.menulast) > 0: # TODO: Temporary pseudo-separator
            self.interface_separa()
        for menu in self.menulast: # TODO: Temporary
            self.menubar.Append(menu[0], menu[1]) # 0:menu, 1:name
        self.menulast = []

    def process_pending(self):
        """Pending process."""
        wx.SafeYield()

    def retitle(self):
        """Retitle."""
        txt = self.title
        if self.menus.is_loaded():
            txt += ' - ' + self.menus.get_name()
        if isinstance(self.path_local, str):
            txt += ' - ' + self.path_local
        self.SetTitle(txt)

    def path_local_set(self, newpath):
        """Set local path."""
        self.filemanager.clear() # Clear buffers here and in menu_load
        self.panelB.forget()     # Forget plot association
        try:
            if newpath is None:
                os.chdir(self.path_exe)
            else:
                os.chdir(newpath)
        except OSError:
            self.errormsg('Error changing current working directory')
            return False
        self.path_local = newpath
        logging.debug(f"path_local {str(self.path_local)}")
        self.retitle()
        self.path_local_write(self.path_local) # Store for the next execution
        return True

    def path_local_get(self):
        """Get local path."""
        if (isinstance(self.path_local, str)):
            return self.path_local
        else:
            return ""

    def path_local_read(self):
        """Return path to last folder.""" 
        path = None
        if self.path_local_save_filename is not None:
            try:
                file1 = open(self.path_local_save_filename, encoding='utf-8')
                path = file1.read()
                file1.close()
            except IOError as e:
                logging.debug(f"Unable to read path in {self.path_local_save_filename}: {repr(e)}.")
                pass
            logging.debug(f"Read path {path} in {self.path_local_save_filename}.")
        return path

    def path_local_write(self, path):
        """Write local path."""
        if self.path_local_save_filename is not None:
            logging.debug(f"PATH_WRITE {path}")
            try:
                file1 = open(self.path_local_save_filename,"w")
                #print('s', type(path), 'd', path)
                #print(type(path.encode('utf-8')), path.encode('utf-8'))
                file1.write(path)
                file1.close()
            except IOError as e:
                logging.warning(f"PATH_WRITE_FAILED {repr(e)}")
                pass

    def reset_config(self):
        """Reset configuration file."""
        if dialogs.ask_ok_cancel(self, "If you continue, your configuration file copy will be reset. All changes you made to it will be lost."):
            self.copy_config(True)
            self.load_config()
            self.menu_postload(False) # false => does not test mesh existence

    def copy_config(self, force=False):
        """Copy configuration when path_local_config is not None but it does not exist or when forced.

        Args:
            force (bool): Force to copy configuration (by default, False).
        """
        try:
            if self.path_local_config is not None:
                if force or not os.path.exists(self.path_local_config):
                    logging.debug(f"Copying configuration file to {self.path_local_config}...")
                    if not os.path.exists(self.path_local_config_dir):
                        try:
                            os.mkdir(self.path_local_config_dir)
                        except OSError as e:
                            logging.warning(f"Error creating configuration directory: {repr(e)}.")
                    from_file1 = os.path.join(self.path_exe, os.pardir, config.DIR_CONFIG, config.FILE_CONFIG) 
                    from_file2 = os.path.join(self.path_exe, os.pardir, config.DIR_CONFIG, config.FILE_CONFIG_PREFIX + self.title + config.FILE_CONFIG_SUFFIX) 
                    if os.path.exists(from_file1):
                        from_file = from_file1     # config.xml
                    else:
                        if os.path.exists(from_file2):
                            from_file = from_file2 # config.appname.xml
                        else:
                            logging.warning(f"Unable to copy configuration to local folder: path to {from_file1} and {from_file2} does not exist.")
                            return
                    shutil.copy2( from_file, self.path_local_config )
        except (IOError, shutil.Error, OSError) as err:
            self.errormsg(f"Unable to copy configuration to local folder: {repr(err)}")

    def load_config(self):
        """Load configuration file in Window.
        
        Returns:
            bool: True if configuration file was loaded. 
        """
        ok = True
        if self.path_local_config is not None:
            menuconfig = Menus.Menus(self)
            ok &= menuconfig.load_file(self.path_local_config)
            if ok:
                self.config = menuconfig
        else:
            ok &= False
        return ok

    def save_config(self):
        if self.config is not None and self.path_local_config is not None:
            # True: force: para que o garde ainda que un atributo indique o contrario
            self.config.save_menu(self.path_local_config, True)
            self.apply_config()
            # BUG: working dir not set. consider other conditions
#            if self.path_local is not None:
#                self.config.save_data(config.FILE_CONFIG_DAT, force=True)

    def init_config(self):
        """Load initial configuration.

        TODO:
            Check whether materialsdb_name should be defined as "Materials".
        """
        self.languaje          = "English"
        self.project_name      = "&Project"
        self.folder_name       = "Select folder"
        self.applications_name = "Applications"
        self.sample_data_name  = "Sample data"
        self.quit_name         = "&Quit"
        self.help_name         = "&Help"
        self.materialsdb_name  = "&Help"
        self.maximize_window   = False
        self.warning_on_load   = True
        self.splitterA_size    = 240
        self.window_width      = 900
        self.window_height     = 600

    def apply_config(self):
        """Apply configuration file."""
        if self.config is None or not self.apply_config_flag:
            return None
        for menuch in self.config.get_children():
            if menuch.get_name() == config.CONFIG_MENU_NAME:
                # Search GUI options
                for submenuch in menuch.get_children():
                    if submenuch.get_name() == config.CONFIG_SUBMENU_NAME:
                        for structch in submenuch.get_children():
                            if structch.get_name() == config.CONFIG_STRUCT_NAME:
                                for ch in structch.get_children():
                                    try:
                                        if ch.get_name() == config.CONFIG_MAXIMIZE_OPTION:
                                            # Maximize at start (yes/no)
                                            prop = ch.get_elements_selected()[0]
                                            self.maximize_window = (prop.lower() == config.VALUE_YES)
                                        elif ch.get_name() == config.CONFIG_WARNONLOAD_OPTION:
                                            # Enable warning dialog when load app or example (yes/no)
                                            prop = ch.get_elements_selected()[0]
                                            self.warning_on_load = (prop.lower() == config.VALUE_NO)
                                        elif ch.get_name() == config.CONFIG_VERBOSE_OPTION:
                                            # Run GUI in verbose mode: yes/no
                                            prop = ch.get_elements_selected()[0]
                                            if prop.lower() == config.VALUE_YES:
                                                # All messages will be printed
                                                logging.getLogger().setLevel(logging.DEBUG)
                                            else:
                                                # Only logging.warning or superior will be printed
                                                logging.getLogger().setLevel(logging.WARNING)
                                        elif ch.get_name() == config.CONFIG_WIDGETS_PANEL_SIZE:
                                            # Set PanelWidgets size
                                            self.splitterA_size = int(ch.get_elements()[0])
                                            self.splitterA.SetSashPosition(self.splitterA_size, redraw=True) # Change window sash
                                        elif ch.get_name() == config.CONFIG_WINDOW_SIZE:
                                            # Set Window size
                                            for wsch in ch.get_children():
                                                if wsch.get_name() == config.CONFIG_WINDOW_WIDTH:
                                                    self.window_width = int(wsch.get_elements()[0])
                                                if wsch.get_name() == config.CONFIG_WINDOW_HEIGHT:
                                                    self.window_height = int(wsch.get_elements()[0])
                                            self.SetSize((self.window_width,self.window_height)) # Change window
                                    except:
                                        pass
            elif menuch.get_name() == config.CONFIG_INTERNACIONALIZATION:
                # Search internationalization options
                for submenuch in menuch.get_children():
                    if submenuch.get_name() == self.languaje:
                        for structch in submenuch.get_children():
                            if structch.get_name() == 'Project':
                                for ch in structch.get_children():
                                    elementlist = ch.get_elements()
                                    if ch.get_name() == 'Project':
                                        if len(elementlist) > 0:
                                            self.project_name =      ch.get_elements()[0]
                                    elif ch.get_name() == 'Folder':
                                        if len(elementlist) > 0:
                                            self.folder_name =       ch.get_elements()[0]
                                    elif ch.get_name() == 'Applications':
                                        if len(elementlist) > 0:
                                            self.applications_name = ch.get_elements()[0]
                                    elif ch.get_name() == 'Sample data':
                                        if len(elementlist) > 0:
                                            self.sample_data_name =  ch.get_elements()[0]
                                    elif ch.get_name() == 'Quit':
                                        if len(elementlist) > 0:
                                            self.quit_name =         ch.get_elements()[0]
                            if structch.get_name() == 'Help':
                                for ch in structch.get_children():
                                    if ch.get_name() == 'Help':
                                        self.help_name =             ch.get_elements()[0]
                            if structch.get_name() == 'Materials':
                                for ch in structch.get_children():
                                    if ch.get_name() == 'Database':
                                        self.materialsdb_name =      ch.get_elements()[0]

    def copy_materials(self, force=False):
        """Copy materials database when path_local_materials is None or when it is forced.

        Args:
            force (bool): Force to copy materials (by default, False).
        """
        try:
            if self.path_local_materials is not None:
                if force or not os.path.exists(self.path_local_materials):
                    logging.debug(f"Copying materials to {self.path_local_materials}...")
                    if not os.path.exists(self.path_local_materials_dir):
                        try:
                            os.mkdir(     self.path_local_materials_dir)
                        except OSError as e:
                            logging.warning(f"Unable to create materials directory: {e}")
                    from_file1 = os.path.join(self.path_exe, os.pardir, config.DIR_MATERIALS, config.FILE_MATERIALS)
                    from_file2 = os.path.join(self.path_exe, os.pardir, config.DIR_MATERIALS, config.FILE_MATERIALS_PREFIX + self.title + config.FILE_MATERIALS_SUFFIX)
                    if os.path.exists(from_file1):
                        from_file = from_file1     # materials.xml
                    else:
                        if os.path.exists(from_file2):
                            from_file = from_file2 # materials.appname.xml
                        else:
                            logging.warning(f"Unable to copy materials database to local folder: path to {from_file1} and {from_file2} does not exist.")
                            return
                    shutil.copy2(from_file, self.path_local_materials)
        except (IOError, shutil.Error, OSError) as err:
            self.errormsg(f"Unable to copy materials to local folder: {repr(err)}")

    def load_materials(self):
        """Load materials database in Window.
        
        Returns:
            bool: True if materials database was loaded. 
        """
        ok = True
        if self.path_local_materials is not None:
            menumaterials = Menus.Menus(self)
            ok &= menumaterials.load_file(self.path_local_materials)
            if ok:
                self.materials = menumaterials
        else:
            ok &= False
        return ok

    def save_materials(self):
        if self.materials is not None and self.path_local_materials is not None:
            # True: force: para que o garde ainda que un atributo indique o contrario
            self.materials.save_menu(self.path_local_materials, True)

            # BUG: working dir not set. consider other conditions
            if self.path_local is not None:
                self.materials.save_data(config.FILE_MATERIALS_DAT, force=True)

    def reset_materials(self):
        """Reset material database user copy."""
        if dialogs.ask_ok_cancel(self, "If you continue, your materials database copy will be reset. All changes you made to it will be lost."):
            self.copy_materials(True)
            self.load_materials()
            self.menu_postload(False) # Argument False: do not test mesh existence

    def get_extra(self, prefix=''):
        """Create a leaf so solver can knows where the materials are."""
        l = Leaf.Leaf()
        l.set_name('materialsDB')
        l.tag = 'leaf'
        l.get_attribs()['type'] = 'file'
        l.set_elements_nosel([prefix+config.FILE_MATERIALS_DAT])
        s = SubMenu.SubMenu()
        s.tag = 'submenu'
        s.set_name('Open')
        s.add_child(l)
        m = Menu.Menu()
        m.tag = 'menu'
        m.set_name(config.NAME_MATERIALS_FILE)
        m.add_child(s)
        return m

    def save_all(self):
        """Save all."""
        extra = [self.get_extra()]
        self.menus.save_menu()
        self.menus.save_data(extras=extra)
        self.save_materials()
        self.save_config()

    def menu_load(self, changed):
        """Load the current working directory."""
        self.filemanager.clear() # Clear buffers here and in menu_load
        self.panelB.forget()     # Forget plot association
        file_to = config.FILE_MENULOCAL
        logging.debug(f"load {str(file_to)}")
        ok = True
        newmenu = Menus.Menus(self)
        result = newmenu.load_file(file_to)
        if not result:
            ok = False
        else:
            if changed is True:
                self.logger.end()
                self.logger.change()
                name = newmenu.get_name()
                self.logger.start(name)
            else:
                self.logger.add_text('Menu updated!\n')
                name = None
            self.menus = newmenu
            self.menu_postload()
        return ok

    def menu_postload(self, hard=False):
        """Take actions after menu loading.
        
        Args:
             hard: (bool) If True, test the existence of files and folders (by default, False).
        """
        if self.materials is not None:
            # Add materials tree
            for ch in self.materials.get_children():
                ch.get_attribs()[config.AT_SAVETHIS] = config.VALUE_FALSE # Set materials["savethis"] = "false" to avoid save it when menu changes. Instead, use materials.save_menu(...,force=True).
                ch.get_data()[config.IS_MATERIALS] = True    # Allow folder/file leafs to save absolute paths.
                self.menus.del_childs_by_name(ch.get_name()) # Avoid that materials database reset increment the number of menu items.
                self.menus.add_child(ch)                     # Reset ch parent.
        self.menus.reindex(self.apps_index)
        self.interface_all()
        self.retitle()
        logging.debug(f"Data file: {self.menus.get_datafile()}.")
        logging.debug(f"To save:   {self.menus.to_save()}.")
        if hard:
            # Test the existence of files and folders.
            errors = self.menus.pretest()
            if len(errors)>0:
                self.errormsg('\n'.join(errors))

    def menu_copy_load(self, dirs):
        """Load Menu copy. 
        
            Returns:
                True: copyed, False: not copyed, None: error.
            
            Note:
                It is called from panelWidgets.add_widget when copymenu.
        """
        logging.debug(f"Menu_copy_load: {dirs}.")
        ret = None
        if self.warning_on_load or dialogs.ask_ok_cancel(self, 'If you continue, some files in the current working folder may be overwritten.'):
            self.logger.end() #Close log to avoid error when attempt to delete an open file
            if self.menu_copy2(dirs):
                if self.menu_load(True):
                    ret = True
        else:
            ret = False
        return ret

    def remove_file_folder(self, name):
        """Remove File/folder."""
        ok = True
        try:
            if os.path.exists(name):
                if os.path.isdir(name):
                    shutil.rmtree(name)
                else:
                    os.unlink(name)
        except (IOError, shutil.Error, OSError) as err:
            ok = False
            logging.debug(repr(err))
            self.errormsg(f"Error erasing data ({name}) from working folder")
        return ok

    def menu_copy2(self, dirs):
        """Copy menu, version 2."""
        ok = True
        dir_from = os.path.join(self.appsdir, dirs)
        dir_to = '.'
        logging.debug(f"copy {dir_from} -> {dir_to}")
        try:
            files1 = os.listdir(dir_from)
            # Avoid .hidden files
            files2 = []
            dirs2  = []
            for file in files1:
                if len(file) > 0 and file[0] != '.':
                    if os.path.isdir(os.path.join(dir_from, file)):
                        dirs2.append(file)
                    else:
                        files2.append(file)
            logging.debug(f"copying files {files2}")
            logging.debug(f"copying dirs {dirs2}")
            for file in files2:
                file_from = os.path.join(dir_from, file)
                affected = os.path.join(dir_to, file)
                if not self.remove_file_folder(affected):
                    ok = False
                    return ok
                shutil.copy2(file_from, dir_to)
            for dir in dirs2:
                subdir_from = os.path.join(dir_from, dir)
                subdir_to = os.path.join(dir_to, dir)
                if os.path.isdir(subdir_to):
                    shutil.rmtree(subdir_to)
                affected = os.path.join(dir_to, dir)
                if not self.remove_file_folder(affected):
                    ok = False
                    return ok
                shutil.copytree( subdir_from , subdir_to )
        except (IOError, shutil.Error, OSError) as err:
            ok = False
            self.errormsg(f"Error copying files to working folder.\n{repr(err)}.")
        return ok

    def event_folder(self):
        """Event folder.

        Note:
            If a folder was not selected in this execution, select the one from the execution.
        """
        self.has_app = False
        if self.path_local is None:
            temp = self.path_local_saved if self.path_local_saved is not None else ''
        else:
            temp = self.path_local_get()
        path_new = dialogs.get_folder(self,temp)
        if path_new is not None:
            if self.path_local_set(path_new):
                self.has_folder = True
                self.logger.end()
                self.menus = Menus.Menus(self)
                if os.path.exists(config.FILE_MENULOCAL):
                    if not self.menu_load(True):
                        self.menu_postload()
                    self.load_remotedata() #reload remote data because it is working folder-dependant.
                    return True
                else: # in case of failure
                    appsdir = self.calculate_appsdir()
                    apps = Applications2.Applications2()
                    ok = apps.read(appsdir)
                    if not ok:
                        self.errormsg('Error reading application list')
                    if len(apps.get()) == 1: # when it is only one application, load it
                        self.only_one_app = True
                        if True:             # when it is only one application, load first sample data 
                            self.only_one_example = True
                            if self.menu_copy2(apps.get()[0][2]+'/'+apps.get()[0][3][0][2]):
                                if self.menu_load(True):
                                    self.load_remotedata() #reload remote data because it is working folder-dependant.
                                    return True
                self.menu_postload()
        return False

    def load_lastfolder(self):
        """Automatically load from last folder."""
        path_new = None
        path_file_lf = os.path.join(self.configdir, config.FILE_LASTFOLDER)
        if os.path.exists(path_file_lf) and self.configdir is not None:
            self.path_local_save_filename = path_file_lf
            file1 = open(self.path_local_save_filename, encoding='utf-8')
            path_new = file1.read()
            file1.close()
            logging.debug(path_new)
            if path_new is not None and os.path.exists(path_new):
                if self.path_local_set(path_new):
                    self.has_folder = True
                    # Load Applications menu
                    self.logger.end()
                    self.menus = Menus.Menus(self)
                    if os.path.exists(config.FILE_MENULOCAL):
                        if self.menu_load(True):
                            self.has_app = True
                            return True
                    # In case Application is not loaded
                    self.menu_postload()
        return False

    def event_close(self, event):
        """Event close."""
        logging.debug('closing [x]')
        self.panelA.display_set(None)
        self.logger.end()
        self.save_all()
        self.panelB.rem()
        event.Skip()

    def event_menu(self, event):
        """Event menu."""
        id = event.GetId()
        if (id % config.ID_SPACES == config.SPACE_MENU):
            if (id==self.ID_EXIT):
                self.Close()
                return # This avoids to save twice on menu exit
        self.panelA.display_set(None)
        self.save_all()
        self.apply_config_flag = False
        if (id % config.ID_SPACES == config.SPACE_MENU):
            index = id // config.ID_SPACES
            if (id==self.ID_EXIT):
                self.Close()
            elif (id==self.ID_OPEN):
                result = self.event_folder()
            elif (id==self.ID_TDEL):
                self.panelB.rem()
            elif (id==self.ID_DUMP):
                self.menus.dump()
            elif (id==self.ID_SAVE):
                self.menus.save_data() #Avoid saving without working folder
            else:
                try:
                    submenu = self.menu_help.get_index( index )
                except:
                    submenu = None
                # Event control, Apps and Sample data menus style
                if submenu is not None:
                    if submenu.get_attribs()[config.AT_HELP] == config.VALUE_TRUE:
                        try:
                            dirname = submenu.get_attribs()[config.AT_SOURCE]
                        except:
                            dirname = submenu.get_attribs()['name']
                        if not self.launch_auto(os.path.join(dirname, 'index.xhtml')):
                            if not self.launch_auto(os.path.join(dirname, 'index.html')):
                                if not self.launch_auto(os.path.join(dirname, 'index.htm')):
                                    self.errormsg('File '+dirname+'/'+'index.[xhtml | html | htm] not exists.')
        elif (id % config.ID_SPACES == config.SPACE_MENU_DYN):
            index = id // config.ID_SPACES
            submenu = self.menus.get_index( index )
            if submenu is None:
                if self.application is not None:
                    submenu = self.application.get_index( index )
                if submenu is None:
                    if self.sample_data is not None:
                        submenu = self.sample_data.get_index( index )
                if submenu is None:
                    if self.config is not None:
                        submenu = self.config.get_index( index )
                        self.apply_config_flag = True
            if (submenu is None):
                logging.debug(f"{str(index)} {str(id)}")
            else:
                logging.debug(f"{str(index)} {str(id)} {submenu.get_name()}")
                #Application load
                copyvalue = submenu.get_attribs().get(config.AT_COPY)
                if copyvalue is not None:
                    if self.menu_copy_load(copyvalue):
                        if not self.has_app:
                            self.has_app = True
                            self.interface_all()
                actions = submenu.get_actions()
                for action in actions:
                    name = action.get('name')
                    self.reload_menu = action.get('reload') == config.VALUE_TRUE
                    if name   == 'exec':
                        self.menu_exec2(action)
                    elif name == 'exec_custom':
                        self.menu_exec_custom(action)
                    elif name == 'exec_ssh':
                        self.menu_exec_ssh(action)
                    elif name == 'exec_qsub':
                        self.menu_exec_qsub(action)
                    elif name == 'status_qsub':
                        self.menu_status_qsub(action)
                    elif name == 'get_qsub':
                        self.menu_get_qsub(action)
                    elif name == 'kill_exec':
                        self.menu_kill()
                    elif name == 'close_plots':
                        self.panelB.rem()
                    elif name == 'reset_materials':
                        self.reset_materials()
                    elif name == 'reset_config':
                        self.reset_config()
                    else:
                        self.errormsg('Unknown action: ' + name)
                children1 = submenu.get_children()
                children2 = [] # there are not hidden children
                for c in children1:
                    if not c.is_hidden():
                        children2.append(c)
                num = len(children2)
                display = None
                if num == 1:
                    display = children2[0]
                    self.panelA.display_set(display)

    def launch_auto(self, path, allow_home=True):
        """Add launch control."""
        path1 = os.path.abspath(os.path.join(self.configdir,config.DIR_DOCS,path))
        path2 = os.path.abspath(os.path.join(self.path_exe,os.pardir,config.DIR_DOCS,path))
        if allow_home and os.path.exists(path1):
            self.launch('file:///'+path1)
            return True
        elif os.path.exists(path2):
            self.launch('file:///'+path2)
            return True
        return False

    def launch(self, url, opt=None):
        """Launch."""
        logging.debug(f"launching {str(url)}")
        wx.LaunchDefaultBrowser(url.replace(' ', '%20')) # Avoid constant wx.BROWSER_NEW_WINDOW fail

    def add_text(self, txt):
        """Add logger text."""
        self.logger.add_text(txt)

    def tabular_onclose(self):
        """Capture onclose event on tabular data window.""" 
        logging.debug('tabular_onclose')
        self.tabular = None

    def tabular_close(self):
        """Close tabular data window."""
        logging.debug('tabular_close')
        if self.tabular is not None:
            self.tabular.Close()
            self.tabular.Destroy() 
            self.tabular = None
    def tabular_show(self, struct, fromfile=False):
        """Show tabular data.
        
        Note:
            It shows a window with tabular data of the children of struct.
        """
        logging.debug(f"tabular_show {str(struct)}")
        if self.tabular is None:
            self.tabular = WindowTabular.WindowTabular(self, self.tabular_onclose)
        self.tabular.display(struct, fromfile)

    def gr2_data_from_struct(self,struct):
        """Send struct data to a .gr2 file."""
        child = struct.get_children()
        name = struct.get_name()
        struct.get_attribs()['data'] = 'file:'+os.path.join(self.configdir,'2d_graph.gr2')
        fn = struct.get_attribs().get('data')
        if fn is not None:
            i = fn.find(':')
            filename = fn[i+1:]
        else:
            struct.get_attribs()['data'] = os.path.join(self.configdir,'2d_graph.gr2')
            logging.debug(len(child))
        if len(child) >= 2:
            sigs = []
            xlegend = child[0].get_name()
            xvalues = child[0].get_elements()
            xaxis = {'legend':xlegend,'values':xvalues}
            for i in range(1,len(child)):
                ylegend = child[i].get_name()
                yvalues = child[i].get_elements()
                yaxis = {'legend':ylegend,'values':yvalues}
                sigs.append({'xaxis':xaxis,'yaxis':yaxis})
            signals = {'filename':filename,'title':name,'xlabel':xlegend,'ylabel':ylegend,'signals':sigs} # Temporary
            return signals
        else:
            self.errormsg('Two or more data arrays needed')
            return None

    def write_gr2_from_struct(self,struct):
        """Write file .gr2 using 2d_graph input format.
       
        TODO:
           Check that file is not written in ../code but in the local data folder (path_local_get()?)
           Check that list are written with the format [ , , , ].
        """
        signals = self.gr2_data_from_struct(struct)
        if signals is not None:
            sigs = signals.get('signals')
            try:
                filegr2 = open(signals.get('filename'),'w')
                filegr2.write(str(len(sigs))+'\n')
                for sig in sigs:
                    xvalues = sig.get('xaxis').get('values')
                    yvalues = sig.get('yaxis').get('values')
                    if len(xvalues)<= 0 or len(yvalues) <=0:
                        self.errormsg('Error showing plot: No scalar data to plot.')
                        return None
                    filegr2.write(str(len(xvalues)) + '\n')
                    for v in xvalues:
                        filegr2.write(str(v) + ' ')
                    filegr2.write('\n')
                    for v in yvalues:
                        filegr2.write(str(v) + ' ')
                    filegr2.write('\n')
                filegr2.write(signals.get('title') + '\n')
                filegr2.write('X (' +signals.get('xlabel') + ')' + '\n')
                filegr2.write('Y' + '\n')
                for sig in sigs:
                    filegr2.write(sig.get('yaxis').get('legend') + '\n')
                filegr2.close()
            except IOError:
                logging.warning(f"Problem writing data to temporary file: {str(filegr2)}")
                return None
        return signals.get('filename')

    def graph2d_from_struct(self,struct):
        """Get graph2d data from struct."""
        filename = self.write_gr2_from_struct(struct)
        if filename is not None:
            self.panelB.add_update(struct)
            try:
                os.remove(filename)
            except IOError as e:
                logging.debug(f"Error deleting temporary file {str(filename)}: {repr(e)}")

    def tabular_update(self, struct):
        """Update window with tabular data of the children of struct.
        
        Note:
            If the window has children of other struct, does nothing.
        """
        logging.debug(f"tabular_update {str(struct)}")
        if self.tabular is not None:
            self.tabular.update(struct)

    def htmlhelp_show(self, struct):
        """Show HTML help."""
        data = struct.get_attribs().get(config.AT_HELPWINDOWDATA)
        configuration = struct.get_attribs().get(config.AT_HELPWINDOWCONFIG)
        if data is None:
            self.errormsg('Error creating help window: there is no data to show.')
            return
        instpath = os.path.join(self.path_exe, os.pardir) # Installation directory
        ret = WindowHtml.add_pre(self, data, configuration, instpath)
        if ret is not None:
            self.errormsg('Error creating help window: ' + ret)
            return

    def copy_remotedata(self, force=False):
        """Copy when remote data does not exist."""
        try:
            if self.path_local_remotedata is not None:
                if force or not os.path.exists(self.path_local_remotedata):
                    logging.debug(f"Copying remote data file to {self.path_local_remotedata} ...")
                    from_file1 = os.path.join(self.path_exe, os.pardir, config.FILE_REMOTEDATA)
                    if os.path.exists(from_file1):
                        from_file = from_file1
                    else:
                        logging.warning(f"Error copying remote data file to local folder. File {from_file1} not found.")
                        return
                    shutil.copy2(from_file, self.path_local_remotedata)
        except (IOError, shutil.Error, OSError) as err:
            logging.debug(repr(err))
            self.errormsg(f"Error copying remote data file {from_file} to local folder: {repr(e)}")

    def load_remotedata(self):
        """Load remote data."""
        if self.path_local is not None and os.path.exists(config.FILE_REMOTEDATA):
            filename = config.FILE_REMOTEDATA
        else:
            filename = self.path_local_remotedata
        try:
            file = open(filename,'rb')
        except Exception as e:
            if not 'queuing' in self.remotedata:
                self.remotedata['queuing'] = 'qrsh -q general -j y -V -cwd'
            logging.debug(f"Incident loading remote data (always happens the first time): {repr(e)}")
            return
        for line in file:
            if line[-1] == '\n':
                line = line[:-1]
            parts = line.split(None,1)
            if len(parts) < 2:
                continue
            if parts[0]   ==    'user':
                self.remotedata['user']    = parts[1]
            elif parts[0] ==    'host':
                self.remotedata['host']    = parts[1]
            elif parts[0] ==    'key':
                self.remotedata['key']     = parts[1]
            elif parts[0] ==    'queuing':
                self.remotedata['queuing'] = parts[1]
        file.close()

    def save_remotedata(self):
        """Save remote data."""
        if self.path_local_remotedata is None:
            return
        try:
            file = open(self.path_local_remotedata,'wb')
        except Exception as e:
            self.errormsg(f"Error saving remote data: {repr(e)}")
            return
        user    = self.remotedata.get('user')    if self.remotedata.get('user')    is not None else ''
        host    = self.remotedata.get('host')    if self.remotedata.get('host')    is not None else ''
        key     = self.remotedata.get('key')     if self.remotedata.get('key')     is not None else ''
        queuing = self.remotedata.get('queuing') if self.remotedata.get('queuing') is not None else ''
        file.write('user '   +user+    '\n')
        file.write('host '   +host+    '\n')
        file.write('key '    +key+     '\n')
        file.write('queuing '+queuing+ '\n')
        file.close()
        if self.path_local is not None:
            shutil.copy2(self.path_local_remotedata, config.FILE_REMOTEDATA)
        
    def menu_exec_ssh(self, action):
        """Execute via SSH."""
        if self.taskscurrent>=0 and self.taskscurrent<len(self.taskssave):
            self.errormsg('There are pending processes to run. Meanwhile, new threads will not start.')
            return
        nt = self.num_threads()
        if nt != 0:
            self.errormsg(f"There are already {str(nt)} threads running. A new thread will not be created.")
            return
        params = action.get('params')
        if params is None or len(params)<1:
            self.errormsg('Error in menu: missing executable name')
            return
        # Dialog
        dialog = WindowRemote.WindowRemote(self, self.remotedata)
        r = dialog.ShowModal()
        if r == wx.ID_OK:
            datanew = dialog.get_data()
            for d,v in datanew.items():
                self.remotedata[d] = v
            self.save_remotedata()
        dialog.Destroy()
        dialog = None
        if r != wx.ID_OK:
            return
        # End dialog
        options = {}
        options['host']    = self.remotedata.get('host')
        options['user']    = self.remotedata.get('user')
        options['passw']   = self.remotedata.get('pass')
        options['keyfile'] = self.remotedata.get('key')
        options['queuing'] = self.remotedata.get('queuing')
        options['executables'] = params
        thread = ThreadRemote.ThreadRemote(self, options)
        self.add_thread(thread)
        thread.start()

    def menu_exec_qsub(self, action):
        """Execute via Qsub."""
        if self.taskscurrent>=0 and self.taskscurrent<len(self.taskssave):
            self.errormsg("There are pending processes to run. Will not start a thread")
            return
        nt = self.num_threads()
        if nt != 0:
            self.errormsg(f"There are already {str(nt)} threads running. A new thread will not be created.")
            return
        params = action.get('params')
        if params is None or len(params)<1:
            self.errormsg(f"Execution via Qsub: missing executable name")
            return
        # Dialog
        dialog = WindowRemote.WindowRemote(self, self.remotedata)
        r = dialog.ShowModal()
        if r == wx.ID_OK:
            datanew = dialog.get_data()
            for d,v in datanew.items():
                self.remotedata[d] = v
            self.save_remotedata()
        dialog.Destroy()
        dialog = None
        if r != wx.ID_OK:
            return
        # End dialog
        options = {}
        options['host']    = self.remotedata.get('host')
        options['user']    = self.remotedata.get('user')
        options['passw']   = self.remotedata.get('pass')
        options['keyfile'] = self.remotedata.get('key')
        options['queuing'] = self.remotedata.get('queuing')
        options['executables'] = params
        thread = ThreadRemoteQsub.ThreadRemoteQsub(self, self, options)
        self.add_thread(thread)
        thread.start()

    def menu_status_qsub(self, action):
        """Status of Qsub."""
        if self.taskscurrent >= 0 and self.taskscurrent<len(self.taskssave):
            self.errormsg("There are pending processes to run. A thread will not start.")
            return
        nt = self.num_threads()
        if nt != 0:
            self.errormsg(f"There are already {str(nt)} threads running. Another thread will not be created.")
            return
        # Dialog
        dialog = WindowStatus.WindowStatus(self, None)
        r = dialog.ShowModal()
        return

    def menu_get_qsub(self, action):
        """Get Qsub."""
        if self.taskscurrent>=0 and self.taskscurrent<len(self.taskssave):
            self.errormsg("There are pending processes to run. A new thread will not start.")
            return
        nt = self.num_threads()
        if nt != 0:
            self.errormsg(f"There are already {str(nt)} threads running. Another thread will not be created.")
            return
        # Dialog
        self.qsubdata['queuing'] = 'get'
        dialog = WindowStatus.WindowStatus(self, self.qsubdata)
        r = dialog.ShowModal()
        if r == wx.ID_OK:
            datanew = dialog.get_data()
            for d,v in datanew.items():
                self.qsubdata[d] = v
        dialog.Destroy()
        dialog = None
        if r != wx.ID_OK:
            return
        # End dialog
        options = {}
        options['host']         = self.qsubdata.get('host')
        options['user']         = self.qsubdata.get('user')
        options['passw']        = self.qsubdata.get('pass')
        options['keyfile']      = self.qsubdata.get('key')
        options['queuing']      = self.qsubdata.get('queuing')
        options['state']        = self.qsubdata.get('state')
        options['exec_command'] = self.qsubdata.get('exec_command')
        options['local_dir']    = self.qsubdata.get('local_dir')
        options['remote_dir']   = self.qsubdata.get('remote_dir')
        options['job_id']       = self.qsubdata.get('job_id')
        thread = ThreadRemoteQsub.ThreadRemoteQsub(self, self, options)
        self.add_thread(thread)
        thread.start()

    def event_end_process(self, event):
        """Event of end process."""
        self.end_process(event.ExitCode,custom_command=True)

    def end_process(self, exitcode, stopped=False, custom_command=False):
        """End process."""
        outerrtxt = self.process.read().lstrip()
        logging.debug(f"End {exitcode} {stopped}")
        if not stopped:
            self.timer.Stop()
            self.logger.add_text(str(outerrtxt))
        txt = self.panelC.stop_code(exitcode)
        if not stopped:
            self.process.ended()
        self.logger.add_text('SOLVER '+str(self.taskscurrent)+' STOP: '+txt+'\n')
        error = exitcode != 0
        # Cancel execution when existing errors
        name = None
        if error:
            res = None
            if self.taskscurrent >= 0 and self.taskscurrent < len(self.taskssave):
                name = self.taskssave[self.taskscurrent].get('path')
                if name is None:
                    name = self.taskssave[self.taskscurrent].get('text')
        else:
            res = self.next_task2(custom_command)
        # If True, start another one
        if res is None: # Finished series
            txt = self.panelC.time()
            elapsed = self.panelC.endall()
            if len(elapsed) > 0:
                elapsed = ' elapsed: ' + elapsed
            self.logger.add_text('SOLVERS STOP: '+txt+elapsed+'\n')
            self.taskscurrent = -2
            if self.reload_menu:      # Reload menu at solvers stop
                self.menu_load(False) # Reload menu at solvers stop
        if res is False:              # Error in the last command
            txt = self.panelC.time()
            elapsed = self.panelC.endall()
            if len(elapsed) > 0:
                elapsed = ' elapsed: ' + elapsed
            self.logger.add_text('SOLVERS STOP ERROR: '+txt+elapsed+'\n')
            self.taskscurrent = -2
        if error:
            self.logger.add_text("'"+str(name)+f"' returned exit code {str(exitcode)}.\n\nERROR MESSAGE:\n=================\n"+str(outerrtxt))
            self.errormsg("'"+str(os.path.normpath(name))+f"' returned exit code {str(exitcode)}.\n\nERROR MESSAGE:\n=================\n"+str(outerrtxt))

    def event_timer(self, event):
        """Event timer."""
        self.event_timer2()
    
    def event_timer2(self):
        """Event timer, version 2."""
        self.logger.add_text(self.process.read())

    def menu_exec_custom(self, action):
        """Custom execution."""
        params = action.get('params')
        if params is None or len(params)<1:
            self.errormsg("Custom execution: missing executable name.")
            return        
        # Dialog
        dialog = WindowCustom.WindowCustom(self,action)
        r = dialog.ShowModal()
        if r == wx.ID_OK:
            salida = dialog.get_custom_command()
            if salida is not None and isinstance(salida,str):
                action['data'] = salida
            elif salida is not None:
                action['params'] = salida
        dialog.Destroy() # Collect data
        dialog = None
        if r != wx.ID_OK:
            return
        # End dialog
        self.menu_exec2(action, True)

    def menu_exec2(self, action, custom_command=False):
        """Menu execution, version 2."""
        if self.process.is_running():
            self.errormsg("There is a running process. Another one cannot start.")
            return
        if self.taskscurrent>=0 and self.taskscurrent<len(self.taskssave):
            self.errormsg('There are pending processes to run. Will not start another one')
            return
        nt = self.num_threads()
        if nt != 0:
            self.errormsg(f"There are already {str(nt)} threads running. Another thread will not be created")
            return
        params = action.get('params')
        for param in params:
            if True:
                executable = None
                executable1 = os.path.join(self.configdir,config.DIR_SOLVERS,param.get('text'))
                executable2 = os.path.join(self.path_exe,os.pardir,config.DIR_SOLVERS,param.get('text'))
                # TODO: action[1] can be 'executable', then 'executable.exe' in Windows in suitable, but not in posix/mac/...: os.name
                exesallowed = os.name != 'posix'
                if   os.path.isfile(executable1) or (exesallowed and os.path.isfile(executable1+'.exe')):
                    executable = executable1
                elif os.path.isfile(executable2) or (exesallowed and os.path.isfile(executable2+'.exe')):
                    executable = executable2
                if executable is None:
                    pass
                else:
                    param['path'] = executable
                if custom_command and action.get('data') is not None:
                    param.get('attrib')['data'] = action.get('data')
        self.taskssave    = params
        self.taskscurrent = -1
        txt = self.panelC.start()
        self.logger.add_text('SOLVERS START: '+txt+'\n')
        self.next_task2(custom_command)

    def next_task2(self, custom_command=False):
        """Next task, version 2."""
        if self.taskscurrent < -1:
            return None
        self.taskscurrent += 1
        ret = self.exec2(custom_command)
        if ret is False: # Error starting execucion
            self.end_process(None, True, custom_command) # Continue with others
        return ret

    def exec2(self,custom_command=False):
        """Execution, version 2."""
        if self.taskscurrent < 0 or self.taskscurrent >= len(self.taskssave):
            return None
        param = self.taskssave[self.taskscurrent]
        if 'path' in param:
            command = '"'+param.get('path')+'"'
        else:
            command = param.get('text')
        args = param.get('attrib').get('args')
        if args is not None and args != '':
            command += ' ' + args
        if custom_command and param.get('attrib').get('data') is not None:
            command = param.get('attrib').get('data') + ' ' + command
        result = self.process.start(command, None)
        txt = self.panelC.time()
        if result is True:
            self.logger.add_text('SOLVER '+ str(self.taskscurrent) +' START: '+          txt+': '+command+'\n')
            self.timer.Start(200)
        else:
            self.logger.add_text('SOLVER '+ str(self.taskscurrent) +' failed to start: '+txt+': '+command+'\n')
        return result is True

    def menu_kill(self):
        """Menu kill."""
        self.process.kill()

    def num_threads(self):
        """Get number of threads."""
        return len(self.threads)

    def stop_threads(self):
        """Stop threads."""
        while self.threads:
            thread = self.threads[0]
            thread.stop() # not needed
            self.threads.remove(thread) 
 
    def add_thread(self, thread):
        """Add thread."""
        logging.debug(f"window: thread added: {str(thread)}")
        self.threads.append(thread)

    def finished_thread(self, thread):
        """Finished thread."""
        logging.debug(f"window: thread ended: {str(thread)}")
        self.threads.remove(thread)
