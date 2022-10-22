#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Declaration of global variables."""
import os

ID_SPACES      = 5 #: (int) Spacing among IDs.
SPACE_MENU     = 0
SPACE_MENU_DYN = 2
PATHS_RELATIVE = True
                            
DIR_APPS      = "apps"
DIR_SOLVERS   = "solvers"
DIR_DOCS      = "help"
DIR_MATERIALS = "materialsDB"
DIR_CONFIG    = "config"
DIR_IMAGES    = "images"
                            
FILE_MENULOCAL          = "local.mnu.xml"
FILE_MENULOCALSOLVER    = "local.dat.xml"
FILE_LASTFOLDER         = "lastfolder.dat"
FILE_REMOTEDATA         = "remote-run-data.txt"
FILE_RUNSTATUS          = "run-status.txt"
FILE_MATERIALS          = "materials.xml"
FILE_MATERIALS_PREFIX   = "materials."
FILE_MATERIALS_SUFFIX   = ".xml"
FILE_MATERIALS_DAT      = "materials.dat.xml"
NAME_MATERIALS_DATABASE = "Materials database"
NAME_MATERIALS_FILE     = "Materials file"
                            
FILE_CONFIG        = "config.xml"
FILE_CONFIG_PREFIX = "config."
FILE_CONFIG_SUFFIX = ".xml"
FILE_CONFIG_DAT    = "config.dat.xml"
NAME_CONFIG_FILE   = "Config file"
                            
CONFIG_MENU_NAME            = "Configuration"
CONFIG_SUBMENU_NAME         = "Preferences"
CONFIG_STRUCT_NAME          = "Options"
CONFIG_MAXIMIZE_OPTION      = "Maximize at start"
CONFIG_WARNONLOAD_OPTION    = "Show warning on load"
CONFIG_VERBOSE_OPTION       = "Run GUI in verbose mode"
CONFIG_WIDGETS_PANEL_SIZE   = "Default widgets panel width"
CONFIG_WINDOW_SIZE          = "Default window size"
CONFIG_WINDOW_WIDTH         = "Window width"
CONFIG_WINDOW_HEIGHT        = "Window height"
CONFIG_INTERNACIONALIZATION = "Internationalization"

VALUE_YES   = "yes"
VALUE_NO    = "no"
VALUE_TRUE  = "true"
VALUE_FALSE = "false"
                           
PLOTTED            = "plotted"
PLOT               = "plot"
PLOT_MESH          = "mesh"
PLOT_ADD_MESH      = "add_mesh"
PLOT_POINT_DATA    = "pointdata"
PLOT_CELL_DATA     = "celldata"
PLOT_MATERIALS     = "materials"
PLOT_DATA          = "data"
PLOT_EVOLUTION     = "evolution"
PLOT_INTERPOLATION = "interpolation"
PLOT_MESH_NAME     = "mesh_name"
PLOT_FORMULA       = "formula"

AT_SHOWVALUES = "showvalues"
AT_CUSTOMIZE  = "customize"
AT_SOURCE     = "source"
AT_DIM        = "dim"
AT_PARAM      = "parametrize"
AT_SELECTED   = "selected"
AT_TYPE       = "type"
AT_SUBTYPE    = "subtype"
AT_TITLE      = "title"
AT_HIDDEN     = "hidden"
AT_HELP       = "help"
AT_SAVETHIS   = "savethis" #: (str) Note: Not used in local.mnu.xml (Only in Menu).
AT_COPY       = "copymenu"  #: (str) Note: Not used in local.mnu.xml.
AT_TABULAR    = "tabular"
AT_SHOW       = "show"
AT_PLOT       = "plot"
AT_ALL        = "all"
AT_MATRIX     = "matrix"
AT_HELPWINDOWDATA   = "helpdata"
AT_HELPWINDOWCONFIG = "helpconfig"

IS_MATERIALS             = "is_materials"
IS_CONFIG                = "is_config"
PATHS_RELATIVE_MATERIALS = False