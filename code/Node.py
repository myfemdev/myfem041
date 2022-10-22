#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os.path
import logging
import trees
import config
import NoDeep
import Source
import Formulas
from v_vtk import sourceVTK2

class Node():
    def __init__(self):
        self.children = [] #: (list) Node children.
        self.attribs = {} #: (dict) Node attributes.
        self.tag     = "-"
        self.parent  = NoDeep.NoDeep(None)
        # We assume that deepcopy'ed defaults do not have the following attributes (if they have them, protect then with NoDeep).
        self.data         = {} #: (dict) Custom data. 
        self.dependencies = {} #: (dict) Dependencies (protect with NoDeep).
        self.influences   = {} #: (dict) Influences   (protect with NoDeep).

    def get_data(self):
        """Returns Node data attribute."""
        return self.data

    def combine(self, node):
        """Merges two nodes.
        
        Args:
            node: (Node) Node to combine with.

        Note: 
            Call self.set_children_parents() afterwards.
        """
        self.children = node.children[:]
        for a in node.attribs:
            self.attribs[a] = node.attribs[a]

    def plot_able(self):
        """Check whether or not there is a place to create a plot."""
        return (self.tag == "struct" or self.tag == "leaf") and \
            self.get_attribs().get(config.PLOTTED) != config.VALUE_TRUE and \
            (self.get_attribs().get(config.PLOT) is not None)
  

    def plot_has(self):
        """Check whether or not there is a plot."""
        return (self.tag == "struct" or self.tag == "leaf") and \
            self.get_attribs().get(config.PLOTTED) == config.VALUE_TRUE and \
            (self.get_attribs().get(config.PLOT) is not None)

    def plot_next_has(self):
        """Return the next plot, if exists."""
        this = self.get_parent()
        while this is not None:
            if this.plot_has():
                return this
            this = this.get_parent()
        return None

    def apply_to_parent_plots(self, func):
        """Apply function to parent plots."""
        temp = self.plot_next_has()
        while temp is not None:
            func(temp)
            temp = temp.plot_next_has()

    def apply_to_all_plots(self, func):
        """Apply function to all plots (and apply it to self at the end)."""
        self.apply_to_parent_plots(func)
        if self.plot_has():
            func(self)

    def add_parameters(self, data):
        """Add parameters to every child."""
        for child in self.get_children():
            child.add_parameters(data)

    def get_parent(self):
        """Get node item."""
        return self.parent.geti()

    def set_parent(self, parent):
        """Set node item."""
        self.parent.seti(parent)

    def set_children_parents(self):
        """Recursively set node item."""
        for child in self.children:
            child.set_parent(self)
            child.set_children_parents()

    def get_path(self, sep="/", root=False):
        """Set path with node names from root to input node.
        
        Args:
            sep: (str) Separation character.
            root: (bool) Whether or not to add root node to path.

        Returns:
            (str) Path with node names from root to input node.
        """
        path = []
        this = self
        while this is not None:
            path.append(this.get_name())
            this = this.get_parent()
        if not root and len(path)>0:
            del path[-1]
        path.reverse()
        return sep + sep.join(path)

    def get_top(self):
        """Get root node name."""
        last = self
        top = self
        while top is not None:
            last = top
            top = top.get_parent()
        return last

    def get_first_attrib(self, name):
        """ Return attribute from the first ancestor where it is defined."""
        this = self
        while this is not None:
            value = this.get_attribs().get(name)
            if value is not None:
                return value
            this = this.get_parent()
        return None

    def del_childs_by_name(self, name):
        """Delete attribute of every direct children.""" 
        index = len(self.children) - 1
        while index>=0:
            if self.children[index].get_name() == name:
                del self.children[index]
            index -= 1

    def add_child(self, child):
        """Append child to self."""
        child.set_parent(self)
        self.children.append(child)

    def add_child_start(self, child):
        """Insert first child in self."""
        child.set_parent(self)
        self.children.insert(0,child)

    def del_child(self, index):
        """Deletes a child by index."""
        del self.children[index]

    def del_children(self):
        """Deletes node children."""
        del self.children[:]

    def get_children(self):
        """Returns node children.""" 
        return self.children

    def get_to_source(self):
        """Return name list of direct children."""
        result = []
        for child in self.get_children():
            result.append(child.get_name())
        return result

    def get_to_source_if_sel_if(self):
        """Get list of selected children (or every children for those self who are not single selection structs or charlist leafs without showfile).
        
        Note:
             Similar code in Widget.py.

        TODO:
             Check whether sel detects when selection is NOT single selection. In that case, change name and behavior to 'single_sel'. 
        """ 
        sel = False # Variable sel is True for single selection structs and charlist leafs without showfile
        if self.tag == 'struct':
            selection = self.get_attribs().get("selection")
            sel = selection == 'single'
        elif self.tag == 'leaf':
            type = self.get_attribs().get('type')
            if type == "charlist" and self.get_attribs().get('showfile') is None:
                sel = True
        result = []
        for child in self.get_children():
            if not sel or child.get_attribs().get(config.AT_SELECTED): 
                result.append(child.get_name())
        return result

    def get_first_name(self):
        """Get name of the first child in leafs.
        
        TODO:
            In leafs with source, non-selected elements are missing?
        """
        if self.get_tag() != "leaf": 
            logging.warning("Method node.get_first_name() was called on element different from a leaf.")
        ch = self.get_children()
        return ch[0].get_name() if len(ch)>0 else ""

    def get_first_selected_name(self):
        """Get name of the first selected children in leafs.
        
        TODO:
            In leafs with source, non-selected elements are missing?
        """
        if self.get_tag() != "leaf": 
            logging.warning("Method node.get_first_selected_name() was called on element different from a leaf.")
        ch = self.get_children()
        if len(ch)>0:
            for child in ch:
                if (child.get_attribs().get("selected")=="true"):
                    return child.get_name()
        return None

    def has_source(self):
        """Checks whether or not there are something to show in the left side.
        
        Returns:
            True if attribs["source"] is defined or attribs["showvalues"] == "true".
        """
        return self.attribs.get(config.AT_SOURCE) is not None or self.attribs.get(config.AT_SHOWVALUES) == config.VALUE_TRUE

    def get_attribs(self):
        """Return node attributes."""
        return self.attribs

    def get_name(self):
        """Get node name attribute."""
        name = self.attribs.get("name")
        return name if name is not None else "-"

    def get_title(self):
        """Get node title attribute."""
        title = self.attribs.get("title")
        return title if title is not None else "-"

    def set_name(self, name):
        """Set node name attribute."""
        self.get_attribs()["name"] = name

    def set_title(self, title):
        """Set node title attribute."""
        self.get_attribs()["title"] = title

    def get_tag(self):
        """Get node tag attribute."""
        return self.tag

    def is_hidden(self):
        """Check whether attribs["hidden"] == "true"."""
        return self.get_attribs().get(config.AT_HIDDEN) == config.VALUE_TRUE

    def load(self, item):
        """Load item.
        
        Args:
            item: (Node) item to load.
        """
        self.tag     = item.tag
        self.attribs = item.attrib.copy() #: Note: item will presumably be an Element object.

    @staticmethod
    def filter_attribs(old):
        """Filter keys present at input.
        
        Args:
            old: (dict) Existing dictionary.
            
        Returns:
            new: (dict) only keys 'name', 'type', 'subtype' and 'totalnum' are copied from input.
        """
        new = {}
        valid_keys = ["name", "type", "subtype", "totalnum"]
        for key in old:
            if key in valid_keys:
                new[key] = old[key]
        return new

    def get_name_for_mesh(self):
        """Get mesh name attribute."""
        mesh_name = self.attribs.get(config.PLOT_MESH_NAME)
        if mesh_name is None:
            ch = self.get_children()
            if len(ch) == 0:
                return "(empty)"
            elif len(ch) > 1:
                return "(overflow)"
            else:
                return ch[0].get_name()
        parsed = self.parse_source_string_1(mesh_name)
        if parsed[0] is None:
            logging.error(f"Unable to parse source 'mesh_name': {mesh_name}.")
        elif parsed[0] == 0: # data
            return parsed[1]
        elif parsed[0] == 2: # menu
            res = self.parse_path_varx(parsed[1], True, False)
            if isinstance(res, str):
                logging.error(f"Unable to parse menu 'mesh_name' {mesh_name}: {res}.")
            elif len(res) != 1:
                logging.error(f"Unable to parse menu 'mesh_name' {mesh_name}: dimension {len(res)} > 1 in {res}.")
            else:
                return res[0]
        else:
            logging.error(f"Attribute 'mesh_name' {mesh_name} only allows prefixes 'data:' or 'menu:'")
        return "!" # TODO: None causes errors

    def get_data5(self, pointers=False):
        """Get data from an XML plot? element."""
        result = {'filenames':[],'filesmesh':[],'filenames_mn':[],'filesmesh_mn':[],'filenames_for':[]}
        # Default values
        result['fieldtype'] = 'scalar'
        mesh      = self.get_attribs().get(config.PLOT_MESH)
        add_mesh  = self.get_attribs().get(config.PLOT_ADD_MESH)
        point     = self.get_attribs().get(config.PLOT_POINT_DATA)
        cell      = self.get_attribs().get(config.PLOT_CELL_DATA)
        data      = self.get_attribs().get(config.PLOT_DATA)
        formula   = self.get_attribs().get(config.PLOT_FORMULA)
        evolution = self.get_attribs().get(config.PLOT_EVOLUTION)
        if evolution is None:
            result['evolution']       = 'Time'
            result['evolution_lower'] = 'time'
            result['evolution_upper'] = 'Time'
        else: # TODO: improve upper/lower case logic
            result['evolution'] = evolution
            evu = evolution
            if len(evolution) > 0:
                evu = evu[0].lower() + evu[1:]
            result['evolution_lower'] = evu
            evu = evolution
            if len(evolution) > 0:
                evu = evu[0].upper() + evu[1:]
            result['evolution_upper'] = evu
        interpolation = self.get_attribs().get(config.PLOT_INTERPOLATION)
        result['interpolation'] = interpolation != config.VALUE_FALSE
        objects = [] if pointers else None # whether or not to compute dependencies
        field = None
        if point is not None:
            result['fielddomain'] = 'point'
            field = point
        if cell is not None:
            result['fielddomain'] = 'cell'
            field = cell
        if point is not None and cell is not None:
            return 'Both "celldata" and "pointdata" attributes present'
        if data is not None and (mesh is not None or add_mesh is not None or point is not None or cell is not None):
            return '"data" attribute is incompatible with "mesh","add_mesh","pointdata","celldata"'
        if data is not None and formula is not None:
            return '"data" attribute is incompatible with "formula" attribute'
        if data is None and mesh is None and formula is None:
            return 'Missing "mesh" or "data" or "formula" attributes'
        if formula is not None:
            fv = Formulas.extract_parts(formula)
            if isinstance(fv, str):
                return 'Error in formula: '+fv
            result['formula_text'] = fv[0]
            result['formula_vars'] = fv[1]
            result['formula_data'] = []
            result['formula_is']   = True
            for i in range(len(result['formula_vars'])):
                var   = result['formula_vars'][i]
                name  = var[0]
                type_ = var[1]
                path  = var[2]
                if type_ == 'menu':
                    nodes = self.parse_path_varx(path, True, False, objects, True, None)
                    logging.debug(f"Value of parse_path_varx.objects: {list(map(Node.get_path, objects))}.")
                    if isinstance(nodes, str): # error
                        return f"Invalid path: {nodes}"
                    if nodes is None: # invalid node
                        return f"Invalid path: {path}"
                    if len(nodes) != 1:
                        return f"Invalid number of referenced nodes: {len(nodes)}"
                    node = nodes[0]
                else:
                    node = None
                var.append(node) # 3
                type2 = None
                if type_ == 'menu':
                    if node.get_attribs().get(config.PLOT_MESH)    is not None or \
                       node.get_attribs().get(config.PLOT_FORMULA) is not None: # there is a graph
                        type2 = 'menu-field'
                    elif node.get_tag() == 'leaf' and (node.get_attribs().get(config.AT_TYPE) == 'float' or \
                                                       node.get_attribs().get(config.AT_TYPE) == 'complex'): # it is a leaf-float
                        type2 = 'menu-value'
                    else:
                        return 'Type of node unknown'
                else:
                    type2 = type_
                var.append(type2)
                if type2 == 'menu-field':
                    res = node.get_data5(pointers)
                    if isinstance(res, str):
                        return f"Error in dependence of formula: {res}"
                    if objects is not None:
                        objects.extend(res.get('dependencies'))
                        logging.debug(f"Value of subdata.dependencies: {list(map(Node.get_path,res.get('dependencies')))}.")
                    if res.get('fielddomain') != result.get('fielddomain'):
                        return f"Mismatched field domains: {result.get('fielddomain')} and {res.get('fielddomain')}"
                    result['formula_data'].append(res)
                    result['filenames_for'].extend(res.get('filenames'))
                    result['filenames'].extend(res.get('filenames'))
                elif type2 == 'menu-value':
                    if objects is not None:
                        objects.append(node)
                    result['formula_data'].append(None)
                elif type2 == 'data': # not allowed
                    result['formula_data'].append(None)
                else:
                    result['formula_data'].append(None)
        if mesh is not None: # >= 0
            mesh_names = []
            parsed = self.parse_source_string_1(mesh) # type, string
            if parsed[0] is None:
                return 'Error parsing "mesh" source string'
            elif parsed[0] == 1: # file
                res = self.parse_path_varx(parsed[1], False, False, objects, mesh_names=mesh_names)
            elif parsed[0] == 2: # menu
                res = self.parse_path_varx(parsed[1], True,  True,  objects, mesh_names=mesh_names)
            else:
                return '"mesh" attribute only allows "file:" and "menu:" prefixes'
            if isinstance(res, str):
                return res
            result['filesmesh1'] = res
            result['filesmesh'].extend(res)
            result['filenames'].extend(res)
            result['filesmesh1_mn'] = mesh_names
            result['filesmesh_mn'].extend(mesh_names)
            result['filenames_mn'].extend(mesh_names)
        if add_mesh is not None: # >= 0
            mesh_names = []
            parsed = self.parse_source_string_1(add_mesh) # type, string           
            if parsed[0] is None:
                return 'Error parsing "add_mesh" source string'
            elif parsed[0] == 1: # file
                res = self.parse_path_varx(parsed[1], False, False, objects, mesh_names=mesh_names)
            elif parsed[0] == 2: # menu
                res = self.parse_path_varx(parsed[1], True,  True,  objects, mesh_names=mesh_names)
            else:
                return '"add_mesh" attribute only allows "file:" and "menu:" prefixes'
            if isinstance(res, str):
                return res
            result['filesmesh2'] = res
            result['filesmesh'].extend(res)
            result['filenames'].extend(res)
            result['filesmesh2_mn'] = mesh_names
            result['filesmesh_mn'].extend(mesh_names)
            result['filenames_mn'].extend(mesh_names)
        if field is not None: # = 1
            mesh_names = []
            tupf = self.parse_source_string_2(field, objects, mesh_names=mesh_names)
            if tupf[0] is None:
                return f"Error parsing '{result.get('fielddomain')} data' source string"
            if tupf[0] != 0 and tupf[0] != 1 and tupf[0] != 2:
                return '"celldata" and "pointdata" attributes only allow "data:", "file:" and "menu:" prefixes'
            if tupf[2] is not None and result.get('dim') is None:
                result['dim'] = tupf[2]
            if tupf[0] == 1 or tupf[0] == 2:
                result['filenames'].append(tupf[1])
                result['filesfield'] = [tupf[1]]
                result['filenames_mn'].extend(mesh_names)
                result['filesfield_mn'] = mesh_names
                temp = os.path.basename(tupf[1])
                i = temp.rfind('.')
                if i <= 0: # we use <= instead of < because '.mff' returns '.mff', not ''
                    temp2 = temp
                else:
                    temp2 = temp[:i]
                result['fieldname'] = temp2
            elif tupf[0] == 0:
                result['fieldname'] = tupf[1]
        if data is not None: # = 1 (for example, .gr2)
            tupm = self.parse_source_string_2(data, objects) # type, string, dim(int or None)
            if tupm[0] is None:
                return 'Error parsing "data" source string'
            if tupm[0] != 1 and tupm[0] != 2:
                return '"data" attribute only allows "file:" and "menu:" prefixes'
            graph2d_filenames = self.parse_path_varx(tupm[1], False, False, objects) # graph2d allows 'menu:'
            result['filesdata'] = graph2d_filenames
            for f in graph2d_filenames:
                result['filenames'].append(f)
        if objects is not None:
            logging.debug(f"Start pointers for {self.get_path()}:")
            for o in objects:
                logging.debug(f": {o.get_path()}")
            logging.debug(f"End pointers total {len(objects)}")            
        result['dependencies'] = objects
        return result

    def get_tracker5(self, filemanager, predata=None):
        """ Get ..."""
        is_nodepvd = False # is_nodepvd = True for several PVD
        data = predata if predata is not None else self.get_data5()
        if isinstance(data, str):
            return data
        tracker = None
        filenames      = data.get('filenames')
        meshfilenames  = data.get('filesmesh')  # all
        mesh1filenames = data.get('filesmesh1') # main
        mesh2filenames = data.get('filesmesh2') # secondary
        fieldfilenames = data.get('filesfield')
        datafilenames  = data.get('filesdata')
        numfilenames   = len(filenames)
        fm_mn          = data.get('filesmesh_mn')  # meshnames
        ff_mn          = data.get('filesfield_mn') # fieldnames
        fortext        = data.get('formula_text')
        forvars        = data.get('formula_vars')
        if fortext is not None and forvars is not None:
            tracker = filemanager.get_tracker_formula(self, fortext, forvars, data)
            tracker.set_data(fortext, forvars, data)
            return tracker
        if numfilenames == 0:
            return filemanager.get_tracker_void()
        if datafilenames is not None:
            if len(datafilenames) == 1:
                return filemanager.get_tracker_file(datafilenames[0]) # data
            elif len(datafilenames) > 1: # Create TrackerNodeFiles from several TrackerFiles (for example, Plot2DGraph)
                trackers =  [filemanager.get_tracker_file(dfn) for dfn in datafilenames] # data
                tracker  =   filemanager.get_tracker_node_files(self, is_nodepvd)
                tracker.set_trackers(trackers, len(datafilenames)) # length of group 1 / group 2
                return tracker
        fieldfilename = None
        if fieldfilenames is not None:
            if len(fieldfilenames) == 1:
                fieldfilename = fieldfilenames[0]
            elif len(fieldfilenames) > 1:
                return 'Only one field file allowed'
        if meshfilenames is None:
            return filemanager.get_tracker_void()
        elif len(meshfilenames) == 0:
            return filemanager.get_tracker_void()
        elif len(meshfilenames) == 1:
            if fieldfilename is None  or not fieldfilename.lower().endswith('.mff'):
                if meshfilenames[0] == '':
                    return 'Empty mesh file name'
                tracker = filemanager.get_tracker_mesh_file(meshfilenames[0])
                if tracker is None:
                    return f"Unsupported filename extension of file '{meshfilenames[0]}'"
                else:
                    tracker.set_size1(len(mesh1filenames))
                    if fm_mn is not None and len(fm_mn) == 1:
                        tracker.set_name(fm_mn[0])
                    return tracker
            else:
                if meshfilenames[0].lower().endswith('.mfm') and fieldfilename.lower().endswith('.mff'):
                    tracker = filemanager.get_tracker_mfm_mff_files(meshfilenames[0], fieldfilename, \
                        {'dim':data.get('dim'), 'fieldname':data.get('fieldname'), \
                        'fielddomain':data.get('fielddomain'), 'fieldtype':data.get('fieldtype')})
                    tracker.set_size1(len(mesh1filenames))
                    if fm_mn is not None and ff_mn is not None and len(fm_mn) == 1 and len(ff_mn) == 1:
                        tracker.set_name(fm_mn[0]+' + '+ff_mn[0])
                    return tracker
                else:
                    if meshfilenames[0] == '':
                        return 'Empty mesh file name'
                    if fieldfilename == '':
                        return 'Empty field file name'
                    return f"Unsupported mesh and field extensions ({meshfilenames[0]}, {fieldfilename}): only .mfm and .mff are allowed."
        else:
            trackers = []
            if fieldfilename is None  or not fieldfilename.lower().endswith('.mff'):
                # Create trackers for every file
                i = 0
                if meshfilenames[0].lower().endswith('.pvd'): # Check PVD trackernodefile
                    is_nodepvd = True
                for meshfile in meshfilenames:
                    if meshfile == '':
                        return 'Empty mesh file name'
                    trackertemp = filemanager.get_tracker_mesh_file(meshfile)
                    if trackertemp is None:
                        return f"Unsupported filename extension in '{meshfile}'"                        
                    if fm_mn is not None and len(fm_mn) > i:
                        trackertemp.set_name(fm_mn[i])
                    if is_nodepvd:
                        trackertemp.recalculate() # Init trackervtk to get times
                    trackers.append(trackertemp)
                    i += 1
            else:
                # Add the same field to every mesh
                i = 0
                for meshfile in meshfilenames:
                    if meshfile.lower().endswith('.mfm') and fieldfilename.lower().endswith('.mff'):
                        trackertemp = filemanager.get_tracker_mfm_mff_files(meshfile, fieldfilename, \
                            {'dim':data.get('dim'), 'fieldname':data.get('fieldname'), \
                            'fielddomain':data.get('fielddomain'), 'fieldtype':data.get('fieldtype')})
                        if fm_mn is not None and ff_mn is not None and len(fm_mn) > i and len(ff_mn) == 1:
                            trackertemp.set_name(fm_mn[i]+' + '+ff_mn[0])
                        trackers.append(trackertemp)
                    else:
                        if meshfile == '':
                            return 'Empty mesh file name'
                        if fieldfilename == '':
                            return 'Empty field file name'
                        return f"Unsupported mesh and file extensions ({meshfile}, {fieldfilename}): only .mfm and .mff are allowed."
                    i += 1
            tracker = filemanager.get_tracker_node_files(self, is_nodepvd)
            tracker.set_trackers(trackers, len(mesh1filenames)) # length of group 1 / group 2
        return tracker

    @staticmethod
    def parse_source_string_1(source):
        """Parse string prefix:sufix applying to prefix transformation f(data/file/menu) = 0/1/2.
        
        Returns:
            If source is prefix:sufix, return (f(prefix), sufix).
            Otherwise, return (None, '').
        """
        ret = (None,'')
        if source is None:
            return ret
        i = source.find(':')
        if i < 0:
            return ret
        pre  = source[:i]
        post = source[i+1:]
        if   pre == "data":
            pretype = 0
        elif pre == "file":
            pretype = 1
        elif pre == "menu":
            pretype = 2
        else:
            return ret
        return (pretype, post)

    
    def parse_source_string_2(self, source, objects=None, mesh_names=None):
        """Parse XML attribute mesh="prefix:value".
        
        Returns:
            (enum): ([None,0,1,2] , data/file , dim)
        """
        ret = (None,'',None)
        if source is None:
            return ret
        i = source.find(':')
        if i < 0:
            return ret
        pre  = source[:i]
        post = source[i+1:]
        file = post
        dim = None
        if   pre == "data":
            pretype = 0
        elif pre == "file":
            pretype = 1
            if mesh_names is not None:
                mesh_names.append(os.path.basename(file))
        elif pre == "menu":
            pretype = 2
        else:
            return ret
        if pretype == 2: # menu
            node = self.get_node_from_path(post)
            if isinstance(node, str):
                return ret
            else: # filename
                file = node.get_first_selected_name()
                if file is None:
                    file = node.get_first_name()
                if objects is not None:
                    objects.append(node)
                dimstr = node.get_attribs().get("dim") # dim
                if dimstr is not None:
                    try:
                        dim = int(dimstr)
                    except ValueError as e:
                        logging.error(f"Unable to parse XML attribute mesh='menu:...', dimension {dimstr}: {repr(e)}.")
                if mesh_names is not None:
                    mesh_names.append(node.get_name_for_mesh())
        return (pretype, file, dim)

    def get_from_path(self, path, for_leaf_file=False, objects=None, for_node=False):
        """Get either leaf name or children list."""
        temp = Source.Source.parse_simple_path(path) # evaluate escaped characters
        if not isinstance(temp, list):
            return "Parsing path: '"+temp+"'"
        node = self.get_node_from_segments(temp)
        if node is None:
            return "Path not found: '"+path+"'"
        else:
            if for_node:
                if objects is not None:
                    objects.append(node) # add node to dependency list (repeated)
                return ([node], None, [node])
            # ask name for the current element or for its children
            get_name = len(temp)>0 and temp[-1] == 3 # 3 means "..."
            dim = node.get_attribs().get('dim')
            if get_name:
                array = [node.get_name()]
            else:
                if for_leaf_file:
                    array = [node.get_first_name()]        # for leaf-file, get name or "" if there is no filename
                else:
                    array = node.get_to_source_if_sel_if() # for other nodes, get children list: [] ... ['a'] ... ['a','b'] ...
                if objects is not None:
                    objects.append(node) # add node to dependency list (repeated)
            return (array, {'dim':dim}, [node])

    def parse_path_var(self, path, is_menu, for_leaf_file=False, objects=None, for_node=False, mesh_names=None):
        """Parse variable path and return list of filenames from leaf-file.
        
        Args:
             path: (str) Variable path.
             is_menu: (bool) If True, search in menu:; if False, build filename with file:
             for_leaf_file: (bool) Used when we need filenames.
             for_node: (list) Output node list instead of string list cadeas (requires is_menu=True).

        Returns:
             (list) Filenames from leaf-file.
        """
        parts = Source.Source.extract_var(path)
        if isinstance(parts,list):
            if parts[1] is None: # There are no variables
                if not is_menu:
                    txt = Source.Source.desescape(parts[0])
                    if mesh_names is not None:
                        mesh_names.append(os.path.basename(txt))
                    return [txt]
                array = self.get_from_path(parts[0], for_leaf_file, objects, for_node)
                if isinstance(array, tuple):
                    if mesh_names is not None:
                        mesh_names.extend(list(map(Node.get_name_for_mesh, array[2])))
                    return array[0]
                else:
                    return f"Error parsing '{parts[0]}': {array}"
            else: # There are one variable
                array = self.get_from_path(parts[1], False, objects, False) #
                if isinstance(array, tuple):
                    total = []
                    for a in array[0]:
                        if is_menu:
                            aesc = Source.Source.escape(a)
                            strn = parts[0] + aesc + parts[2]
                            array2 = self.get_from_path(strn, for_leaf_file, objects, for_node)
                            if isinstance(array2, tuple):
                                if mesh_names is not None:
                                    mesh_names.extend(list(map(Node.get_name_for_mesh, array2[2])))
                                total.extend(array2[0])
                            else:
                                return f"Error parsing '{strn}': {array2}"
                        else:
                            txt = Source.Source.desescape(parts[0]) + a + Source.Source.desescape(parts[2])
                            total.append(txt)
                            if mesh_names is not None:
                                mesh_names.append(os.path.basename(txt))
                    return total
                else:
                    return f"Error in variable '{parts[1]}': {array}"
        else:
            return f"Error extracting variable from '{path}': {parts}"

    def parse_path_varx(self, path, is_menu, for_leaf_file=False, objects=None, for_node=False, mesh_names=None):
        """ Switch to choose between old parser, for only one variable and the new one, several variables.
        
        TODO:
            Comment or delete parse_path_var.
        """
        return self.parse_path_vars(path, is_menu, for_leaf_file, objects, for_node, mesh_names)

    def parse_path_vars(self, path, is_menu, for_leaf_file=False, objects=None, for_node=False, mesh_names=None):
        """Adaptation of parse_path_var for several variables."""
        temp = Source.Source.extract_vars(path)
        if isinstance(temp, tuple):
            tree = temp[0]
        else:
            return f"Error extracting variables from '{path}': {temp}"
        if isinstance(tree, list):
            strings = self.parse_inner_vars(tree, objects, False)
            if not isinstance(strings, list):
                return f"Error parsing variables: {strings}"
            result = []
            if is_menu:
                for string in strings:
                    one = self.get_from_path(string, for_leaf_file, objects, for_node)
                    if isinstance(one, tuple):
                        result.extend(one[0])
                        if mesh_names is not None:
                            mesh_names.extend(list(map(Node.get_name_for_mesh, one[2])))
                    else:
                        pass
            else:
                for string in strings:
                    txt = Source.Source.desescape(string)
                    result.append(txt)
                    if mesh_names is not None:
                        mesh_names.append(os.path.basename(txt))
            return result
        else:
            return f"Error extracting variables from '{path}'."

    def parse_inner_vars(self, tree, objects=None, process=True):
        """Parse lower levels in the variable tree."""
        strings = None
        for element in tree:
            if isinstance(element, list):
                morestrings = self.parse_inner_vars(element, objects)
                if isinstance(morestrings, str):
                    return morestrings
                if strings is None:
                    strings = morestrings
                else:
                    newstrings = []
                    for i in range(len(strings)):
                        for j in range(len(morestrings)):
                            newstrings.append(strings[i] + morestrings[j])
                    strings = newstrings
            else:
                if strings is None:
                    strings = [element]
                else:
                    for i in range(len(strings)):
                        strings[i] += element
        if strings is None:
            strings = []
        if process:
            result = []
            for string in strings:
                one = self.get_from_path(string, False, objects, False)
                if isinstance(one, tuple):
                    for a in one[0]: # Scape a name-derived variable
                        aesc = Source.Source.escape(a)
                        result.append(aesc)
                else:
                    pass
            return result
        else:
            return strings

    def get_node_from_segments(self, segments):
        """ Get node from segments.
        
        Args:
            segments: (list) processed segments, without \, from Sources.Sources.parse_simple_path(): 1/2/3 for '.'/'..'/'...'
        """
        this = self
        l = len(segments)
        if l < 1: # not possible
            return this
        if l == 1 and segments[0] == '': # initially empty string
            return this
        if segments[0] == '': # starts by slash
            this = self.get_top()
            ri = 1
        else:
            ri = 0
        if l > 0 and segments[-1] == '': # permits, for example, '/', '/place/', '/a/b/c/'
            rf = l-1   # ended with slash, dismissed
        else:
            rf = l # not ended with slash, dismissed
        rango = list(range(ri,rf))
        for index in rango:
            if segments[index] == 1: # .
                continue
            if segments[index] == 2: # ..
                this = this.get_parent()
                continue
            if segments[index] == 3: # ...
                continue
            children = this.get_children()
            name = segments[index]
            match = None
            for a in children:
                if a.get_name() == name:
                    match = a
                    break
            if match is not None:
                this = match
            else:
                this = None
                break
        return this

    def get_node_from_path(self, path):
        """ Get node from path.
        
        Returns
            (str, Node) String or Node
        """
        temp = Source.Source.parse_simple_path(path) # Evaluate escaped characters
        if not isinstance(temp,list):
            return f"Parsing path: '{temp}'"
        node = self.get_node_from_segments(temp)
        if node is None:
            return f"Path not found: '{path}'"
        return node

    def get_source_showvalues(self):
        """ Get source showvalues attribute.
        
        Note:
            mesh="..." (pointdata="data:..." | celldata="data:...") showvalues="true"
        """
        showvalues = self.get_attribs().get(config.AT_SHOWVALUES)
        if showvalues != config.VALUE_TRUE:
            return "Error: missing source= or showvalues='true'"
        data = self.get_data5()
        if isinstance(data, str):
            return data
        logging.debug(data)        
        field = data.get('fieldname')
        if field is None:
            return "Error: No field specified for showvalues='true'"
        # Text
        window = self.get_top().get_window()
        filemanager = window.filemanager
        cb = AuxCall(window)
        callback = cb.call
        callbacks = cb.calls
        callbacks("Reading references from mesh file ...\n")
        tracker = self.get_tracker5(filemanager, data)
        logging.debug(tracker)        
        if isinstance(tracker, str):
            return tracker
        result = tracker.update()
        if result is None:
            callbacks("References: error\n")
            return "Error reading references from mesh file"
        refs = tracker.get_refs()
        # If references were not obtained when converting from mfm/unv to vtk, get them from vtkUnstructuredGridReader or vtkXMLUnstructuredGridReader
        if refs is None:
            callbacks("References: Read VTK\n")
            fielddata = {}
            fielddata['name'] = field
            fielddata['domain'] = data.get('fielddomain')
            # Obtain only principals (not additional) with field for TrackerNodeFiles and others
            src = tracker.get_src_group_f(1, fielddata)
            # Obtain only principals (not additional) for TrackerNodeFiles
            if src is None: # It never enters or return None; it can return an empty UnstructuredGrid.
                return "Error reading references: no main meshes found"
            if isinstance(src, str):
                return f"Error reading references: {src}"
            rs = sourceVTK2.get_values(src, data.get('fielddomain'), field, [0]) # it can be cached in tracker
            if not isinstance(rs,list):
                return f"Error calculating references: {rs}"
            return list(map(str, rs))
        else:
            if result is True:
                callbacks("References: Read\n")
            elif result is False:
                callbacks("References: Cached\n")
        if refs is None or field not in refs or refs.get(field) is None:
            res = f"Error reading references: source field '{field}' does not exist"
        else:
            res = refs.get(field)
        return res

    def get_source_all(self):
        """ Get source attributes.
        
        Note:
            source = "data:..." "menu:..."
            Suitable when calling node.has_source()
        """
        source = self.get_attribs().get(config.AT_SOURCE)
        if source is not None:
            parsed = self.parse_source_string_1(source) # TODO: rewrite to not go to other nodes
            if parsed[0] is None:
                return f"Unable to parse 'source' value {source}"
            elif parsed[0] == 1:
                sourcefile = None
                source = []
                if os.path.isfile(parsed[1]):
                    sourcefile = open(parsed[1])
                    try:
                        line = sourcefile.readline()
                        while line:
                            line = line.strip('\n').strip()
                            source.append(line)
                            line = sourcefile.readline()
                    finally:
                        if sourcefile is not None:
                            sourcefile.close()
                return source
            elif parsed[0] != 2:
                return f"Unable to parse 'source' value {source}: only prefixes 'menu:' or 'file:' are allowed."
            source_path = parsed[1]
            # Required in variables and relative paths to get a materials list specified in another leaf
            return self.parse_path_varx(source_path,True,False)
        return self.get_source_showvalues()

    def get_remote_source(self, menus):
        """ Get remote source.
        
        Notes:
            menus is not used here.
        """
        res = self.get_source_all()
        if isinstance(res, str):
            return f"Error obtaining references: {res}"
        return res

    def set_dependencies(self, objects, deep=False):
        """Set primary dependencies or influences."""
        self.dependencies = {}
        if objects is None:
            return
        for o in objects:
            self.dependencies[id(o)] = o
            if deep:
                o.add_influence(self)

    def add_dependencies(self, objects, deep=False):
        """Set secondary dependencies or influences."""
        if objects is None:
            return
        for o in objects:
            self.dependencies[id(o)] = o
            if deep:
                o.add_influence(self)

    def clear_dependencies(self, deep=False):
        """Clear dependencies."""
        if deep:
            for key, value in self.dependencies.items():
                value.remove_influence(self)
        self.dependencies = {}

    def add_influence(self, i):
        """Add influence."""
        self.influences[id(i)] = i

    def is_dependency(self, o):
        """Check whether or not is a dependency."""
        return self.dependencies.get(id(o)) is o

    def remove_influence(self, value):
        """Remove influence."""
        if id(value) in self.influences:
            del self.influences[id(value)]

    def call_influences(self, func):
        """Call influences."""
        for key, value in self.influences.items():
            if value.is_dependency(self):
                func(value, self) # update graph

    def has_influences(self):
        """Check whether or not has influences."""
        for key, value in self.influences.items():
            if value.is_dependency(self):
                return True
        return False

class AuxCall():
    def __init__(self, window):
        self.window = window

    def call(self, text):
        if self.window is not None:
            self.window.add_text(text)
            self.window.process_pending()

    def calls(self, text):
        if self.window is not None:
            logging.debug(text)
