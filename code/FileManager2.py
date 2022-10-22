#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""FileManager2 class.

This class manages files to plot and to convert.
"""
import os.path
import logging
import FileTrack2
import FileManager2Trackers as FMT

class FileManager2:
    """FileManager2 class."""
    def __init__(self):
        self.callback = None
        self.num      = 0
        self.trackers = []
        # Reusable buffers
        self.buffer0  = {}   #: (str)
        self.buffer1  = {}   #: (str)
        self.buffer2  = {}   #: (str, num/str)
        self.buffer3  = {}   #: (str, str, num/str)
        self.buffer4  = {}   #: (str)
        self.buffer5  = {}   #: (str)
        self.buffer6  = {}   #: (Node)
        self.buffer7  = None #: void
        self.buffer8  = {}   #: (Node)

    def set_callback(self, callback=None):
        """Set callback for output messages."""
        self.callback = callback

    def get_callback(self):
        """Set callback for output messages.
        
        TODO:
            Avoid "getters" and "setters".
        """
        return self.callback

    def clear(self):
        """Clear internal data."""
        self.num = 0
        del self.trackers[:]
        self.buffer0.clear()
        self.buffer1.clear()
        self.buffer2.clear()
        self.buffer3.clear()
        self.buffer4.clear()
        self.buffer5.clear()
        self.buffer6.clear()
        self.buffer7 = None
        self.buffer8.clear()

    def get_num(self):
        """Get increased num (for Trackers)."""
        num = self.num
        self.num = self.num + 1
        return num

    def get_tracker_file(self, filename):
        """Get file tracker."""
        if filename in self.buffer0:
            logging.debug(f"0. Using {filename}")
            return self.buffer0[filename]
        else:
            logging.debug(f"0. Creating {filename}")
            tracker = FMT.TrackerFile(self, filename)
            self.trackers.append(tracker)
            self.buffer0[filename] = tracker
            return tracker

    def get_tracker_vtk_file(self, vtkfile):
        """Get VTK file tracker."""
        if vtkfile in self.buffer1:
            logging.debug(f"1. Using {vtkfile}")
            return self.buffer1[vtkfile]
        else:
            logging.debug(f"1. Creating {vtkfile}")
            tracker = FMT.TrackerVTKFile(self, vtkfile)
            self.trackers.append(tracker)
            self.buffer1[vtkfile] = tracker
            return tracker

    def get_tracker_mfm_file(self, mesh, dim):
        """Get MFM mesh file tracker."""
        if (mesh,dim) in self.buffer2:
            logging.debug(f"2. Using {mesh}, {dim}")
            return self.buffer2[(mesh,dim)]
        else:
            logging.debug(f"2. Creating {mesh}, {dim}")
            tracker = FMT.TrackerMFMFile(self, mesh, dim)
            self.trackers.append(tracker)
            self.buffer2[(mesh,dim)] = tracker
            return tracker

    def get_tracker_mfm_mff_files(self, mesh, field, params):
        """Get MFM mesh and MFF field file tracker."""
        dim = params.get('dim')
        if (mesh,field,dim) in self.buffer3:
            logging.debug(f"3. Using {mesh}, {field}, {dim}")
            return self.buffer3[(mesh,field,dim)]
        else:
            logging.debug(f"3. Creating {mesh}, {field}, {dim}")
            tracker = FMT.TrackerMFMMFFFiles(self, mesh, field, params)
            self.trackers.append(tracker)
            self.buffer3[(mesh,field,dim)] = tracker
            return tracker

    def get_tracker_unv_file(self, unvfile):
        """Get UNV mesh file tracker."""
        if unvfile in self.buffer4:
            logging.debug(f"4. Using {unvfile}")
            return self.buffer4[unvfile]
        else:
            logging.debug(f"4. Creating {unvfile}")
            tracker = FMT.TrackerUNVFile(self, unvfile)
            self.trackers.append(tracker)
            self.buffer4[unvfile] = tracker
            return tracker

    def get_tracker_pvd_file(self, pvdfile):
        """Get PVD file tracker."""
        if pvdfile in self.buffer5:
            logging.debug(f"5. Using {pvdfile}")
            return self.buffer5[pvdfile]
        else:
            logging.debug(f"5. Creating {pvdfile}")
            tracker = FMT.TrackerPVDFile(self, pvdfile)
            self.trackers.append(tracker)
            self.buffer5[pvdfile] = tracker
            return tracker

    def get_tracker_mesh_file(self, filename, dim=None):
        """Get mesh file tracker."""
        if filename.lower().endswith('.vtu'):
            return self.get_tracker_vtk_file(filename)
        elif filename.lower().endswith('.vtk'):
            return self.get_tracker_vtk_file(filename)
        elif filename.lower().endswith('.unv'):
            return self.get_tracker_unv_file(filename)
        elif filename.lower().endswith('.mfm'):
            return self.get_tracker_mfm_file(filename, dim)
        elif filename.lower().endswith('.pvd'):
            return self.get_tracker_pvd_file(filename)
        return None

    def get_tracker_node_files(self, node, is_nodepvd=False):
        """Get node file tracker."""
        if node in self.buffer6:
            logging.debug(f"6. Using {node}")
            return self.buffer6[node]
        else:
            logging.debug(f"6. Creating {node}")
            tracker = FMT.TrackerNodeFiles(self, node,is_nodepvd)
            self.trackers.append(tracker)
            self.buffer6[node] = tracker
            return tracker

    def get_tracker_void(self):
        """Get void tracker."""
        if self.buffer7 is not None:
            logging.debug(f"7. Using")
            return self.buffer7
        else:
            logging.debug(f"7. Creating")
            tracker = FMT.TrackerVoid(self)
            self.trackers.append(tracker)
            self.buffer7 = tracker
            return tracker

    def get_tracker_formula(self, node, text, variables, data):
        """Get formula tracker."""
        if node in self.buffer8:
            logging.debug(f"8. Using {node}")
            return self.buffer8[node]
        else:
            logging.debug(f"8. Creating {node}")
            tracker = FMT.TrackerFormula2(self, node, text, variables, data)
            self.trackers.append(tracker)
            self.buffer8[node] = tracker
            return tracker
