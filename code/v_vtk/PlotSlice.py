#!/usr/bin/env python
# -*- coding: utf-8 -*-



# mostra unha seccion do obxecto



import config
from v_vtk import Plot

import wx
import vtk
import v_vtk.ClickLabel3  as ClickLabel



# mostrar etiquetas con el valor del campo en los puntos donde se hace 'doble click'
interactive2 = True



class PlotSlice(Plot.Plot):

    def __init__(self, parent):
        Plot.Plot.__init__(self, parent)

        self.clicker = None



    def get_options(self):
        # para que cree un wxVTKRenderWindowInteractor e non un wxVTKRenderWindow
        return {'interactor':True,'title':'Slice'}



    # para cuando cambia el tiempo y por tanto, self.src
    def src_update1(self, changes):
        if changes.get('new'):
            self.cutter.SetInputConnection(self.src.GetOutputPort())
# actualizado con adjust_abs_rel()
#        if changes.get('changed'):
#            sr = self.src.GetOutput().GetScalarRange()
#            self.pdM.SetScalarRange(sr)
        if changes.get('changed'):
            self.update_outline(self.src)
        if self.has_field:
            if changes.get('new'):
                self.change_abs_rel(self.src)
            if changes.get('changed'):
                self.adjust_abs_rel()
        if changes.get('changed'):
            if vtk.vtkVersion.GetVTKMajorVersion() < 6:
                self.planeI.SetInput(self.src.GetOutput())
            else:
                self.planeI.SetInputConnection(self.src.GetOutputPort())
            self.planeI.PlaceWidget() # actualiza caixa do plano
        self.src_update_clicker(self.clicker, self.src, changes)
        return True



    def range_update3(self, range):
        self.pdM.SetScalarRange(range)
        self.pdM.GetLookupTable().SetTableRange(range)



    def plot(self, struct):

        # creates self. data1, legend, filename, fieldname, dim, has_field, tracker, revision
        if not self.call_config(struct):
            return

        # creates self.src
        if not self.call_src():
            return

        # postponed processing
        if self.has_field:
            self.add_scalarbar_1()
            self.add_abs_rel_1()
        self.add_outline_1()
        self.add_plane_1()
        self.add_sw_1()
        self.add_opacity_1(selection=0) # Opacity: 100%/75%/50%/25%/0%


# vector
        self.plane = vtk.vtkPlane()
        self.origin = self.src.GetOutput().GetCenter()
        print('center', self.origin)
#        self.plane.SetOrigin(self.origin)
#        self.plane.SetNormal(1, 1, 1)

        self.cutter = vtk.vtkCutter()
        self.cutter.SetInputConnection(self.src.GetOutputPort())
        self.cutter.SetCutFunction(self.plane)
        
        self.pdM = vtk.vtkPolyDataMapper()
        self.pdM.SetInputConnection(self.cutter.GetOutputPort())
        if self.has_field:
            self.pdM.ScalarVisibilityOn()
        else:
            self.pdM.ScalarVisibilityOff()

        self.scalarrange.local_set(self.src.GetOutput().GetScalarRange())

        # reverse rainbow [red->blue] -> [blue->red]
        look = self.pdM.GetLookupTable()
        #self.add_scalarbar_2(look)

        self.add_outline_2(self.src)

        self.cutA = vtk.vtkActor()
        self.cutA.SetMapper(self.pdM)
        self.cutA.GetProperty().SetRepresentationToSurface()
        self.cutA.GetProperty().SetColor(Plot.mesh_color)
        self.add_opacity_2([self.cutA]) # Opacity: 100%/75%/50%/25%/0%
        self.rens[0].AddActor(self.cutA)
        
        # po??endo a scalarbar aqu?? e non antes, queda ben dibuxada desde o primeiro (non negra)
        if self.has_field:
            self.add_scalarbar_2(look)
            self.add_abs_rel_2(self.src, self.cutter)
            
        self.add_sw_2(self.cutA)
        
# mover plano interactivamente
        #self.planeI = vtk.vtkPlaneWidget()
        self.planeI = vtk.vtkImplicitPlaneWidget()
        seeds = vtk.vtkPolyData()
        if vtk.vtkVersion.GetVTKMajorVersion() < 6:
            self.planeI.SetInput(self.src.GetOutput())
        else:
            self.planeI.SetInputConnection(self.src.GetOutputPort())

        self.add_plane_2(self.planeI)
        
        self.planeI.SetOrigin(self.origin[0],self.origin[1],self.origin[2])
        #self.planeI.SetResolution(20)
        #planeWidget.NormalToXAxisOn()
        #planeWidget.SetRepresentationToOutline()
        #self.planeI.PlaceWidget()
        self.planeI.GetPolyData(seeds)
        self.planeI.SetPlaceFactor(1.0) # factor * bounds
        self.planeI.OutsideBoundsOn() # not on PlaneWidget
        self.planeI.OutlineTranslationOff()
        self.planeI.DrawPlaneOff()
        self.planeI.ScaleEnabledOff()
        self.planeI.PlaceWidget()
        #self.planeI.PlaceWidget(self.src.GetOutput().GetBounds())
        self.planeI.AddObserver("EndInteractionEvent", self.event_end)

        self.set_iren()
        self.planeI.SetInteractor(self.iren)
        self.planeI.On()

#        self.lineI.AddObserver("StartInteractionEvent", self.event_start)
#        self.lineI.AddObserver("InteractionEvent", self.event)
#        self.lineI.AddObserver("EndInteractionEvent", self.event_end)
# /mover plano interactivamente

        if interactive2 and self.has_field:
            #self.set_iren() # necesario ^
            self.clicker = ClickLabel.ClickLabel()
            self.clicker.set_point_cell(self.data1.get('fielddomain'))            
            self.clicker.set_objects(self.cutter, self.rens[0], self.iren, self.widget)
            self.clicker.set_props([self.cutA])
            self.clicker.setup()

        o = self.planeI.GetOrigin()
        n = self.planeI.GetNormal()
        print('initial: o,n', o, n)

        self.set_params(struct, o, n)
        
        self.copy_params(struct)

        self.done = True

        

    def event_end(self, obj, event):
        o = self.planeI.GetOrigin()
        n = self.planeI.GetNormal()
        #print('o,n', o, n
        self.plane.SetOrigin(o)
        self.plane.SetNormal(n)
        self.set_params(self.struct, o, n, True)
        
        if self.has_field:
            self.update_abs_rel()
            if self.clicker is not None:
                self.clicker.update_src()
            #self.widget.Render() # not needed



# fai falta
    def update(self, struct):
        self.copy_params(struct)
        self.widget.Render()



# write params to menu xml structure on first load. only writes if there is no data
    def set_params(self, struct, origin, normals, force=False):
        ch = struct.get_children()

        if len(ch) < 2:
            print('Error: incorrect number of childs in PlotSlice')
            return
        
        nums = ch[0].get_elements()
        if len(nums) == 0 or force:
            nums = [ str(origin[0]), str(origin[1]), str(origin[2]) ]
            ch[0].set_elements_nosel(nums)
            self.panel_widgets.update_widget_struct(ch[0])

        nums = ch[1].get_elements()
        if len(nums) == 0:
            nums = [ str('0.0'), str('0.0'), str('1.0') ]
            ch[1].set_elements_nosel(nums)
            self.panel_widgets.update_widget_struct(ch[1])
        elif force:
            nums = [ str(normals[0]), str(normals[1]), str(normals[2]) ]
            ch[1].set_elements_nosel(nums)
            self.panel_widgets.update_widget_struct(ch[1])



    def copy_params(self, struct):

        result = self.read_2_vectors(struct, [' in PlotSlice',' in origin',' in normal'])

        if result is not None:
            self.planeI.SetOrigin(result[0])
            self.plane.SetOrigin(result[0])
            self.planeI.SetNormal(result[1])
            self.plane.SetNormal(result[1])
            
            if self.has_field:
                self.update_abs_rel()
                if self.clicker is not None:
                    self.clicker.update_src()
