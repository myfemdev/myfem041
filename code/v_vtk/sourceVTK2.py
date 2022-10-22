#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Module to manage VTK sources, version 2."""
import sys
import FilePVD
import logging


vtkimagefileformats = ('.png','.jpg','.jpeg','.bmp','.tif')
    
def get_clon(o):
    """Get VTK clon."""
    import vtk # Avoid importing everywhere
    ugrid = vtk.vtkUnstructuredGrid()
    ugrid.SetPoints(o.GetPoints())
    ugrid.SetCells(o.GetCellTypesArray(), o.GetCellLocationsArray(), o.GetCells())
    return ugrid

def get_double_array():
    """Get VTK double array."""
    import vtk # Avoid importing everywhere
    return vtk.vtkDoubleArray()

def get_wrap(o):
    """Get VTK wrap."""
    import vtk # Avoid importing everywhere
    src = vtk.vtkPassThrough()
    if vtk.vtkVersion.GetVTKMajorVersion() < 6:
        src.SetInput(o)
    else:
        src.SetInputData(o)
    return src    

def get_source(filename):
    """Get VTK source."""
    import vtk # Avoid importing everywhere    
    logging.debug(f"sourceVTK2: creating Reader for {str(filename)}")
    filenameencoded = filename#.encode(sys.getfilesystemencoding()) # needed (at least) in Windows # Python 2
    if filename.lower().endswith('.pvd'):
        times = FilePVD.read(filename)
        type = 'pvd'
        src = [type,filenameencoded,times]
    else:
        if filename.lower().endswith('.vtk'):
            type = 'vtk'
            src = vtk.vtkUnstructuredGridReader()
            src.ReadAllScalarsOn()
            src.ReadAllVectorsOn()
            src.SetFileName(filenameencoded)
        elif filename.lower().endswith('.vtu'):
            type = 'vtu'
            src = vtk.vtkXMLUnstructuredGridReader()
            src.SetFileName(filenameencoded)
        elif filename.lower().endswith(vtkimagefileformats):
            createReader = vtk.vtkImageReader2Factory()
            src = createReader.CreateImageReader2(filename)
            src.SetFileName(filename)
        else:
            return 'sourceVTK2: file extension of \''+filename+'\' not recognized'
        try:
            src.Update()
        except Exception as x:
            return 'sourceVTK2: unable to read '+ type +' file: \''+filename+'\': ' + repr(x)
        unselect_source(src)
    return src


def unselect_source(src):
    """Unselect source."""
    ou = src.GetOutput()
    pd = ou.GetPointData()
    cd = ou.GetCellData()
    pd.SetActiveVectors(None)
    cd.SetActiveVectors(None)
    pd.SetActiveScalars(None)
    cd.SetActiveScalars(None)

def get_append(sources):
    """CreateVTK Append.

    Notes:
        - When len(sources) == 0:
          ERROR: In /build/buildd/vtk-5.2.1/Filtering/vtkDemandDrivenPipeline.cxx, line 725:
          vtkStreamingDemandDrivenPipeline (0x8b02000): Input port 0 of algorithm vtkAppendFilter(0x8b01d60) has 0 connections but is not optional.
    """
    import vtk # Avoid importing everywhere
    src = vtk.vtkAppendFilter()
    for s in sources:
        src.AddInputConnection(s.GetOutputPort())
    return src

def config_append(src, sources):
    """Append configuration."""
    src.RemoveAllInputs()
    for s in sources:
        src.AddInputConnection(s.GetOutputPort())
    return src

def get_void():
    """Return an empty object suitable for representation."""
    import vtk # Avoid importing everywhere
    u = vtk.vtkUnstructuredGrid()
    src = vtk.vtkPassThrough()
    if vtk.vtkVersion.GetVTKMajorVersion() < 6:
        src.SetInput(u)
    else:
        src.SetInputData(u)
    return src

def get_values(src, cell_point, name, discard=[]):
    """Return (string or list) values."""
    out = src.GetOutput()
    if cell_point == 'cell':
        d = out.GetCellData()
    elif cell_point == 'point':
        d = out.GetPointData()
    else:
        return 'Only cell or point data allowed.'
    array = d.GetArray(name)
    if array is None:
        return f"Field name '{name}' not found in {cell_point} data."    
    size = array.GetNumberOfTuples()
    comp = array.GetNumberOfComponents()
    if comp != 1:
        return f"The number of components of \'{name}' is different from one."
    v = set()
    for i in range(size):
        v.add(array.GetValue(i))
    if discard is not None:
        for d in discard:
            v.discard(d)
    l = sorted(v)
    return l

def printn(ds):
    """Printn."""
    pdp = ds.GetPointData()
    logging.debug(pdp.GetNumberOfArrays())
    for i in range(pdp.GetNumberOfArrays()):
        logging.debug(f"name {str(i)} {str(pdp.GetArrayName(i))} {str(pdp.GetAbstractArray(i).GetNumberOfComponents())}")
    pdc = ds.GetCellData()
    logging.debug(pdc.GetNumberOfArrays())
    for i in range(pdc.GetNumberOfArrays()):
        logging.debug(f"name {str(i)} {str(pdc.GetArrayName(i))} {str(pdc.GetAbstractArray(i).GetNumberOfComponents())}")

def prints(src):
    """Prints."""
    printn(src.GetOutput())

def complement_missing_fields(sources):
    """Complement missing fields.
    
    Notes:
        In each source, celldata or pointdata names cannot be duplicated.
        In the global set of sources, two celldata or pointdata with same name and different number of components are now allowed.
    """
    logging.debug('Complementing')
    namesp = {}
    namesc = {}
    for s in sources: # Read all fields 
        o   = s.GetOutput()
        pd  = o.GetPointData()
        cd  = o.GetCellData()
        pdn = pd.GetNumberOfArrays()
        cdn = cd.GetNumberOfArrays()
        for i in range(pdn):
            name = pd.GetArrayName(i)
            namesp[name] = pd.GetAbstractArray(i).GetNumberOfComponents()
        for i in range(cdn):
            name = cd.GetArrayName(i)
            namesc[name] = cd.GetAbstractArray(i).GetNumberOfComponents()
    i = 0 # TODO: Complete the remainig ones
    for s in sources:
        added   = False
        namespl = {}
        namescl = {}
        o   = s.GetOutput()
        pd  = o.GetPointData()
        cd  = o.GetCellData()
        pn  = o.GetNumberOfPoints()
        cn  = o.GetNumberOfCells()
        pdn = pd.GetNumberOfArrays()
        cdn = cd.GetNumberOfArrays()
        for i in range(pdn):
            name = pd.GetArrayName(i)
            namespl[name] = True
        for i in range(cdn):
            name = cd.GetArrayName(i)
            namescl[name] = True
        for n, c in namesp.items():
            if n not in namespl:
                add_missing_field(i, 'point', pd, pn, n, c)
                added = True
        for n, c in namesc.items():
            if n not in namescl:
                add_missing_field(i, 'cell', cd, cn, n, c)
                added = True
        if added: # Is it necessary?
            s.Update()
        i += 1

def add_missing_field(index, domain, data, number, name, components):
    import vtk # Avoid importing everywhere
    logging.debug(f"Adding {str(index)} {str(domain)} {str(type(data))} {str(name)} {str(components)}")
    a = vtk.vtkDoubleArray()
    a.SetNumberOfComponents(components)
    a.SetNumberOfTuples(number)
    a.SetName(name)
    risco = True
    if risco: # That is maybe faster
        l = components * number
        n = 0
        while n < l:
            a.SetValue(n,0.0)
            n += 1
    else: # That is maybe safer
        t = 0
        while t < number:
            c = 0
            while c < components:
                a.SetComponent(t,c,0.0)
                c += 1
            t += 1
    data.AddArray(a)

def has_field(src, field):
    """Has field.

    Returns:
        True if it has the field, False it not, None if error.
    """
    if field is None:
        return True
    d = None
    if field.get('domain') == 'point':
        d = src.GetOutput().GetPointData()
    elif field.get('domain') == 'cell':
        d = src.GetOutput().GetCellData()
    if d is None:
        return None
    a = None
    if False:
        a = d.GetArray(field.get('name'))
    else:
        n = d.GetNumberOfArrays()
        for i in range(n):
            name = d.GetArrayName(i)
            if name == field.get('name'):
                a = d.GetAbstractArray(i)
    if a is None:
        return False
    c = a.GetNumberOfComponents()
    if field.get('components') is not None:
        if c.GetNumberOfComponents() != field.get('components'):
            return False
    return True
