#!/usr/bin/env python
# -*- coding: utf-8 -*-




import wx
import dialogs
import sys
import vtk



def save_renderwindow(parent, renderwindow, title=None):
#    formats = "JPEG (*.jpg)|*.jpg" + \
#        "|PNG (*.png)|*.png" + \
#        "|BMP (*.bmp)|*.bmp" + \
#        "|TIFF (*.tiff)|*.tiff" + \
#        "|PDF (*.pdf)|*.pdf" + \
#        "|EPS (*.eps)|*.eps" + \
#        "|PS (*.ps)|*.ps" + \
#        "|SVG (*.svg)|*.svg"
    extensions = ['.jpg','.png','.bmp','.tiff']
    formats = "JPEG (*.jpg)|*.jpg" + \
        "|PNG (*.png)|*.png" + \
        "|BMP (*.bmp)|*.bmp" + \
        "|TIFF (*.tiff)|*.tiff"

    [file,ext_index] = dialogs.get_file_save(parent,".","",formats,title)
    if file is None:
        return

    print("file:"+file)

    point = file.rfind('.')
    if point == -1:
        base = file
        extension = extensions[ext_index]
    else:
        base = file[:point]
        extension = file[point:]

#    splitname = file.split('.')
#    if len(splitname) == 1:
#        base = file
#        extension = extensions[ext_index]
#    else:
#        base = splitname[0]
#        extension = splitname[1]

    error = None
    
    if extension == '.jpg' or extension == '.png' or \
            extension == '.bmp' or extension == '.tiff':
        error = save_image(renderwindow, base, extension)
    elif extension == '.pdf' or extension == '.eps' or \
            extension == '.svg' or extension == '.ps':
        error = save_exporter(renderwindow, base, extension)
    else:
        error = 'Unknown file extension: ' + extension

    if error is not None:
        dialogs.show_error(parent, error)

def save_movie_renderwindow(parent, renderwindow, title=None):

    codec = ['AVI','FFMPEGHQ','FFMPEGLQ','OGGTHEORA','MPEG2']
    extensions = ['.avi','.avi','.avi','.ogv','.mpg']
    formats = "AVI (*.avi)|*.avi" + \
        "|FFMPEG (HQ) (*.avi)|*.avi" + \
        "|FFMPEG (LQ) (*.avi)|*.avi" + \
        "|OGGTHEORA (*.ogv)|*.ogv" + \
        "|MPEG2 (*.mpg)|*.mpg"
    file = None
    ext_index = None
    [file,ext_index] = dialogs.get_file_save(parent,".","",formats,title)
    if file is None:
        return None

    point = file.rfind('.')
    if point == -1:
        base = file
        extension = extensions[ext_index]
    else:
        base = file[:point]
        extension = file[point:]
    return [base+extension,codec[ext_index]]


def save_image(renderwindow, base, extension):
    file = (base+extension)#.encode(sys.getfilesystemencoding()) #Python 2
    print('save_image'+file)
    
    w2i = vtk.vtkWindowToImageFilter()
    w2i.SetInput(renderwindow)
    w2i.Update()
    if extension == '.jpg':
        image = vtk.vtkJPEGWriter()
        image.SetQuality(100)
    elif extension == '.png':
        image = vtk.vtkPNGWriter()
    elif extension == '.bmp':
        image = vtk.vtkBMPWriter()
    elif extension == '.tiff':
        image = vtk.vtkTIFFWriter()
        image.SetCompressionToNoCompression()
    else:
        return "Image exporter: unknown file extension: " + extension
    
    image.SetInputConnection(w2i.GetOutputPort())
    image.SetFileName(file)
    renderwindow.Render()
    image.Write()
    
    return None



def save_exporter(renderwindow, base, extension):
    #base = base.encode(sys.getfilesystemencoding()) #Python 2
    print('save_exporter', base, '+', extension)
    
    try:
        exporter = vtk.vtkGL2PSExporter()
    except AttributeError as e:
        return 'Can\'t export image: ' + str(e)
        
    exporter.DrawBackgroundOff()
    exporter.SetInput(renderwindow)
    exporter.SetFilePrefix(base)
    
    if extension == '.pdf':
        exporter.SetFileFormatToPDF()
    elif extension == '.eps':
        exporter.SetFileFormatToEPS()
    elif extension == '.ps':
        exporter.SetFileFormatToPS()
    elif extension == '.tex':
        exporter.SetFileFormatToTeX()
    elif extension == '.svg':
        exporter.SetFileFormatToSVG()
        exporter.CompressOff()
    else:
        return 'Exporter: unknown file extension: ' + extension

    exporter.Write()
    
    return None
