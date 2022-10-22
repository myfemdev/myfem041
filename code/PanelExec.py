#!/usr/bin/env python
# -*- coding: utf-8 -*-




import wx
import datetime



class PanelExec(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)

        self.txtout = wx.TextCtrl(self, wx.ID_ANY, style=wx.TE_MULTILINE | wx.TE_READONLY | wx.TE_DONTWRAP)
        self.txt = wx.StaticText(self, wx.ID_ANY, '...')

        self.box = wx.BoxSizer(wx.VERTICAL)
        self.box.Add(self.txtout, 1, wx.EXPAND )
        self.box.Add(self.txt, 0, wx.EXPAND )
#        self.SetAutoLayout(True)
#        self.SetSizerAndFit(self.box)
        self.SetSizer(self.box)

        self.dtimestart = None
        self.timestart = ''
        self.dtimestop = None
        self.timestop = ''
        self.end = ''

        self.update()



    def clear_text(self):
        self.txtout.Clear()



    def add_text(self, text):
        self.txtout.AppendText(text)



    def update(self):
        txt = ''
        if len(self.timestart)>0:
            txt += 'Started: ' + self.timestart
        if len(self.timestart)>0 and len(self.timestop)>0:
            txt += '  '
        if len(self.timestop)>0:
            txt += 'Finished: ' + self.timestop
        if len(self.end)>0:
            txt += ' ' + self.end

        self.txt.SetLabel(txt)


    def clear(self):
        self.txtout.Clear()
        self.dtimestart = None
        self.dtimestop = None
        self.timestart = ''
        self.timestop = ''
        self.end = ''
        self.update()



    def clear_st(self):
        self.dtimestart = None
        self.dtimestop = None
        self.timestart = ''
        self.timestop = ''
        self.end = ''
        self.update()



    def start(self):
        self.dtimestart = self.timenow()
        self.timestart = self.timef(self.dtimestart)
        self.timestop = ''
        self.end = ''
        self.update()
        return self.timestart



    def stop(self):
        self.dtimestop = self.timenow()
        self.timestop = self.timef(self.dtimestop)
        self.update()
        return self.timestop



    def stop_code(self, code):
        self.dtimestop = self.timenow()
        self.timestop = self.timef(self.dtimestop) + '  Exit Code: ' + str(code)
        self.update()
        return self.timestop



    def endall(self): # all solvers ended
        self.end = ''
        elapsed = self.elapsed()
        if elapsed is not None:
            self.end += ' Elapsed time: ' + elapsed
        else:
            self.end += ' END'
        self.update()
        if elapsed is not None:
            return elapsed
        else:
            return ''



    def time(self):
        return self.timef(self.timenow())



    def timenow(self):
        return datetime.datetime.now()



    def timef(self, time_):
        return time_.strftime('%Y-%m-%d %H:%M:%S')        



    def elapsed(self):
        if self.dtimestart is None or self.dtimestop is None:
            return None

        diff = self.dtimestop - self.dtimestart
        
        txt = ''
        
        days = diff.days
        hours = diff.seconds//3600
        rest = diff.seconds%3600
        minutes = rest//60
        seconds = rest%60
        milli = diff.microseconds//1000
        
        if days>0:
            txt += days + ' days '

        txt += str(hours).zfill(2) + ':' + \
            str(minutes).zfill(2) + ':' + \
            str(seconds).zfill(2) + '.' + \
            str(milli).zfill(3)
            
        return txt
