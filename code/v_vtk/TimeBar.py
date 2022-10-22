#!/usr/bin/env python
# -*- coding: utf-8 -*-




import wx


#class TimeBar(wx.BoxSizer):
class TimeBar(wx.Panel):
    def __init__(self, parent, plot):
        #wx.BoxSizer.__init__(self, wx.HORIZONTAL)
        wx.Panel.__init__(self, parent)

        self.plot = plot
        self.data1 = plot.data1
        self.parent = self
        self.sizer = wx.BoxSizer(wx.HORIZONTAL)

        self.buttons = []
        b = wx.Button(self.parent, wx.ID_ANY, '|<', style=wx.BU_EXACTFIT)
        b.Bind(wx.EVT_BUTTON, self.b_first)
        b.SetToolTip(wx.ToolTip('First frame'))
        self.buttons.append(b)
        b = wx.Button(self.parent, wx.ID_ANY, '<|', style=wx.BU_EXACTFIT)
        b.Bind(wx.EVT_BUTTON, self.b_previous)
        b.SetToolTip(wx.ToolTip('Previous frame'))
        self.buttons.append(b)
        b = wx.Button(self.parent, wx.ID_ANY, '>', style=wx.BU_EXACTFIT) # ">" "||"
        b.Bind(wx.EVT_BUTTON, self.b_play)
        b.SetToolTip(wx.ToolTip('Play/Pause'))
        self.buttons.append(b)
        b = wx.Button(self.parent, wx.ID_ANY, '|>', style=wx.BU_EXACTFIT)
        b.Bind(wx.EVT_BUTTON, self.b_next)
        b.SetToolTip(wx.ToolTip('Next frame'))
        self.buttons.append(b)
        b = wx.Button(self.parent, wx.ID_ANY, '>|', style=wx.BU_EXACTFIT)
        b.Bind(wx.EVT_BUTTON, self.b_last)
        b.SetToolTip(wx.ToolTip('Last frame'))
        self.buttons.append(b)
        
        for button in self.buttons:
            self.sizer.Add(button, 0, wx.ALIGN_CENTER_VERTICAL)

        esize = (50,-1)

        l = wx.StaticText(self.parent, wx.ID_ANY, '  '+ self.data1.get('evolution_upper') +': ')
        self.sizer.Add(l, 0, wx.ALIGN_CENTER_VERTICAL) #Old wx: |wx.ALIGN_RIGHT)
        self.txt_time = e = wx.TextCtrl(self.parent, wx.ID_ANY, size=esize, style=wx.TE_PROCESS_ENTER)
        e.Bind(wx.EVT_TEXT_ENTER , self.b_time)
        #self.sizer.Add(e, 0, wx.FIXED_MINSIZE)
        self.sizer.Add(e, 0, wx.ALIGN_CENTER_VERTICAL)
        l = wx.StaticText(self.parent, wx.ID_ANY, '  Start '+ self.data1.get('evolution_lower') +': ')
        self.sizer.Add(l, 0, wx.ALIGN_CENTER_VERTICAL) #Old wx: |wx.ALIGN_RIGHT)
        self.txt_start = e = wx.TextCtrl(self.parent, wx.ID_ANY, size=esize)
        self.sizer.Add(e, 0, wx.ALIGN_CENTER_VERTICAL)
        l = wx.StaticText(self.parent, wx.ID_ANY, '  End '+ self.data1.get('evolution_lower') +': ')
        self.sizer.Add(l, 0, wx.ALIGN_CENTER_VERTICAL) #Old wx: |wx.ALIGN_RIGHT)
        self.txt_end = e = wx.TextCtrl(self.parent, wx.ID_ANY, size=esize)
        self.sizer.Add(e, 0, wx.ALIGN_CENTER_VERTICAL)
        l = wx.StaticText(self.parent, wx.ID_ANY, '  Duration: ')
        self.sizer.Add(l, 0, wx.ALIGN_CENTER_VERTICAL) #Old wx: |wx.ALIGN_RIGHT)
        self.txt_dur = e = wx.TextCtrl(self.parent, wx.ID_ANY, '10.0', size=esize)
        self.sizer.Add(e, 0, wx.ALIGN_CENTER_VERTICAL)

        self.bsave = wx.Button(self.parent, wx.ID_ANY, 'Save animation', style=wx.BU_EXACTFIT)
        self.bsave.Bind(wx.EVT_BUTTON, self.save_movie)
        self.bsave.SetToolTip(wx.ToolTip('Exports render window to a movie'))
        self.sizer.Add(self.bsave, 0, wx.ALIGN_CENTER_VERTICAL)

        self.SetSizer(self.sizer)
        
        self.sizer.Layout() # se non os 'e' quedan mal debuxados

        #self.SetBackgroundColour(wx.Colour.NullColour) # invalidades themes
        #self.SetBackgroundColour(wx.Colour(100,50,20)) # invalidades themes


    def save_movie(self, event):
        movie_opt = {}
        movie_opt['movie'] = True

        file_ext = self.plot.save_as_call(event,movie_opt['movie'])
        if file_ext is not None:
            movie_opt['file'] = file_ext[0]
            movie_opt['codec'] = file_ext[1]
        try:
            movie_opt['start_time'] = float(self.txt_start.GetValue())
            movie_opt['end_time'] = float(self.txt_end.GetValue())
            movie_opt['duration'] = float(self.txt_dur.GetValue())
        except ValueError as v:
            self.plot.window.errormsg('Can not convert times to a floating point numbers')

        if self.plot.start_movie_saving(movie_opt):
            self.b_play(event)

    def b_first(self, event):
        print('first')
        self.plot.time_first()
        self.put_status(self.plot.get_status())


    def b_previous(self, event):
        print('previous')
        self.plot.time_previous()
        self.put_status(self.plot.get_status())


    def b_play(self, event):
        print('play/pause')
        self.plot.time_play()
        self.put_status(self.plot.get_status())


    def b_next(self, event):
        print('next')
        self.plot.time_next()
        self.put_status(self.plot.get_status())


    def b_last(self, event):
        print('last')
        self.plot.time_last()
        self.put_status(self.plot.get_status())


    def b_time(self, event):
        print('time')
        value = self.txt_time.GetValue()
        try:
            vald = float(value)
        except ValueError as v:
            self.plot.window.errormsg('Can not convert \''+value+'\' to a floating point number')
        else:
            self.plot.time_goto(vald)
        self.put_status(self.plot.get_status())



    def put_status(self, status):
        self.buttons[0].Enable(status.get('previous') is not False)
        self.buttons[1].Enable(status.get('previous') is not False)
        #self.buttons[2].Enable(status.get('previous') is not False)
        self.buttons[3].Enable(status.get('next') is not False)
        self.buttons[4].Enable(status.get('next') is not False)



    def set_is_playing(self, value):
        if value:
            self.buttons[2].SetLabel('||')
            self.bsave.Disable()
        else:
            self.buttons[2].SetLabel('>')
            self.bsave.Enable()


    def get_time_range(self):
        txt0 = self.txt_start.GetValue()
        txt1 = self.txt_end.GetValue()

        d0 = None
        try:
            d0 = float(txt0)
        except ValueError as v:
            self.plot.window.errormsg('First '+ self.data1.get('evolution_lower') +' range value error: can not convert \''+txt0+'\' to a floating point number')
            
        d1 = None
        try:
            d1 = float(txt1)
        except ValueError as v:
            self.plot.window.errormsg('Second '+ self.data1.get('evolution_lower') +' range value error: can not convert \''+txt1+'\' to a floating point number')

        return (d0,d1)



    def get_duration(self):
        txt0 = self.txt_dur.GetValue()

        d0 = None
        try:
            d0 = float(txt0)
        except ValueError as v:
            self.plot.window.errormsg('Duration value error: can not convert \''+txt0+'\' to a floating point number')
            
        return d0



    def set_start_time(self, value):
        self.txt_start.ChangeValue(str(value))

    def set_end_time(self, value):
        self.txt_end.ChangeValue(str(value))

    def set_time(self, value):
        self.txt_time.ChangeValue(str(value))
