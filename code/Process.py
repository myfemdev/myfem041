#!/usr/bin/env python
# -*- coding: utf-8 -*-




import wx
import os
import sys
import logging



class Process():



    def __init__(self, parent):
        self.parent = parent
        self.process = None
        self.process_pid = None
        self.process_out = None
        self.process_err = None
        self.process_in = None



    def is_running(self):
        return self.process is not None



    def start(self, command, stdin):
        if self.process is not None:
            #print 'process: busy' #code prior version 0.0.1
            logging.warning('process: busy')
            return False

        #print 'process: executing', command, #code prior version 0.0.1
        logging.debug('process: executing'+str(command))
        if stdin is not None:
            #print 'with', stdin #code prior version 0.0.1
            logging.debug('with: executing'+str(stdin))
        #else:
            #print #code prior version 0.0.1
            
        self.process = wx.Process(self.parent)
        self.process.Redirect()
        self.process_pid = wx.Execute(command, wx.EXEC_ASYNC, self.process)
        #print 'process: exec', self.process_pid #code prior version 0.0.1
        logging.debug('process: exec'+str(self.process_pid))
        if self.process_pid==0:
            #print 'process: error' #code prior version 0.0.1
            logging.error('process: error')
            #self.process_clear() # fallaba aqui
            self.ended() # parece ser buen reemplazo para lo anterior
            return None
        self.process_in = self.process.GetOutputStream()
        #print 'stream pread', stream
        if self.process_in is not None and stdin is not None:
            #print 'process: write', self.process_in.write(stdin), self.process_in.LastWrite() #code prior version 0.0.1
            logging.debug('process: write'+str(self.process_in.write(stdin))+str(self.process_in.LastWrite()))
        self.process.CloseOutput()
        self.process_out = self.process.GetInputStream()
        self.process_err = self.process.GetErrorStream()
        #print 'stream pwrite', self.process_out
        #print 'stream perror', self.process_err
        return True



    def ended(self):
        self.process_out = None
        self.process_err = None
        self.process_in = None
        self.process.Destroy()
        del self.process
        self.process = None
        self.process_pid = None



    def kill(self):
        if self.process is not None:
            #pid = self.process.GetPid()
            #print pid, self.process_pid
            if self.process_pid is not None and self.process_pid > 0:
                #print 'process: SIGTERM', wx.Process.Kill(self.process_pid, wx.SIGTERM) #code prior version 0.0.1
                logging.debug('process: SIGTERM'+str(wx.Process.Kill(self.process_pid, wx.SIGTERM)))
                if os.name == 'nt':
                    #print 'process: SIGKILL', wx.Process.Kill(self.process_pid, wx.SIGKILL) #code prior version 0.0.1
                    logging.debug('process: SIGKILL'+str(wx.Process.Kill(self.process_pid, wx.SIGKILL)))

#    def read(self):
#        txt = ''
#        if self.process is not None:
#            if self.process_out is not None:
#                while self.process.IsInputAvailable():
#                    txt += self.tryunicode(self.process_out.read())
#                    #print 'process: read', self.process_out.LastRead() #code prior version 0.0.1
#                    logging.debug('process: read'+str(self.process_out.LastRead()))
#            if self.process_err is not None:
#                while self.process.IsErrorAvailable():
#                    txt += self.tryunicode(self.process_err.read())
#                    #print 'process: error', self.process_err.LastRead() #code prior version 0.0.1
#                    logging.debug('process: error'+str(self.process_err.LastRead()))
#        return txt

    def read(self):
        """Read either the input or the error stream of a wx.Process.
        
        Notes:
            Read using GetInputStream().Read() or GetErrorStream().Read() methods of wx class `InputStream`_.
            The result of those methods is of Bytes type, and it is converted to str before in the return command.
        .. _InputStream:
            https://wxpython.org/Phoenix/docs/html/wx.InputStream.html
        """
        btext = b''
        if self.process is not None:
            if self.process_out is not None:
                while self.process.IsInputAvailable():
                    btext += self.process_out.read()
            if self.process_err is not None:
                while self.process.IsErrorAvailable():
                    btext += self.process_err.read()
        return btext.decode(sys.getfilesystemencoding()) # Return a str

    def readstdout(self):
        btext = ''
        if self.process is not None:
            if self.process_out is not None:
                while self.process.IsInputAvailable():
                    btext += self.process_out.read()
        return btext.decode(sys.getfilesystemencoding()) # Return a str

    def readstderr(self):
        btext = ''
        if self.process is not None:
            if self.process_err is not None:
                while self.process.IsErrorAvailable():
                    btext += self.process_err.read()
        return btext.decode(sys.getfilesystemencoding()) # Return a str

    @staticmethod
    def tryunicode(stri):
        """Try to log a Unicode text.

        Note:
            It is a static method.
        """
        txt = ' '
        try:
            txt += stri
        except UnicodeDecodeError as u:
            logging.debug('process: UnicodeDecodeError1'+str(u))
            try:
                txt += str(stri, errors='ignore')
            except UnicodeDecodeError as u:
                logging.debug('process: UnicodeDecodeError2'+str(u))
                txt += "unicode?"
        return txt