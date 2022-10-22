#!/usr/bin/env python
# -*- coding: utf-8 -*-



import FileMrw



class FileMrwModulef(FileMrw.FileMrw):

    def __init__(self, callback=None):
        FileMrw.FileMrw.__init__(self, callback)
        self.numElements = []
        self.numNodes = []
        self.numVertex = []

        self.dim = None
        self.lnn = None
        self.lnv = None
        self.lne = None
        self.lnf = None

        self.mm = []
        self.nrc = []
        self.nra = []
        self.nrv = []
        self.z = []
        self.nsd = []

    def GetValues(self):
        print('self.numElements ' + str((self.numElements)))
        print('self.numNodes ' + str((self.numNodes)))
        print('self.numVertex ' + str((self.numVertex)))
        print('self.mm ' + str(len(self.mm)))
        print('self.nrc ' + str(len(self.nrc)))
        print('self.nra ' + str(len(self.nra)))
        print('self.nrv ' + str(len(self.nrv)))
        print('self.z ' + str(len(self.z)))
        print('self.nsd ' + str(len(self.nsd)))
            
        
    def write1dArray(self,array,openfile):
        if (len(array) == 0):
            return -1
        for x in array:
            openfile.write(str(x) + ' ')
        openfile.write('\n')
        return 0

    
    def save2(self,openfile):

        info = [self.numElements,self.numNodes,self.numVertex,self.dim,self.lnn,self.lnv,self.lne,self.lnf]
        if self.write1dArray(info,openfile) == 0: pass#print 'escribe nel,nnod,nver,dim,lnn,lnv,lne,lnf'
        if self.write1dArray(self.mm,openfile) == 0: pass#print 'escribe mm'
        if self.write1dArray(self.nrc,openfile) == 0: pass#print 'escribe nrc'
        if self.write1dArray(self.nra,openfile) == 0: pass#print 'escribe nra'
        if self.write1dArray(self.nrv,openfile) == 0: pass#print 'escribe nrv'
        if self.write1dArray(self.z,openfile) == 0: pass#print 'escribe z'
        if self.write1dArray(self.nsd,openfile) == 0: pass#print 'escribe nsd'

        return True

