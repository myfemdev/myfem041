#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging


class FileMrw():



    def __init__(self, callback=None):
        self.callback = callback
        self.filename = None
        self.attribs = {}



    def read(self, filename, attribs):
        self.filename = filename
        self.attribs = attribs



    def cc(self, txt):
        if self.callback is not None:
            #self.callback(txt) #code prior version 0.0.1
            logging.debug(txt)



#    @staticmethod
#    def build(typename, callback=None):
#        if typename == 'scalars':
#            return FileMrwScalars.FileMrwScalars(callback)
#        elif typename == 'reconvxx':
#            return FileMrwReconvxx.FileMrwReconvxx(callback)
#        else:
#            return None


    # to-do: ponher o indice da palabra para cada linha

    @staticmethod
    def split(filename):
        tokens = []
        data = {}
        data['error'] = None
        data['data'] = tokens

        numl = 0
        numt = 0
        f = None
        try:
            f = open(filename, 'rt')
            for line in f:
                temp = line.split()
                tokens.extend(temp)
                numt += len(temp)
                numl += 1
        except IOError as x:
            data['error'] = repr(x)
        finally:
            if f is not None:
                f.close()
                f = None

        data['lines'] = numl
        data['words'] = numt

        return data


# file write open,close


    @staticmethod
    def read_ints(array, index, num):
        if len(array) < index + num: # adicional
            return False
        try:
            result = list(map(int,array[index:index+num]))
            return result
        except ValueError as e:
            return False
        return False



    @staticmethod
    def read_ints_1(array, index, num):
        if len(array) < index + num: # adicional
            return False
        try:
            result = list(map(lambda x: int(x)-1, array[index:index+num]))
            return result
        except ValueError as e:
            return False
        return False



    @staticmethod
    def read_floats(array, index, num):
        if len(array) < index + num: # adicional
            return False
        try:
            result = list(map(float, [s.replace('D','E') for s in array[index:index+num]]))
            return result
        except ValueError as e:
            return False
        return False



    @staticmethod
    def filewriteopen(filename):
        try:
            f = open(filename, 'wt')
        except IOError as e:
            pass
        else:
            return f
        return False



    @staticmethod
    def fileclose(f):
        try:
            f.close()
        except IOError as e:
            pass
        else:
            return True
        return False



    def save(self, filename):
        f = FileMrw.filewriteopen(filename)
        if f is False:
            return False
        res = self.save2(f)
        FileMrw.fileclose(f)
        return res



    def save2(self, openfile):
        return False



#import FileMrwScalars
#import FileMrwReconvxx
