#!/usr/bin/env python
# -*- coding: utf-8 -*-



import trees
import utils



class Parametrize():



    def __init__(self):
        self.params = {}
        self.paramsl = []
        self.comb = None



    def setp(self, params):
        self.params = params
        self.paramsl = []
        self.comb = None
        for node in self.params:
            self.paramsl.append([node,self.params[node]])



    def get_num_params(self):
        return len(self.params)



    def get_num_choices(self):
        num = 1
        for param in self.params:
            num *= len(self.params[param])
        return num



    def get_num_choices_i(self, i):
        return len(self.paramsl[i][1])



    def get_choices(self):
        if self.comb is None:
            self.comb = []
            self.for_each_choice_j(self.comb)
        return self.comb



    def for_each_choice_j(self, choices, indexes=[]):
        i = len(indexes)
        if i >= len(self.paramsl):
            return
        last = (i + 1 == len(self.paramsl))
        index = 0
        while index < len(self.paramsl[i][1]):

            indexes.append(index)

            if not last:
                self.for_each_choice_j(choices, indexes)
            else:
                choices.append(indexes[:])

            indexes.pop()

            index += 1



    def save_xml(self, filename):
        r = trees.ET.Element('parameters_file')
        tree = trees.ET.ElementTree(r)

        root = tree.getroot()

        parms = trees.ET.Element('parameters')
        parms.attrib['num'] = str(len(self.paramsl))

        p = 0
        while p < len(self.paramsl):
            param = self.paramsl[p]
            par = trees.ET.Element('parameter')
            par.attrib['name'] = param[0].get_path()
            par.attrib['num'] = str(len(param[1]))
            par.attrib['index'] = str(p)
            v = 0
            while v < len(param[1]):
                val = trees.ET.Element('value')
                val.attrib['index'] = str(v)
                val.text = param[1][v]
                par.append(val)
                v += 1
            parms.append(par)
            p += 1

        root.append(parms)

        choices = self.get_choices()

        c = trees.ET.Element('combinations')
        c.attrib['num'] = str(len(choices))

        cn = 0
        while cn < len(choices):
            ch = trees.ET.Element('combination')
            ch.attrib['index'] = str(cn)
#            ch.attrib['num'] = str(len(self.paramsl))
            p = 0
            while p < len(choices[cn]):
                v = trees.ET.Element('parameter')
                v.attrib['index'] = str(p)
                v.text = str(choices[cn][p])
                ch.append(v)
                p += 1
            n = trees.ET.Element('dirname')
            n.text = Parametrize.dirname(choices[cn])
            ch.append(n)
            c.append(ch)
            cn += 1
        root.append(c)


        utils.indent(root)

        tree.write(filename, "utf-8")



    @staticmethod
    def name(array):
        text = ''
        i = 0
        if i < len(array):
            text += str(array[i])
        i += 1
        while i < len(array):
            text += '_' + str(array[i])
            i += 1
        return text


    @staticmethod
    def dirname(array):
        name = Parametrize.name(array)
        return 'execdir_' + name



    def index_convert(self, index):
        return Parametrize.index_convert_st(index, self.paramsl)



    @staticmethod
    def index_convert_st(index, paramsl):
        res = {}
        i = 0
        for a in paramsl:
            struct = a[0]
            res[struct] = a[1][index[i]]
            i += 1
        return res

