#!/usr/bin/python

import drel_lex
import drel_yacc
import sys
import CifFile 
import StarFile

class Process:
	def execute_method(self):
		#create our lexer and parser
	      	self.lexer = drel_lex.lexer
      		self.parser = drel_yacc.parser
       		#use a simple dictionary
       		self.testdic = CifFile.CifDic("cif_short.dic", "DDLm")
       		self.testblock = CifFile.CifFile("c2ctest5.cif") ["c2ctest5"]
       		#create the global namespace
       		self.namespace = self.testblock.keys()
       		self.namespace = dict(map(None,self.namespace,self.namespace))
       		self.parser.loopable_cats = ["import"]
		self.parser.special_id = [self.namespace]
       		self.parser.withtable = {}
       		self.parser.target_id = None
       		self.parser.indent = ""
		#get valuename from cmdline
		valuename = sys.argv[1]

		f = open("method_expression")
		expression = f.readline()
		str_list = []
		str_list.append(expression)

		while expression:
			expression = f.readline()
			str_list.append(expression)
		
		expression = "".join(str_list)
		
		self.parser.target_id = valuename
		res = self.parser.parse(expression + "\n", lexer=self.lexer)
		realfunc = drel_yacc.make_func(res, "myfunc", valuename)
		print "Compiling dREL....."
		exec realfunc
		realres = myfunc(self.testdic,self.testblock)
		print "Generated value: %s" % realres
		fout = open("method_output", 'w')
		print>>fout, realres

		#method returns realres as the value that would be missing

		#for validation
		#failUnless(realres == value)
	
p = Process()
p.execute_method()
