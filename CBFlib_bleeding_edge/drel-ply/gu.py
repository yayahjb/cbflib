#!/usr/bin/python

import drel_lex
import drel_yacc
import sys

class Process:
	def execute_method(self):
		self.lexer = drel_lex.lexer
		self.parser = drel_yacc.parser
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

		res = self.parser.parse(expression + "\n", lexer=self.lexer)
		realfunc = drel_yacc.make_func(res, "myfunc", valuename, have_sn=False)
		exec realfunc
		realres = myfunc(self,self)
		fout = open("method_output", 'w')
		print>>fout, realres

		#method returns realres as the value that would be missing

		#for validation
		#failUnless(realres == value)

p = Process()
p.execute_method()
