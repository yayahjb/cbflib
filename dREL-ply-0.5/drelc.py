#!/usr/bin/python

import drel_lex
import drel_yacc
import sys
import CifFile 
import StarFile

class Process:
	def execute_method(self):
		valuename = sys.argv[1]
		#print "Valuename: %s" % valuename 
		datablock_name = sys.argv[2]
		#print "Datablock: %s" % datablock_name
		#dictionary = sys.argv[3]
		#cbf_handle = sys.argv[3]
		#print "CBF handle: %s" % cbf_handle

		#create our lexer and parser
	      	self.lexer = drel_lex.lexer
      		self.parser = drel_yacc.parser
       		#use a simple dictionary
       		testdic = CifFile.CifDic("cif_expanded.dic", grammar='DDLm')
       		self.testblock = CifFile.CifFile("cbf_data_debug") [datablock_name]
                self.testblock.assign_dictionary(testdic)
                realres = self.testblock[valuename]
       		#create the global namespace
		print "Generated value: %s" % realres
		fout = open("method_output", 'w')
		print>>fout, realres

		#method returns realres as the value that would be missing

		#for validation
		#failUnless(realres == value)
	
p = Process()
p.execute_method()
