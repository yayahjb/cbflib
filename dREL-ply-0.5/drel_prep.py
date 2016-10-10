#!/usr/bin/python

import sys

class Process:
	def execute_method(self):
		valuename = sys.argv[1]
		editname = valuename + '_local'
		print valuename
		print editname
		f = open("cbf_data_debug", "r")
		data = f.readline()
		str_list = []

		while data:
			data = f.readline()
			print data
			str = data.replace(valuename, editname)
			print str
			str_list.append(str)
		
		str = "".join(str_list)
		fout = open("cbf_data_debug_changed", "w")
		print>>fout, str
p = Process()
p.execute_method()
