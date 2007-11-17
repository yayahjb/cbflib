
import pydoc, pycbf, sys
f = open(sys.argv[1],"w")
pydoc.pager=lambda text: f.write(text)
pydoc.TextDoc.bold = lambda self,text : text
pydoc.help(pycbf)
