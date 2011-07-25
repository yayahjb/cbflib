# cbf_tkfiledialog.py
#
# wrapper for tkfiledialog.py modified to
# read or write a cbf
#
# copyright (c) 2011 Herbert J. Bernstein
#
# for incorporation into the CBFlib package API
#
######################################################################
#                                                                    #
# YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL #
#                                                                    #
# ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  #
# OF THE LGPL                                                        #
#                                                                    #
######################################################################

########################### GPL NOTICES ##############################
#                                                                    #
# This program is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU General Public License as     #
# published by the Free Software Foundation; either version 2 of     #
# (the License, or (at your option) any later version.               #
#                                                                    #
# This program is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      #
# GNU General Public License for more details.                       #
#                                                                    #
# You should have received a copy of the GNU General Public License  #
# along with this program; if not, write to the Free Software        #
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           #
# 02111-1307  USA                                                    #
#                                                                    #
######################################################################

######################### LGPL NOTICES ###############################
#                                                                    #
# This library is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU Lesser General Public         #
# License as published by the Free Software Foundation; either       #
# version 2.1 of the License, or (at your option) any later version. #
#                                                                    #
# This library is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  #
# Lesser General Public License for more details.                    #
#                                                                    #
# You should have received a copy of the GNU Lesser General Public   #
# License along with this library; if not, write to the Free         #
# Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    #
# MA  02110-1301  USA                                                #
#                                                                    #
######################################################################



import pycbf
try:
  import Tkinter as TKINTER
  import tkFileDialog as TKFILEDIALOG
except ImportError:
  import tkinter as TKINTER
  import tkinter.filedialog as TKFILEDIALOG
import thread
import time

parent = None

def dummyparent(x,y):
    global parent
    parent = TKINTER.Tk()
    width = parent.winfo_screenwidth()
    height = parent.winfo_screenheight()
    parent.geometry("%dx%d+%d+%d" % (width,height,x,y))
    parent.withdraw()
    parent.mainloop()

def save_cbf(cbf = None, file_extensions=None, x=100, y=100, title="Save CBF", maxfiles=20, \
             mode="wide", flags=pycbf.MSG_DIGEST, encoding=0, root=None):
    """
        cbf_tkfiledialog.save_cbf

        Use an askopenfilename dialog box to write an existing
        cbf into the specified file.  An exception is raised
        if the cbf is None and None is returned.  If the write
        is successful, the cbf is returned.

        file_extensions is either None, to use the default list
        of extensions ["*.cbf", "*.cif", "*.dic", "*.txt", "*.*"],
        or a specific list of extensions to use.  The actual
        extension chosen determines whether a CBF or CIF is written.

        (x,y) are for compatibility with the vpython version of
        this call.  In this TK version, they are only used
        as top left corner offsets for a dummy root window if
        none is specified.

        title is the dialog box title.

        maxfiles is for compatibility with the vpython version of
        this call and is not used in this TK version.

        mode is either "wide" or "narrow" to specify writing 2048
        character line length files or 80 character line length
        files

        flags is the bit-wise or of any flags needed for the
        read.  The default is pycbf.MSG_DIGEST

        encoding is the bit-wise or of any encoding flags to
        be used for writing binary sections.

        root is the parent window for the dialog window.  If None
        then an invisible root window is created and destroyed.

        
    """

    if cbf == None:
        raise ValueError("To save a cbf, the cbf must be created first.")
    if maxfiles < 5: maxfiles = 5
    if mode != "wide" and mode != "narrow" :
        raise ValueError("To save a file, mode must be wide or narrow.")
    defaultextension = ".cbf"
    if file_extensions is not None:
        if not isinstance(file_extensions, (list,tuple)):
            file_extensions = [file_extensions]
            defaultextension = file_extensions
        filetypes = []
        for filetype in file_extensions:
            if filetype[0] != "*" and filetype[0] != ".": filetype="."+filetype
            if filetype[0] != "*": filetype = "*"+filetype
            description = ""
            if filetype.lower() == "*.cbf": description = "Crystallographic Binary Files"
            if filetype.lower() == "*.cif": description = "Crystallographic Information Files"            
            if filetype.lower() == "*.dic": description = "Crystallographic Information File Dictionaries"            
            if filetype.lower() == "*.txt": description = "Text Files"            
            if filetype.lower() == "*.*": description = "All Files"            
            filetypes.append((description,filetype))
    else:
        filetypes = [("Crystallographic Binary Files","*.cbf"),\
                    ("Crystallographic Information Files","*.cif"),\
                    ("Crystallographic Information File Dictionaries","*.dic"),\
                    ("Text files","*.txt"), \
                    ("All files","*.*")]
    if cbf == None: return None
    parent = root
    if root == None:
        thread.start_new_thread(dummyparent,(x,y))
        while parent==None: time.sleep(1)
    filename = TKFILEDIALOG.asksaveasfilename(defaultextension=defaultextension, \
            title=title, \
            filetypes = [("Crystallographic Binary Files","*.cbf"),\
                    ("Crystallographic Information Files","*.cif"),\
                    ("Crystallographic Information File Dictionaries","*.dic"),\
                    ("Text files","*.txt"), \
                    ("All files","*.*")])
    if root == None:
        parent.destroy()
    if filename == '':
        return None
    if filename[-1] == '|' or filename[-1] == ' ':
        filename = filename[:-1]
    if filename == '':
        return None
    t = filename.split(".")
    ext = ''
    if len(t) > 0:
        ext = '.'+t[-1]
    elif t[0] == '':
        return None
    if file_extensions is not None:
        if ext.lower() != (file_extensions[0]).lower:
            filename += file_extensions[0]
            ext = file_extensions[0];
    ciforcbf = pycbf.CBF
    if ext.lower() == ".cif" or ext.lower() == ".dic" or ext.lower() == ".txt":
        ciforcbf = pycbf.CIF
    try:
        if mode.lower() == "wide":
            cbf.write_widefile(0,ciforcbf,flags,encoding)
        else:
            cbf.write_file(0,ciforcbf,flags,encoding)
        menus.visible = 0
        del menus
        currentdisplay.select()
        return cbf
    except:
        return None


def get_cbf(cbf = None, file_extensions=None, x=100, y=100, title="Open CBF", maxfiles=20, \
           mode='wide', flags=pycbf.MSG_DIGEST, root = None):
    """
        cbf_tkfiledialog.get_cbf

        Use an askopenfilename dialog box to read the selected
        file into a new cbf (if cbf==None), or into an existing
        cbf.  In either case the cbf object into which the read
        was done is returned.  It there is an error None is
        returned.

        file_extensions is either None, to use the default list
        of extensions ["*.cbf", "*.cif", "*.dic", "*.txt", "*.*"],
        or a specific list of extensions to use.

        (x,y) are for compatibility with the vpython version of
        this call.  In this TK version, they are only used
        as top left corner offsets for a dummy root window if
        none is specified.

        title is the dialog box title.

        maxfiles is for compatibility with the vpython version of
        this call and is not used in this TK version.

        mode is either "wide" or "narrow" to specify reading 2048
        character line length files or 80 character line length
        files

        flags is the bit-wise or of any flags needed for the
        read.  The default is pycbf.MSG_DIGEST

        root is the parent window for the dialog window.  If None
        then an invisible root window is created and destroyed.

        
    """
    global parent
    if maxfiles < 5: maxfiles = 5
    if mode != "wide" and mode != "narrow" :
        raise ValueError("To read a file, mode must be wide or narrow.")
    if file_extensions is not None:
        if not isinstance(file_extensions, (list,tuple)):
            file_extensions = [file_extensions]
        filetypes = []
        for filetype in file_extensions:
            if filetype[0] != "*" and filetype[0] != ".": filetype="."+filetype
            if filetype[0] != "*": filetype = "*"+filetype
            description = ""
            if filetype.lower() == "*.cbf": description = "Crystallographic Binary Files"
            if filetype.lower() == "*.cif": description = "Crystallographic Information Files"            
            if filetype.lower() == "*.dic": description = "Crystallographic Information File Dictionaries"            
            if filetype.lower() == "*.txt": description = "Text Files"            
            if filetype.lower() == "*.*": description = "All Files"            
            filetypes.append((description,filetype))
    else:
        filetypes = [("Crystallographic Binary Files","*.cbf"),\
                    ("Crystallographic Information Files","*.cif"),\
                    ("Crystallographic Information File Dictionaries","*.dic"),\
                    ("Text files","*.txt"), \
                    ("All files","*.*")]
    parent = root
    if root == None:
        thread.start_new_thread(dummyparent,(x,y))
        while parent==None: time.sleep(1)
    else:
        parent = root
    filename = TKFILEDIALOG.askopenfilename( \
            title=title, \
            filetypes = filetypes, \
            parent = parent)
    if root == None:
        parent.destroy()
    if filename is None:
        return None
    if cbf == None:
      cbf = pycbf.cbf_handle_struct()
    try:
      if mode=="wide":
        cbf.read_widefile(str(filename),flags)
      else:
        cbf.read_file(str(filename),flags)
      return cbf
    except:
      return None
