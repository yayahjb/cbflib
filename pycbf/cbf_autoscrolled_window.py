# cbf_autoscrolled_window.py
# utilities to create windows with a both a vertical
# scroll bar and a horizontal scroll bar with each scroll bar
# disappearing when the window is large enough
#
# copyright (c) 2011 Herbert J. Bernstein
# portions from
#    http://effbot.org/zone/tkinter-autoscrollbar.htm
#  Autohiding Scrollbars
#  August 08, 1998 | Fredrik Lundh
#
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



import sys
try:
  import Tkinter as TKINTER
  import tkFileDialog as TKFILEDIALOG
except ImportError:
  try:
    import tkinter as TKINTER
    import tkinter.filedialog as TKFILEDIALOG
  except ImportError:
    print("CBF: cbf_autoscrolled_window requires Tkinter")
    sys.exit(1)





#################################################################
#  The following autoscrollbar logic from               
#    http://effbot.org/zone/tkinter-autoscrollbar.htm
#  Autohiding Scrollbars
#  August 08, 1998 | Fredrik Lundh
#
class AutoScrollbar(TKINTER.Scrollbar):
    # a scrollbar that hides itself if it's not needed.  only
    # works if you use the grid geometry manager.

    __disabled = False
    
    def set(self, lo, hi):
        if self.__disabled or (float(lo) <= 0.0 and float(hi) >= 1.0):
            # grid_remove is currently missing from Tkinter!
            self.tk.call("grid", "remove", self)
        else:
            self.grid()
        TKINTER.Scrollbar.set(self, lo, hi)
    def pack(self, **kw):
        raise TclError, "cannot use pack with this widget"
    def place(self, **kw):
        raise TclError, "cannot use place with this widget"
#################################################################
    def disable(self):
        self.__disabled = True
        self.tk.call("grid", "remove", self)
    def enable(self):
        self.__disabled = False
        self.grid()

class cbf_autoscrolled_window(TKINTER.Frame):
    """ Class cbf_autoscrolled window returns an object
        giving a frame and a canvas that are have visible
        scroll bars when needed.

        Use:
          window=cbf_autoscrolled_window(rootwin)
          ... build scene in window.frame
          window.frame.update_idletasks()
          window.canvas.config(scrollregion=window.canvas.bbox(TKINTER.ALL))

          rootwin.mainloop()

        The frame is accessible as window.frame
        The canvas is accessible as window.canvas
    """

          
  
    def __cbf_create_autoscrolled_window(self,rootwin):
        rootwin.grid_columnconfigure(0,weight=1)
        rootwin.grid_columnconfigure(1,weight=0,minsize=8,pad=0)
        rootwin.grid_rowconfigure(0,weight=1)
        rootwin.grid_rowconfigure(1,weight=0,minsize=8,pad=0)
        self.__hsb = AutoScrollbar(rootwin,orient=TKINTER.HORIZONTAL)
        self.__hsb.grid(row=1,column=0, sticky="news")
        self.__vsb = AutoScrollbar(rootwin)
        self.__vsb.grid(row=0,column=1, sticky="news")
        self.canvas = TKINTER.Canvas(rootwin, xscrollcommand=self.__hsb.set, \
                            yscrollcommand=self.__vsb.set)
        self.__vsb.config(command=self.canvas.yview)
        self.__hsb.config(command=self.canvas.xview)
        self.canvas.grid(row=0,column=0,sticky="news")    
        self.frame = TKINTER.Frame(self.canvas)
        self.canvas.create_window(0, 0, anchor=TKINTER.NW, window=self.frame)
        self.frame.update_idletasks()
        self.canvas.config(scrollregion=self.canvas.bbox(TKINTER.ALL))
        self.frame.rowconfigure(1, weight=0, pad=0, minsize=10)
        self.frame.rowconfigure(0, weight=1, pad=0, minsize=8)
        self.frame.columnconfigure(1, weight=0, pad=0, minsize=10)
        self.frame.columnconfigure(0, weight=1, pad=0, minsize=8)


    def __init__(self,rootwin):
        self.root=rootwin
        self.canvas=None
        self.__hsb=None
        self.__vsb=None
        self.frame=None
        self.__cbf_create_autoscrolled_window(rootwin)

    def horizontal_scroll_disable(self):
        self.__hsb.disable()

    def horizontal_scroll_enable(self):
        self.__hsb.enable()
        
    def vertical_scroll_disable(self):
        self.__vsb.disable()
        
    def vertical_scroll_enable(self):
        self.__vsb.disable()


