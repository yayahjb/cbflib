#!/usr/bin/env python


import pycbf

# Some cbf helper functions - obj would be a cbf_handle_struct object

def writewavelength(obj,wavelength):
    obj.set_wavelength(float(wavelength))

def writecellpar(obj,cifname,value):
    obj.find_category("cell")
    obj.find_column(cifname)
    obj.set_value(value)

def writecell(obj,cell):
    """
    call with cell = (a,b,c,alpha,beta,gamma)
    """
    obj.find_category("cell")
    obj.find_column("length_a")
    obj.set_value(str(cell[0]))
    obj.find_column("length_b")
    obj.set_value(str(cell[1]))
    obj.find_column("length_c")
    obj.set_value(str(cell[2]))
    obj.find_column("angle_alpha")
    obj.set_value(str(cell[3]))
    obj.find_column("angle_beta")
    obj.set_value(str(cell[4]))
    obj.find_column("angle_gamma")
    obj.set_value(str(cell[5]))

def writeUB(obj,ub):
    """
    call with ub that can be indexed ub[i][j]
    """
    obj.find_category("diffrn_orient_matrix")
    for i in (1,2,3):
        for j in (1,2,3):
            obj.find_column("UB[%d][%d]"%(i,j))
            obj.set_value(str(ub[i-1][j-1]))
            
def writedistance(obj,distance):
    obj.set_axis_setting("DETECTOR_Z",float(distance),0.)
        

def writebeam_x_mm(obj,cen):
    obj.set_axis_setting("DETECTOR_X",float(cen),0.)

def writebeam_y_mm(obj,cen):
    obj.set_axis_setting("DETECTOR_Y",float(cen),0.)

def writeSPECcmd(obj,s):
    obj.find_category("diffrn_measurement")
    obj.find_column("details")
    obj.set_value(s)

def writeSPECscan(obj,s):
    obj.find_category("diffrn_scan")
    obj.find_column("id")
    obj.set_value("SCAN%s"%(s))
    obj.find_category("diffrn_scan_axis")
    obj.find_column("scan_id")
    obj.rewind_row()
    for i in range(obj.count_rows()):
        obj.select_row(i)
        obj.set_value("SCAN%s"%(s))
    obj.find_category("diffrn_scan_frame")
    obj.find_column("scan_id")
    obj.rewind_row()
    obj.set_value("SCAN%s"%(s))


def writepixelsize_y_mm(obj,s):
    """
    Units are mm for cif
    """
    # element number  = assume this is first and only detector
    element_number = 0
    # axis number = faster or slower... ? Need to check precedence ideally...
    obj.find_category("array_structure_list")
    obj.find_column("axis_set_id")
    obj.find_row("ELEMENT_Y")
    obj.find_column("precedence")
    axis_number = obj.get_integervalue()
    
    obj.set_pixel_size(element_number, axis_number, float(s) )
    
    obj.find_category("array_structure_list_axis")
    obj.find_column("axis_id")
    obj.find_row("ELEMENT_Y")
    obj.find_column("displacement")
    obj.set_doublevalue("%.6g",float(s)/2.0)
    obj.find_column("displacement_increment")
    obj.set_doublevalue("%.6g",float(s))

def writepixelsize_x_mm(obj,s):
    # element number  = assume this is first and only detector
    element_number = 0
    # axis number = faster or slower... ? Need to check precedence ideally...
    obj.find_category("array_structure_list")
    obj.find_column("axis_set_id")
    obj.find_row("ELEMENT_X")
    obj.find_column("precedence")
    axis_number = obj.get_integervalue()
    
    obj.set_pixel_size(element_number, axis_number, float(s) )
    
    obj.find_category("array_structure_list_axis")
    obj.find_column("axis_id")
    obj.find_row("ELEMENT_X")
    obj.find_column("displacement")
    obj.set_doublevalue("%.6g",float(s)/2.0)
    obj.find_column("displacement_increment")
    obj.set_doublevalue("%.6g",float(s))

def writeintegrationtime(obj,s):
    obj.find_category("diffrn_scan_frame")
    obj.find_column("integration_time")
    obj.set_value(str(s).replace("\000",""))

def writenfast(obj,s):
    obj.find_category("array_structure_list")
    obj.find_column("index")
    obj.find_row("1")
    obj.find_column("dimension")
    obj.set_value(str(s))

def writenslow(obj,s):
    obj.find_category("array_structure_list")
    obj.find_column("index")
    obj.find_row("2")
    obj.find_column("dimension")
    obj.set_value(str(s))


functiondict = {
    "lambda"   : writewavelength,
    "beam_x_mm"   : writebeam_x_mm,
    "beam_y_mm"   : writebeam_y_mm,
    "distance" : writedistance,
    "UB"       : writeUB,
    "cell"     : writecell,
    "cmd"      : writeSPECcmd,
    "scan"     : writeSPECscan,
    "nfast"    : writenfast,
    "nslow"    : writenslow,
    "pixelsize_y_mm" : writepixelsize_y_mm,
    "pixelsize_x_mm" : writepixelsize_x_mm,
    "integration_time_sec" : writeintegrationtime,
    "tth"      : lambda obj,value : obj.set_axis_setting(
                                "DETECTOR_TWO_THETA_VERTICAL",float(value),0.),
    "chi"      : lambda obj,value : obj.set_axis_setting(
                                     "GONIOMETER_CHI",float(value),0.),
    "th"       : lambda obj,value : obj.set_axis_setting(
                                     "GONIOMETER_THETA",float(value),0.),
    "phi"      : lambda obj,value : obj.set_axis_setting(
                                     "GONIOMETER_PHI",float(value),0.),
    "lc_a"     : lambda obj,value : writecellpar(obj,"length_a",value),
    "lc_b"     : lambda obj,value : writecellpar(obj,"length_b",value),
    "lc_c"     : lambda obj,value : writecellpar(obj,"length_c",value),
    "lc_al"    : lambda obj,value : writecellpar(obj,"angle_alpha",value),
    "lc_be"    : lambda obj,value : writecellpar(obj,"angle_beta",value),
    "lc_ga"    : lambda obj,value : writecellpar(obj,"angle_gamma",value)
    }

"""
    #
    # Not implementing these for now
    lc_ra
    lc_rc 0.4742
    lc_rb 1.16
    energy 13
    cp_phi -180
    alpha 7.3716
    lc_ral 90
    cp_tth -180
    lc_rga 90
    beta 17.572
    omega -2.185
    h 0.21539
    k 0.01957
    l 5.9763
    cp_chi -180
    lc_rbe 90
    cp_th -180
    azimuth 0
"""

# Finally a class for creating header files.
# It reads a template and then offers a processfile command 
# for running over a file series

class cifheader:
    
    def __init__(self,templatefile):
        self.cbf=pycbf.cbf_handle_struct()
        self.cbf.read_template(templatefile)
        from readmarheader import marheaderreader
        self.marheaderreader = marheaderreader()

        
    def processfile(self,filename, outfile=None,
                    format="mccd",
                    **kwds):
        outfile=outfile.replace(format,"cif")
        
        if format == "mccd":
            items = self.marheaderreader.get_header(filename)

        if format == "bruker":
            pass
        if format == "edf":
            pass
        
        self.items=items
        
        # Take the image header items as default
        self.updateitems(items)

        # Allow them to be overridden
        self.updateitems(kwds)

        # Write the file
        self.writefile(outfile)


        
    def writefile(self,filename):
        self.cbf.write_file(filename,pycbf.CIF,pycbf.MIME_HEADERS,
                            pycbf.ENC_BASE64)
        

    def updateitems(self,dict):
        names = dict.keys()
        for name in names:
            value = dict[name]
            # use a dictionary of functions
            if functiondict.has_key(name):
                # print "calling",functiondict[name],value
                apply(functiondict[name],(self.cbf,value))
            else:
                #print "ignoring",name,value
                pass

        
if __name__=="__main__":
    import sys
    
    obj=cifheader("xmas_cif_template.cif")

    ub = [[0.11, 0.12, 0.13] , [0.21, 0.22, 0.23], [0.31, 0.32, 0.33]]

    for filename in sys.argv[1:]:
        fileout = filename.split("/")[-1]
        obj.processfile(filename, outfile=fileout, UB=ub, distance=123.456)
