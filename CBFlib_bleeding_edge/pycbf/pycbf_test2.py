
import pycbf
obj = pycbf.cbf_handle_struct()
obj.read_file("../adscconverted.cbf",0)
obj.select_datablock(0)
g = obj.construct_goniometer()
print "Rotation axis is",g.get_rotation_axis()
d = obj.construct_detector(0)
print "Beam center is",d.get_beam_center()
