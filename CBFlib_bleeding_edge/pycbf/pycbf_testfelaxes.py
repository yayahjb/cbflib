import pycbf, sys

image_file = sys.argv[1]

cbf = pycbf.cbf_handle_struct()
cbf.read_widefile(image_file, pycbf.MSG_DIGEST)

for element in range(64):
    d = cbf.construct_detector(element)
    print "element:", element

    v00 = d.get_pixel_coordinates(0, 0)
    v01 = d.get_pixel_coordinates(0, 1)
    v10 = d.get_pixel_coordinates(1, 0)
    v11 = d.get_pixel_coordinates(1, 1)

    print (0, 0), "v00", v00
    print (0, 1), "v11", v01
    print (1, 0), "v10", v10
    print (1, 1), "v11", v11

    print "surface axes:",  d.get_detector_surface_axes(0), d.get_detector_surface_axes(1)

    print d.get_detector_surface_axes(0), "has", cbf.count_axis_ancestors(d.get_detector_surface_axes(0)), "ancestors"
    print d.get_detector_surface_axes(1), "has", cbf.count_axis_ancestors(d.get_detector_surface_axes(1)), "ancestors"

    cur_axis = d.get_detector_surface_axes(0)
    count = cbf.count_axis_ancestors(cur_axis)

    for index in range(count):
        print "axis", cur_axis, "index: ", index
        print "    equipment", cbf.get_axis_equipment(cur_axis)
        print "    depends_on", cbf.get_axis_depends_on(cur_axis)
        print "    equipment_component", cbf.get_axis_equipment_component(cur_axis)
        print "    vector", cbf.get_axis_vector(cur_axis)
        print "    offset", cbf.get_axis_offset(cur_axis)
        print "    rotation", cbf.get_axis_rotation(cur_axis)
        print "    rotation_axis", cbf.get_axis_rotation_axis(cur_axis)
        cur_axis = cbf.get_axis_depends_on(cur_axis)


