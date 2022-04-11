# cbf_axis_test.py
# simple program to open a CBF and examine
# the AXIS category information

import sys
import pycbf


def get_axis_chain(axis_table,axisname):
    """ get_axis_chain
        axis_table is a dictionary indexed by axis names
        each row of axis_table is a dictionary with the
        following keys:
           "id":  the axis name
           "type": the axis type:
                      "general"
                      "rotation"
                      "translation"
           "equipment": the particular equipment supported
           "depends_on":  the axis on which this one depends
           "vector": the vector in the form [x,y,z]
           "offset": the offset in the form [x,y,z]
               For both the vector and offset, if the values
               are not "." they are converted to numbers

        axisname is the axis for which the chain is needed

        the return is a dictionary indexed by the position
            in the chain working from the axisname argument
            down.  The final "." is not included
        
    """
    axis_chain = {}
    chainlength = 0
    nextaxis = axisname;
    while nextaxis != "." and nextaxis in axis_table:
        axis_row = axis_table[nextaxis]
        axis_chain[chainlength] = nextaxis
        nextaxis = axis_row["depends_on"]
        chainlength = chainlength+1
    return axis_chain
        
    

def get_table_entry(xcbf,xcol):
    """ get_table_entry
        xcbf is a cbf_handle_struct (a CBF)
        xcol is the column for which a value is desired
    """
    try:
        xcbf.find_column(xcol)
        xtemp = xcbf.get_value()
        return xtemp
    except:
        return "."

object = pycbf.cbf_handle_struct()
try:
    cbf = sys.argv[1]
    object.read_file(cbf,pycbf.MSG_DIGEST)
    foundfile = 1
except: 
    foundfile = 0
#
#  Minimal file open logic depending on the user
#  to know the exact path
#
#  Exit on an empty string
#
while foundfile == 0:
    cbf = raw_input("path to cbf: ")
    if cbf == "":
        foundfile = -1
    else:
      try:
        object.read_file(cbf,pycbf.MSG_DIGEST)
        foundfile = 1
      except:
        print("Unable to open ",cbf)

if foundfile == -1:
    sys.exit(0)
object.rewind_datablock()
try:
    object.find_category("axis")
except:
    print("No axis category found")
    sys.exit()
axis_table = {}
axis_objects = {}
base_axis_table = {}
dependent_table = {}
equipment_table = {}

axis_rows = object.count_rows()
print(axis_rows, "rows in axis table")
if axis_rows < 1:
    print("Axis category has no rows")
    sys.exit()
for jrow in range(axis_rows):
    object.rewind_column()
    # print("jrow: ", jrow)
    object.select_row(jrow)
    temp_dictionary = {}
    id = get_table_entry(object,"id")
    temp_dictionary["id"] = id
    temp_dictionary["type"] = get_table_entry(object,"type")
    equipment = get_table_entry(object,"equipment")
    if equipment in equipment_table:
        equipment_table[equipment].append(id)
    else:
        equipment_table[equipment] = [id]
    temp_dictionary["equipment"] = equipment
    depends_on = get_table_entry(object,"depends_on")
    temp_dictionary["depends_on"] = depends_on
    if depends_on in dependent_table:
        dependent_table[depends_on].append(id)
    else:
        dependent_table[depends_on] = [id]
    vector_1 = get_table_entry(object,"vector[1]")
    vector_2 = get_table_entry(object,"vector[2]")
    vector_3 = get_table_entry(object,"vector[3]")
    offset_1 = get_table_entry(object,"offset[1]")
    offset_2 = get_table_entry(object,"offset[2]")
    offset_3 = get_table_entry(object,"offset[3]")
    if offset_1 != ".":
        offset_1 = float(offset_1)
    if offset_2 != ".":
        offset_2 = float(offset_2)
    if offset_3 != ".":
        offset_3 = float(offset_3)
    if vector_1 != ".":
        vector_1 = float(vector_1)
    if vector_2 != ".":
        vector_2 = float(vector_2)
    if vector_3 != ".":
        vector_3 = float(vector_3)

    temp_dictionary["vector"] = [vector_1,vector_2,vector_3]
    temp_dictionary["offset"] = [offset_1,offset_2,offset_3]
    axis_table[temp_dictionary["id"]] = temp_dictionary
    base_axis_table[temp_dictionary["id"]] = temp_dictionary["depends_on"]
        

#print("axis table: ")
for key in axis_table.keys():
    print(key+": vector: ",  axis_table[key]["vector"],", offset: ", axis_table[key]["offset"])
    print("axis chain:", get_axis_chain(axis_table,key))
