
import pycbf
object = pycbf.cbf_handle_struct() # FIXME
object.read_file("../img2cif_packed.cif",pycbf.MSG_DIGEST)
object.rewind_datablock()
print "Found",object.count_datablocks(),"blocks"
object.select_datablock(0)
print "Zeroth is named",object.datablock_name()
object.rewind_category()
categories = object.count_categories()
for i in range(categories):
    print "Category:",i,
    object.select_category(i)
    category_name = object.category_name()
    print "Name:",category_name,
    rows=object.count_rows()
    print "Rows:",rows,
    cols = object.count_columns()
    print "Cols:",cols
    loop=1
    object.rewind_column()
    while loop is not 0:
        column_name = object.column_name()
        print "column name \"",column_name,"\"",
        try:
           object.next_column()
        except:
           break
    print
    for j in range(rows):
        object.select_row(j)
        object.rewind_column()
        print "row:",j
        for k in range(cols):
            name=object.column_name()
            print "col:",name,
            object.select_column(k)
            typeofvalue=object.get_typeofvalue()
            print "type:",typeofvalue
            if typeofvalue.find("bnry") > -1:
                print "Found the binary!!",
                s=object.get_integerarray_as_string()
                print type(s)
                print dir(s)
                print len(s)
                try:
                   import Numeric
                   d = Numeric.fromstring(s,Numeric.UInt32) 
                   # Hard wired Unsigned Int32
                   print d.shape
                   print d[0:10],d[d.shape[0]/2],d[-1]
                   d=Numeric.reshape(d,(2300,2300))
#                   from matplotlib import pylab
#                   pylab.imshow(d,vmin=0,vmax=1000)
#                   pylab.show()
                except ImportError:
                   print "You need to get Numeric and matplotlib to see the data"
            else:
                value=object.get_value()
                print "Val:",value,i
    print
del(object)
#
print dir()
#object.free_handle(handle) 
