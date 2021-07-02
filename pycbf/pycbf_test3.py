
import pycbf, unittest
class GenericTests(unittest.TestCase):

    def test_get_local_integer_byte_order(self):
        #print(bytes(pycbf.get_local_integer_byte_order()))
        self.assertEqual( bytes(pycbf.get_local_integer_byte_order()),
                         bytes(b'little_endian'))

    def test_get_local_real_byte_order(self):
        #print(bytes(pycbf.get_local_real_byte_order()))
        self.assertEqual( bytes(pycbf.get_local_real_byte_order()),
                          bytes(b'little_endian'))

    def test_get_local_real_format(self):
        #print(bytes(pycbf.get_local_real_format()))
        self.assertEqual( bytes(pycbf.get_local_real_format()), 
                          bytes(b'ieee 754-1985'))

    def test_compute_cell_volume(self):
        #print(pycbf.compute_cell_volume((2.,3.,4.,90.,90.,90.)))
        self.assertEqual( pycbf.compute_cell_volume((2.,3.,4.,90.,90.,90.)),
                           24.0)
if __name__=="__main__":
    unittest.main()

