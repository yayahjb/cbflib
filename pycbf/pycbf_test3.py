from __future__ import print_function

import pycbf, unittest
class GenericTests(unittest.TestCase):

    def test_get_local_integer_byte_order(self):
        self.assertEqual( pycbf.get_local_integer_byte_order(),
                          'little_endian')

    def test_get_local_real_byte_order(self):
        self.assertEqual( pycbf.get_local_real_byte_order() ,
                          'little_endian')

    def test_get_local_real_format(self):
        self.assertEqual( pycbf.get_local_real_format(),
                          'ieee 754-1985')

    def test_compute_cell_volume(self):
        self.assertEqual( pycbf.compute_cell_volume((2.,3.,4.,90.,90.,90.)),
                           24.0)
if __name__=="__main__":
    unittest.main()

