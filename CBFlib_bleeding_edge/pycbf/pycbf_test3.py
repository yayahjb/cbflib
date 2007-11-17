
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
if __name__=="__main__":
    unittest.main()

