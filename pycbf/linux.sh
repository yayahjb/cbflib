
nuweb pycbf
latex pycbf
nuweb pycbf
latex pycbf
dvipdfm pycbf
nuweb pycbf
lynx -dump CBFlib.html > CBFlib.txt
python make_pycbf.py 
swig -python pycbf.i
python setup.py build 
rm _pycbf.so
cp build/lib.linux-i686-2.4/_pycbf.so .
python pycbf_test1.py
python pycbf_test2.py
pydoc -w pycbf
python makeflatascii.py pycbf_ascii_help.txt
