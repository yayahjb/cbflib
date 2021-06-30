
nuweb pycbf
latex pycbf
nuweb pycbf
latex pycbf
dvipdfm pycbf
nuweb pycbf
C:\python24\python make_pycbf.py > TODO.txt
"C:\program files\swigwin-1.3.31\swig.exe" -python pycbf.i
C:\python24\python setup.py build --compiler=mingw32
copy build\lib.win32-2.4\_pycbf.pyd .
REM C:\python24\python pycbf_test1.py
C:\python24\python pycbf_test2.py
C:\python24\python pycbf_test3.py
C:\python24\lib\pydoc.py -w pycbf
C:\python24\python makeflatascii.py pycbf_ascii_help.txt
