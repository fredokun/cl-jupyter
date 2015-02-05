#! /bin/sh

ipython3 console --existing --transport="tcp" --ip="127.0.0.1" --shell=40000 --iopub=40001 --hb=40002 --no-secure "--Session.key=b''"
