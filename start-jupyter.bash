#! /bin/bash -v
#
# Copyright (C) 2016 M. Edward (Ed) Borasky <znmeb@znmeb.net>
# License: MIT

source ~/jupyter/bin/activate
source ~/.profile
ipcluster start &
jupyter notebook --port=8888 --ip=0.0.0.0 --no-browser
