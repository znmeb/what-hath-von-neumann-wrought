#! /bin/bash
#
# Copyright (C) 2016 M. Edward (Ed) Borasky <znmeb@znmeb.net>
# License: MIT

source ~/jupyter/bin/activate
source ~/.profile
jupyter nbconvert reveal.ipynb --to slides --post serve
