#! /bin/bash

# Bitsavers
wget --mirror --convert-links --backup-converted --html-extension \
  http://bitsavers.trailing-edge.com/pdf/univOfIllinoisUrbana/illiac/ILLIAC/

# Nash from Archive.org
aria2c https://archive.org/download/illiacprogrammin00univ/illiacprogrammin00univ_archive.torrent

# Fosdick from Archive.org
aria2c https://archive.org/download/guidetoilliacpro00fosd/guidetoilliacpro00fosd_archive.torrent
