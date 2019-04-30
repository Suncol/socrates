##-----------------------------------------------------------------------------
##  First character of this file is # : it is a C-shell script
##  It works with the C-shell (launched by /bin/csh from command line).
##-----------------------------------------------------------------------------

set echo
set label = "v6s09"
set addpath = "/tmp/simonc/v6s09/"
set addfile = "dvout.dat"
cd /tmp/simonc/ascii/
cp -p $addpath$addfile .
gunzip /tmp/simonc/ascii/$label.tar.gz
tar -uf  $label.tar $addfile
gzip $label.tar
rm $addfile
