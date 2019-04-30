##-----------------------------------------------------------------------------
##  First character of this file is # : it is a C-shell script
##  It works with the C-shell (launched by /bin/csh from command line).
##-----------------------------------------------------------------------------

set echo
set name = "v1.8c"
set workdir = "/home/simonc/socrates"
mkdir $workdir/$name
cd $workdir/$name
pwd
uncompress -c $workdir/$name.tar.Z > $name.tar
tar -xf $name.tar
rm $name.tar
