##-----------------------------------------------------------------------------
##  First character of this file is # : it is a C-shell script
##  It works with the C-shell (launched by /bin/csh from command line).
##-----------------------------------------------------------------------------

set echo
pwd
set old = "v7s04e"
set new = "v7s04s"
set srcdir = "/home/simonc/socrates/vgz"
set tmpdir = "/tmp/simonc/ascii/temp"
set rundir = "/home/simonc/socrates/tools"

mkdir $tmpdir
cd $tmpdir
mkdir $old
mkdir $new
cd $old
cp $srcdir/$old.tar.gz .
gunzip $old.tar.gz
tar -xf $old.tar
rm fort.* diag.out socrates.acc socrates.out
rm preproc/*.f preproc/wrk/*.F
cd ../$new
cp $srcdir/$new.tar.gz .
gunzip $new.tar.gz
tar -xf $new.tar
rm fort.* diag.out socrates.acc socrates.out
rm preproc/*.f preproc/wrk/*.F
cd $tmpdir
diff -rsiw $old $new > $rundir/diff_$old.$new

cd $rundir
rm -fr $tmpdir

