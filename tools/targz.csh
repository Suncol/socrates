##-----------------------------------------------------------------------------
##  First character of this file is # : it is a C-shell script
##  It works with the C-shell (launched by /bin/csh from command line).
##-----------------------------------------------------------------------------

set echo
set label = "v6s30"
set run = "b"
set targetdir = "/home/simonc/socrates/vgz"
cd $home/socrates/$label
rm -f $targetdir/$label$run.tar $targetdir/$label$run.tar.gz
tar -cf $targetdir/$label$run.tar *.inp
tar -uf $targetdir/$label$run.tar *.csh
tar -uf $targetdir/$label$run.tar src/*.f
tar -uf $targetdir/$label$run.tar src/Make*
tar -uf $targetdir/$label$run.tar preproc/*
tar -uf $targetdir/$label$run.tar preproc/in/*
tar -uf $targetdir/$label$run.tar data/init_no_nc/*
mv data/*.nc .
tar -uf $targetdir/$label$run.tar data/*
mv *.nc data/.
gzip $targetdir/$label$run.tar
