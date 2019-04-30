This is the SOCRATES model forked at BIRA-IASB, version 07.18.01
This software is licensed according to the terms in ../LICENSE.txt 

SOCRATES is a 2-D Global Chemistry-Climate Model (GCCM)
covering -85 degrees South to 85 degrees North by 5 degrees latitude
and from surface to 120km log-p altitude.
It was developed originally at NCAR (PI: Guy Brasseur ; gpbrasseur@gmail.com ).

This is a forked version developed at BIRA-IASB by 
simon.chabrillat@aeronomie.be to study the Mesosphere - Lower Thermosphere.
There are many pubications related to this model, see ./readme/README_science.txt

#####################################    WARNING    ############################################
This 2D model was used to study the middle atmosphere, i.e. in approx altitude range 20km-110km.
Do not expect useful results :
- for altitudes below ~30 km or above ~100km 
- in day-to-day variations 
- in longitude variations (there are none)
- if you study tidal processes
- if you study polar ozone depletion processes
Variations with solar local time are resolved only as far as they're driven by photochemistry.
##############################################################################################

Contents of this directory:
---------------------------

./data_in/  : input data files to run the model, including a sample set of initial conditions
              data_in/v7s11_1996-12-17.save.nc

./data_out/ : suggested directory to write output files of socrates runs;
              originally contains ./data_out/SAMPLE/ with some output from the SAMPLE run
              and ./data_out/test/ to write output of first test run

./namelists/ : suggested directory to store the input namelist (ASCII) files which provide 
               the settings for eacg socrates run
               
./readme/    : documentation; you should carefully read ./readme/README_usage.txt.and 
               ./readme/README_science.txt . The subdirectories contain old information for
               internal purposes and are partly obsolete
               
./src/    : the actual source code of the model, in fortran, with a Makefile
            ./src/preproc/ is the pre-processor which generated the sources for the chemical solver
                           (probably useless as it was not used or maintained since 1999).

./tools/  : some auxiliary tools. They are not documented and may be obsolete
               
./view2d/ : a large graphical application written in the IDL language to visualize the netcdf
            output of SOCRATES
