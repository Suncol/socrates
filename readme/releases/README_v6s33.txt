               General introduction to SOCRATES v6s33
               --------------------------------------
               
                                Simon Chabrillat (simonc@oma.be), Nov. 2001
                             
To install:

> gunzip v6s33.tar.gz
> tar -xvf v6s33.tar

You need to ftp the initial conditions netCDF file save.9.28.1995.nc and
move it to the data directory:

> cd v6s33/data
> ftp ftp.oma.be
ftp> cd dist/simonc/socrates
ftp> get save.9.28.1995.nc
ftp> quit

Here is what you get:

README/chem_tsteps.txt -> info on the variables, modules and files relative to 
                          the tunable chemical timesteps
README/preproc.txt     -> how to use the chemical preprocessor (socpp)
README/add_species.tx  -> how to use the chemical preprocessor (socpp)if one 
                          wants to add a species (and eventually a 
                          photodissociation process)
README/tech_note.txt   -> info on how to use the SOCRATES model. 
                          Slightly outdated, but still usefull.
README/myversions.txt  -> most important differences between my versions
                          and versions by Rashid Khoshravi versions 
                          (rashid@acd.ucar.edu)
                          
src/*.f       -> source code for the SOCRATES model
src/Makefile* -> to compile the model
data/*        -> input data file. Place the initial conditions netCDF file
                 save.9.28.1995.nc here
preproc/*     -> input files and source code 
view2d/*      -> view2d 4.6, a post-processing graphical application written
                 with IDL to browse the netCDF output files

Scientific developements and results related to this model are in my PhD thesis:
"Modélisation du changemenent global dans l'atmosphère moyenne"
available at 
ftp://ftp.oma.be/dist/simonc/thesis.pdf
It is written in french, but all figures are annotated in english.

This code is intended *only* for Guy Brasseur's team at the max Planck 
Institute for Meteorology in Hamburg. Please do *not* forward this code to
external collaborators - give them my email adress (simonc@oma.be), 
they can contact me.

Good luck!
