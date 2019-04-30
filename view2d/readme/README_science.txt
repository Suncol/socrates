
  A BRIEF SUMMARY OF THE SOCRATES VERSIONS AND SOME BIBLIOGRAPHICAL REFERENCES 
  ============================================================================
                               
                                            Simon Chabrillat ( simonc@oma.be )
                                               last updated on 28 January 2016
1. Warning and credentials
__________________________

This software is licensed according to the terms in ../LICENSE.txt 

The SOCRATES model was developed until ~2005 with 
Guy Brasseur (gpbrassuer@gmail.com) as Principal Investigator.

The information given here reflects only my knowledge of the SOCRATES 
development history and is given for clarity to the users of "my" versions. 
Last email adresses that I have for the authors of the other branches :

- Rashid Khosravi, developed at NCAR/ACD :                rashid@ucar.edu
- Aleksandr Gruzdev, developed at MPI-Hamburg :           gruzdev@dkrz.de
  also works at the Obukhov Institute in Moscow :         a.n.gruzdev@mail.ru
- Simon Chabrillat (myself), NCAR/ACD then at BIRA-IASB : simonc@oma.be

2. The three development branches of SOCRATES
_____________________________________________

There are three lines of development for the SOCRATES model.
We could call them, by chronological order of publication :
1) the NCAR/ACD version. Reference:
  Khosravi et al., JGR vol. 107, No. D18, p.4358, doi:10.1029/2001JD001235, 2002
2) the BIRA-IASB versions, i.e. "my" versions. References:
  Chabrillat et al., GRL, vol.  29 p.1729, doi:10.1029/ 2002GL015309, 2002 ;
  section 2.1 in Kazil et al., vol. 108, p. 4432, doi:10.1029/2002JD003024, 2003 ;
  Chabrillat and Fonteyn, Advances in Space Research, Volume 32, Issue 9, pp.1689-1700, 2003
3) the Moscow/Hamburg version. Reference:
  Gruzdev and Brasseur, JGR vol.110, doi:10.1029/2003JD004410, 2005
  
In my personal opinion, the NCAR/ACD version (also called version 3) is the 
least advanced for modelling the MLT (Mesosphere and Lower Thermosphere, 
i.e. above ~55 km altitude). It was the first version published, and has the 
advantage of an extensive publication in JGR. 
Photochemistry is solved with 8 timesteps per day (if I remember well).
Instabilities in the numerical/physical treatment of the heat and transport 
budget led the developers to force the heat budget with a supplementary
relaxation term, which obliged the temperatures to converge to the CIRA 
climatology with a relaxation time of 50 days.
The model could be obtained at http://acd.ucar.edu/models/SOCRATES/
   (as of 2015/01/25, the page is still active but the download link is dead)


The BIRA-IASB versions have been optimized to study the MLT, especially the 
photochemical processes, and the radiative and chemical components of the heat budget:
1) The timestep for chemistry is constant, 20 minutes by default 
   (but 5 minutes recommended if CPU power allows it). 
2) The solution of the thermodynamic equation is stable in this version and does 
   not require any relaxation to climatological values.
   But I was not able to obtain realistic temperatures in the middle atmosphere,
   whatever the tuning of the Gravity Wave (GW) parameterization.
   This led me to prescribe the dynamical forcing by GW breaking as a function of
   latitude, altitude and season (see my thesis for full description).
   So this branch does not either have full coupling between dynamics and temperature.
3) In the latest version, the solar flux is scaled according to the daily 
   observations of Lyman-alpha and E10.7 between Feb 1947 and July 2002.
   This data was obtained from an early version of SOLAR2000
   ( contact: Kent Tobiska, ktobiska@spacenvironment.net, http://spacewx.com/ ).
   In this respect, the version distributed here *does*"take interannual variability 
   into account - but only w.r.t. the solar cycle, not the tropospheric dynamics...
   Before Feb 1947 and after July 2002, the solar cycle is modelled with a 
   sinus function.
4) The whole code is fortran90 code (but fixed-format, i.e. *.f) and 
   can run on several CPUs using OpenMP parallelization.
The model can be obtained at ftp://ftp.oma.be/dist/simonc/socrates/
but it is expected that new users will contact me before installing and using the 
model (see accompanying README_v7s17.txt) .
Regarding the version history, more details are given below.

The Moscow/Hamburg version has also been optimized to study the MLT, especially
the dynamical processes, and the diffusive component of the heat budget.
In this version, the dynamics/temperature coupling is finally solved "cleanly" :
"I have implemented into the model a number of corrections and improvements, 
concerning, in particular, with parameterization of the forcing of the meridional 
circulation by diffusion, transport of heat, treating of molecular thermal 
conductivity, decay of gravity waves above the turbopause. 
But the most important improvement is another numerical scheme for solution of 
the transport heat equation." 
Gruzdev, personal communication, November 2003
(i.e. the version used for the JGR paper could have been further improved !).
So this is the most sophisticated version for dynamics, but I do not know the
level of quality of the photochemistry module. I also think that since it has 
full coupling between dynamics and temperature, the corresponding outputs
(winds and temperature) should be checked more carefully for each simulation.

3. The published versions of the BIRA-IASB branch of SOCRATES
_____________________________________________________________

At BIRA-IASB, Simon Chabrillat developed intensively SOCRATES from 1998 to 2003.
The versions were numbered v[x]s[yy] until v7s18, which was published on a public
ftp site from 2009 untile January 2016.
On 26 January 2016, the source code was made public on Github with a release number
folowing the standard version numbering scheme. 
v7s18 became 07.18.00 with major version=07, minor version=18, build=00.

The most important versions are described in publications and briefly mentioned below.
All peer-reviewed papers published by Simon Chabrillat are available on 
- a public ftp directory: ftp://ftp-ae.oma.be/dist/from_Simon.Chabrillat/papers/
- ResearchGate: https://www.researchgate.net/profile/Simon_Chabrillat

Version 6 had chemistry and transport treated in separate time loops as in the NCAR's branch:
variable timestep duration for chemistry (shorter timesteps around sunrise and sunset)
and transport applied at noon (introducing discontinuities in time).
v6s32: Version described in my thesis ( ftp://.../papers/2001-Chabrillat_thesis.pdf )
v6s38: Version used for the GRL paper on molecular diffusion
                     ( ftp://...papers/2002-Chabrillat-etal_molecular-diffusion.pdf )

Version 7 introduced timesteps of constant duration (default 20 minutes) for the 
chemistry, with transport (advection) of the species divided into sub-timesteps of the 
same duration (NOTE: this is ony a numerical improvement; SOCRATES remains a 2D-model where
the winds are zonally averaged residual winds and remain constant during 24h!).
v7s09: Version described and used for the Adv. in Space Res. paper on long-term changes 
         of the MLT        ( ftp://.../papers/2003-Chabrillat-Fonteyn_MLT-changes.pdf )
v7s11: Version described and used for the JGR paper on the UBAIM model of the ionosphere.
       Section 2.1 of this paper is the best "peer-reviewed" description of the model 
       distributed here.            ( ftp://.../papers/2003-Kazil-etal_ions-UBAIM.pdf )
       
The last version - 07.18 - includes the output of monthly averaged fields in a separate 
netCDF file, a realistic forcing by the solar flux between 1947 and 2002 (see above) and 
Lyman-alpha parameters which finally reflect the values published in the 1997 GRL paper 
(no big impact expected from this update though).

The detailed version history is described in files v6s_diary.txt and v7s_diary.txt .
Of special note is a failed attempt (v7s13 & v7s14) to implement the major improvements 
to dynamics and heat following the version by Gruzdev and Brasseur. 
There is also a file "to-do_list.txt" which lists all desirable improvements to implement in
the model. Do not hesitate to do anything you like. Please keep me informed !


4. Bibliographical references for the BIRA-IASB branch of SOCRATES
__________________________________________________________________

Kazil et al. (2003) describe the version distributed here.
This paper, and most other papers which describe or use my versions of SOCRATES, are available at
                      ftp://ftp.oma.be/dist/from_Simon.Chabrillat/papers/                      
                      https://www.researchgate.net/profile/Simon_Chabrillat
                      
Please ask me ( simonc@oma.be ) to know which reference is best to cite. 
It depends on the content of your work.

@phdthesis{Chabrillat-2001,
   author    = {Chabrillat, Simon},
   title     = {Mod\'elisation du changement global dans l'atmosph\`ere moyenne},
   year      = {2001},
   address   = {ftp://ftp.oma.be/dist/simonc/thesis.pdf},
   school    = {Universit\'e Libre de Bruxelles},
   keywords  = {SOCRATES, mlt_change-model}
}


@article{Chabrillat/et-al-2002,
   author    = {Chabrillat, Simon and Kockarts, Gaston and Fonteyn, Dominique and
      Brasseur, Guy},
   title     = {Impact of molecular diffusion on the {CO}$_2$ distribution and
       the temperature in the mesosphere},
   journal   = grl,
   pages     = {10.1029/2002GL015309},
   year      = {2002},
   volume    = {29},
   number    = {15},
   keywords  = {moldiff}
}

@article{Chabrillat/Fonteyn-2003,
   author    = {Chabrillat, Simon and Fonteyn, Dominique},
   title     = {Modelling long-term changes of mesospheric temperature 
      and chemistry},
   journal   = asr,
   pages     = {1689--1700},
   year      = {2003},
   volume    = {32},
   number    = {9},
   issn = "0273-1177",
   doi = "10.1016/S0273-1177(03)90464-9",
   url = "http://www.sciencedirect.com/science/article/pii/S0273117703904649",
   keywords  = {mlt_change-model, SOCRATES-ref}
}

@article{Kazil/et-al-2003,
   author    = {Kazil, Jan and Kopp, Ernest and Chabrillat, Simon 
      and Bishop, James},
   title     = {The University of Bern Atmospheric Ion Model: Time-dependent 
      modeling of the ions in the mesosphere and lower thermosphere},
   journal   = {J. Geophys. Res.},
   pages     = {doi:10.1029/2001JD001276},
   year      = {2003},
   volume    = {108},
   number    = {D14},
   my-notes  = {have:paper-file-drafts},
   keywords  = {model2D, SOCRATES-ref}
}
