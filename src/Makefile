# Makefile to compile and link the SOCRATES model with Intel compiler and linker ifort

SRCS = imp_slv.f                         \
       main.f              chem.f              chemdr.f                    \
       absdep_pho.f        advect_chem.f       aero_init.f                 \
       aerosol_pho.f       aero_surf_area.f            \
       arbitrary_gw_dyn.f  ascii_output.f              \
       arch_plug.f         archive.f                   \
       asmith.gwave.dyn.f  atmcond.f                   \
       boundy.f            check.f             cst_boundy.f                \
       colden_pho.f        crscor_pho.f        describe_sim.f              \
       diags_chem.f        dvout_arch.f                \
       dyn_adam.f          dyn_balance.f               \
       dyn_bvfreq.f        dyn_diffus_heat.f   dyn_main.f                  \
       dyn_solve.f         dyn_u.f             dyn_wup.f                   \
       dynf_pwave.f        dynf_tide.f         dynf_total.f                \
       dynf_tropo.f        dynf_qbo.f	       dynf_qbo2.f            fomichev98.f                \
       gltime.f            h2o_special.f       hdiff.f                     \
       heat_coeff.f        heat_ircool.f               \
       heat_rates.f        hines97.gwave.dyn.f init_from_ascii.f           \
       interp_pho_chem.f   kyy_spec.f          kzz_spec.f                  \
       lya_srb_pho.f       minsch93_pho.f      mlynczak93.pho.f            \
       monthly_avg.f       msis_hwm_plug.f     opticp_pho.f                \
       pho_chem.f          pho_init.f          pho.f                       \
       plug_pho.f          pottemp.f         	qbo.f                       \
       rates_pho.f         read_save.f         save_arch.f                 \
       scadep_pho.f        set_arch.f                  \
       shap.f              sigcfc_pho.f                \
       solcycles.f         special_no_prod.f   sun_chem.f                  \
       surf_cover.f        tables_pho.f        tables_sun.f                \
       test_sun_utils.f    total_heat_rate.f   total_molec.f               \
       trends_boundy.f     troe.f              tropk.f                     \
       tropo_heat.f        twstr_pho.f         tau1_diags.pho.f            \
       usrrxt.f            vdiff.f             washout.f                   \
       socpp-imp_prod_loss.f    socpp-linmat.f    socpp-nlnmat.f   \
       socpp-soc_inti.f         socpp-lu_slv.f    socpp-setrxt.f   \
       socpp-adjrxt.f           socpp-second.f    socpp-lu_fac.f    

MODS = preprocessed.mod.o  controls.mod.o      dynam.mod.o     chem.mod.o      \
       heat.mod.o          pho.mod.o                 \
       wave2.mod.o         date_utils.mod.o    nf_new.mod.o    nf_utils.mod.o  \
       numerical.mod.o     ascii_utils.mod.o   sun_utils.mod.o fit_utils.mod.o \
       transform.mod.o     msis.mod.o          hwm.mod.o 
###---------------------------------------
### Compiling with the intel fortran compiler "ifort"
FC = ifort -c -r8 -O3 -qopenmp
#FC = ifort -c -r8 -O3
#####---------------------------------------
### Debugging when compiling with ifort:
# -g -check bounds -assume gfullpath -check bounds \
# -check overflow -check underflow -fpe0 -warn argument_checking
###---------------------------------------
### Compiling with the GNU fortran compiler "gfortran"
#FC = gfortran -c -fdefault-real-8 -O3 -fopenmp

src_obj = $(MODS) $(SRCS:.f=.o)

socrates : $(src_obj)
### Compiling source files with special needs (note ther are not listed in SRCS above)
#	ifort -c -r8 -O2 pottemp.f    # -O3 pottemp.f -> segmentation fault
#	ifort -c -r8 -O2 -qopenmp pottemp.f
###---------------------------------------
### Canonical linking with netCDF3, worked until ~2009:
#	ifort -o ../$@ $(src_obj) pottemp.o -L/usr/local/lib -lnetcdf -openmp
###---------------------------------------
### linking on kronos with ifort; last successful link on 2016/01/25; 
### ******NOTE **** as installed on kronos in 2016/01:
### netcdf3 is now in netcdf4 which requires curl, HDF5, and HDF4 (which istelf requires szip,jpeg,iomp5)
#	ifort -o ../$@ $(src_obj) pottemp.o  -qopenmp  \

	ifort -o ../$@ $(src_obj) -qopenmp \
		-Wl,--start-group /usr/local/netcdf4f/lib/libnetcdff.a /usr/local/netcdf4c/lib/libnetcdf.a -Wl,--end-group -lcurl \
		-Wl,--start-group /usr/local/hdf5/lib/libhdf5hl_fortran.a /usr/local/hdf5/lib/libhdf5_hl.a /usr/local/hdf5/lib/libhdf5_fortran.a /usr/local/hdf5/lib/libhdf5.a -Wl,--end-group \
		-Wl,--start-group /usr/local/hdf4/lib/libmfhdf.a /usr/local/hdf4/lib/libdf.a -Wl,--end-group -lsz -ljpeg -lz -liomp5
#####---------------------------------------
### Debugging when linking with ifort:
# -g -check bounds -assume gfullpath -check bounds \
# -check overflow -check underflow -fpe0 -warn argument_checking \

all :	$(src_obj)

.f.o :
	$(FC) $*.f

clean :
	rm -f $(src_obj)

clean_all :
	rm -f *.mod *.o
