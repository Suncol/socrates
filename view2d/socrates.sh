MYNAME=$(basename $0)
usage="###### ${MYNAME} Usage: ${MYNAME} [run_label]"
if [ $# -ne 1 ] ; then
    echo $usage 1>&2
    exit 1
fi
if [ ! -x ./socrates ] ; then
    echo "###### ${MYNAME} error: executable ./socrates not found! Did you build correctly?"
    exit 9
fi

export OMP_DYNAMIC=FALSE
export OMP_NUM_THREADS=24
export GFORTRAN_UNBUFFERED_ALL=y   # useful to see log-file advancing (with "tail -f")

run_label=$1
in_nml=./namelists/${run_label}.inp
if [ ! -r $in_nml ] ; then
    echo "###### $MYNAME error: input namelist $in_nml not found!"
    exit 2
fi
mkdir -p ./data_out/${run_label}
outlog=./data_out/${run_label}/${run_label}.log

echo 'SOCRATES starts on ' > ${outlog}
date >> ${outlog}
echo '=====================================================================================' >> ${outlog}
echo '' >> ${outlog}
./socrates < $in_nml >> ${outlog} 2>&1
echo "${MYNAME}: SOCRATES running, stdout and stderr re-directed to ${outlog}"
echo '' >>  ${outlog}
echo '=====================================================================================' >>  ${outlog}
echo 'SOCRATES finished on ' >>  ${outlog}
date >> ${run_label}.log
echo "${MYNAME}: SOCRATES run finished, see ${outlog}"
