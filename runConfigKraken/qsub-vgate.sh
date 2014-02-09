#!/bin/bash
#      ____    _ __           ____               __    ____
#     / __/___(_) /  ___ ____/ __ \__ _____ ___ / /_  /  _/__  ____
#    _\ \/ __/ / _ \/ -_) __/ /_/ / // / -_|_-</ __/ _/ // _ \/ __/
#   /___/\__/_/_.__/\__/_/  \___\_\_,_/\__/___/\__/ /___/_//_/\__(_) 
#
#
# Copyright 2010 SciberQuest Inc.
#
# No permission is granted to reproduce this software.
#
# This is experimental software and is provided ‘‘as is’’, with no
# warranties of any kind whatsoever, no support, no promise of updates,
# or printed documentation.
#==============================================================================

if [ -z "$1" ]
then
  echo "Error. No run size."
  exit
fi
N_PROCS=$1
shift 1

if [ -z "$1" ]
then
  echo "Warning. Setting MPICH_MPIIO_ALIGN=-1" 1>&2
  CB_ALIGN=-1
else
  CB_ALIGN=$1
fi
shift 1

JID_DEP=$1
shift 1

if [[ -z "$JID_DEP" ]]
then
  NULL_JOB=runConfigKraken/null.qsub
  JID_DEP=`qsub -j oe -o /dev/null -M burlen.loring@gmail.com -m ae -N null_job -l size=12,walltime=00:30:00 $NULL_JOB`
  if [[ -z "$JID_DEP" ]]
  then
    echo "Error starting dependency chain with $NULL_JOB" 1>&2
    exit
  fi
fi

#  con-6e9-gate-1000-file_per_proc
#  con-6e9-gate-2500-file_per_proc
#  con-6e9-gate-5000-file_per_proc
#  con-6e9-gate-7500-file_per_proc
#  con-6e9-gate-10000-file_per_proc
#  con-6e9-gate-12500-file_per_proc
#  con-6e9-gate-15000-file_per_proc


#CONFIG=(
#  con-256M-file_per_proc
#  con-256M-file_per_proc
#  con-256M-file_per_proc
#  )
#RUN_ID=con-256M-fpp

#CONFIG=(
#  con-256M-gate-file_per_proc
#  con-256M-gate-file_per_proc
#  con-256M-gate-file_per_proc
#  )
#RUN_ID=con-256M-gfpp

#CONFIG=(
#  con-256M-shared-ind
#  con-256M-shared-ind
#  con-256M-shared-ind
#  )
#RUN_ID=con-256M-msi 

CONFIG=(
  con-256M-gate-shared-ind
  con-256M-gate-shared-ind
  con-256M-gate-shared-ind
  )
RUN_ID=con-256M-gmsi 

#CONFIG=(
#  con-256M-shared-col
#  con-256M-shared-col
#  con-256M-shared-col
#  )
#RUN_ID=con-256M-msc 

#  con-6e9-gate-10-file_per_proc
#  con-6e9-gate-50-file_per_proc
#  con-6e9-gate-100-file_per_proc
#  con-6e9-gate-300-file_per_proc
#  con-6e9-gate-500-file_per_proc
#  con-6e9-gate-700-file_per_proc

#CONFIG=(
#  con-6e9-gate-1000-file_per_proc
#  con-6e9-gate-1500-file_per_proc
#  con-6e9-gate-1500-file_per_proc
#  con-6e9-gate-1500-file_per_proc
#  )
#RUN_ID=con-256M-vgate-sm


#  con-6e9-gate-50-shared-ind
#  con-6e9-gate-75-shared-ind
#  con-6e9-gate-700-shared-ind
#  con-6e9-gate-1000-shared-ind
 
#CONFIG=(
#  con-6e9-gate-100-shared-ind
#  con-6e9-gate-200-shared-ind
#  con-6e9-gate-300-shared-ind
#  con-6e9-gate-500-shared-ind
#  )
#RUN_ID=con-256M-vgate-mpi

EXE=./IOBenchmarkContiguous

for f in "${CONFIG[@]}"
do
  if [[ ! -e "./runConfigKraken/$f.conf" ]]
  then
    echo "ERROR $f."
    exit
  fi
done


JID_DEP=`runConfigKraken/qsub-single.sh ./runConfigKraken $EXE $CB_ALIGN $N_PROCS $RUN_ID $JID_DEP "${CONFIG[@]}"`
if [[ -z "$JID_DEP" ]]
then
  echo "Error: Job submission failed." 1>&2
  exit
fi

echo $JID_DEP

