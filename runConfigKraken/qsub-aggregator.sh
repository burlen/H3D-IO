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
  JID_DEP=`qsub -j oe -o /dev/null -M burlen.loring@gmail.com -m ae -N null_job -l size=12,walltime=03:00:00 $NULL_JOB`
  if [[ -z "$JID_DEP" ]]
  then
    echo "Error starting dependency chain with $NULL_JOB" 1>&2
    exit
  fi
fi

PREFIXES="ug-2048 con-5e9"

for PREFIX in $PREFIXES
do
	
  case `expr ${PREFIX:0:2}` in 

    "ug" )
      EXE=./IOBenchmarkUniformGrid
      ;;

    "co" )
      EXE=./IOBenchmarkContiguous
      ;;

  esac
 
    
  # $PREFIX-1024ag-shared-col

  CONFIG=(
    $PREFIX-64ag-shared-col
    $PREFIX-128ag-shared-col
    $PREFIX-160ag-shared-col
    $PREFIX-256ag-shared-col
    $PREFIX-512ag-shared-col
    )

  for f in "${CONFIG[@]}"
  do
    if [[ ! -e "./runConfigKraken/$f.conf" ]]
    then
      echo "ERROR $f."
      exit
    fi
  done

  RUN_ID=$PREFIX-agg

  JID_DEP=`runConfigKraken/qsub-single.sh ./runConfigKraken $EXE $CB_ALIGN $N_PROCS $RUN_ID $JID_DEP "${CONFIG[@]}"`
  if [[ -z "$JID_DEP" ]]
  then
    echo "Error: Job submission failed." 1>&2
    exit
  fi

done

echo $JID_DEP

