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


#if [ -z $1 ]
#then
#  echo "Error. No run prefix." 1>&2
#  exit
#fi
#PREFIX=$1
#shift 1

if [ -z $1 ]
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

PREFIX=6e9

#  con-$PREFIX-gate-shared-col
#  con-$PREFIX-gate-shared-ind
#  con-$PREFIX-shared-ind
#  con-$PREFIX-shared-col

CONFIG=(
  con-$PREFIX-file_per_proc
  con-$PREFIX-gate-file_per_proc
  )

JID_DEP=`runConfigKraken/qsub-seq-1.sh ./runConfigKraken ./IOBenchmarkContiguous $CB_ALIGN $JID_DEP "${CONFIG[@]}"`
if [[ -z "$JID_DEP" ]]
then
  echo "Error job submit." 1>&2
  exit
fi

echo $JID_DEP

