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

#------------------------------------------------------------------------------
function print_usage()
{
  echo 1>&2
  echo "Usage: $1" 1>&2
  echo "\$1 = /path/to/config/files" 1>&2
  echo "\$2 = /path/to/executable" 1>&2
  echo "\$3 = MPICH_MPIIO_CB_ALIGN" 1>&2
  echo "\$4 ... config names" 1>&2
  echo 1>&2
}

#------------------------------------------------------------------------------
function submit_sequence()
{
  #N_PROCS="48"
  N_PROCS="120 288 516 1020 2040 4092 8184 16380 32760"
  #65532 65536 131072 262144 524288

  JOB_NAME=$1
  shift 1

  BATCH_SCRIPT=$1
  shift 1

  CLEANUP_SCRIPT=$1
  shift 1

  EXE=$1
  shift 1

  CONFIG_FILE=$1
  shift 1

  LOG_FILE=$1
  shift 1

  WORK_DIR=$1
  shift 1

  CB_ALIGN=$1
  shift 1

  NULL_JOB=$1
  shift 1

  if [[ -n "$CB_ALIGN" ]]
  then
    MPICH_MPIIO_CB_ALIGN=$CB_ALIGN
  fi


  for i in $N_PROCS
  do
    ## make a directory for the data
    #T_WORK_DIR=`mktemp -d $WORK_DIR/XXXXXXXX` || exit

    # assign variable to apss to batch script
    V="H3DIO_EXE=$EXE,H3DIO_CONFIG_FILE=$CONFIG_FILE,H3DIO_LOG_FILE=$LOG_FILE,H3DIO_WORK_DIR=$WORK_DIR,MPICH_MPIIO_CB_ALIGN=$CB_ALIGN"

    # report to calling terminal 
    echo "$CONFIG_FILE $LOG_FILE $i $JID_DEP $WORK_DIR" 1>&2

    # submit the job
    JID_DEP=`qsub -j oe -M burlen.loring@gmail.com -m ae -N $JOB_NAME-$i -l size=$i,walltime=01:20:00 -v $V -W depend=afterok:$JID_DEP $BATCH_SCRIPT`
    if [[ -z "$JID_DEP" ]]
    then
      # echo "Error failed to start job."
      exit
    fi

    ## submit the cleanup
    #JID_DEP=`qsub -j oe -M burlen.loring@gmail.com -m ae -N cleanup-$JOB_NAME-$i -l size=12,walltime=01:30:00 -v $V -W depend=afterok:$JID_DEP $CLEANUP_SCRIPT`
    #if [[ -z "$JID_DEP" ]]
    #then
    #  # echo "Error failed to start job."
    #  exit
    #fi

  done
}

#
# submit a sequence for each configuration passed on the command line.
#
# Usage:
# qsub.sh $1 $2 $3 $4 ... $N
#
# $1    = config files path
# $2    = executable
# $3    = cb_align (-1,0,1,2)
# $4-$N = configurations
#

CONFIG_HOME=$1
shift 1

EXE=$1
shift 1

CB_ALIGN=$1
shift 1

JID_DEP=$1
shift 1

if [[ -z "$CONFIG_HOME" || -z "$EXE" || -z "$CB_ALIGN" || -z "$JID_DEP" || -z "$1" ]]
then
  echo "CONFIG_HOME=$CONFIG_HOME" 1>&2
  echo "EXE=$EXE" 1>&2
  echo "CB_ALIGN=$CB_ALIGN" 1>&2
  echo "JOB_SIZE=$JOB_SIZE" 1>&2
  echo "JOB_ID=$JOB_ID" 1>&2
  echo "JID_DEP=$JID_DEP" 1>&2
  echo "JOBS=$1" 1>&2
  print_usage $0
  exit
fi

BATCH_SCRIPT=$CONFIG_HOME/io-benchmark.qsub
CLEANUP_SCRIPT=$CONFIG_HOME/io-cleanup.qsub

for JOB_NAME in $*
do
  submit_sequence $JOB_NAME $BATCH_SCRIPT $CLEANUP_SCRIPT $EXE $CONFIG_HOME/$JOB_NAME.conf log/$JOB_NAME.log ./data/ $CB_ALIGN $CONFIG_HOME/null.qsub
done

echo $JID_DEP
 
