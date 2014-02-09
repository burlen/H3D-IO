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
  echo 1>&2
  echo "\$1 = /path/to/config/files" 1>&2
  echo "\$2 = /path/to/executable" 1>&2
  echo "\$3 = MPICH_MPIIO_CB_ALIGN" 1>&2
  echo "\$4 = job size" 1>&2
  echo "\$5 = job id (used to set log file)" 1>&2
  echo "\$6... \$N = config1 ... configN" 1>&2
  echo 1>&2
}

#------------------------------------------------------------------------------
function submit_single()
{
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

  JOB_SIZE=$1
  shift 1

  JOB_ID=$1
  shift 1

  MAX_RUN_TIME=$1
  shift 1

  if [[ -n "$CB_ALIGN" ]]
  then 
    MPICH_MPIIO_CB_ALIGN=$CB_ALIGN
  fi

  if [[ -z "$MAX_RUN_TIME" ]]
  then 
    MAX_RUN_TIME=02:00:00
  fi

  # make a directory for the data
  # T_WORK_DIR=`mktemp -d $WORK_DIR/XXXXXXXX` || exit

  # assign variableto pass to batch script
  V="H3DIO_EXE=$EXE,H3DIO_CONFIG_FILE=$CONFIG_FILE,H3DIO_LOG_FILE=$LOG_FILE,H3DIO_WORK_DIR=$WORK_DIR,MPICH_MPIIO_CB_ALIGN=$CB_ALIGN"

  # report status to calling term
  echo "$JOB_ID $CONFIG_FILE $LOG_FILE $JOB_SIZE $JID_DEP $WORK_DIR $MAX_RUN_TIME" 1>&2

  # submit run
  JID_DEP=`qsub -j eo -M burlen.loring@gmail.com -m bea -N $JOB_ID_$JOB_NAME -l size=$JOB_SIZE,walltime=$MAX_RUN_TIME -v $V -W depend=afterok:$JID_DEP $BATCH_SCRIPT`
  if [[ -z "$JID_DEP" ]]
  then
    echo "Error job sumbission failed." 1>&2
    exit
  fi
  
  # submit cleanup
  #JID_DEP=`qsub -j oe -M burlen.loring@gmail.com -m ae -N cleanup-$JOB_ID_$JOB_NAME -l size=12,walltime=01:00:00 -v $V -W depend=afterok:$JID_DEP $CLEANUP_SCRIPT`
  #if [[ -z "$JID_DEP" ]]
  #then
  #  echo "Error job sumbission failed." 1>&2
  #  exit
  #fi
}

#==============================================================================

CONFIG_HOME=$1
shift 1

EXE=$1
shift 1

CB_ALIGN=$1
shift 1

JOB_SIZE=$1
shift 1

JOB_ID=$1
shift 1

JID_DEP=$1
shift 1

RUN_TIME=$1
shift 1

if [[ -z "$CONFIG_HOME" || -z "$EXE" || -z "$CB_ALIGN" || -z "$JOB_ID" || -z "$JID_DEP" || -z "$RUN_TIME" || -z "$1" ]]
then
  echo "CONFIG_HOME=$CONFIG_HOME" 1>&2
  echo "EXE=$EXE" 1>&2
  echo "CB_ALIGN=$CB_ALIGN" 1>&2
  echo "JOB_SIZE=$JOB_SIZE" 1>&2
  echo "JOB_ID=$JOB_ID" 1>&2
  echo "JID_DEP=$JID_DEP" 1>&2
  echo "RUN_TIME=$RUN_TIME" 1>&2
  echo "JOBS=$1" 1>&2
  print_usage $0
  exit
fi

BATCH_SCRIPT=$CONFIG_HOME/io-benchmark.qsub
CLEANUP_SCRIPT=$CONFIG_HOME/io-cleanup.qsub

for JOB_NAME in $*
do
  submit_single $JOB_NAME $BATCH_SCRIPT $CLEANUP_SCRIPT $EXE $CONFIG_HOME/$JOB_NAME.conf log/$JOB_ID.log ./data $CB_ALIGN $JOB_SIZE $JOB_ID $RUN_TIME
done

echo $JID_DEP
 
