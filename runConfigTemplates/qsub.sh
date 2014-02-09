#!/bin/bash

N_PROCS="60 120 288 516 1020 2040 4092 8184 16380 32760"
#65536 131072 262144 524288
JOB_NAME=`echo $1 | cut -d/ -f2 | cut -d. -f 1`

for i in $N_PROCS
do
  echo "qsub $1 -j oe -M burlen.loring@gmail.com -m abe -N $JOB_NAME-$i -l size=$i,walltime=04:00:00"
  qsub $1 -j oe -M burlen.loring@gmail.com -m ae -N $JOB_NAME-$i -l size=$i,walltime=04:00:00
done
