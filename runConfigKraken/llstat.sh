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

# collect stats on an OST

LOGFILE=$1
shift 1

OSCS=`ls /proc/fs/lustre/osc/ | grep OST`
OSC=`N=$RANDOM; let N%=336; let N+=1; echo $OSCS | cut -d" " -f $N`

echo >> $LOGFILE
echo "#################################" >> $LOGFILE
echo -n "#" >> $LOGFILE
date >> $LOGFILE
echo "# $OSC" >> $LOGFILE
echo "#################################" >> $LOGFILE

llstat -i 2 -g $OSC >> $LOGFILE


