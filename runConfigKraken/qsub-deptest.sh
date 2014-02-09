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

JID=`qsub -j oe -o /dev/null -M burlen.loring@gmail.com -m ae -l size=12,walltime=00:02:00 date.qsub`
JID=`qsub -j oe -o /dev/null -M burlen.loring@gmail.com -m ae -l size=12,walltime=00:02:00 -W depend=afterok:$JID date.qsub`
JID=`qsub -j oe -o /dev/null -M burlen.loring@gmail.com -m ae -l size=12,walltime=00:02:00 -W depend=afterok:$JID date.qsub`

