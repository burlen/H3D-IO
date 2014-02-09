!      ____    _ __           ____               __    ____
!     / __/___(_) /  ___ ____/ __ \__ _____ ___ / /_  /  _/__  ____
!    _\ \/ __/ / _ \/ -_) __/ /_/ / // / -_|_-</ __/ _/ // _ \/ __/
!   /___/\__/_/_.__/\__/_/  \___\_\_,_/\__/___/\__/ /___/_//_/\__(_) 
!
!
! Copyright 2010 SciberQuest Inc.
!
! No permission is granted to reproduce this software.
!
! This is experimental software and is provided ‘‘as is’’, with no
! warranties of any kind whatsoever, no support, no promise of updates,
! or printed documentation.
!==============================================================================

module IOBenchmarkCOmmandLineModule

contains

  !------------------------------------------------------------------------------
  subroutine ProcessCommandLine(  &
      worldRank,                  &
      configFileName,             &
      logFileName,                &
      workingDir,                 &
      iErr)
    use mpi

    implicit none
    integer worldRank
    character(len=*) configFileName
    character(len=*) logFileName
    character(len=*) workingDir
    character(len=512) cWorkingDir
    character(len=256) debugRanks
    logical workingDirExists
    integer nArgs
    integer workingDirLen
    integer logFileNameLen
    integer iErr
    integer iargc

    configFileName=""
    logFileName=""
    workingDir=""
    debugRanks=""

    nArgs=iargc()

    if (worldRank.eq.0) then

    if (nArgs.lt.3) then
        write(0,*)"Error: Incorrect usage of command."
        write(0,*)"     You must specify:"
        write(0,*)"         1) the run configuration file in position 1."
        write(0,*)"         2) the log file name in position 2."
        write(0,*)"         3) the working directory in position 3."
        stop
      end if
      call getarg(1,configFileName)
      call getarg(2,logFileName)
      call getarg(3,workingDir)

      ! if the working dir doesn't exist, mkdir it
      ! gnu gfortran

#if defined __GFORTRAN__
      inquire(file=workingDir,exist=workingDirExists)
#elif defined  __INTEL_COMPILER
      inquire(directory=workingDir,exist=workingDirExists)
#endif
      if (.not.workingDirExists) then

        cWorkingDir=trim(workingDir)//char(0)

        call lfcreatedir(cWorkingDir,iErr)
        if (iErr.ne.0) then
          write(0,*)"Error: Failed to create the working directory at ",trim(workingDir)
          return
        end if

      end if

      workingDirLen=len_trim(workingDir)
      logFileNameLen=len_trim(logFileName)

    end if

    ! distribute logFileName and workingDir
    call MPI_Bcast(workingDirLen,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(workingDir,workingDirLen,MPI_CHARACTER,0,MPI_COMM_WORLD,iErr)

    call MPI_Bcast(logFileNameLen,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(logFileName,logFileNameLen,MPI_CHARACTER,0,MPI_COMM_WORLD,iErr)

    if (nArgs.gt.3) then
      call getarg(4,debugRanks)
      call gdbattachranks("xterm"//char(0),trim(debugRanks)//char(0))
    end if

  end subroutine

end module
