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

module LogFileModule
  use DoubleVectorModule
  use PosixFileModule
  use mpi

  !============================================================================
  type LogFile
    type(PosixFile),pointer :: File
    type(DoubleVector),pointer :: EventStart
    type(DoubleVector),pointer :: EventEnd
  end type

  !----------------------------------------------------------------------------
  interface NewLogFile
    module procedure NewLogFileDefault
    module procedure NewLogFileCopy
  end interface

  !----------------------------------------------------------------------------
  interface LogFileWrite
    module procedure LogFileWriteChar
    module procedure LogFileWriteString
  end interface

contains

  !----------------------------------------------------------------------------
  function NewLogFileDefault() result(fh)
    implicit none
    type(LogFile), pointer :: fh
    integer iErr

    allocate(fh,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Could not allocate LogFile."
      stop
    end if

    fh%File => NewPosixFile()
    fh%EventStart => NewDoubleVector()
    fh%EventEnd => NewDoubleVector()

  end function


  !----------------------------------------------------------------------------
  function NewLogFileCopy(other) result(fh)
    implicit none
    type(LogFile) other
    type(LogFile), pointer :: fh
    integer iErr

    allocate(fh,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Could not allocate LogFile."
      stop
    end if

    ! copy the file handle
    fh%File => NewPosixFile(other%File)

    ! not copying event states
    fh%EventStart => NewDoubleVector()
    fh%EventEnd => NewDoubleVector()

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteLogFile(fh)
    implicit none
    type(LogFile), pointer :: fh
    integer iErr
    integer nLeft

    if (.not.associated(fh)) return

    call PosixFileClose(fh%File,iErr)
    call DeletePosixFile(fh%File)

    nLeft=DoubleVectorGetSize(fh%EventStart)
    if (nLeft.ne.0) then
      write(0,*)'Error: EventStart stack is not empty.',nLeft
    end if

    nLeft=DoubleVectorGetSize(fh%EventStart)
    if (nLeft.ne.0) then
      write(0,*)'Error: EventEnd stack is not empty.',nLeft
    end if

    call DeleteDoubleVector(fh%EventStart)
    call DeleteDoubleVector(fh%EventEnd)

    deallocate(fh)
    nullify(fh)

  end subroutine

  !----------------------------------------------------------------------------
  ! A communicator should be set prior to open if one wants comm based logging.
  ! The default is for all processes to open the file.
  subroutine LogFileOpen(fh,fileName,comm,iErr)
    implicit none
    type(LogFile) fh
    integer comm
    character(len=*) fileName
    integer iErr

    iErr=0

    call PosixFileSetComm(fh%File,comm,iErr)
    if (iErr.ne.0) return

    iErr=PosixFileOpen( &
        fh%File,        &
        fileName,       &
        -1,             &
        "unknown",      &
        "write",        &
        "append",       &
        "unformatted",  &
        "stream")
    if (iErr.ne.0) return

  end subroutine

  !----------------------------------------------------------------------------
  subroutine LogFileWriteHeader(fh,label,iErr)
    implicit none
    type(LogFile) fh
    character(len=*) label
    character(len=1024) buffer
    character(len=8) date
    character(len=10) time
    character(len=256) eStr
    integer iErr

    iErr=0

    date=""
    time=""

    if (LogFileRankIsActive(fh)) then

      call date_and_time(date, time)

      write(buffer,fmt='(13A)',iostat=ierr,iomsg=eStr)                &
        "#################################################",char(10), &
        "# ",trim(label),char(10),                                    &
        "# ",trim(date),",",trim(time),char(10),                      &
        "#################################################",char(10)
      if (iErr.ne.0) then
        write(0,*)"Error: Failed to write string."
        write(0,*)"Error: ",eStr
        return
      end if

      call LogFileWrite(fh,buffer,iErr)

    end if

  end subroutine

  !---------------------------------------------------------------------------
  subroutine LogFileWriteChar(fh,text,iErr)
    implicit none
    type(LogFile) fh
    character(len=*) text
    integer iErr

    iErr=0

    call PosixFileWrite(fh%File,text,iErr)

  end subroutine


  !---------------------------------------------------------------------------
  subroutine LogFileWriteString(fh,str,iErr)
    implicit none
    type(LogFile) fh
    type(String) str
    integer iErr

    iErr=0

    call PosixFileWrite(fh%File,str,iErr)

  end subroutine

  !---------------------------------------------------------------------------
  subroutine LogFileMarkEvent(time,iErr)
    use mpi

    implicit none
    real*8 time
    integer iErr

    iErr=0

    time=MPI_Wtime()

  end subroutine

  !---------------------------------------------------------------------------
  subroutine LogFileMarkEventStart(fh,iErr)
    implicit none
    type(LogFile) fh
    real*8 time
    integer iErr

    call LogFileMarkEvent(time,iErr)
    call DoubleVectorPush(fh%EventStart,time)

  end subroutine

  !---------------------------------------------------------------------------
  subroutine LogFileMarkEventEnd(fh,iErr)
    implicit none
    type(LogFile) fh
    real*8 time
    integer iErr

    call LogFileMarkEvent(time,iErr)
    call DoubleVectorPush(fh%EventEnd,time)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine LogFileWriteEvent(fh,eventName,iErr)
    implicit none
    type(LogFile) fh
    character(len=*) eventName
    real*8 startt,endt,ellapsedt
    character(len=512) buffer
    integer iErr

    iErr=0

    startt=DoubleVectorPop(fh%EventStart)
    endt=DoubleVectorPop(fh%EventEnd)

    ellapsedt=endt-startt

    if (PosixFileRankIsActive(fh%File)) then

      write(buffer,iostat=iErr,fmt='(A24,I24,3E24.15,A)') &
          trim(eventName),                                &
          PosixFileGetWorldRank(fh%File),                 &
          startt,                                         &
          endt,                                           &
          ellapsedt,                                      &
          char(10)
      if (iErr.ne.0) return

      call PosixFileWrite(fh%File,buffer,iErr)

    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine LogFileClose(fh,iErr)
    implicit none
    type(LogFile) fh
    integer iErr

    call PosixFileClose(fh%File,iErr)

  end subroutine

  !---------------------------------------------------------------------------
  function LogFileRankIsActive(fh) result(isActive)
    implicit none
    type(LogFile) fh
    logical isActive

    isActive=PosixFileRankIsActive(fh%File)

  end function

  !----------------------------------------------------------------------------
  subroutine LogFileSetComm(fh,comm,iErr)
    implicit none
    type(LogFile) fh
    integer comm
    integer iErr

    iErr=0

    ! note, it's OK to change the comm on a connected file.
    ! close runs on all processes.

    call PosixFileSetComm(fh%File,comm,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine LogFileSetFileName(fh,fileName,iErr)
    implicit none
    type(LogFile) fh
    character(len=*) fileName
    integer iErr

    call PosixFileSetFileName(fh%File,fileName,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  function LogFileGetFileName(fh) result(fileName)
    implicit none
    type(LogFile) fh
    character(len=512) fileName

    fileName=PosixFileGetFileName(fh%File)

  end function

end module









