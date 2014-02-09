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

module PosixFileModule
  use mpi
  use StringModule
  use UnitManagerModule
  !============================================================================
  type PosixFile
    integer WorldRank               ! my rank in comm world
    integer WorldSize               ! size of comm world
    !--------------------------------
    character(len=512) FileName     ! name of the file
    integer UnitNo                  ! unit number to open the file on
    integer IORank                  ! opperate on the file only when IORank==Worldrank
    integer IOComm                  ! communicator for collective access
    type(String),pointer :: Buffer  ! IO is fully buffered
    logical LocalBuffer             ! Set when buffer is mine (vs. shared)
    !--------------------------------
    character(len=32) StatusSpec    !
    character(len=32) ActionSpec    ! optional
    character(len=32) PositionSpec  ! file attributes from fortran standard
    character(len=32) FormSpec      !
    character(len=32) AccessSpec    !

  end type

  !---------------------------------------------------------------------------
  ! generic contructor interface
  interface NewPosixFile
    module procedure NewPosixFileDefault
    module procedure NewPosixFileCopy
  end interface

  !---------------------------------------------------------------------------
  interface PosixFileWrite
    module procedure PosixFileWriteChar
    module procedure PosixFileWriteString
  end interface

contains

  !----------------------------------------------------------------------------
  function NewPosixFileDefault() result(fh)
    use mpi
    implicit none
    type(PosixFile), pointer :: fh
    integer iErr

    allocate(fh,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate PosixFile."
      stop
    end if

    call PosixFileSetWorldSize(fh)
    call PosixFileSetWorldRank(fh)

    fh%FileName=""
    fh%UnitNo=-1

    fh%IORank=fh%WorldRank
    fh%IOComm=MPI_COMM_SELF

    fh%Buffer => NewString()
    fh%LocalBuffer=.true.

    fh%StatusSpec="unknown"
    fh%ActionSpec="readwrite"
    fh%PositionSpec="asis"
    fh%FormSpec="unformatted"
    fh%AccessSpec="sequential"

  end function

  !----------------------------------------------------------------------------
  ! Copy the file, the new file will use the same buffer as the
  ! original. All other members are isolated. The purpose behind
  ! this is so that files open on different communicators can share
  ! the buffer. Only one should physically write.
  function NewPosixFileCopy(other) result(fh)
    use mpi
    implicit none
    type(PosixFile) other
    type(PosixFile), pointer :: fh
    integer iErr

    allocate(fh,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate PosixFile."
      stop
    end if


    fh%WorldRank=other%WorldRank
    fh%WorldSize=other%WorldSize

    fh%FileName=other%FileName
    fh%UnitNo=other%UnitNo

    fh%IORank=other%IORank
    fh%IOComm=other%IOComm

    fh%Buffer=>other%Buffer
    fh%LocalBuffer=.false.

    fh%StatusSpec=other%StatusSpec
    fh%ActionSpec=other%ActionSpec
    fh%PositionSpec=other%PositionSpec
    fh%FormSpec=other%FormSpec
    fh%AccessSpec=other%AccessSpec

  end function

  !----------------------------------------------------------------------------
  subroutine DeletePosixFile(fh)
    implicit none
    type(PosixFile), pointer :: fh

    if (.not.associated(fh)) return

    ! only delete if we own it, maybe a reference
    ! to a shared buffer.
    if (fh%LocalBuffer) then
      call DeleteString(fh%Buffer)
    endif

    deallocate(fh)
    nullify(fh)

  end subroutine

  !----------------------------------------------------------------------------
  function PosixFileOpen(     &
      fh,                     &
      fileName,               &
      unitNo,                 &
      statusSpec,             &
      actionSpec,             &
      positionSpec,           &
      formSpec,               &
      accessSpec) result(iErr)
    implicit none
    type(PosixFile) fh
    character(len=*) fileName
    integer,optional :: unitNo
    character(len=*),optional :: statusSpec
    character(len=*),optional :: actionSpec
    character(len=*),optional :: positionSpec
    character(len=*),optional :: formSpec
    character(len=*),optional :: accessSpec
    integer iErr

    iErr=0

    call StringTruncate(fh%Buffer)

    fh%FileName=fileName

    if (present(unitNo)) then
      fh%UnitNo=unitNo
    else
      fh%UnitNo=-1
    end if

    ! use defaults from f2003 standard
    if (present(statusSpec)) then
      fh%StatusSpec=trim(statusSpec)
    end if

    if (present(actionSpec)) then
      fh%ActionSpec=trim(actionSpec)
    end if

    if (present(positionSpec)) then
      fh%PositionSpec=trim(positionSpec)
    end if

    if (present(formSpec)) then
      fh%FormSpec=trim(formSpec)
    end if

    if (present(accessSpec)) then
      fh%AccessSpec=trim(accessSpec)
    end if

  end function

  !----------------------------------------------------------------------------
  subroutine PosixFileOpenInternal(fh,iErr)
    implicit none
    type(PosixFile) fh
    integer iErr
    type(UnitManager),pointer :: unitMan
    character(len=512) eStr

    iErr=0

    if (fh%UnitNo.lt.0) then

      unitMan => NewUnitManager()
      fh%UnitNo=UnitManagerGetUnit(unitMan,iErr)
      call DeleteUnitManager(unitMan)
      if (iErr.ne.0) then
        return
      end if

    end if


    open(                       &
      unit=fh%UnitNo,           &
      file=fh%FileName,         &
      status=fh%StatusSpec,     &
      action=fh%ActionSpec,     &
      position=fh%PositionSpec, &
      form=fh%FormSpec,         &
      access=fh%AccessSpec,     &
      iostat=iErr,              &
      iomsg=eStr)
    if (iErr.ne.0) then
      write(0,*)"Error: ",trim(eStr)
      write(0,*)"Error: Failed to open file ",trim(fh%FileName),"."
      return
    end if

  end subroutine

  !----------------------------------------------------------------------------
  ! write a string to the file, no carige return.
  subroutine PosixFileWriteChar(fh,data,iErr)
    implicit none
    type(PosixFile) fh
    character(len=*) data
    integer iErr

    iErr=0

    if (fh%IORank.eq.fh%WorldRank) then

      call StringAppend(fh%Buffer,data)

    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine PosixFileWriteString(fh,str,iErr)
    implicit none
    type(PosixFile) fh
    type(String) str
    integer iErr

    iErr=0

    if (fh%IORank.eq.fh%WorldRank) then

      call StringAppend(fh%Buffer,str)

    end if

  end subroutine


  !----------------------------------------------------------------------------
  ! write a string to the file, no carige return.
  subroutine PosixFileFlush(fh,iErr)
    implicit none
    type(PosixFile) fh
    integer iErr
    integer writerRank,lastRank

    iErr=0

    ! serialize IO for multiple
    lastRank=fh%WorldSize-1
    do writerRank=0,lastRank

      if ((writerRank.eq.fh%WorldRank)              &
        .and.(StringGetSize(fh%Buffer).gt.0)) then

        call PosixFileOpenInternal(fh,iErr)
        if (iErr.ne.0) stop
        call StringWrite(fh%Buffer,fh%UnitNo,iErr)
        if (iErr.ne.0) stop
        call PosixFileCloseInternal(fh,iErr)
        if (iErr.ne.0) stop

      end if

      call MPI_Barrier(MPI_COMM_WORLD,iErr)

    end do

    call StringTruncate(fh%Buffer)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine PosixFileCloseInternal(fh,iErr)
    implicit none
    type(PosixFile) fh
    integer iErr
    character(len=512) eStr

    iErr=0

    if ((fh%UnitNo.gt.0)    &
      .and.(fh%UnitNo.ne.0) &
      .and.(fh%UnitNo.ne.5) &
      .and.(fh%UnitNo.ne.6)) then

      close(unit=fh%UnitNo,iostat=iErr,iomsg=eStr)
      if (iErr.ne.0) then
        write(0,*)"Error: ",trim(eStr)
        write(0,*)"Error: Failed to close file."
      end if

      fh%UnitNo=-1

    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine PosixFileClose(fh,iErr)
    implicit none
    type(PosixFile) fh
    integer iErr

    iErr=0

    call PosixFileFlush(fh,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine PosixFileSetComm(fh,comm,iErr)
     implicit none
    type(PosixFile) fh
    integer comm
    integer iErr

    iErr=0

    select case (comm)

      case (MPI_COMM_WORLD)
        ! one process operates
        fh%IOComm=MPI_COMM_WORLD
        fh%IORank=0

      case (MPI_COMM_SELF,MPI_COMM_NULL)
        ! all processes operate
        fh%IOComm=MPI_COMM_SELF
        fh%IORank=fh%WorldRank

      case default
        ! lowest ranking process in each comm operates
        call MPI_Allreduce( &
            fh%WorldRank,   &
            fh%IORank,      &
            1,              &
            MPI_INTEGER,    &
            MPI_MIN,        &
            comm,           &
            iErr)

    end select

  end subroutine

  !----------------------------------------------------------------------------
  subroutine PosixFileUnSetComm(fh,iErr)
    implicit none
    type(PosixFile) fh
    integer iErr

    iErr=0

    fh%IOComm=MPI_COMM_NULL
    fh%IORank=fh%WorldRank

  end subroutine


  !----------------------------------------------------------------------------
  function PosixFileGetComm(fh) result(comm)
    implicit none
    type(PosixFile) fh
    integer comm

    comm=fh%IOComm

  end function

  !----------------------------------------------------------------------------
  subroutine PosixFileSetFileName(fh,fileName,iErr)
    implicit none
    type(PosixFile) fh
    character(len=*) fileName
    integer iErr

    iErr=0

    fh%FileName=fileName

  end subroutine

  !----------------------------------------------------------------------------
  function PosixFileGetFileName(fh) result(fileName)
    implicit none
    type(PosixFile) fh
    character(len=512) fileName

    fileName=fh%FileName

  end function

  !----------------------------------------------------------------------------
  subroutine PosixFileSetWorldSize(fh)
    implicit none
    type(PosixFile) fh
    integer iErr

    call MPI_Comm_size(MPI_COMM_WORLD,fh%WorldSize,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine PosixFileSetWorldRank(fh)
    implicit none
    type(PosixFile) fh
    integer iErr

    call MPI_Comm_rank(MPI_COMM_WORLD,fh%WorldRank,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  function PosixFileGetWorldRank(fh) result(wrank)
    implicit none
    type(PosixFile) fh
    integer wrank

    wrank=fh%WorldRank

  end function

  !---------------------------------------------------------------------------
  function PosixFileRankIsActive(fh) result(isActive)
    implicit none
    type(PosixFile) fh
    logical isActive

    isActive=.false.

    if (fh%WorldRank.eq.fh%IORank) then
      isActive=.true.
    end if

  end function


end module
