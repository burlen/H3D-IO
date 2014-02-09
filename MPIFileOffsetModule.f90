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

module MPIFileOffsetModule
  use mpi

  !============================================================================
  ! Maps distributed, contigous segments of data into a file
  ! in rank order.
  type MPIFileOffset
    integer Comm                                   ! Decomposition communicator
    integer CommSize                               ! Number of processes in decomp comm
    integer CommRank                               ! Local process rank in decomp comm
    integer(kind=MPI_OFFSET_KIND) BaseOffset       ! current shared file pointer (units of bytes)
    integer(kind=MPI_OFFSET_KIND) OpOffset         ! offset from base, this process operating loc (units of bytes)
    integer OpCount                                ! number of data type units
    integer DataType                               ! MPI type of data in memory
  end type

  interface NewMPIFileOffset
    module procedure NewMPIFileOffsetDefault
    module procedure NewMPIFileOffsetComm
  end interface

contains

  !----------------------------------------------------------------------------
  function NewMPIFileOffsetDefault() result(fo)
    implicit none
    type(MPIFileOffset),pointer :: fo
    integer iErr

    allocate(fo,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: failed to allocate MPIFileOffset."
      stop
    end if

    fo%Comm=MPI_COMM_NULL
    fo%CommSize=-1
    fo%CommRank=-1

    fo%BaseOffset=0
    fo%OpOffset=0

    fo%OpCount=0
    fo%DataType=MPI_BYTE

  end function

  !----------------------------------------------------------------------------
  function NewMPIFileOffsetComm(comm) result(fo)
    implicit none
    type(MPIFileOffset),pointer :: fo
    integer comm
    integer iErr

    fo => NewMPIFileOffsetDefault()

    call MPIFileOffsetSetCommunicator(fo,comm,iErr)
    if (iErr.ne.0) then
      write(0,*)'Error: Failed to set communicator.'
      stop
    endif

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteMPIFileOffset(fo)
    implicit none
    type(MPIFileOffset),pointer :: fo
    integer iErr

    if (.not.associated(fo)) return

    call MPIFileOffsetSetCommunicator(fo,MPI_COMM_NULL,iErr)

    deallocate(fo)
    nullify(fo)

  end subroutine


  !----------------------------------------------------------------------------
  subroutine MPIFileOffsetReset(fo)
    implicit none
    type(MPIFileOffset) fo

    fo%BaseOffset=0
    fo%OpOffset=0
    fo%DataType=MPI_BYTE
    fo%OpCount=0

  end subroutine


  !----------------------------------------------------------------------------
  subroutine MPIFileOffsetSetCommunicator(fo,comm,iErr)
    implicit none
    type(MPIFileOffset) fo
    integer comm
    integer iErr

    iErr=0

    if (fo%Comm.ne.MPI_COMM_NULL) then

      call MPI_Comm_free(fo%Comm,iErr)
      if (iErr.ne.0) return

    end if

    fo%Comm=MPI_COMM_NULL
    fo%CommSize=-1
    fo%CommRank=-1

    if (comm.eq.MPI_COMM_NULL) return

    call MPI_Comm_dup(comm,fo%Comm,iErr)
    if (iErr.ne.0) return

    call MPI_Comm_size(comm,fo%CommSize,iErr)
    call MPI_Comm_rank(comm,fo%CommRank,iErr)

  end subroutine

  !---------------------------------------------------------------------------
  ! this is a collective operation all processes must participate.
  subroutine MPIFileOffsetSeekF(fo,tCount,dataType,iErr)
    use mpi
    implicit none
    type(MPIFileOffset) fo
    integer*8 tCount
    integer tSize,dataType
    integer iErr
    integer*8 nLocal
    integer*8, allocatable :: nRemote(:)
    integer i

    iErr=0

    allocate(nRemote(0:fo%CommSize-1))

    ! as a convinience save this information.
    fo%OpCount=tCount
    fo%DataType=dataType

    ! compute our offset (in bytes) in a rank ordered file.

    call MPI_Type_size(dataType,tSize,iErr)
    if (iErr.ne.0) return

    nLocal=tCount*tSize

    call MPI_Allgather( &
          nLocal,       &
          1,            &
          MPI_INTEGER8, &
          nRemote,      &
          1,            &
          MPI_INTEGER8, &
          fo%Comm,      &
          iErr)
    if (iErr.ne.0) return

    ! our operating offset starts at the end of all the lower ranked
    ! processes data.
    fo%OpOffset=fo%BaseOffset
    do i=0,fo%CommRank-1
      fo%OpOffset=fo%OpOffset+nRemote(i)
    end do

    ! update the base offset to point to the next byte after
    ! the accumalted operations
    do i=0,fo%CommSize-1
      fo%BaseOffset=fo%BaseOffset+nRemote(i)
    end do

    deallocate(nRemote)

  end subroutine

  !----------------------------------------------------------------------------
  function MPIFileOffsetGetOpOffset(fo) result(offset)
    implicit none
    type(MPIFileOffset) fo
    integer(kind=MPI_OFFSET_KIND) offset

    offset=fo%OpOffset

  end function

  !----------------------------------------------------------------------------
  function MPIFileOffsetGetOpCount(fo) result(opCount)
    implicit none
    type(MPIFileOffset) fo
    integer opCount

    opCount=fo%OpCount

  end function

  !----------------------------------------------------------------------------
  subroutine MPIFileOffsetSetDataType(fo,dataType)
    implicit none
    type(MPIFileOffset) fo
    integer dataType

    fo%DataType=dataType

  end subroutine

  !----------------------------------------------------------------------------
  function MPIFileOffsetGetDataType(fo) result(t)
    implicit none
    type(MPIFileOffset) fo
    integer t

    t=fo%DataType

  end function

end module
