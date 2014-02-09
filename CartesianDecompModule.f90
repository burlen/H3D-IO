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

! CartesianDecomp
! H3D MPI cartesian communicator
module CartesianDecompModule
  use mpi
  use StringModule
  use BoxModule
  !============================================================================
  type CartesianDecomp
    integer   WorldRank                 ! my rank in comm world
    integer   WorldSize                 ! number of processes in the communicator
    !------------------------------------
    integer   Comm                      ! optimized cartesian communicator
    logical   Periodicity(3)            ! periodic BC flags
    logical   Reorder                   ! if set optimize the communicator
    integer   Dimensionality            ! dimensionality (1,2, or 3)
    integer   ProcExt(3)                ! number of sub-simGrids
    integer   ProcCoords(3)             ! index space coordinates into decomp
    type(Box),pointer :: SimGrid        ! simulation domain
    type(Box),pointer :: LocalGrid      ! local portion of the domain
  end type CartesianDecomp

  !----------------------------------------------------------------------------
  interface NewCartesianDecomp
    module procedure NewCartesianDecomp0
    module procedure NewCartesianDecomp1
  end interface NewCartesianDecomp


  !----------------------------------------------------------------------------
  interface CartesianDecompStream
    module procedure CartesianDecompStreamChar
    module procedure CartesianDecompStreamString
  end interface

  !----------------------------------------------------------------------------
  interface SetWorldSize
    module procedure CartesianDecompSetWorldSize
  end interface

  !----------------------------------------------------------------------------
  interface SetWorldRank
    module procedure CartesianDecompSetWorldRank
  end interface

contains
  !----------------------------------------------------------------------------
  ! Default constructor -- initialized to invalid values
  function NewCartesianDecomp0() result(decomp)
    use mpi

    implicit none
    type(CartesianDecomp), pointer :: decomp
    integer iErr

    nullify(decomp)

    allocate(decomp,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error failed to allocate CartesianDecomp."
      stop
    end if

    call SetWorldSize(decomp)
    call SetWorldRank(decomp)

    decomp%Comm=MPI_COMM_NULL
    decomp%Periodicity(:)=.true.
    decomp%Reorder=.true.
    decomp%Dimensionality=2
    decomp%ProcExt(:)=1
    decomp%ProcCoords(:)=0


    decomp%SimGrid => NewBox()
    decomp%LocalGrid => NewBox()

  end function

  !----------------------------------------------------------------------------
  ! Constructor specialization -- ready to use.
  function NewCartesianDecomp1(nX) result(decomp)

    implicit none
    type(CartesianDecomp), pointer :: decomp
    integer nX(3)

    decomp => NewCartesianDecomp()

    call CartesianDecompSetSimGridExt(decomp,nX)
    call CartesianDecompCreateComm(decomp)
    call CartesianDecompDefineLocalGrid(decomp)

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteCartesianDecomp(decomp)
    use mpi
    implicit none
    type(CartesianDecomp),pointer :: decomp
    integer iErr

    if (.not.associated(decomp)) return

    if ((decomp%Comm.ne.MPI_COMM_NULL)  &
      .and.(decomp%Comm.ne.MPI_COMM_WORLD)) then

      call MPI_Comm_free(decomp%Comm,iErr)

    endif

    call DeleteBox(decomp%SimGrid)
    call DeleteBox(decomp%LocalGrid)

    deallocate(decomp)
    nullify(decomp)

  end subroutine

  !----------------------------------------------------------------------------
  function CartesianDecompGetSimGrid(decomp) result(simGrid)
    implicit none
    type(CartesianDecomp) decomp
    type(Box) simGrid

    simGrid=decomp%SimGrid

  end function

  !----------------------------------------------------------------------------
  function CartesianDecompGetLocalGrid(decomp) result(localGrid)
    implicit none
    type(CartesianDecomp) decomp
    type(Box),pointer :: localGrid

    nullify(localGrid)

    localGrid => decomp%LocalGrid

  end function

  !----------------------------------------------------------------------------
  ! Set number of procs
  subroutine CartesianDecompSetWorldSize(decomp)
    use mpi

    implicit none
    type(CartesianDecomp) decomp
    integer iErr

    call MPI_Comm_size(MPI_COMM_WORLD,decomp%WorldSize,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! Set rank
  subroutine CartesianDecompSetWorldRank(decomp)
    use mpi

    implicit none
    type(CartesianDecomp) decomp
    integer iErr

    call MPI_Comm_rank(MPI_COMM_WORLD,decomp%WorldRank,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! Set flags for periodic BC
  subroutine CartesianDecompSetPeriodicity(decomp,pi,pj,pk)
    implicit none
    type(CartesianDecomp) decomp
    logical, intent(in) :: pi,pj,pk    ! periodicity flags

    decomp%Periodicity(1)=pi
    decomp%Periodicity(2)=pj
    decomp%Periodicity(3)=pk

  end subroutine

  !----------------------------------------------------------------------------
  ! Set flags for optimal comm ordering
  subroutine CartesianDecompSetReorder(decomp,reo)
    implicit none
    type(CartesianDecomp) decomp
    logical, intent(in) :: reo         ! reorder flag

    decomp%Reorder=reo

  end subroutine

  !----------------------------------------------------------------------------
  ! Set the dimensionality (1,2, or 3D) of the communicator
  subroutine CartesianDecompSetDimensionality(decomp,d)
    implicit none
    type(CartesianDecomp) decomp
    integer, intent(in) :: d   ! dimensionality

    decomp%Dimensionality=d

  end subroutine

  !----------------------------------------------------------------------------
  ! Set the grid extents, also set's the dimensionality for 1D
  ! problem nx=m,nz=ny=1. for a 2D problem nx=m,ny=n,nz=1, for 
  ! a 3D problem nx=m,ny=n,nz=p n,m,p!=1.
  subroutine CartesianDecompSetSimGridExt(decomp,nX)
    implicit none
    type(CartesianDecomp) decomp
    integer, intent(in) :: nX(3) ! number of cells

    decomp%ProcExt(:)=1

    ! H3D doesn't decompose the communicator in the i direction
    ! hence communicator dimmesnions are either 1 for a 1D or 2D
    ! run or 2 for a 3D run.
    if (nX(3).eq.1)then
      decomp%Dimensionality=1
    else
      decomp%Dimensionality=2
    endif

    call BoxSetExtents(decomp%SimGrid,1,nX(1),1,nX(2),1,nX(3))

  end subroutine

  !----------------------------------------------------------------------------
  ! Initializes the object passed in, dims is one of 1,2, or 3D 
  ! on the domain (ilo,ihi,jlo,jhi,klo,khi)
  subroutine CartesianDecompCreateComm(decomp)
    use mpi

    implicit none
    type(CartesianDecomp) decomp
    integer :: iErr

    decomp%ProcExt(1:decomp%Dimensionality)=0

    call MPI_Dims_create(       &
        decomp%WorldSize,       &
        decomp%Dimensionality,  &
        decomp%ProcExt,         &
        iErr)
    if (iErr.ne.0) return

    call MPI_Cart_create(       &
        MPI_COMM_WORLD,         &
        decomp%Dimensionality,  &
        decomp%ProcExt,         &
        decomp%Periodicity,     &
        decomp%Reorder,         &
        decomp%Comm,            &
        iErr)
    if (iErr.ne.0) return

    call MPI_Cart_get(          &
        decomp%Comm,            &
        decomp%Dimensionality,  &
        decomp%ProcExt,         &
        decomp%Periodicity,     &
        decomp%ProcCoords,      &
        iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! domain decomosition along a single dimension divides n evenly into
  ! nprocs, distributing whats left to the first remainder procs.
  subroutine CartesianDecompDefineLocalGrid(decomp)
    implicit none
    type(CartesianDecomp) decomp
    integer :: nLocal,nLeft,coord
    integer :: nX(3)
    integer :: i

    ! TODO GHOST CELLS!!!

    call BoxGetDimensions(decomp%SimGrid,nX)

    ! H3D currently only decomposes j,k, i direction is set explicitly
    ! to the domain extents.
    decomp%LocalGrid%I(1)=1
    decomp%LocalGrid%I(2)=nX(1)

    do i=1,2

      nLocal = nX(i+1)/decomp%ProcExt(i)
      nLeft = mod(nX(i+1),decomp%ProcExt(i))

      coord=decomp%ProcCoords(i)

      if (coord.lt.nLeft) then
        ! an extra cell is added to these to use up the remainder
        decomp%LocalGrid%I(2*i+1)=coord*(nLocal+1)+1
        decomp%LocalGrid%I(2*i+2)=decomp%LocalGrid%I(2*i+1)+nLocal
      else
        decomp%LocalGrid%I(2*i+1)=coord*nLocal+nLeft+1
        decomp%LocalGrid%I(2*i+2)=decomp%LocalGrid%I(2*i+1)+nLocal-1
      endif

    end do

  end subroutine


  !----------------------------------------------------------------------------
  ! Compute some basic stats about the domain decomposition and
  ! stream them into a buffer.
  function CartesianDecompStreamChar(decomp) result(buffer)
    implicit none
    type(CartesianDecomp) decomp
    character(len=256) buffer
    integer iErr
    integer minLocalGridSize,maxLocalGridSize,localGridSize,simGridSize

    buffer=""

    simGridSize=BoxGetSize(decomp%SimGrid)
    localGridSize=BoxGetSize(decomp%LocalGrid)

    call MPI_Reduce(      &
        localGridSize,    &
        minLocalGridSize, &
        1,                &
        MPI_INTEGER,      &
        MPI_MIN,          &
        0,                &
        decomp%Comm,      &
        iErr)
    if (iErr.ne.0) return

    call MPI_Reduce(      &
        localGridSize,    &
        maxLocalGridSize, &
        1,                &
        MPI_INTEGER,      &
        MPI_MAX,          &
        0,                &
        decomp%Comm,      &
        iErr)
    if (iErr.ne.0) return


    if (decomp%WorldRank.eq.0) then

      buffer=                                                             &
          trim(StringNameValue("ProcExt",decomp%ProcExt)) //              &
          trim(StringNameValue("SimGridSize",simGridSize))  //            &
          trim(StringNameValue("MinLocalGridSize",minLocalGridSize))  //  &
          trim(StringNameValue("MaxLocalGridSize",maxLocalGridSize))

    end if

  end function

  !----------------------------------------------------------------------------
  subroutine CartesianDecompStreamString(decomp,str)
    implicit none
    type(CartesianDecomp) decomp
    type(String) str
    integer iErr
    integer minLocalGridSize,maxLocalGridSize,localGridSize,simGridSize

    simGridSize=BoxGetSize(decomp%SimGrid)
    localGridSize=BoxGetSize(decomp%LocalGrid)

    call MPI_Reduce(      &
        localGridSize,    &
        minLocalGridSize, &
        1,                &
        MPI_INTEGER,      &
        MPI_MIN,          &
        0,                &
        decomp%Comm,      &
        iErr)
    if (iErr.ne.0) return

    call MPI_Reduce(      &
        localGridSize,    &
        maxLocalGridSize, &
        1,                &
        MPI_INTEGER,      &
        MPI_MAX,          &
        0,                &
        decomp%Comm,      &
        iErr)
    if (iErr.ne.0) return

    if (decomp%WorldRank.eq.0) then

      call StringAppend(str,trim(StringNameValue("ProcExt",decomp%ProcExt)))
      call StringAppend(str,trim(StringNameValue("SimGridSize",simGridSize)))
      call StringAppend(str,trim(StringNameValue("MinLocalGridSize",minLocalGridSize)))
      call StringAppend(str,trim(StringNameValue("MaxLocalGridSize",maxLocalGridSize)))

    end if

  end subroutine


  !----------------------------------------------------------------------------
  subroutine CartesianDecompPrintSelf(decomp,unit)
    implicit none
    type(CartesianDecomp) decomp 
    integer unit

    if (decomp%WorldRank.eq.0) then
      write(unit,'(I5,A,I5)')decomp%WorldRank," WorldSize=",decomp%WorldSize
      !write(unit,'(I5,A,I15)')decomp%WorldRank,"Comm=",decomp%Comm
      !write(unit,'(I5,A,3(L2),A)')decomp%WorldRank,"Periodicity=(",decomp%Periodicity,")"
      !write(unit,'(I5,A,L2,A)')decomp%WorldRank,"Reorder=(",decomp%Reorder,")"
      !write(unit,'(I5,A,I1)')"Dimensionality=",decomp%Dimensionality
      write(unit,'(I5,A,3(I2),A)')decomp%WorldRank," ProcExt=(",decomp%ProcExt,")"
      write(unit,'(I5,A,$)')decomp%WorldRank," SimGrid="; call BoxPrintSelf(decomp%SimGrid,unit)
    endif

    write(unit,'(I5,A,3(I5),A)')decomp%WorldRank," ProcCoords=(",decomp%ProcCoords,")"
    write(unit,'(I5,A,$)')decomp%WorldRank," LocalGrid="; call BoxPrintSelf(decomp%LocalGrid,unit)

  end subroutine

end module
