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
module MPIFileModule
  use UnitManagerModule
  use StringModule
  use BoxModule
  use CartesianDecompModule
  use MPIFileOffsetModule
  use LogFileModule
  use MPIFileHintModule
  use mpi
  use iso_c_binding

  !============================================================================
  ! File handle type for MPI IO
  ! This object and associated code handles splitting communicator
  ! and file for a single dataset.
  type MPIFile
    integer               WorldComm                ! all proccesses including the gate master
    integer               WorldRank                ! my rank in comm world
    integer               WorldSize                ! number of processes
    integer               MasterRank               ! process that controls the gating, does no IO
    integer               GateOp                   ! 0=none, 1=stripe by comm color, 2=stripe by process id
    integer               FileSplitOp              ! 0=none shared file 1=all file per proc, 2=comm single file per comm
    integer               CommSplitOp              ! 0=none COMM_WORLD, 1=all COMM_SELF, 2=slab comms, 3=stripe comms
    integer               DataType                 ! elemental data type
    character(len=1024)   FileName                 ! Name of file this process writes
    integer               Mode                     ! MPI file mode flags
    integer               NFiles                   ! Number of subfiles for split
    integer               IOComm                   ! processes to participate in IO
    integer               IORank                   ! my rank in io comm
    integer               IOSize                   ! number of processes participating in io
    integer               CommId                   ! IO Communicator Id
    integer               NComms                   ! Number of IO communicators
    integer               Handle                   ! MPI file handle/fortran unit
    integer               FilePointerType          ! 0=ExplicitOffset, 1=IndepFilePointer, 2=SharedFilePointer
    logical               UseMPI                   ! switches between POSIX and MPI-IO api
    logical               UseCollectiveAPI         ! switches between colective and independent IO ops
    integer               FileHints                ! MPI information object
    integer               FileView                 ! Describes data layout in the file
    integer               MemView                  ! Describes memory layout in the file
    integer               WriteGateSize            ! Number concurrent writes
    integer               OpenGateSize             ! Number concurrent opens
    integer               CloseGateSize            ! Number concurrent closes
    integer               GateId                   ! Gate group id TODO remove?
    integer               NGates                   ! Number of gates TODO remove?
    type(CartesianDecomp), pointer :: UGridDecomp  ! Data decompositon (Uniform grid mode only)
    type(MPIFileOffset), pointer :: FilePointer    ! Shared file pointer (Contiguous mode only)
    type(LogFile),pointer :: Logger                ! IO benchmark log file
  end type

  !----------------------------------------------------------------------------
  interface NewMPIFile
    module procedure NewMPIFileDefault
    module procedure NewMPIFileUniformGrid
    module procedure NewMPIFileContiguous
  end interface

  !----------------------------------------------------------------------------
  interface MPIFileWrite
    ! these wrap our internal write routine which takes
    ! a c_ptr object. These cast from the native fortran
    ! type to the c_ptr. The internal write routine casts
    ! the c_ptr back into a fortran pointer and passes
    ! it into mpi. We're doing this so that we do not
    ! have to duplicate all of the complex write code
    ! for each different data type and array shape.
    module procedure MPIFileWrite3Real4
    module procedure MPIFileWrite1Real4
    module procedure MPIFileWrite3Real8
    module procedure MPIFileWrite1Real8
    module procedure MPIFileWrite3Int4
    module procedure MPIFileWrite1Int4
    module procedure MPIFileWrite3Int8
    module procedure MPIFileWrite1Int8
  end interface

  !----------------------------------------------------------------------------
  interface MPIFilePrintSelf
    module procedure MPIFilePrintSelfString
    module procedure MPIFilePrintSelfUnit
  end interface

  !----------------------------------------------------------------------------
  interface MPIFileSetDecomp
    module procedure MPIFileSetUniformGridDecomp
  end interface

contains
  !----------------------------------------------------------------------------
  ! Default constructor
  function NewMPIFileDefault() result(fh)
    use mpi

    implicit none
    type(MPIFile), pointer :: fh
    integer iErr

    nullify(fh)

    allocate(fh,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: failed to allocate MPIFile."
      stop
    end if

    fh%WorldComm=MPI_COMM_NULL
    fh%WorldSize=-1
    fh%WorldRank=-1
    fh%GateOp=0
    fh%FileSplitOp=0
    fh%CommSplitOp=0
    fh%DataType=MPI_REAL
    fh%FileName=""
    fh%Handle=0
    fh%Mode=0
    fh%IOComm=MPI_COMM_NULL
    fh%IOSize=-1
    fh%IORank=-1
    fh%WorldRank=-1
    fh%CommId=-1
    fh%NComms=1
    fh%FileView=MPI_DATATYPE_NULL
    fh%MemView=MPI_DATATYPE_NULL
    fh%FilePointerType=1 ! indep
    fh%UseCollectiveAPI=.true.
    fh%UseMPI=.true.
    fh%FileHints=MPI_INFO_NULL
    fh%NFiles=1
    fh%MasterRank=0
    fh%OpenGateSize=0
    fh%WriteGateSize=0
    fh%CloseGateSize=0
    fh%GateId=1
    fh%NGates=1
    nullify(fh%UGridDecomp)
    nullify(fh%FilePointer)
    nullify(fh%Logger)

  end function

  !------------------------------------------------------------------------------
  ! Uniform Grid Constructor
  function NewMPIFileUniformGrid( &
      comm,                       &
      masterRank,                 &
      fileName,                   &
      mode,                       &
      dataType,                   &
      decomp,                     &
      commSplitOp,                &
      nComms,                     &
      fileSplitOp,                &
      gateOp,                     &
      openGateSize,               &
      writeGateSize,              &
      closeGateSize,              &
      API,                        &
      fileHints,                  &
      logger,                     &
      iErr) result(fh)
    implicit none
    type(MPIFile), pointer :: fh
    integer comm
    integer masterRank
    character(len=*) :: fileName
    integer dataType
    integer mode
    type(CartesianDecomp), pointer :: decomp
    integer nComms
    integer commSplitOp
    integer fileSplitOp
    integer openGateSize
    integer writeGateSize
    integer closeGateSize
    integer gateOp
    integer API
    integer fileHints
    type(LogFile),pointer :: logger
    integer iErr

    nullify(fh)

    fh => NewMPIFile()
    if (.not.associated(fh)) then
      iErr=1
      return
    end if

    ! paramters
    call MPIFileSetDataType(fh,dataType,iErr)
    call MPIFileSetFileName(fh,fileName,iErr)
    call MPIFileSetMode(fh,mode,iErr)
    call MPIFileSetDecomp(fh,decomp,iErr)
    call MPIFileSetNComms(fh,nComms,iErr)
    call MPIFileSetAPI(fh,API,iErr)
    call MPIFileSetFilePointerType(fh,1,iErr)
    call MPIFileSetOpenGateSize(fh,openGateSize,iErr)
    call MPIFileSetWriteGateSize(fh,writeGateSize,iErr)
    call MPIFileSetCloseGateSize(fh,closeGateSize,iErr)

    ! communicator
    call MPIFileSetCommunicator(fh,comm,masterRank,iErr)
    if (iErr.ne.0) return
    call MPIFileSetCommSplitOperation(fh,commSplitOp,iErr)
    if (iErr.ne.0) return
    ! gating
    call MPIFileSetGateOperation(fh,gateOp,iErr)
    if (iErr.ne.0) return
    ! file
    call MPIFileSetFileSplitOperationUGrid(fh,fileSplitOp,iErr)
    if (iErr.ne.0) return
    ! hints
    call MPIFileSetFileHints(fh,fileHints,iErr)
    if (iErr.ne.0) return

    ! logger
    fh%Logger => NewLogFile(logger)
    call LogFileSetComm(fh%Logger,MPI_COMM_SELF,iErr)
    call MPIFileLogNComms(fh,iErr)
    call MPIFileLogIOAPI(fh,iErr)

    ! open the file
    call MPIFileOpen(fh,iErr)

  end function

  !------------------------------------------------------------------------------
  ! Contiguous Constructor
  function NewMPIFileContiguous( &
      comm,                      &
      masterRank,                &
      fileName,                  &
      mode,                      &
      commSplitOp,               &
      nComms,                    &
      fileSplitOp,               &
      gateOp,                    &
      openGateSize,              &
      writeGateSize,             &
      closeGateSize,             &
      API,                       &
      fileHints,                 &
      logger,                    &
      iErr) result(fh)
    implicit none
    type(MPIFile), pointer :: fh
    integer comm
    integer masterRank
    character(len=*) :: fileName
    integer mode
    integer nComms
    integer commSplitOp
    integer fileSplitOp
    integer openGateSize
    integer writeGateSize
    integer closeGateSize
    integer gateOp
    integer API
    integer fileHints
    type(LogFile),pointer :: logger
    integer iErr

    nullify(fh)

    fh => NewMPIFile()
    if (.not.associated(fh)) then
      iErr=1
      return
    end if

    ! paramters
    call MPIFileSetFileName(fh,fileName,iErr)
    call MPIFileSetMode(fh,mode,iErr)
    call MPIFileSetNComms(fh,nComms,iErr)
    call MPIFileSetAPI(fh,API,iErr)
    call MPIFileSetFilePointerType(fh,0,iErr)
    call MPIFileSetOpenGateSize(fh,openGateSize,iErr)
    call MPIFileSetWriteGateSize(fh,writeGateSize,iErr)
    call MPIFileSetCloseGateSize(fh,closeGateSize,iErr)

    ! communicator
    call MPIFileSetCommunicator(fh,comm,masterRank,iErr)
    if (iErr.ne.0) return
    call MPIFileSetCommSplitOperation(fh,commSplitOp,iErr)
    if (iErr.ne.0) return
    ! file pointer
    fh%FilePointer => NewMPIFileOffset(fh%IOComm)
    ! gating
    call MPIFileSetGateOperation(fh,gateOp,iErr)
    if (iErr.ne.0) return
    ! file
    call MPIFileSetFileSplitOperationContig(fh,fileSplitOp,iErr)
    if (iErr.ne.0) return
    ! hints
    call MPIFileSetFileHints(fh,fileHints,iErr)
    if (iErr.ne.0) return
    ! note: in above sequence the order is important

    ! logger
    fh%Logger => NewLogFile(logger)
    call LogFileSetComm(fh%Logger,MPI_COMM_SELF,iErr)
    call MPIFileLogNComms(fh,iErr)
    call MPIFileLogIOAPI(fh,iErr)

    ! open the file
    call MPIFileOpen(fh,iErr)
    if (iErr.ne.0) return

  end function

  !------------------------------------------------------------------------------
  subroutine DeleteMPIFile(fh)
    implicit none
    type(MPIFile), pointer :: fh
    integer iErr

    if (.not.associated(fh)) return

    if (fh%Handle.ne.0) then
      write(0,*)'Error: File was not closed.'
    endif

    call MPIFileSetCommunicator(fh,MPI_COMM_NULL,-1,iErr)

    if (fh%FileHints.ne.MPI_INFO_NULL) then
      call MPI_Info_free(fh%FileHints,iErr)
      fh%FileHints=MPI_INFO_NULL
    end if

    if (fh%FileView.ne.MPI_DATATYPE_NULL) then
      call MPI_Type_free(fh%FileView,iErr)
      fh%FileView=MPI_DATATYPE_NULL
    end if

    if (fh%MemView.ne.MPI_DATATYPE_NULL) then
      call MPI_Type_free(fh%MemView,iErr)
      fh%MemView=MPI_DATATYPE_NULL
    end if

    call DeleteMPIFileOffset(fh%FilePointer)
    nullify(fh%FilePointer)

    call DeleteLogFile(fh%Logger)
    nullify(fh%FilePointer)

    deallocate(fh)
    nullify(fh)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileSetMemView(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr
    type(Box), pointer :: simGrid,localGrid

    simGrid => fh%UGridDecomp%SimGrid
    localGrid => fh%UGridDecomp%LocalGrid

    ! in memory - no ghosts
    call MPI_Type_contiguous(    &
          BoxGetSize(localGrid), &
          fh%DataType,           &
          fh%MemView,            &
          iErr)
    if (iErr.ne.0) return

    call MPI_Type_commit(fh%MemView,iErr)
    if (iErr.ne.0) return

    ! TODO handle ghost cells
    ! call MPIFileCreateSubArray(simGrid,localGrid,fh%DataType,fh%MemView,iErr)

  end subroutine

  !---------------------------------------------------------------------------
  ! for uniform grid io
  subroutine MPIFileSetFileViewShared(fh,iErr)
    implicit none
    type(MPIFile) fh
    type(Box), pointer :: simGrid,localGrid
    integer iErr

    iErr=0

    ! data layouts
    simGrid => fh%UGridDecomp%SimGrid
    localGrid => fh%UGridDecomp%LocalGrid

    ! in file
    call MPIFileCreateSubArray(simGrid,localGrid,fh%DataType,fh%FileView,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! for uniform grid io
  subroutine MPIFileSetFileViewFilePerProc(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr
    type(Box), pointer :: simGrid,localGrid

    simGrid => fh%UGridDecomp%SimGrid
    localGrid => fh%UGridDecomp%LocalGrid

    ! in memory - no ghosts
    call MPI_Type_contiguous(BoxGetSize(localGrid),fh%DataType,fh%FileView,iErr)
    if (iErr.ne.0) return

    call MPI_Type_commit(fh%FileView,iErr)
    if (iErr.ne.0) return

    ! TODO handle ghost cells
    ! call MPIFileCreateSubArray(simGrid,localGrid,fh%DataType,fh%MemView,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! for uniform grid io
  subroutine MPIFileSetFileViewSlab(fh,iErr)
    implicit none
    type(MPIFile) fh
    type(Box),pointer :: simGrid,localGrid,fileGrid
    integer kDecomp           ! k index into cartesian decomp
    integer kDecompMax        ! largest index
    integer kFileLo,kFileHi   ! k index into computation grid
    integer nLarge,slabHeight
    integer iErr

    iErr=0

    ! sanity check, for a split file there should be at least
    ! 2 comms
    if (fh%NComms.lt.2) then
      write(0,*)"Error: Split file requires at least two comms."
      iErr=1
      return
    end if

    kDecomp=fh%UGridDecomp%ProcCoords(2)
    kDecompMax=fh%UGridDecomp%ProcExt(2)

    ! sanity check, can't have more slabs than process groups
    if (kDecompMax.lt.fh%NComms) then
      fh%NComms=kDecompMax
    end if

    ! Given a 1D decomposition along the z dimension of the cartesian
    ! process group figure out which sub group(slab) we belong in.
    slabHeight=kDecompMax/fh%NComms
    nLarge=mod(kDecompMax,fh%NComms)

    ! Create data layouts

    ! file domain covers multiple slabs
    if (fh%CommId.lt.nLarge) then
      kFileLo=fh%CommId*(slabHeight+1)
      kFileHi=kFileLo+slabHeight
    else
      kFileLo=nLarge+fh%CommId*slabHeight
      kFileHi=kFileLo+slabHeight-1
    end if

    simGrid => fh%UGridDecomp%SimGrid
    localGrid => fh%UGridDecomp%LocalGrid

    ! in file - slab is a contigous in i-j, k block
    fileGrid => NewBox( &
        simGrid%I(1),   &
        simGrid%I(2),   &
        simGrid%I(3),   &
        simGrid%I(4),   &
        kFileLo,        &
        kFileHi)

    write(0,*)kDecomp,kDecompMax
    write(0,*)slabHeight
    write(0,*)kFileLo,kFileHi
    call BoxPrintSelf(simGrid,0)
    call BoxPrintSelf(localGrid,0)
    call BoxPrintSelf(fileGrid,0)

    call MPIFileCreateSubArray(fileGrid,localGrid,fh%DataType,fh%FileView,iErr)
    call DeleteBox(fileGrid)
    if (iErr.ne.0) return

  end subroutine

  !---------------------------------------------------------------------------
  subroutine MPIFileSetFileViewStriped(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr

    !iErr=0
    iErr=iand(fh%Mode,0) ! avoid compiler warning

    ! use comm split file.
    write(0,*)"Error: MPIFileSetFileViewStriped is not implemented."
    stop

  end subroutine

  !------------------------------------------------------------------------------
  ! This configures the object in split file mode, where processes assigned the
  ! same color write to the same file. It creates the top level directory to hold
  ! the files, and sets the views etc. The objects CommId must be set appropriately
  ! first.
  subroutine MPIFileInitializeCommSplitFile(fh,iErr)
    implicit none
    type(MPIFile) fh
    logical exists
    character*1024 subFileName,cFileName
    integer iErr

    iErr=0

    ! sanity - comm better be split.
    if (fh%NComms.lt.2) then
      write(0,*)"Error: Split by comm requires more than 1 comm."
      iErr=1
      return
    end if

    ! sanity - comm id better be set.
    if (fh%CommId.lt.0) then
      write(0,*)"Error: CommId has not been set."
      iErr=1
      return
    end if

    call MPIFileStartLogEvent(fh)

    ! number of sub-files should match number of comms, else
    ! MPI_File_open fails.
    fh%NFiles=fh%NComms

    ! one process handles file deletion/creation
    if (fh%IORank.eq.0) then

      cFileName=trim(fh%FileName)//char(0)


#if defined __GFORTRAN__
      inquire(file=fh%FileName,exist=exists)
#elif defined  __INTEL_COMPILER
      inquire(directory=fh%FileName,exist=exists)
#endif
      if (exists) then
        ! remove the existing file.
        call lfdeletesplit(cFileName,iErr)
        if (iErr.ne.0) then
          write(0,*)"Error: could not remove existing split file at: ",trim(fh%FileName)
          return
        end if
      end if

      ! create the new split file
      call lfcreatedir(cFileName,iErr)
      if (iErr.ne.0) then
        write(0,*)"Error: could not create split file at: ",trim(fh%FileName)
        return
      end if

    end if

    ! set this processes file name
    write(subFileName,'(2A,I0.5)')trim(fh%FileName),"/",fh%CommId
    fh%FileName=trim(subFileName)

    call MPIFileEndLogEvent(fh,"File_init_split")

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileInitializeSharedFile(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr
    ! logical exists
    ! character*1024 cFileName

    iErr=0

    ! sanity check , must have set the comm color
    if (fh%CommId.lt.0) then
      write(0,*)"Error: CommId has not been set."
      iErr=1
      return
    end if

    call MPIFileStartLogEvent(fh)

    fh%NFiles=1

    ! NOTE: disabled for consistency with file-per-proc case
    ! in MPI case no syncronization is needed because it is
    ! a collective operation.
    !
    ! TODO: verify MPI_File_open resets striping parameters
    ! on existing files

    ! if (fh%IORank.eq.0) then
    !   ! one process handles file deletion
    !
    !   cFileName=trim(fh%FileName)//char(0)
    !
    !   inquire(file=fh%FileName,exist=exists)
    !   if (exists) then
    !
    !     ! remove the existing file.
    !     call lfdelete(cFileName,iErr)
    !     if (iErr.ne.0) then
    !       write(0,*)"Error: could not remove existing file at: ",trim(fh%FileName)
    !       return
    !     end if
    !
    !   end if
    !
    ! end if

    call MPIFileEndLogEvent(fh,"File_init_shared")

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileInitializeFilePerProc(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr
    character(len=512) fileName
    ! character(len=512) cFileName
    ! integer rankId,maxRankId
    ! logical exists

    iErr=0
    fileName=""
    ! cFileName=""

    call MPIFileStartLogEvent(fh)

    ! NOTE: disabled , syncronization needed to avoid
    ! race condition harms performance.
    ! TODO: verify llapi_file_create resets striping parameters
    ! on existing files

    ! if (fh%WorldRank.eq.fh%MasterRank) then
    !   ! one process cleans up any existing files.
    !
    !   maxRankId=fh%IOSize-1
    !   do rankId=0,maxRankId
    !
    !     fileName=                &
    !         trim(fh%FileName) // &
    !         "." //               &
    !         trim(ConvertTight(rankId))
    !
    !     cFileName=trim(fileName)//char(0)
    !
    !     inquire(file=fileName,exist=exists)
    !     if (exists) then
    !
    !       call lfdelete(cFileName,iErr)
    !       if (iErr.ne.0) then
    !         write(0,*)"Error: could not remove existing file at: ",trim(fileName)
    !         return
    !       end if
    !
    !     end if
    !
    !   end do
    !
    !   call MPI_Barrier(MPI_COMM_WORLD,iErr)
    !
    ! else
    !   ! other process must wait for rank 0 to delete to avoid a
    !   ! race condition between their open and rank 0 delete
    !   call MPI_Barrier(MPI_COMM_WORLD,iErr)
    !
    ! end if

    ! set up for file per io
    fh%NFiles=fh%IOSize

    fileName=                &
        trim(fh%FileName) // &
        "." //               &
        trim(ConvertTight(fh%IORank))

    fh%FileName=fileName

    call MPIFileEndLogEvent(fh,"File_init_per_proc")

  end subroutine

  !----------------------------------------------------------------------------
  ! split the file, set the views
  subroutine MPIFileSetFileSplitOperationUGrid(fh,operation,iErr)
    implicit none
    type(MPIFile) fh
    integer operation
    integer iErr

    iErr=0

    fh%FileSplitOp=operation

    ! uniform grid
    ! set up a mem view of data on a grid (possibly with ghosts)
    call MPIFileSetMemView(fh,iErr)
    if (iErr.ne.0) return

    ! set a file view to a subarray region of the global grid
    ! and (optionally) split the file.
    select case (operation)

      case (0)
        ! none - single file
        call MPIFileInitializeSharedFile(fh,iErr)
        if (iErr.ne.0) return
        call MPIFileSetFileViewShared(fh,iErr)
        if (iErr.ne.0) return

      case (1)
        ! all - single file per process
        call MPIFileInitializeFilePerProc(fh,iErr)
        if (iErr.ne.0) return
        call MPIFileSetFileViewFilePerProc(fh,iErr)
        if (iErr.ne.0) return

      case (2)
        ! comm - single file per comm
        call MPIFileInitializeCommSplitFile(fh,iErr)
        if (iErr.ne.0) return
        call MPIFileSetFileViewSlab(fh,iErr)
        if (iErr.ne.0) return

      case default
        ! invalid
        write(0,*)"Error: invalid file split operation requested."
        iErr=1
        return

    end select

  end subroutine

  !----------------------------------------------------------------------------
  ! split the file, set the views
  subroutine MPIFileSetFileSplitOperationContig(fh,operation,iErr)
    implicit none
    type(MPIFile) fh
    integer operation
    integer iErr

    iErr=0

    fh%FileSplitOp=operation

    ! contigous chunks
    ! use the default file view (ie. a stream of bytes)

    select case (operation)

      case (0)
        ! none - single file
        call MPIFileInitializeSharedFile(fh,iErr)
        if (iErr.ne.0) return

      case (1)
        ! all - single file per process
        call MPIFileInitializeFilePerProc(fh,iErr)
        if (iErr.ne.0) return

      case (2)
        ! comm - single file per comm
        call MPIFileInitializeCommSplitFile(fh,iErr)
        if (iErr.ne.0) return

      case default
        ! invalid
        write(0,*)"Error: invalid file split operation requested."
        iErr=1
        return

    end select

  end subroutine

  !----------------------------------------------------------------------------
  ! All processes share a comm, this is the recommended way to use
  ! MPI IO.
  subroutine MPIFileInitializeCommSingle(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr

    iErr=0

    fh%CommId=0
    fh%NComms=1

  end subroutine

  !----------------------------------------------------------------------------
  ! Each process has it's own communicator. This is equivalent to
  ! posix, single file per process IO.
  subroutine MPIFileInitializeCommIndividual(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr

    iErr=0

    fh%CommId=fh%IORank
    fh%NComms=fh%IOSize

  end subroutine

  !----------------------------------------------------------------------------
  ! Assign color such that each is associated with a contiguous slab of
  ! the simulation domain. Kdecomp is the k-direction coordinate of the
  ! cartesian comunicator decompositoon,see MPI_Cart_create. The nSlabs
  ! paramter controls how many new colors are produced.
  subroutine MPIFileInitializeCommSlab(fh,iErr)
    use mpi

    implicit none
    type(MPIFile) fh
    integer kDecomp,kDecompMax
    integer nLarge,slabHeight,smallSlabStart
    ! integer jDecompMax,njDecomps
    ! integer procsPerNode,procsPerFile,magicNo
    integer iErr

    iErr=0

    ! jDecompMax=fh%UGridDecomp%ProcExt(1)
    kDecomp=fh%UGridDecomp%ProcCoords(2)
    kDecompMax=fh%UGridDecomp%ProcExt(2)

    ! interpret -1 to mean an automitic split.
    if (fh%NComms.eq.-1) then

      ! use the number of processes in the z-rdirection
      ! of our coartesian decomp.
      fh%NComms=kDecompMax

      ! NOTE: nothing wrong with the following.

      ! ! WARNING:
      ! ! 12 is no. of cores per node on kraken, 160 is the max
      ! ! no. procs that can access a file on lustre. So this
      ! ! default is taylored for Kraken.
      ! procsPerNode=12
      ! procsPerFile=160
      ! magicNo=procsPerFile*procsPerNode
      ! 
      ! ! if this is not zero then we want more than
      ! ! one slab per comm. note: currently iDecompMax=1
      ! njDecomps=jDecompMax/magicNo
      ! 
      ! if (njDecomps.gt.1) then
      !   ! fewer comms, ie. multiple slabs per comm
      !   fh%NComms=kDecompMax/njDecomps
      ! 
      ! else
      !   ! we already have more than the magic number in a single
      !   ! slab (the finest unit of measurement for slab io),
      !   fh%NComms=kDecompMax
      ! 
      ! end if

    end if

    ! Warn - NComms==1 means no split.
    if (fh%NComms.lt.2) then
      write(0,*)"Error: Slab split comm. NComms < 2."
      iErr=1
      return
    end if

    ! sanity check, can't have more slabs than process groups
    if (kDecompMax.lt.fh%NComms) then
      fh%NComms=kDecompMax
    end if

    ! Given a 1D decomposition along the z dimension of the cartesian
    ! process group figure out which sub group(slab) we belong in.
    slabHeight=kDecompMax/fh%NComms
    nLarge=mod(kDecompMax,fh%NComms)

    smallSlabStart=nLarge*(slabHeight+1)

    if (kDecomp.lt.smallSlabStart) then
      fh%CommId=kDecomp/(slabHeight+1)
    else
      fh%CommId=nLarge+(kDecomp-smallSlabStart)/slabHeight
    endif

    call MPIFileSplitCommInternal(fh,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! Assigns a color  in stripes of fixed size. note: this is not
  ! related to file system striping.
  subroutine MPIFileInitializeCommStripe(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer stripeSize
    integer procsPerNode,procsPerFile
    integer iErr

    iErr=0

    ! sanity - can't have more comms than processes
    if (fh%NComms.gt.fh%IOSize) then
      fh%NComms=fh%IOSize
    end if

    if (fh%NComms.eq.-1) then
      ! interpret -1 to mean an automitic split.

      ! WARNING:
      ! 12 is no. of cores per node on kraken, 160 is the max
      ! no. procs that can access a file on lustre. So this
      ! default is taylored for Kraken.
      procsPerNode=12
      procsPerFile=160
      stripeSize=procsPerFile*procsPerNode

    else
      ! split as user specified

      ! Warn - NComms==1 means no split.
      if (fh%NComms.lt.2) then
        write(0,*)"Warning: Stripe comm. NComms < 2."
      end if

      stripeSize=fh%IOSize/fh%NComms

    end if


    if (stripeSize.eq.0) then
      write(0,*)"Error: NComms > WorldSize."
      iErr=1
      return
    end if

    fh%CommId=fh%IORank/stripeSize
    fh%NComms=fh%IOSize/stripeSize
    if(mod(fh%IOSize,stripeSize).ne.0) fh%NComms=fh%NComms+1

    call MPIFileSplitCommInternal(fh,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! Split sthe communicator by color, processes with the same color
  ! are in the same communicator.
  subroutine MPIFileSplitCommInternal(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr

    stop

    call MPI_Comm_split(MPI_COMM_WORLD,fh%CommId,fh%IORank,fh%IOComm,iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to split the communicator."
    end if

  end subroutine

  !----------------------------------------------------------------------------
  ! Split the communicator
  subroutine MPIFileSetCommSplitOperation(fh,operation,iErr)
    implicit none
    type(MPIFile) fh
    integer operation
    integer iErr

    iErr=0

    fh%CommSplitOp=operation

    select case (operation)

      case (0)
        ! single comm
        call MPIFileInitializeCommSingle(fh,iErr)
        if (iErr.ne.0) return

      case (1)
        ! individual comms
        call MPIFileInitializeCommIndividual(fh,iErr)
        if (iErr.ne.0) return

      case (2)
        ! slab comms
        call MPIFileInitializeCommSlab(fh,iErr)
        if (iErr.ne.0) return

      case (3)
        ! stripe comms
        call MPIFileInitializeCommStripe(fh,iErr)
        if (iErr.ne.0) return

      case default
        ! invalid
        write(0,*)"Error: invalid comm split operation requested."
        iErr=1
        return

    end select

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileSetOpenGateSize(fh,openGateSize,iErr)
    implicit none
    type(MPIFile) fh
    integer openGateSize
    integer iErr

    iErr=0

    fh%OpenGateSize=openGateSize

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileSetWriteGateSize(fh,writeGateSize,iErr)
    implicit none
    type(MPIFile) fh
    integer writeGateSize
    integer iErr

    iErr=0

    fh%WriteGateSize=writeGateSize

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileSetCloseGateSize(fh,closeGateSize,iErr)
    implicit none
    type(MPIFile) fh
    integer closeGateSize
    integer iErr

    iErr=0

    fh%CloseGateSize=closeGateSize

  end subroutine

  !----------------------------------------------------------------------------
  ! Assign a gate color to each process.
  subroutine MPIFileSetGateOperation(fh,operation,iErr)
    implicit none
    type(MPIFile) fh
    integer operation
    integer iErr

    ! This subroutine is not useful with the new master-slave gating.
    ! NOTE: master-slave gating broke gate-by-comm.

    iErr=0

    fh%GateOp=operation

    ! Assign a gate color to each process.
    select case (operation)
      case (0)
        ! none
        fh%GateId=1
        fh%NGates=1

      case (1)
        ! gate by comm color
        fh%GateId=fh%CommId/fh%WriteGateSize+1
        fh%NGates=fh%NComms/fh%WriteGateSize
        if(mod(fh%NComms,fh%WriteGateSize).ne.0) fh%NGates=fh%NGates+1

      case (2)
        ! gate by process id
        fh%GateId=fh%IORank/fh%WriteGateSize+1
        fh%NGates=fh%IOSize/fh%WriteGateSize
        if(mod(fh%IOSize,fh%WriteGateSize).ne.0) fh%NGates=fh%NGates+1

      case default
        write(0,*)"Error: invalid gate operation requested."
        iErr=1
    end select

  end subroutine

  !----------------------------------------------------------------------------
  ! Create an MPI sub array type (layout)
  subroutine MPIFileCreateSubArray(globalGrid,localGrid,etype,layout,iErr)
    implicit none
    type(Box) globalGrid    ! global array extents
    type(Box) localGrid     ! local array entents
    integer etype           ! elemental type
    integer layout          ! return the MPI sub array type
    integer globalGridDims(3),localGridDims(3)
    integer start(3)
    integer iErr

    iErr=0

    call BoxGetDimensions(globalGrid,globalGridDims)
    call BoxGetDimensions(localGrid,localGridDims)
    start(1)=localGrid%I(1)-globalGrid%I(1) !  convert to c-index!
    start(2)=localGrid%I(3)-globalGrid%I(3)
    start(3)=localGrid%I(5)-globalGrid%I(5)
    call MPI_Type_create_subarray(  &
        3,                          &
        globalGridDims,             &
        localGridDims,              &
        start,                      &
        MPI_ORDER_FORTRAN,          &
        etype,                      &
        layout,                     &
        iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to create the subarry."
      return
    end if

    call MPI_Type_commit(layout,iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to commit the subarray."
    end if

    !     write(0,*        )"Data type created."
    !     write(0,'(A,$)'  )"SimGrid:"; call BoxPrintSelf(globalGrid,0)
    !     write(0,'(A,3I5)')"DomainDims:",globalGridDims
    !     write(0,'(A,$)'  )"LocalGrid:"; call BoxPrintSelf(localGrid,0)
    !     write(0,'(A,3I5)')"SubDomainDims:",localGridDims
    !     write(0,'(A,3I5)')"Starts:",start

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileOpenInternal(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer(kind=MPI_OFFSET_KIND) offset
    integer stripeCount,stripeSize
    integer iErr,iErr2,eStrLen
    character(len=512) eStr
    character(len=512) cFileName
    eStrLen=512

    iErr=0

    ! Open the file
    if (.not.fh%UseMPI) then
      ! POSIX IO

      ! get the striping paramters from the hints.
      call MPIFileHintGet(fh%FileHints,"striping_factor",stripeCount,iErr)
      if (iErr.ne.0) then
        write(0,*)"Error: no stripe_count hint."
        return
      endif
      if (stripeCount.ne.1) then
        write(0,*)"Warning: stripe_count is not 1."
      endif

      call MPIFileHintGet(fh%FileHints,"striping_unit",stripeSize,iErr)
      if (iErr.ne.0) then
        write(0,*)"Error: no stripe_size hint."
        return
      endif
      if ((fh%FilePointerType.eq.1).and.(stripeSize.lt.0)) then
        ! Uniform grid, we can set the stripe size to the data size.
        call MPI_Type_size(fh%DataType,stripeSize,iErr)
        stripeSize=stripeSize*BoxGetSize(fh%UGridDecomp%LocalGrid)
      endif

      ! Convert the requested stripe size to a mupltiple of lustre's
      ! page size
      call lfpagealign(stripeSize)

      cFileName=trim(fh%FileName)//char(0)

      call lfopen(        &
            cFileName,    &
            -1,           &
            -1,           &
            stripeSize,   &
            stripeCount,  &
            -1,           &
            fh%Handle,    &
            iErr)
      if (iErr.ne.0) then
        write(0,*)fh%IORank,' Error: Failed to open file ',trim(fh%FileName),'.'
        return
      endif

      ! update the hint as to what stripe size was used. The call to open
      ! will modify to the nearest larger page aligned size.
      call MPIFileHintSet(fh%FileHints,"stripe_size",stripeSize,iErr)

    else
      ! MPI-IO
      call MPI_File_open(  &
            fh%IOComm,     &
            fh%fileName,   &
            fh%Mode,       &
            fh%FileHints,  &
            fh%Handle,     &
            iErr)
      if (iErr.ne.0) then
        call MPI_Error_string(iErr,eStr,eStrLen,iErr2)
        write(0,*)fh%IORank,' Error: Failed to open file ',trim(fh%FileName),'.'
        write(0,*)fh%IORank,trim(eStr)
        return
      endif

      ! short circuit if nothing explicitly set. In the case of
      ! contigous decomp we'll use the default view.
      if (fh%FileView.eq.MPI_DATATYPE_NULL) return

      ! MPI-IO Set the file view
      offset=0
      call MPI_File_set_view( &
          fh%Handle,          &
          offset,             &
          fh%DataType,        &
          fh%FileView,        &
          "native",           &
          fh%FileHints,       &
          iErr)
      if (iErr.ne.0) then
        write(0,*)fh%IORank,' Error: Failed to set file view.'
        return
      endif

    endif

    ! record the hints in use on the newly opened file
    ! hinst are used to store io settings even for POSIX IO
    call MPIFileLogFileHints(fh,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileSetFileViewInternal(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer(kind=MPI_OFFSET_KIND) offset
    integer iErr,eStrLen
    eStrLen=512

    iErr=0

    ! short circuit for POSIX IO
    if (.not.fh%UseMPI) return

    ! short circuit if nothing explicitly set. In the case of
    ! contigous decomp we'll use the default view.
    if (fh%FileView.eq.MPI_DATATYPE_NULL) return

    ! MPI-IO Set the file view
    offset=0
    call MPI_File_set_view(  &
        fh%Handle,           &
        offset,              &
        fh%DataType,         &
        fh%FileView,         &
        "native",            &
        fh%FileHints,        &
        iErr)
    if (iErr.ne.MPI_SUCCESS) then
      write(0,*)fh%IORank,' Error: Failed to set file view.'
      iErr=1
      return
    endif

  end subroutine

  !------------------------------------------------------------------------------
  ! Write a subarry to a file. Set the hint cb_enable to false for independent IO.
  subroutine MPIFileWriteInternal(fh,caData,iErr)
    implicit none
    type(MPIFile) fh
    type(c_ptr) :: caData
    integer(c_int8_t),pointer :: aData
    integer(c_size_t) aDataSize
    integer iErr,iErr2,eStrLen,stat(MPI_STATUS_SIZE)
    character(len=512) eStr
    logical sync

    eStrLen=512
    iErr=0

    ! convert from the c_ptr object back to the traditional
    ! fortran pointer, integer is not neccessarilly the right
    ! type but it doesn't matter we only need to pass the
    ! address of the first element of the array.
    call c_f_pointer(caData,aData,[1])

    ! POSIX IO
    if (.not.fh%UseMPI) then

      ! get number of bytes to write.
      aDataSize=0
      select case (fh%FilePointerType)

          case (0)
            ! explicit offset
            call MPI_Type_size(                             &
                  MPIFileOffsetGetDataType(fh%FilePointer), &
                  aDataSize,                                &
                  iErr)
            aDataSize=aDataSize*MPIFileOffsetGetOpCount(fh%FilePointer)

          case (1)
            ! independent file pointer
            call MPI_Type_size(fh%MemView,aDataSize,iErr)

          case default
          ! invalid
            write(0,*)"Error: Invalid FilePointerType ",fh%FilePointerType,"."
            iErr=1
            return

      end select
      if (iErr.ne.0) then
        write(0,*)"Error: Failed to get type size."
        return
      endif

      call lfwrite(fh%Handle,aData,aDataSize,iErr)
      if (iErr.ne.0) then
        write(0,*)"Error: POSIX write failed."
        write(0,*)"handle=",fh%Handle
        write(0,*)"size=",aDataSize
        return
      end if
#ifdef SYNC_POSIX
#warning WARNING: Sync()ing enabled for POSIX IO!
      call lffdatasync(fh%Handle,iErr)
#endif

      ! return the rest of this sub applies only to MPI-IO.
      return
    endif

    ! MPI-IO

    ! multiple communicators are operating on the same file
    ! in this case explicit synchronization is needed.
    sync=((fh%NFiles.eq.1).and.(fh%NComms.gt.1))
    if (sync) then
       call MPI_File_sync(fh%Handle,iErr)
     endif

    if (fh%UseCollectiveAPI) then
      ! Collective API

      select case (fh%FilePointerType)

        case (0)
          ! Explicit offset
          call MPI_File_write_at_all(                     &
                fh%Handle,                                &
                MPIFileOffsetGetOpOffset(fh%FilePointer), &
                aData,                                    &
                MPIFileOffsetGetOpCount(fh%FilePointer),  &
                MPIFileOffsetGetDataType(fh%FilePointer), &
                stat,                                     &
                iErr)
#ifdef SYNC_MPIIO_CB
#warning WARNING: Sync()ing enabled for MPI-IO with collective buffering!
          call MPI_File_sync(fh%Handle,iErr)
#endif

        case (1)
          ! Independent file pointer
          call MPI_File_write_all(  &
                fh%Handle,          &
                aData,              &
                1,                  &
                fh%MemView,         &
                stat,               &
                iErr)
#ifdef SYNC_MPIIO_CB
          call MPI_File_sync(fh%Handle,iErr)
#endif

        ! case (2)
        !   ! Shared file pointer
        !   call MPI_File_write_ordered(...)

        case default
          write(0,*)"Error: Invalid FilePointerType ",fh%FilePointerType,"."
          iErr=1
          return

      end select

    else
      ! Indepedent (non-collective) API

      select case (fh%FilePointerType)

        case (0)
          ! Explicit offset
          call MPI_File_write_at(                         &
                fh%Handle,                                &
                MPIFileOffsetGetOpOffset(fh%FilePointer), &
                aData,                                    &
                MPIFileOffsetGetOpCount(fh%FilePointer),  &
                MPIFileOffsetGetDataType(fh%FilePointer), &
                stat,                                     &
                iErr)
#ifdef SYNC_MPIIO
#warning WARNING: Sync()ing enabled for MPI-IO!
          call MPI_File_sync(fh%Handle,iErr)
#endif

        case (1)
          ! Independent file pointer
          call MPI_File_write( &
                fh%Handle,     &
                aData,         &
                1,             &
                fh%MemView,    &
                stat,          &
                iErr)
#ifdef SYNC_MPIIO
          call MPI_File_sync(fh%Handle,iErr)
#endif
        ! case (2)
        !   ! Shared file pointer
        !   call MPI_File_write_shared(...)

        case default
          write(0,*)"Error: Invalid FilePointerType ",fh%FilePointerType,"."
          iErr=1
          return

      end select

    endif

    ! multiple communicators are operating on the same file
    ! in this case explicit synchronization is needed.
    if (sync) then
       call MPI_File_sync(fh%Handle,iErr)
     endif

    if (iErr.ne.0) then
      call MPI_Error_string(iErr,eStr,eStrLen,iErr2)
      write(0,*)fh%IORank,'Error: Could not write to file ',trim(fh%FileName)
      write(0,*)fh%IORank,trim(eStr)
      iErr=1
      return
    endif

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileCloseInternal(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr,iErr2,eStrLen
    character(len=512) eStr
    eStrLen=512

    iErr=0

    eStr="No additional information."

    if (.not.fh%UseMPI) then

      call lfclose(fh%Handle,iErr)

    else

      call MPI_File_close(fh%Handle,iErr)
      if (iErr.ne.0) then
        call MPI_Error_string(iErr,eStr,eStrLen,iErr2)
      endif

    endif

    if (iErr.ne.0) then
      write(0,*)fh%IORank,'Error: Could not close file ',trim(fh%FileName)
      write(0,*)fh%IORank,trim(eStr)
    endif

    fh%Handle=0

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileOpen(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr,iErr2
    integer stat(MPI_STATUS_SIZE)
    integer oneIoPendingTag,oneIoCompleteTag
    logical oneIoComplete
    integer nRanksToOpen,nRanksOpened,nRanksOpening,nOpenerSlots
    integer buf

    iErr=0
    iErr2=0

    if ((fh%OpenGateSize.lt.1).or.(fh%UseMPI)) then
      ! ungated.
      ! NOTE: MPI-IO, open is a collective operation

      if (fh%WorldRank.ne.fh%MasterRank) then

        !write(0,*)fh%WorldRank,"Ungated open"

        call MPIFileStartLogEvent(fh)
        call MPIFileOpenInternal(fh,iErr)
        call MPIFileEndLogEvent(fh,"File_open_elap")

      end if

      return

    end if

    ! gated open
    oneIoPendingTag=100
    oneIoCompleteTag=101

    if (fh%WorldRank.eq.fh%MasterRank) then
      ! master services worker's requests

      nOpenerSlots=min(fh%WorldSize-1,fh%OpenGateSize)
      nRanksOpening=0
      nRanksOpened=fh%WorldSize-1
      nRanksToOpen=fh%WorldSize-1

      !write(0,*)"nOpenerSlots",nOpenerSlots

      ! do until all workers have opened
      do while ((nRanksToOpen.gt.0).or.(nRanksOpened.gt.0))

        ! check for workers that have finished
        oneIoComplete=.false.
        do while (.true.)

          call MPI_Iprobe(        &
                MPI_ANY_SOURCE,   &
                oneIoCompleteTag, &
                fh%WorldComm,     &
                oneIoComplete,    &
                stat,             &
                iErr2)

          ! break, none of the workers completed
          if (.not.oneIoComplete) exit

          !write(0,*)stat(MPI_SOURCE),"acknowleded complete."

          ! one completed IO, post the pending recv
          call MPI_Recv(          &
                buf,              &
                0,                &
                MPI_INTEGER,      &
                stat(MPI_SOURCE), &
                oneIOCompleteTag, &
                fh%WorldComm,     &
                stat,             &
                iErr2)

          ! update count of completed openrs
          nRanksOpened=nRanksOpened-1

          ! update the count of active openrs
          nRanksOpening=nRanksOpening-1

        enddo

        ! currently all available writing slots are in use. continue
        ! to check for workers that have finished.
        if (nRanksOpening.ge.nOpenerSlots) cycle

        ! break, all workers have completed
        if (nRanksOpened.eq.0) cycle

        ! fill up the available writing slots
        do while ((nRanksOpening.lt.nOpenerSlots).and.(nRanksToOpen.gt.0))

          ! set one free by receiving his synchronous send
          call MPI_Recv(         &
                buf,             &
                0,               &
                MPI_INTEGER,     &
                MPI_ANY_SOURCE,  &
                oneIOPendingTag, &
                fh%WorldComm,    &
                stat,            &
                iErr2)

          !write(0,*)stat(MPI_SOURCE),"set free"

          ! update count of active openrs
          nRanksOpening=nRanksOpening+1

          ! update count of pending openrs
          nRanksToOpen=nRanksToOpen-1

        end do

      enddo

      !write(0,*)fh%WorldRank,"master done."

    else

      call MPIFileStartLogEvent(fh)

      !write(0,*)fh%WorldRank,"waiting..."

      ! worker waits for OK from master to open
      call MPI_Ssend(         &
            buf,              &
            0,                &
            MPI_INTEGER,      &
            fh%MasterRank,    &
            oneIoPendingTag,  &
            fh%WorldComm,     &
            iErr2)

      !write(0,*)fh%WorldRank,"opening..."

      ! worker opens
      call MPIFileStartLogEvent(fh)
      call MPIFileOpenInternal(fh,iErr)
      call MPIFileEndLogEvent(fh,"File_open_indi")

      ! worker updates master
      call MPI_Send(          &
            buf,              &
            0,                &
            MPI_INTEGER,      &
            fh%MasterRank,    &
            oneIoCompleteTag, &
            fh%WorldComm,     &
            iErr2)

      call MPIFileEndLogEvent(fh,"File_open_elap")

      !write(0,*)fh%WorldRank,"done."

    endif

  ! report error to calling context
  if (iErr.ne.0) return

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileClose(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr,iErr2
    integer stat(MPI_STATUS_SIZE)
    integer oneIoPendingTag,oneIoCompleteTag
    logical oneIoComplete
    integer nRanksToClose,nRanksClosed,nRanksClosing,nCloserSlots
    integer buf

    iErr=0
    iErr2=0

    if ((fh%CloseGateSize.lt.1).or.(fh%UseMPI)) then
      ! ungated.
      ! NOTE: MPI-IO close is a collective operation

      if (fh%WorldRank.ne.fh%MasterRank) then

        !write(0,*)fh%WorldRank,"Ungated close"

        call MPIFileStartLogEvent(fh)
        call MPIFileCloseInternal(fh,iErr)
        call MPIFileEndLogEvent(fh,"File_close_elap")

      end if

      return

    end if

    ! gated close
    oneIoPendingTag=102
    oneIoCompleteTag=103

    if (fh%WorldRank.eq.fh%MasterRank) then
      ! master services worker's requests

      nCloserSlots=min(fh%WorldSize-1,fh%CloseGateSize)
      nRanksClosing=0
      nRanksClosed=fh%WorldSize-1
      nRanksToClose=fh%WorldSize-1

      !write(0,*)"nCloserSlots",nCloserSlots

      ! do until all workers have completed
      do while ((nRanksToClose.gt.0).or.(nRanksClosed.gt.0))

        ! check for workers that have finished
        oneIoComplete=.false.
        do while (.true.)

          call MPI_Iprobe(        &
                MPI_ANY_SOURCE,   &
                oneIoCompleteTag, &
                fh%WorldComm,     &
                oneIoComplete,    &
                stat,             &
                iErr2)

          ! break, none of the workers completed
          if (.not.oneIoComplete) exit

          !write(0,*)stat(MPI_SOURCE),"acknowleded complete."

          ! one completed IO, post the pending recv
          call MPI_Recv(          &
                buf,              &
                0,                &
                MPI_INTEGER,      &
                stat(MPI_SOURCE), &
                oneIOCompleteTag, &
                fh%WorldComm,     &
                stat,             &
                iErr2)

          ! update count of completed workers
          nRanksClosed=nRanksClosed-1

          ! update the count of active workers
          nRanksClosing=nRanksClosing-1

        enddo

        ! currently all available slots are in use. continue
        ! to check for workers that have finished.
        if (nRanksClosing.ge.nCloserSlots) cycle

        ! break, all workers have completed
        if (nRanksClosed.eq.0) cycle

        ! fill up the available slots
        do while ((nRanksClosing.lt.nCloserSlots).and.(nRanksToClose.gt.0))

          ! set one wroker free by receiving his synchronous send
          call MPI_Recv(          &
                buf,              &
                0,                &
                MPI_INTEGER,      &
                MPI_ANY_SOURCE,   &
                oneIOPendingTag,  &
                fh%WorldComm,     &
                stat,             &
                iErr2)

          !write(0,*)stat(MPI_SOURCE),"set free"

          ! update count of active workers
          nRanksClosing=nRanksClosing+1

          ! update count of pending workers
          nRanksToClose=nRanksToClose-1

        end do

      enddo

      !write(0,*)fh%WorldRank,"Master done."

    else

      call MPIFileStartLogEvent(fh)

      !write(0,*)fh%WorldRank,"waiting..."

      ! worker waits for OK from master
      call MPI_Ssend(         &
            buf,              &
            0,                &
            MPI_INTEGER,      &
            fh%MasterRank,    &
            oneIoPendingTag,  &
            fh%WorldComm,     &
            iErr2)

      !write(0,*)fh%WorldRank,"closing..."

      ! worker goes
      call MPIFileStartLogEvent(fh)
      call MPIFileCloseInternal(fh,iErr)
      call MPIFileEndLogEvent(fh,"File_close_indi")

      ! worker tells master he's done
      call MPI_Send(          &
            buf,              &
            0,                &
            MPI_INTEGER,      &
            fh%MasterRank,    &
            oneIoCompleteTag, &
            fh%WorldComm,     &
            iErr2)

      call MPIFileEndLogEvent(fh,"File_close_elap")

      !write(0,*)fh%WorldRank,"done."

    endif

    ! report error to calling context
    if (iErr.ne.0) return

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite3Real4(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    real*4, target :: aData(:,:,:)
    real*4, pointer :: pAdata
    integer iErr

    pAdata=>aData(1,1,1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine


  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite1Real4(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    real*4, target :: aData(:)
    real*4, pointer :: pAdata
    integer iErr

    pAdata=>aData(1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite3Real8(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    real*8, target :: aData(:,:,:)
    real*8, pointer :: pAdata
    integer iErr

    pAdata=>aData(1,1,1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine


  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite1Real8(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    real*8, target :: aData(:)
    real*8, pointer :: pAdata
    integer iErr

    pAdata=>aData(1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine


  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite3Int4(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    integer*4, target :: aData(:,:,:)
    integer*4, pointer :: pAdata 
    integer iErr

    pAdata=>aData(1,1,1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite1Int4(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    integer*4, target :: aData(:)
    integer*4, pointer :: pAdata 
    integer iErr

    pAdata=>aData(1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite3Int8(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    integer*8, target :: aData(:,:,:)
    integer*8, pointer :: pAdata
    integer iErr

    pAdata=>aData(1,1,1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileWrite1Int8(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    integer*8, target :: aData(:)
    integer*8, pointer :: pAdata
    integer iErr

    pAdata=>aData(1)

    call MPIFileWriteGated(fh,c_loc(pAdata),iErr)

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileWriteGated(fh,aData,iErr)
    implicit none
    type(MPIFile) fh
    type(c_ptr) :: aData
    integer iErr,iErr2
    integer stat(MPI_STATUS_SIZE)
    integer oneIoPendingTag,oneIoCompleteTag
    logical oneIoComplete
    integer nRanksToWrite,nRanksWrote,nRanksWriting,nWriterSlots
    integer buf

    iErr=0
    iErr2=0

    if ((fh%WriteGateSize.lt.1).or.(fh%UseCollectiveAPI)) then
      ! ungated
      ! NOTE: MPI-IO w collective buffering cannot be gated.

      if (fh%WorldRank.ne.fh%MasterRank) then

        !write(0,*)fh%WorldRank,"Ungated write"

        call MPIFileStartLogEvent(fh)
        call MPIFileWriteInternal(fh,aData,iErr)
        call MPIFileEndLogEvent(fh,"File_write_elap")

      end if

      return

    end if

    ! gated write
    oneIoPendingTag=104
    oneIoCompleteTag=105

    if (fh%WorldRank.eq.fh%MasterRank) then
      ! master services worker's requests to write

      nWriterSlots=min(fh%WorldSize-1,fh%WriteGateSize)
      nRanksWriting=0
      nRanksWrote=fh%WorldSize-1
      nRanksToWrite=fh%WorldSize-1

      !write(0,*)"nWriterSlots",nWriterSlots

      ! do until all workers have writen
      do while ((nRanksToWrite.gt.0).or.(nRanksWrote.gt.0))

        ! check for workers that have finished
        oneIoComplete=.false.
        do while (.true.)

          call MPI_Iprobe(        &
                MPI_ANY_SOURCE,   &
                oneIoCompleteTag, &
                fh%WorldComm,     &
                oneIoComplete,    &
                stat,             &
                iErr2)

          ! break, none of the workers completed the write
          if (.not.oneIoComplete) exit

          !write(0,*)stat(MPI_SOURCE),"acknowleded complete."

          ! one writer completed IO, post the pending recv
          call MPI_Recv(          &
                buf,              &
                0,                &
                MPI_INTEGER,      &
                stat(MPI_SOURCE), &
                oneIOCompleteTag, &
                fh%WorldComm,     &
                stat,             &
                iErr2)

          ! update count of completed writers
          nRanksWrote=nRanksWrote-1

          ! update the count of active writers
          nRanksWriting=nRanksWriting-1

        enddo

        ! currently all available writing slots are in use. continue
        ! to check for workers that have finished.
        if (nRanksWriting.ge.nWriterSlots) cycle

        ! break, all writers have completed
        if (nRanksWrote.eq.0) cycle

        ! fill up the available writing slots
        do while ((nRanksWriting.lt.nWriterSlots).and.(nRanksToWrite.gt.0))

          ! set one writer free by receiving his synchronous send
          call MPI_Recv(         &
                buf,             &
                0,               &
                MPI_INTEGER,     &
                MPI_ANY_SOURCE,  &
                oneIOPendingTag, &
                fh%WorldComm,    &
                stat,            &
                iErr2)

          !write(0,*)stat(MPI_SOURCE),"set free"

          ! update count of active writers
          nRanksWriting=nRanksWriting+1

          ! update count of pending writers
          nRanksToWrite=nRanksToWrite-1

        end do

      enddo

      !write(0,*)fh%WorldRank,"Master done."

    else

      call MPIFileStartLogEvent(fh)

      !write(0,*)fh%WorldRank,"waiting..."

      ! worker waits for OK from master to write
      call MPI_Ssend(        &
            buf,             &
            0,               &
            MPI_INTEGER,     &
            fh%MasterRank,   &
            oneIoPendingTag, &
            fh%WorldComm,    &
            iErr2)

      !write(0,*)fh%WorldRank,"writing..."

      ! worker writes
      call MPIFileStartLogEvent(fh)
      call MPIFileWriteInternal(fh,aData,iErr)
      call MPIFileEndLogEvent(fh,"File_write_indi")

      ! worker updates master
      call MPI_Send(          &
            buf,              &
            0,                &
            MPI_INTEGER,      &
            fh%MasterRank,    &
            oneIoCompleteTag, &
            fh%WorldComm,     &
            iErr2)

      call MPIFileEndLogEvent(fh,"File_write_elap")

      !write(0,*)fh%WorldRank,"done."

    endif

    ! report error to calling context
    if (iErr.ne.0) return

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileWriteBOVHeader(fh,fileName,iErr)
    implicit none
    type(MPIFile) fh
    character(len=*) fileName
    integer iErr
    integer nX(3)
    integer unitNo
    type(UnitManager),pointer :: unitMan

    if (fh%IORank.eq.0) then

      unitMan => NewUnitManager()
      unitNo=UnitManagerGetUnit(unitMan,iErr)
      call DeleteUnitManager(unitMan)
      if (iErr.ne.0) return

      open(unit=unitNo,file=fileName,status="replace",iostat=iErr)
      if (iErr.ne.0) then
        write(0,*)"Error: Failed to create the BOV file at ",trim(fileName),"."
        return
      end if

      call BoxGetDimensions(fh%UGridDecomp%SimGrid,nX)

      write(unitNo,'(A)')"# gda data file header"
      write(unitNo,'(3(A,I5))')"nx=",nX(1)," ny=",nX(2)," nz=",nX(3)
      write(unitNo,'(A)')""
      close(unit=unitNo)

    end if

  end subroutine

  !----------------------------------------------------------------------------
  function MPIFileGetIOComm(fh) result(comm)
    implicit none
    type(MPIFile) fh
    integer comm

    comm=fh%IOComm

  end function

  !----------------------------------------------------------------------------
  function MPIFileGetCommId(fh) result(commId)
    implicit none
    type(MPIFile) fh
    integer commId

    commId=fh%CommId

  end function

  !----------------------------------------------------------------------------
  function MPIFileGetHandle(fh) result(handle)
    implicit none
    type(MPIFile) fh
    integer handle

    handle=fh%Handle

  end function

  !----------------------------------------------------------------------------
  subroutine MPIFileSetMasterRank(fh,rank,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr
    integer rank

    iErr=0

    fh%MasterRank=rank

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileSetCommunicator(fh,comm,masterRank,iErr)
    implicit none
    type(MPIFile) fh
    integer comm
    integer iErr
    integer color
    integer masterRank

    iErr=0

    if (fh%WorldComm.eq.comm) return

    if ((fh%WorldComm.ne.MPI_COMM_NULL)      &
        .and.(fh%WorldComm.ne.MPI_COMM_SELF) &
        .and.(fh%WorldComm.ne.MPI_COMM_WORLD)) then
      call MPI_Comm_free(fh%WorldComm,iErr)
    end if

    if ((fh%IOComm.ne.MPI_COMM_NULL)      &
        .and.(fh%IOComm.ne.MPI_COMM_SELF) &
        .and.(fh%IOComm.ne.MPI_COMM_WORLD)) then
      call MPI_Comm_free(fh%IOComm,iErr)
    end if

    fh%WorldComm=MPI_COMM_NULL
    fh%WorldSize=-1
    fh%WorldRank=-1

    fh%IOComm=MPI_COMM_NULL
    fh%IOSize=-1
    fh%IORank=-1

    if (comm.eq.MPI_COMM_NULL) return

    call MPI_Comm_dup(comm,fh%WorldComm,iErr)
    if (iErr.ne.0) then
      write(0,*)'Error: Failed to duplicate world comm.'
      return
    end if
    call MPI_Comm_size(comm,fh%WorldSize,iErr)
    call MPI_Comm_rank(comm,fh%WorldRank,iErr)

    fh%MasterRank=masterRank

    ! remove the master rank form the IO comm.
    if (fh%WorldRank.eq.masterRank) then
      color=0
    else
      color=1
    end if

    call MPI_Comm_split(fh%WorldComm,color,0,fh%IOComm,iErr)
    if (iErr.ne.0) then
      write(0,*)'Error: Failed to split IO comm.'
      return
    end if
    call MPI_Comm_size(comm,fh%IOSize,iErr)
    call MPI_Comm_rank(comm,fh%IORank,iErr)

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetFileName(fh,fileName,iErr)
    implicit none
    type(MPIFile) fh
    character(len=*) fileName
    integer iErr

    iErr=0

    fh%FileName=fileName

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetAPI(fh,flag,iErr)
    implicit none
    type(MPIFile) fh
    integer flag
    integer iErr

    iErr=0

    fh%UseMPI=(flag.ge.1)
    fh%UseCollectiveAPI=(flag.ge.2)

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetFilePointerType(fh,t,iErr)
    implicit none
    type(MPIFile) fh
    integer t
    integer iErr

    iErr=0

    fh%FilePointerType=t

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetMode(fh,mode,iErr)
    implicit none
    type(MPIFile) fh
    integer mode
    integer iErr

    iErr=0

    fh%Mode=mode

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetDataType(fh,dataType,iErr)
    implicit none
    type(MPIFile) fh
    integer dataType
    integer iErr

    iErr=0

    fh%DataType=dataType

  end subroutine

  !------------------------------------------------------------------------------
  function MPIFileGetDataType(fh,iErr) result(dataType)
    implicit none
    type(MPIFile) fh
    integer dataType
    integer iErr

    iErr=0

    dataType=fh%DataType

  end function

  !------------------------------------------------------------------------------
  subroutine MPIFileSetNFiles(fh,nFiles,iErr)
    implicit none
    type(MPIFile) fh
    integer nFiles
    integer iErr

    iErr=0

    fh%NFiles=nFiles

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetNComms(fh,nComms,iErr)
    implicit none
    type(MPIFile) fh
    integer nComms
    integer iErr

    iErr=0

    fh%NComms=nComms

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetUniformGridDecomp(fh,decomp,iErr)
    implicit none
    type(MPIFile) fh
    type(CartesianDecomp), pointer :: decomp
    integer iErr

    iErr=0

    fh%UGridDecomp=>decomp

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetFilePointer(fh,fp,iErr)
    implicit none
    type(MPIFile) fh
    type(MPIFileOffset), pointer :: fp
    integer iErr

    iErr=0

    call DeleteMPIFileOffset(fh%FilePointer)

    fh%FilePointer=>fp

  end subroutine

  !------------------------------------------------------------------------------
  function MPIFileGetFilePointer(fh,iErr) result(fp)
    implicit none
    type(MPIFile) fh
    type(MPIFileOffset), pointer :: fp
    integer iErr

    iErr=0

    fp => fh%FilePointer

  end function

  !------------------------------------------------------------------------------
  subroutine MPIFileSeekF(fh,tCount,dataType,iErr)
    implicit none
    type(MPIFile) fh
    integer*8 tCount
    integer dataType
    integer iErr

    iErr=0

    call MPIFileOffsetSeekF(fh%FilePointer,tCount,dataType,iErr)

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileSetFileHints(fh,fileHints,iErr)
    implicit none
    type(MPIFile) fh
    integer fileHints
    integer iErr

    iErr=0

    fh%FileHints=fileHints

  end subroutine

  !----------------------------------------------------------------------------
  function MPIFileGetFileHints(fh) result(fileHints)
    implicit none
    type(MPIFile) fh
    integer fileHints

    fileHints=fh%FileHints

  end function

  !------------------------------------------------------------------------------
  subroutine MPIFileInitializeLogger(fh,logFileName,iErr)
    implicit none
    type(MPIFile) fh
    character(len=*) logFileName
    integer iErr

    iErr=0

    call DeleteLogFile(fh%Logger)

    ! if an empty string is passed do nothing.
    if (len_trim(logFileName).ge.1) then

      fh%Logger => NewLogFile()

      call LogFileOpen(fh%Logger,logFileName,fh%IOComm,iErr)
      if (iErr.ne.0) return

    end if

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileStartLogEvent(fh)
    implicit none
    type(MPIFile) fh
    integer iErr

    if (associated(fh%Logger)) then
      call LogFileMarkEventStart(fh%Logger,iErr)
    end if

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileEndLogEvent(fh,str)
    implicit none
    type(MPIFile) fh
    character(len=*) str
    integer iErr

    if (associated(fh%Logger)) then

      call LogFileMarkEventEnd(fh%Logger,iErr)
      call LogFileWriteEvent(fh%Logger,str,iErr)

    end if

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileLogFileHints(fh,iErr)
    implicit none
    type(MPIFile) fh
    type(String),pointer :: str
    integer iErr
    integer info

    iErr=0

    if (associated(fh%Logger)) then

     if (fh%IORank.eq.0) then

        if (fh%Handle.eq.0) then
          write(0,*)"Error: No hints because file is not open."
          return
        end if

        str => NewString()
        if (.not.fh%UseMPI) then
          ! POSIX IO
          info=fh%FileHints
        else
          ! MPI-IO
          call MPI_File_get_info(fh%Handle,info,iErr)
        endif
        call MPIFileHintStream(info,str)
        call LogFileWrite(fh%Logger,str,iErr)
        call DeleteString(str)
      end if

    end if

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine MPIFileLogWriteSize(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer wsize
    integer iErr
    character(512) buffer

    iErr=0

    if (associated(fh%Logger)) then

      ! get number of bytes to write.
      wsize=0
      select case (fh%FilePointerType)

          case (0)
            ! explicit offset
            call MPI_Type_size(MPIFileOffsetGetDataType(fh%FilePointer),wsize,iErr)
            wsize=wsize*MPIFileOffsetGetOpCount(fh%FilePointer)


          case (1)
            ! independent file pointer
            call MPI_Type_size(fh%MemView,wsize,iErr)

          case default
          ! invalid
            write(0,*)"Error: Invalid FilePointerType ",fh%FilePointerType,"."
            iErr=1
            return

      end select

      write(buffer,iostat=iErr,fmt='(A24,2I24,A)') &
          "Write_size",                            &
          fh%IORank,                            &
          wsize,                                   &
          char(10)
      if (iErr.ne.0) return

      call LogFileWrite(fh%Logger,buffer,iErr)

    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileLogNComms(fh,iErr)
  implicit none
    type(MPIFile) fh
    integer iErr

    iErr=0

    if ((associated(fh%Logger)).and.(fh%IORank.eq.0)) then

      call LogFileWrite(                              &
          fh%Logger,                                  &
          StringNameValue("NCommsActual",fh%NComms),  &
          iErr)

    end if

  end subroutine

  !------------------------------------------------------------------------------
  subroutine MPIFileLogIOAPI(fh,iErr)
    implicit none
    type(MPIFile) fh
    integer iErr

    iErr=0

    if ((associated(fh%Logger)).and.(fh%IORank.eq.0)) then

      if (fh%UseCollectiveAPI) then
        ! Collective API

        select case (fh%FilePointerType)

          case (0)
            ! Explicit offset
            call LogFileWrite(                                           &
                  fh%Logger,                                             &
                  StringNameValue("MPI_File_write_at_all",fh%IORank), &
                  iErr)

          case (1)
            ! Independent file pointer
            call LogFileWrite(                                        &
                  fh%Logger,                                          &
                  StringNameValue("MPI_File_write_all",fh%IORank), &
                  iErr)

          ! case (2)
          !   ! Shared file pointer

          case default
            write(0,*)"Error: Invalid FilePointerType ",fh%FilePointerType,"."
            iErr=1
            return

        end select

      else
        ! Indepedent (non-collective) API

        select case (fh%FilePointerType)

          case (0)
            ! Explicit offset
            call LogFileWrite(                                       &
                  fh%Logger,                                         &
                  StringNameValue("MPI_File_write_at",fh%IORank), &
                  iErr)

          case (1)
            ! Independent file pointer
            call LogFileWrite(                                    &
                  fh%Logger,                                      &
                  StringNameValue("MPI_File_write",fh%IORank), &
                  iErr)

          ! case (2)
          !   ! Shared file pointer

          case default
            write(0,*)"Error: Invalid FilePointerType ",fh%FilePointerType,"."
            iErr=1
            return

        end select

      endif

    endif

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFilePrintSelfUnit(fh,unitNo,iErr)
    implicit none
    type(MPIFile) fh
    integer unitNo
    character(len=2048) buffer
    integer iErr
    character(len=256) eStr

    iErr=0

    call MPIFilePrintSelfString(fh,buffer,iErr)
    if (iErr.ne.0) return

    write(unitNo,fmt='(A)',iostat=iErr,iomsg=eStr)trim(buffer)
    if (iErr.ne.0) then
      write(0,*)"Error: ",eStr
    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFilePrintSelfString(fh,buffer,iErr)
    implicit none
    type(MPIFile) fh
    character(len=*) buffer
    character(len=32) rankStr
    integer iErr

    iErr=0

    rankStr=trim(Convert(fh%IORank))

    buffer=""
    buffer=                                                                      &
      trim(StringNameValue(trim(rankStr)//" WorldSize",fh%IOSize)) //            &
      trim(StringNameValue(trim(rankStr)//" NFiles",fh%NFiles)) //               &
      trim(StringNameValue(trim(rankStr)//" FileHints",fh%FileHints)) //         &
      trim(StringNameValue(trim(rankStr)//" FileName",trim(fh%FileName)))  //    &
      trim(StringNameValue(trim(rankStr)//" Comm",fh%IOComm))  //                &
      trim(StringNameValue(trim(rankStr)//" NComms",fh%NComms))  //              &
      trim(StringNameValue(trim(rankStr)//" CommId",fh%CommId))  //              &
      trim(StringNameValue(trim(rankStr)//" FileView",fh%FileView))  //          &
      trim(StringNameValue(trim(rankStr)//" MemView",fh%MemView))  //            &
      trim(StringNameValue(trim(rankStr)//" OpenGateSize",fh%OpenGateSize)) //   &
      trim(StringNameValue(trim(rankStr)//" WriteGateSize",fh%WriteGateSize)) // &
      trim(StringNameValue(trim(rankStr)//" CloseGateSize",fh%CloseGateSize)) // &
      trim(StringNameValue(trim(rankStr)//" NGates",fh%NGates))
      ! trim(StringNameValue(trim(rankStr)//" Logger",fh%Logger)) //             &
      ! trim(StringNameValue(trim(rankStr)//" UGridDecomp",fh%UGridDecomp))

  end subroutine


end module
