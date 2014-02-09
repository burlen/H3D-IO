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


module RunConfigModule
  use StringModule
  use MPIFileHintModule
  use UnitManagerModule
  use mpi

  !============================================================================
  type RunConfig
    integer WorldRank           ! my rank in comm world
    integer WorldSize           ! size of comm world
    !----------------------------
    integer NX(3)               ! grid dimensions array
    !----------------------------
    integer StripeSize          ! Lustre strpe size, actual (-1 ignore)
    integer StripeCount         ! Lustre stripe count, number of OST's to include (-1 ignore)
    !----------------------------
    integer API                 ! selects the api, 0=POSIX,1=MPI,2=MPI with collective buffering
    integer UseCollectiveIO     ! Turns on/off MPI Coll. IO (-1 ignore, 0 disable, 1 enable, 2 automatic)
    integer CollectOp           ! Enumeration:  0=uniform, 1=GPFS, 2=Lustre.  Cray MPT only (-1 ignore)
    integer ClientOSTRatio      ! Ratio Client/OST. MPICH2 1.3 only (-1 ignore)
    integer CollectThreshold    ! Threshold above which collective IO is off. MPICH2 1.3 only. (-1 ignore)
    integer NCIONodes           ! Number of IO nodes to use (-1 ignore)
    integer CIOBufSize          ! Size of buffers for collective IO (-1 ignore)
    integer UseDataSieving      ! Turn on/off data sieving (-1 ignore, 0 disable, 1 enable, 2 automatic)
    integer SieveBufSize        ! Size of data sieving buffer (-1 to ignore)
    integer FileSplitOp         ! Enumeration: 0=none(shared file) 1=all(file per proc), 2=comm(single file per comm)
    integer NFiles              ! Number of sub files in spilt file IO, use 1 for single file IO
    integer CommSplitOp         ! Enumeration: 0=COMM_WORLD, 1=COMM_SELF, 2=slab comms, 3=stripe comms
    integer NComms              ! Number of communicators in split comm IO (-1 automatic)
    integer GateOp              ! Enumeration: 0=none, 1=by comm color, 2=by process id
    integer OpenGateSize        ! number concurrent write (<1 all procs)
    integer WriteGateSize       ! Number concurrent open (<1 all procs)
    integer CloseGateSize       ! Number concurrent close (<1 all procs)
    !----------------------------
    character(len=256) RunName  ! Run identifier based on the configuration file name
  end type

  interface RunConfigStream
    module procedure RunConfigStreamString
  end interface

contains

  !----------------------------------------------------------------------------
  function NewRunConfig() result(rconf)
    implicit none
    type(RunConfig), pointer :: rconf
    integer iErr

    allocate(rconf,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate RunConfig."
      nullify(rconf)
      return
    end if

    !----------------------
    call RunConfigSetWorldSize(rconf)
    call RunConfigSetWorldRank(rconf)
    !----------------------
    rconf%NX(:)=1
    !----------------------
    rconf%StripeSize=-1
    rconf%StripeCount=-1
    !----------------------
    rconf%API=2
    rconf%UseCollectiveIO=-1
    rconf%CollectOp=0
    rconf%NCIONodes=-1
    rconf%CIOBufSize=-1
    rconf%UseDataSieving=-1
    rconf%SieveBufSize=-1
    rconf%FileSplitOp=1
    rconf%NFiles=1
    rconf%CommSplitOp=1
    rconf%NComms=1
    rconf%GateOp=0
    rconf%OpenGateSize=0
    rconf%WriteGateSize=0
    rconf%CloseGateSize=0
    !----------------------
    rconf%RunName=""

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteRunConfig(rconf)
    implicit none
    type(RunConfig), pointer :: rconf

    if (.not.associated(rconf)) return

    deallocate(rconf)
    nullify(rconf)

  end subroutine

  !----------------------------------------------------------------------------
  function RunConfigGetNX(rconf) result(nx)
    implicit none
    type(RunConfig) rconf
    integer nx(3)

    nx=rconf%NX

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetPerProcessDataSize(rconf) result(nb)
    implicit none
    type(RunConfig) rconf
    integer nb

    ! to prevent the accidental use of old
    ! config files.
    if ((rconf%NX(2).ne.0).or.(rconf%NX(3).ne.0)) then
      write(0,*)"Error: Invalid per-process datasize."
      stop
    endif

    nb=rconf%NX(1)

  end function


  !----------------------------------------------------------------------------
  function RunConfigGetStripeSize(rconf) result(stripeSize)
    implicit none
    type(RunConfig) rconf
    integer stripeSize

    stripeSize=rconf%StripeSize

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetStripeCount(rconf) result(stripeCount)
    implicit none
    type(RunConfig) rconf
    integer stripeCount

    stripeCount=rconf%StripeCount

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetUseCollectiveIO(rconf) result(useCollectiveIO)
    implicit none
    type(RunConfig) rconf
    integer useCollectiveIO

    useCollectiveIO=rconf%UseCollectiveIO

  end function


  !----------------------------------------------------------------------------
  function RunConfigGetAPI(rconf) result(api)
    implicit none
    type(RunConfig) rconf
    integer api

    api=rconf%API

    if ((rconf%API.ge.1).and.(rconf%UseCollectiveIO.ge.1)) then

      api=api+1

    end if

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetCollectOp(rconf) result(collectOp)
    implicit none
    type(RunConfig) rconf
    integer collectOp

    collectOp=rconf%CollectOp

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetNCIONodes(rconf) result(nCIONodes)
    implicit none
    type(RunConfig) rconf
    integer nCIONodes

    nCIONodes=rconf%NCIONodes

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetCIOBufSize(rconf) result(cioBufSize)
    implicit none
    type(RunConfig) rconf
    integer cioBufSize

    cioBufSize=rconf%CIOBufSize

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetClientOSTRatio(rconf) result(clientOSTRatio)
    implicit none
    type(RunConfig) rconf
    integer clientOStRatio

    clientOSTRatio=rconf%ClientOSTRatio

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetCollectThreshold(rconf) result(collectThreshold)
    implicit none
    type(RunConfig) rconf
    integer collectThreshold

    collectThreshold=rconf%CollectThreshold

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetUseDataSieving(rconf) result(useDataSieving)
    implicit none
    type(RunConfig) rconf
    integer useDataSieving

    useDataSieving=rconf%UseDataSieving

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetSieveBufSize(rconf) result(sieveBufSize)
    implicit none
    type(RunConfig) rconf
    integer sieveBufSize

    sieveBufSize=rconf%SieveBufSize

  end function


  !----------------------------------------------------------------------------
  function RunConfigGetFileSplitOp(rconf) result(fileSplitOp)
    implicit none
    type(RunConfig) rconf
    integer fileSplitOp

    fileSplitOp=rconf%FileSplitOp

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetNFiles(rconf) result(nFiles)
    implicit none
    type(RunConfig) rconf
    integer nFiles

    nFiles=rconf%NFiles

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetCommSplitOp(rconf) result(commSplitOp)
    implicit none
    type(RunConfig) rconf
    integer commSplitOp

    commSplitOp=rconf%CommSplitOp

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetNComms(rconf) result(nComms)
    implicit none
    type(RunConfig) rconf
    integer nComms

    nComms=rconf%NComms

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetGateOp(rconf) result(gateOp)
    implicit none
    type(RunConfig) rconf
    integer gateOp

    gateOp=rconf%GateOp

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetOpenGateSize(rconf) result(openGateSize)
    implicit none
    type(RunConfig) rconf
    integer openGateSize

    openGateSize=rconf%OpenGateSize

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetWriteGateSize(rconf) result(writeGateSize)
    implicit none
    type(RunConfig) rconf
    integer writeGateSize

    writeGateSize=rconf%WriteGateSize

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetCloseGateSize(rconf) result(closeGateSize)
    implicit none
    type(RunConfig) rconf
    integer closeGateSize

    closeGateSize=rconf%CloseGateSize

  end function

  !----------------------------------------------------------------------------
  function RunConfigGetRunName(rconf) result(runName)
    implicit none 
    type(RunConfig) rconf
    character(len=256) runName

    runName=rconf%runName

   end function 

  !----------------------------------------------------------------------------
  subroutine RunConfigParseCommSplitOp(rconf,str,iErr)
    implicit none
    type(RunConfig) rconf
    character(len=*) str
    integer iErr
    logical comm_world,comm_self,comm_slab,comm_stripe

    iErr=0

    ! identify flags present
    comm_world=(index(str,"comm_world").ne.0)
    comm_self=(index(str,"comm_self").ne.0)
    comm_slab=(index(str,"comm_slab").ne.0)
    comm_stripe=(index(str,"comm_stripe").ne.0)

    ! commm splitting
    if (comm_world) then
      ! none - comm_world
      rconf%CommSplitOp=0
      rconf%NComms=1

    else &
    if (comm_self) then
      ! all - comm_self
      rconf%CommSplitOp=1
      rconf%NComms=rconf%WorldSize

    else &
    if (comm_slab) then
      ! slab
      rconf%CommSplitOp=2

    else &
    if (comm_stripe) then
      ! stripe
      rconf%CommSplitOp=3

    else
      write(0,*)"Error: One of comm_world,comm_self,comm_slab,comm_stripe must be used."
      iErr=1
      return
    end if

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine RunConfigParseFileSplitOp(rconf,str,iErr)
    implicit none
    type(RunConfig) rconf
    character(len=*) str
    integer iErr
    logical file_share,file_per_proc,file_per_comm

    iErr=0

    ! identify flags present
    file_share=(index(str,"file_share").ne.0)
    file_per_proc=(index(str,"file_per_proc").ne.0)
    file_per_comm=(index(str,"file_per_comm").ne.0)

    ! file splitting
    if (file_share) then
      ! none - single file
      rconf%FileSplitOp=0
      rconf%NFiles=1
      if (rconf%UseCollectiveIO.lt.1) then
        rconf%API=1
      else
        rconf%API=2
      end if

    else &
    if (file_per_proc) then
      ! all - file per process
      rconf%FileSplitOp=1
      rconf%NFiles=rconf%WorldSize
      rconf%API=0

    else &
    if (file_per_comm) then
      ! comm - file per communicator
      rconf%FileSplitOp=2
      rconf%NFiles=rconf%NComms
      if (rconf%UseCollectiveIO.lt.1) then
        rconf%API=1
      else
        rconf%API=2
      end if

    else
      write(0,*)"Error: One of file_share,file_per_proc,file_per_comm must be used."
      iErr=1
      return
    end if

  end subroutine


  !-----------------------------------------------------------------------------
  subroutine RunConfigParseGateOp(&
        rconf,                    &
        str,                      &
        openGateSize,             &
        writeGateSize,            &
        closeGateSize,            &
        iErr)
    implicit none
    type(RunConfig) rconf
    character(len=*) str
    integer openGateSize
    integer writeGateSize
    integer closeGateSize
    integer iErr
    logical gate_off,gate_comm,gate_proc

    ! identify flags present
    gate_off=(index(str,"gate_off").ne.0)
    gate_comm=(index(str,"gate_comm").ne.0)
    gate_proc=(index(str,"gate_proc").ne.0)

    iErr=0

    ! gate
    if (gate_off) then
      ! no gating
      rconf%GateOp=0
      rconf%OpenGateSize=0
      rconf%WriteGateSize=0
      rconf%CloseGateSize=0

    else &
    if (gate_comm) then
      ! gate by comm id
      rconf%GateOp=1
      ! interpret -1 to mean all comms
      if (openGateSize.eq.-1) openGateSize=rconf%NComms
      if (writeGateSize.eq.-1) writeGateSize=rconf%NComms
      if (closeGateSize.eq.-1) closeGateSize=rconf%NComms
      ! restrict to valid values
      rconf%OpenGateSize=max(openGateSize,1)
      rconf%OpenGateSize=min(openGateSize,rconf%NComms)
      rconf%WriteGateSize=max(writeGateSize,1)
      rconf%WriteGateSize=min(writeGateSize,rconf%NComms)
      rconf%CloseGateSize=max(closeGateSize,1)
      rconf%CloseGateSize=min(closeGateSize,rconf%NComms)

    else &
    if (gate_proc) then
      ! gate by proc id
      rconf%GateOp=2
      rconf%OpenGateSize=min(openGateSize,rconf%WorldSize)
      rconf%WriteGateSize=min(writeGateSize,rconf%WorldSize)
      rconf%CloseGateSize=min(closeGateSize,rconf%WorldSize)

    else
      write(0,*)"Error: One of gate_off,gate_comm,gate_proc must be used."
      iErr=1
      return
    end if

  end subroutine

  !-----------------------------------------------------------------------------
  subroutine RunConfigLoad(rconf,fileName,iErr)
    implicit none
    type(RunConfig) rconf
    type(UnitManager),pointer :: unitMan
    character(len=*) fileName
    integer iErr
    integer unitNo
    integer idx1,idx2
    ! list IO
    integer nX(3)                         ! grid dimensions array
    integer stripeSize                    ! Lustre strpe size, actual
    integer stripeCount                   ! Lustre stripe count, number of OST's to include
    integer useCollectiveIO               ! Turn on/off MPI collective IO
    integer collectOp                     ! enum collection method.
    integer nCIONodes                     ! Number of IO nodes to use
    integer cioBufSize                    ! Size of buffer on IO nodes
    integer clientOSTRatio                ! Ratio Client/OST.
    integer collectThreshold              ! Threshold above which collective IO is off.
    integer useDataSieving                ! Turn data sieving on/off
    integer sieveBufSize                  ! Size of buffer of data sieving
    character(len=32) fileSplitOp         ! enum splitting method
    character(len=32) commSplitOp         ! enum splitting method
    integer nComms                        ! number of sub comm in spilt comm IO
    character(len=32) gateOp              ! enum IO gate method
    integer openGateSize                  ! number concurrent open
    integer writeGateSize                 ! number concurrent write
    integer closeGateSize                 ! number concurrent close

    ! List IO for run time configuration
    namelist /IOList/       &
        nX,                 &
        stripeSize,         &
        stripeCount,        &
        useCollectiveIO,    &
        collectOp,          &
        nCIONodes,          &
        cioBufSize,         &
        clientOSTRatio,     &
        collectThreshold,   &
        useDataSieving,     &
        sieveBufSize,       &
        fileSplitOp,        &
        commSplitOp,        &
        nComms,             &
        gateOp,             &
        openGateSize,       &
        writeGateSize,      &
        closeGateSize

    iErr=0

    ! one process reads the config and distrubutes to the others
    if (rconf%WorldRank.eq.0) then

      ! get the base of the config file name
      idx2=index(fileName,".conf",.true.)
      if (idx2.eq.0) then
        write(0,*)"Error: configuration file is expected to have the '.conf' extension."
        iErr=1
        return
      end if
      idx2=idx2-1
      idx1=index(fileName,"/",.true.)
      idx1=max(idx1,1)
      rconf%RunName=fileName(idx1:idx2)

      ! Read the run configuration
      unitMan => NewUnitManager()
      unitNo=UnitManagerGetUnit(unitMan,iErr)
      call DeleteUnitManager(unitMan)
      if (iErr.ne.0) return

      open(                 &
          unit=unitNo,      &
          file=fileName,    &
          form='formatted', &
          status='old',     &
          iostat=iErr)
      if (iErr.ne.0) then
        write(0,*)"Error: could not open configuration in ",trim(fileName),"."
        write(0,*)"Expected format:"
        write(0,IOList)
        return
      end if

      read(unit=unitNo,nml=IOList,iostat=iErr)
      close(unit=unitNo)
      if (iErr.ne.0) then
        write(0,*)"Error: could not read configuration in ",trim(fileName),"."
        write(0,*)"Expected format:"
        write(0,IOList)
        return
      endif

      ! page align stripe
      call lfPageAlign(rconf%StripeSize)

      rconf%NX=nX
      rconf%StripeSize=stripeSize
      rconf%StripeCount=stripeCount
      rconf%UseCollectiveIO=useCollectiveIO
      rconf%CollectOp=collectOp
      rconf%NCIONodes=nCIONodes
      rconf%CIOBufSize=cioBufSize
      rconf%ClientOSTRatio=clientOSTratio
      rconf%CollectThreshold=collectThreshold
      rconf%UseDataSieving=useDataSieving
      rconf%SieveBufSize=sieveBufSize
      rconf%NComms=nComms
      rconf%OpenGateSize=openGateSize
      rconf%WriteGateSize=writeGateSize
      rconf%CloseGateSize=closeGateSize

      call RunConfigParseCommSplitOp(rconf,commSplitOp,iErr)
      if (iErr.ne.0) stop

      call RunConfigParseFileSplitOp(rconf,fileSplitOp,iErr)
      if (iErr.ne.0) stop

      call RunConfigParseGateOp(&
            rconf,              &
            gateOp,             &
            openGateSize,       &
            writegateSize,      &
            closeGateSize,      &
            iErr)
      if (iErr.ne.0) stop

    end if

    ! distribute 
    call MPI_Bcast(rconf%NX,3,MPI_INTEGER ,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%StripeSize,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%StripeCount,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%API,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%UseCollectiveIO,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%NCIONodes,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%CIOBufSize,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%ClientOSTRatio,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%UseDataSieving,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%SieveBufSize,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%CollectThreshold,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%CollectOp,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%FileSplitOp,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%NFiles,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%CommSplitOp,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%NComms,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%OpenGateSize,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%WriteGateSize,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%CloseGateSize,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%GateOp,1,MPI_INTEGER,0,MPI_COMM_WORLD,iErr)
    call MPI_Bcast(rconf%RunName,256,MPI_CHARACTER,0,MPI_COMM_WORLD,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  ! NOTE: caller must free the retruned info object.
  function RunConfigGetFileHints(rconf) result(info)
    implicit none
    type(RunConfig) rconf
    integer info
    integer :: iErr

    iErr=0
    info=MPI_INFO_NULL

    call MPI_Info_create(info,iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to create info object."
      return
    end if

    call RunConfigCopyFileHints(rconf,info,iErr)

  end function


  !----------------------------------------------------------------------------
  subroutine RunConfigCopyFileHints(rconf,info,iErr)
    implicit none
    type(RunConfig) rconf
    integer info
    integer iErr

    !if (RunConfigGetCollectOp(rconf).ge.0) then
    !  call lfsetenv(                    &
    !      "MPICH_MPIIO_CB_ALIGN",       &
    !      RunConfigGetCollectOp(rconf), &
    !      iErr)
    !  if (iErr.ne.0) return
    !end if

    !if (RunConfigGetCollectOp(rconf).ge.0) then
    !  call MPIFileHintSet(              &
    !      info,                         &
    !      "cb_align",                   &
    !      RunConfigGetCollectOp(rconf), &
    !      iErr)
    !  if (iErr.ne.0) return
    !end if

    if (RunConfigGetStripeCount(rconf).gt.0) then
      call MPIFileHintSet(                &
          info,                           &
          "striping_factor",              &
          RunConfigGetStripeCount(rconf), &
          iErr)
      if (iErr.ne.0) return
    end if

    if (RunConfigGetStripeSize(rconf).gt.0) then
      call MPIFileHintSet(               &
          info,                          &
          "striping_unit",               &
          RunConfigGetStripeSize(rconf), &
          iErr)
      if (iErr.ne.0) return
    end if

    ! call MPIFileHintSet(               &
    !     info,                          &
    !     "romio_lustre_start_iodevice", &
    !     "-1",                          &
    !     iErr)
    ! if (iErr.ne.0) return

    if (RunConfigGetUseCollectiveIO(rconf).ge.0) then
      call MPIFileHintSet(                    &
          info,                               &
          "romio_cb_write",                   &
          RunConfigGetUseCollectiveIO(rconf), &
          "enable",                           &
          "disable",                          &
          "automatic",                        &
          iErr)
      if (iErr.ne.0) return
    end if

    if ((RunConfigGetUseCollectiveIO(rconf).ge.1)) then
      ! CIO enabled or automatic

      if (RunConfigGetCIOBufSize(rconf).gt.0) then
        call MPIFileHintSet(               &
            info,                          &
            "cb_buffer_size",              &
            RunConfigGetCIOBufSize(rconf), &
            iErr)
        if (iErr.ne.0) return
      end if

      if (RunConfigGetNCIONodes(rconf).gt.0) then
        call MPIFileHintSet(              &
            info,                         &
            "cb_nodes",                   &
            RunConfigGetNCIONodes(rconf), &
            iErr)
        if (iErr.ne.0) return
      end if

#ifdef USE_DEFERED_OPEN
      ! NOTE: On Kraken CB_ALIGN=2 causes hang when used 
      ! with this hint.
      call MPIFileHintSet(                    &
          info,                               &
          "romio_no_indep_rw",                &
          "true",                             &
          iErr)
      if (iErr.ne.0) return
#endif

    end if

    if (RunConfigGetUseDataSieving(rconf).ge.0) then
      call MPIFileHintSet(                   &
          info,                              &
          "romio_ds_write",                  &
          RunConfigGetUseDataSieving(rconf), &
          "enable",                          &
          "disable",                         &
          "automatic",                       &
          iErr)
      if (iErr.ne.0) return
    end if

    if (RunConfigGetUseDataSieving(rconf).gt.0) then
      ! sieving enabled or automatic

      if (RunConfigGetSieveBufSize(rconf).gt.0) then
        call MPIFileHintSet(                 &
            info,                            &
            "ind_wr_buffer_size",            &
            RunConfigGetSieveBufSize(rconf), &
            iErr)
        if (iErr.ne.0) return
      end if

    end if

    if (RunConfigGetClientOSTRatio(rconf).gt.0) then
      call MPIFileHintSet(                    &
          info,                               &
          "romio_lustre_co_ratio",            &
          RunConfigGetClientOSTRatio(rconf),  &
          iErr)
      if (iErr.ne.0) return
    endif

    if (RunConfigGetCollectThreshold(rconf).gt.0) then
      call MPIFileHintSet(                      &
          info,                                 &
          "romio_lustre_coll_threshold",        &
          RunConfigGetCollectThreshold(rconf),  &
          iErr)
      if (iErr.ne.0) return
    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine RunConfigSetWorldSize(rconf)
    use mpi

    implicit none
    type(RunConfig) rconf
    integer iErr

    call MPI_Comm_size(MPI_COMM_WORLD,rconf%WorldSize,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine RunConfigSetWorldRank(rconf)
    use mpi

    implicit none
    type(RunConfig) rconf
    integer iErr

    call MPI_Comm_rank(MPI_COMM_WORLD,rconf%WorldRank,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine RunConfigStreamString(rconf,str)
    implicit none
    type(RunConfig) rconf
    type(String) str

    call StringAppend(str,trim(StringNameValue("WorldSize",rconf%WorldSize)))
    call StringAppend(str,trim(StringNameValue("NX",rconf%NX,"I10","A24,A44")))
    call StringAppend(str,trim(StringNameValue("StripeSize",rconf%StripeSize)))
    call StringAppend(str,trim(StringNameValue("StripeCount",rconf%StripeCount)))
    call StringAppend(str,trim(StringNameValue("API",rconf%API)))
    call StringAppend(str,trim(StringNameValue("UseCollectiveIO",rconf%UseCollectiveIO)))
    call StringAppend(str,trim(StringNameValue("CollectOp",rconf%CollectOp)))
    call StringAppend(str,trim(StringNameValue("NCIONodes",rconf%NCIONodes)))
    call StringAppend(str,trim(StringNameValue("CIOBufSize",rconf%CIOBufSize)))
    call StringAppend(str,trim(StringNameValue("ClientOSTRatio",rconf%ClientOSTRatio)))
    call StringAppend(str,trim(StringNameValue("CollectThreshold",rconf%NCIONodes)))
    call StringAppend(str,trim(StringNameValue("UseDataSieving",rconf%UseDataSieving)))
    call StringAppend(str,trim(StringNameValue("SieveBufSize",rconf%SieveBufSize)))
    call StringAppend(str,trim(StringNameValue("FileSplitOp",rconf%FileSplitOp)))
    call StringAppend(str,trim(StringNameValue("NFiles",rconf%NFiles)))
    call StringAppend(str,trim(StringNameValue("CommSplitOp",rconf%CommSplitOp)))
    call StringAppend(str,trim(StringNameValue("NComms",rconf%NComms)))
    call StringAppend(str,trim(StringNameValue("OpenGateSize",rconf%OpenGateSize)))
    call StringAppend(str,trim(StringNameValue("WriteGateSize",rconf%WriteGateSize)))
    call StringAppend(str,trim(StringNameValue("CloseGateSize",rconf%CloseGateSize)))
    call StringAppend(str,trim(StringNameValue("GateOp",rconf%GateOp)))
#ifdef SYNC_POSIX
    call StringAppend(str,trim(StringNameValue("SYNC_POSIX",1)))
#endif
#ifdef SYNC_MPIIO
    call StringAppend(str,trim(StringNameValue("SYNC_MPIIO",1)))
#endif
#ifdef SYNC_MPIIO_CB
    call StringAppend(str,trim(StringNameValue("SYNC_MPIIO_CB",1)))
#endif

  end subroutine


end module
