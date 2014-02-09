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

! H3D IO uniform grid benchmark


!******************************************************************************
program main
  use IOBenchmarkCommandLineModule
  use RunConfigModule
  use CartesianDecompModule
  use MPIFileModule
  use LogFileModule
  use BoxArrayModule
  use mpi

  implicit none
  type(RunConfig),pointer :: config => null()        !
  type(String),pointer :: logStream => null()        !
  type(CartesianDecomp),pointer :: decomp => null()  ! cartesian communicator
  type(Box),pointer :: localGrid => null()           ! local array bounds
  type(MPIFile),pointer :: gdaFile => null()         ! file for data output
  type(LogFile),pointer :: runLog => null()          ! log file for timings
  real*8, pointer, dimension(:,:,:) :: A => null()   ! local array

  integer worldRank,worldSize

  character(len=512 ) workingDir       ! path to place run results in (passed on command tail)
  character(len=512 ) configFileName   ! file containing runtime configuration (passed on command tail)
  character(len=1024) logFileName      ! file where timing result are written

  integer iErr

  ! debugging
  ! integer stat(MPI_STATUS_SIZE)
  ! type(Box),pointer :: remoteGrid => null()
  ! integer remoteRank

  ! start up
  call MPI_Init(iErr)
  call MPI_Comm_rank(MPI_COMM_WORLD, worldRank, iErr)
  call MPI_Comm_size(MPI_COMM_WORLD, worldSize, iErr)

  ! if (worldRank.eq.0) then
  !   write(0,'(I5,A$)')worldRank," Processing command line..."
  ! end if

  call ProcessCommandLine( &
      worldRank,           &
      configFileName,      &
      logFilename,         &
      workingDir,          &
      iErr)
  if (iErr.ne.0) stop

  ! load run configuration from file
  config => NewRunConfig()
  call RunConfigLoad(config,configFileName,iErr)
  if (iErr.ne.0) stop

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5,A$)')worldRank," Decomposing domain..."
  ! end if

  ! decompose the domain
  decomp => NewCartesianDecomp(RunConfigGetNX(config))

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5$)')worldRank; call BoxPrintSelf(decomp%SimGrid,0)
  !   write(0,'(I5$)')worldRank; call BoxPrintSelf(decomp%LocalGrid,0)
  !   remoteGrid => NewBox()
  !   do remoteRank=1,worldSize-1
  !     call MPI_Recv(remoteGrid%I,6,MPI_INTEGER,remoteRank,1,MPI_COMM_WORLD,stat,iErr)
  !     write(0,'(I5$)')remoteRank; call BoxPrintSelf(remoteGrid,0)
  !   end do
  !   write(0,'(I5,A$)')worldRank," Initializing log..."
  ! else
  !   call MPI_Send(decomp%LocalGrid%I,6,MPI_INTEGER,0,1,MPI_COMM_WORLD,iErr)
  ! end if

  ! create the log file, and log run header and parameters
  runLog => NewLogFile()
  call LogFileOpen(   &
      runLog,         &
      logFileName,    &
      MPI_COMM_WORLD, &
      iErr)
  if (iErr.ne.0) stop

  logStream => NewString()
  call RunConfigStream(config,logStream)
  call CartesianDecompStream(decomp,logStream)
  call LogFileWriteHeader(runLog,"H3D-IO-Benchmark-Uniform-Grid",iErr)
  call LogFileWrite(runLog,logStream,iErr)
  call LogFileMarkEventStart(runLog,iErr)
  call DeleteString(logStream)

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5,A$)')worldRank," Opening data file..."
  ! end if

  ! open the gda file
  gdaFile => NewMPIFileUniformGrid(         &
      MPI_COMM_WORLD,                       &
      worldSize-1,                          &
      trim(workingDir)//"/den_1.gda",       &
      ior(MPI_MODE_WRONLY,MPI_MODE_CREATE), &
      MPI_REAL8,                            &
      decomp,                               &
      RunConfigGetCommSplitOp(config),      &
      RunConfigGetNComms(config),           &
      RunConfigGetFileSplitOp(config),      &
      RunConfigGetGateOp(config),           &
      RunConfigGetOpenGateSize(config),     &
      RunConfigGetWriteGateSize(config),    &
      RunConfigGetCloseGateSize(config),    &
      RunConfigGetAPI(config),              &
      RunConfigGetFileHints(config),        &
      runLog,                               &
      iErr)
  if (iErr.ne.0) stop

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5,A$)')worldRank," Allocating arrays..."
  ! end if


  ! allocate and initialize the local array
  localGrid => CartesianDecompGetLocalGrid(decomp)
  call NewBoxArray(localGrid,A)

  select case (RunConfigGetCommSplitOp(config))

    case (0,1)
      ! 0 -comm self
      ! 1 - comm world
      ! color by proc id
      call BoxArrayInit(localGrid,A,real(worldRank))

    case (2,3)
      ! 2 - slab split
      ! 3 - stripe7
      ! color by comm id
      call BoxArrayInit(localGrid,A,real(MPIFileGetCommId(gdaFile)))

  end select

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5,A$)')worldRank," Writing data..."
  ! end if

  ! write the file
  call MPIFileWrite(gdaFile,A,iErr)
  if (iErr.ne.0) stop

  !call MPI_Barrier(MPI_COMM_WORLD,iErr)
  !if (worldRank.eq.0) then
  !  write(0,*)"OK."
  !  write(0,'(I5,A$)')worldRank," Writing header..."
  !end if


  ! write a header for ParaView
  call MPIFileWriteBOVHeader(               &
      gdaFile,                              &
      trim(workingDir)  //                  &
      trim(RunConfigGetRunName(config)) //  &
      ".bov",                               &
      iErr)

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5,A$)')worldRank," Releasing resources..."
  ! end if

  call MPIFileClose(gdaFile,iErr)

  ! clean up
  call DeleteMPIFile(gdaFile)
  call DeleteBoxArray(A)
  call DeleteCartesianDecomp(decomp)
  call DeleteRunConfig(config)

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  !   write(0,'(I5,A$)')worldRank," Writing log..."
  ! end if

  ! log elapsed time
  call LogFileMarkEventEnd(runLog,iErr)
  call LogFileWriteEvent(runLog,"Total_run_time",iErr)
  call LogFileClose(runLog,iErr)
  call DeleteLogFile(runLog)

  ! call MPI_Barrier(MPI_COMM_WORLD,iErr)
  ! if (worldRank.eq.0) then
  !   write(0,*)"OK."
  ! end if

  call MPI_Finalize(iErr)

  ! write(0,'(I5,A)')worldRank," Done."

  return
end program
