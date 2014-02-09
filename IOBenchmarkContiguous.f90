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

! H3D IO contiguous data benchmark

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
  type(MPIFile),pointer :: partFile => null()        ! file for data output
  type(LogFile),pointer :: runLog => null()          ! log file for timings
  integer masterRank

  real*8, pointer, dimension(:) :: X => null()        ! local particle data
!   real*8, pointer, dimension(:) :: Y => null()
!   real*8, pointer, dimension(:) :: Z => null()
!   real*8, pointer, dimension(:) :: Vx => null()
!   real*8, pointer, dimension(:) :: Vy => null()
!   real*8, pointer, dimension(:) :: Vz => null()
  integer*8 np
  ! integer*8 GetNumberOfLocalParticles

  integer worldRank,worldSize

  character(len=512 ) workingDir       ! path to place run results in (passed on command tail)
  character(len=512 ) configFileName   ! file containing runtime configuration (passed on command tail)
  character(len=1024) logFileName      ! file where timing result are written

  integer iErr

  ! start up
  call MPI_Init(iErr)
  call MPI_Comm_rank(MPI_COMM_WORLD, worldRank, iErr)
  call MPI_Comm_size(MPI_COMM_WORLD, worldSize, iErr)

  masterRank=0

  call ProcessCommandLine(  &
      worldRank,            &
      configFileName,       &
      logFilename,          &
      workingDir,           &
      iErr)
  if (iErr.ne.0) stop

  ! load run configuration from file
  config => NewRunConfig()
  call RunConfigLoad(config,configFileName,iErr)
  if (iErr.ne.0) stop

  ! get the number of bytes assigned to this process.
  np=RunConfigGetPerProcessDataSize(config)
  np=np/8;

  ! ! compute the number of particles owned by this process.
  ! np=GetNumberOfLocalParticles( &
  !       worldRank,              &
  !       worldSize,              &
  !       RunConfigGetNX(config))

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
  call StringAppend(logStream,StringNameValue("NPartLocal",np))
  call LogFileWriteHeader(runLog,"H3D-IO-Benchmark-Contiguous",iErr)
  call LogFileWrite(runLog,logStream,iErr)
  call LogFileMarkEventStart(runLog,iErr)
  call DeleteString(logStream)

  ! open the gda file
  partFile => NewMPIFileContiguous(         &
      MPI_COMM_WORLD,                       &
      masterRank,                           &
      trim(workingDir)//"/part_1.h3dp",     &
      ior(MPI_MODE_WRONLY,MPI_MODE_CREATE), &
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

  if (worldRank.ne.masterRank) then

    ! allocate and initialize the local array
    allocate(X(np))
    ! allocate(Y(np))
    ! allocate(Z(np))
    ! allocate(Vx(np))
    ! allocate(Vy(np))
    ! allocate(Vz(np))

    select case (RunConfigGetCommSplitOp(config))

      case (0,1)
        ! 0 -comm self
        ! 1 - comm world
        ! color by proc id
        call GenerateParticleData( &
              real(worldRank,8),   &
              np,                  &
              X)!,                   &
              !Y,                   &
              !Z,                   &
              !Vx,                  &
              !Vy,                  &
              !Vz)

      case (2,3)
        ! 2 - slab split
        ! 3 - stripe
        ! color by comm id
        call GenerateParticleData(                &
              real(MPIFileGetCommId(partFile),8), &
              np,                                 &
              X)!,                                  &
              !Y,                                  &
              !Z,                                  &
              !Vx,                                 &
              !Vy,                                 &
              !Vz)

    end select
  end if

  ! write the file
  call MPIFileSeekF(partFile,np,MPI_REAL8,iErr)
  if (iErr.ne.0) stop
  call MPIFileWrite(partFile,X,iErr)
  if (iErr.ne.0) stop

!   call MPIFileSeekF(partFile,np,MPI_REAL8,iErr)
!   if (iErr.ne.0) stop
!   call MPIFileWrite(partFile,Y,iErr)
!   if (iErr.ne.0) stop
! 
!   call MPIFileSeekF(partFile,np,MPI_REAL8,iErr)
!   if (iErr.ne.0) stop
!   call MPIFileWrite(partFile,Z,iErr)
!   if (iErr.ne.0) stop
! 
!   call MPIFileSeekF(partFile,np,MPI_REAL8,iErr)
!   if (iErr.ne.0) stop
!   call MPIFileWrite(partFile,Vx,iErr)
!   if (iErr.ne.0) stop
! 
!   call MPIFileSeekF(partFile,np,MPI_REAL8,iErr)
!   if (iErr.ne.0) stop
!   call MPIFileWrite(partFile,Vy,iErr)
!   if (iErr.ne.0) stop
! 
!   call MPIFileSeekF(partFile,np,MPI_REAL8,iErr)
!   if (iErr.ne.0) stop
!   call MPIFileWrite(partFile,Vz,iErr)
!   if (iErr.ne.0) stop

  ! clean up
!   call MPIFilePrintSelf(partFile,0,iErr)
  call MPIFileClose(partFile,iErr)
  call DeleteMPIFile(partFile)

  if (worldRank.ne.masterRank) then
    deallocate(X)
    ! deallocate(Y)
    ! deallocate(Z)
    ! deallocate(Vx)
    ! deallocate(Vy)
    ! deallocate(Vz)
  end if

  call DeleteRunConfig(config)

  ! log elapsed time
  call LogFileMarkEventEnd(runLog,iErr)
  call LogFileWriteEvent(runLog,"Total_run_time",iErr)
  call LogFileClose(runLog,iErr)
  call DeleteLogFile(runLog)

  call MPI_Finalize(iErr)

  return
end program


!----------------------------------------------------------------------------
function GetNumberOfLocalParticles(worldRank,worldSize,n) result(nLocal)
  implicit none
  integer n(3)
  integer*8 npX(3)
  integer*8 nLocal,nLarge,nTotal
  integer worldRank,worldSize

  npX(1)=n(1)
  npX(2)=n(2)
  npX(3)=n(3)

  nTotal=npX(1)*npX(2)*npX(3)

  nLocal=nTotal/worldSize
  nLarge=mod(nTotal,worldSize)

  if (worldRank.lt.nLarge) then
    nLocal=nLocal+1
  end if

  if (nLocal.lt.1) then
    write(0,*)"Error: No particles on ",worldRank
    stop
  endif

end function

!------------------------------------------------------------------------------
subroutine init_random_seed()
  INTEGER :: i, n, clock
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed

  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))

  CALL SYSTEM_CLOCK(COUNT=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  CALL RANDOM_SEED(PUT = seed)

  DEALLOCATE(seed)
end subroutine

!------------------------------------------------------------------------------
subroutine GenerateParticleData(c,n,X)!,Y,Z,Vx,Vy,Vz)
  implicit none
  integer*8 n
  real*8 c
  real*8 X(n)!,Y(n),Z(n)
!   real*8 Vx(n),Vy(n),Vz(n)
  integer i

  call init_random_seed()

  call random_number(X)
!   call random_number(Y)
!   call random_number(Z)
!   call random_number(Vx)
!   call random_number(Vy)
!   call random_number(Vz)

  do i=1,n
    ! center a unit ball on x=c
    X(i)=X(i)-0.5+c
    ! center others on origin
!     Y(i)=Y(i)-0.5
!     Z(i)=Z(i)-0.5
!     Vx(i)=Vx(i)-0.5
!     Vy(i)=Vy(i)-0.5
!     Vz(i)=Vz(i)-0.5

    ! write(0,*)i,',',X(i),',',Y(i),',',Z(i),',',Vx(i),',',Vy(i),',',Vz(i)

  enddo

  return
end subroutine
