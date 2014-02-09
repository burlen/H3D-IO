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

! Write particle data to a single file using MPI collective IO 
!==============================================================================
function WriteParticles(fileName,n,X,Y,Z,Vx,Vy,Vz)
  implicit none
  include 'mpif.h'
  integer WriteParticles, WriteDistributedArray
  character fileName*(*) ! file to write paricles to
  integer n              ! number of particles
  real X(n),Y(n),Z(n),Vx(n),Vy(n),Vz(n) ! data from the simulation
  real P(6*n)
  integer procId
  integer nProcs
  integer iErr
  integer i,q

  ! transfer the data into a single array.
  q=1
  do i=1,n
    P(q)=X(i);  q=q+1
    P(q)=Y(i);  q=q+1
    P(q)=Z(i);  q=q+1
    P(q)=Vx(i); q=q+1
    P(q)=Vy(i); q=q+1
    P(q)=Vz(i); q=q+1
  enddo

  ! Write the particle array to disk
  call MPI_Comm_rank(MPI_COMM_WORLD, procId, iErr) 
  call MPI_Comm_size(MPI_COMM_WORLD, nProcs, iErr) 
  WriteParticles=WriteDistributedArray(procId,nProcs,fileName,P,n*6)

  return
end function

!==============================================================================
! Write a portion of a distributed array to a single file using MPI IO
!==============================================================================
function WriteDistributedArray(procId,nProcs,fileName,A,n)
  implicit none
  include 'mpif.h' 
  integer WriteDistributedArray ! return 0 on error, 1 on success
  character fileName*(*) ! File name to write to
  integer n              ! size of our portion of the distributed array
  real A(n)              ! our portion of the distributed array
  integer procId         ! my rank in the communicator
  integer nProcs         ! number of process in the communicator
  integer (kind=MPI_OFFSET_KIND) writeOffset
  integer fileHandle, iErr,iErr2,eStrLen
  integer stat(MPI_STATUS_SIZE)
  integer mode
  integer arrayLens(0:nProcs-1)
  integer i
  integer SIZE_OF_REAL
  parameter(SIZE_OF_REAL=4)
  character eStr*(1024)
  eStrLen=1024
  WriteDistributedArray=0

  ! collect other processes array lengths
  arrayLens(procId+1)=n
  call MPI_Allgather(n,1,MPI_REAL,arrayLens,1,MPI_REAL,MPI_COMM_WORLD,iErr)
  ! sum them up, we skip over previous processes sub-arrays
  writeOffset=0
  i=0
  do while (i.lt.procId)
    writeOffset=writeOffset+arrayLens(i)
    i=i+1
  end do
  ! convert the offset into bytes
  writeOffset=writeOffset*SIZE_OF_REAL
  
  ! Open the file.
  mode=MPI_MODE_WRONLY+MPI_MODE_CREATE
  call MPI_FILE_OPEN(MPI_COMM_WORLD,fileName,mode,MPI_INFO_NULL,fileHandle,iErr)
  if (iErr.ne.MPI_SUCCESS) then
    call MPI_Error_string(iErr,eStr,eStrLen,iErr2)
    write(0,*)'Error: Could not open file ',fileName
    write(0,*)eStr
    write(0,*)'Write aborted.'
    return
  endif
  ! Write the array
  call MPI_File_write_at_all(fileHandle,writeOffset,A,n,MPI_REAL,stat,iErr)
  call MPI_File_close(fileHandle,iErr2)
  if (iErr.ne.MPI_SUCCESS) then
    call MPI_Error_string(iErr,eStr,eStrLen,iErr2)
    write(0,*)'Error: Could not write ',fileName
    write(0,*)eStr
    write(0,*)'Write aborted.'
    return
  endif

  WriteDistributedArray=1
  return
end function
