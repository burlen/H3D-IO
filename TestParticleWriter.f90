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

program main 
  implicit none
  include 'mpif.h'
  integer procId,nProcs,iErr
  real X(10000), Y(10000), Z(10000)    ! particle position
  real Vx(10000), Vy(10000), Vz(10000) ! particle velocity
  integer ALen      ! actual what we use
  character fileName*(*)
  parameter(fileName='./particles.dat')
  call MPI_Init(iErr) 
  call MPI_Comm_rank(MPI_COMM_WORLD, procId, iErr) 
  call MPI_Comm_size(MPI_COMM_WORLD, nProcs, iErr) 
  if (procId.eq.0) then
    write(0,*)'Distributed Particle Writer Test'
    write(0,*)'Creating: ',fileName
  endif
  ALen=procId+1
  call GenerateData(ALen,X,Y,Z,Vx,Vy,Vz)
  call WriteParticles(fileName,ALen,X,Y,Z,Vx,Vy,Vz)
  call MPI_Finalize(iErr)
  return
end program 
!==============================================================================
subroutine GenerateData(n,X,Y,Z,Vx,Vy,Vz)
  implicit none
  integer n
  real X(n),Y(n),Z(n)
  real Vx(n),Vy(n),Vz(n)
  integer i

   ! This is just some arbitrary data. The file should look
   ! like:
   ! 1.1 1.2 1.3 1.1 1.2 1.3
   ! 2.1 2.2 2.3 1.1 1.2 1.3
   ! 2.1 2.2 2.3 2.1 2.2 2.3
   ! ...
   ! n.1 n.2 n.3 n.1 n.2 n.3
  do i=1,n
    X(i)=real(n)
    X(i)=X(i)+0.1
    Y(i)=real(n)
    Y(i)=Y(i)+0.2
    Z(i)=real(n)
    Z(i)=Z(i)+0.3
    Vx(i)=real(i)
    Vx(i)=Vx(i)+0.1
    Vy(i)=real(i)
    Vy(i)=Vy(i)+0.2
    Vz(i)=real(i)
    Vz(i)=Vz(i)+0.3
  enddo
  return
end subroutine
