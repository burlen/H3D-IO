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

module DoubleVectorModule

  type DoubleVector
    real*8 , pointer, dimension(:) :: DData
    integer*8 DDataAt
    integer*8 DDataEnd
  end type

  interface NewDoubleVector
    module procedure NewDoubleVectorDefault
    module procedure NewDoubleVectorCopy
  end interface

  interface DoubleVectorAppend
    module procedure DoubleVectorAppendN
    module procedure DoubleVectorAppendRange
  end interface

contains

  !----------------------------------------------------------------------------
  function NewDoubleVectorDefault() result(vec)
    implicit none
    type(DoubleVector), pointer :: vec
    integer iErr

    allocate(vec,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate object DoubleVector."
      stop
    end if

    vec%DData => null()
    vec%DDataEnd=0
    vec%DDataAt=1

    call DoubleVectorRealloc(vec,16_8)

  end function

  !----------------------------------------------------------------------------
  function NewDoubleVectorCopy(other) result(vec)
    implicit none
    type(DoubleVector), pointer :: vec
    type(DoubleVector) other
    integer iErr

    allocate(vec,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate object DoubleVector."
      stop
    end if

    call DoubleVectorTruncate(vec)
    call DoubleVectorAppend(vec,other%DData,DoubleVectorGetSize(other))

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteDoubleVector(vec)
    implicit none
    type(DoubleVector), pointer :: vec

    if (.not.associated(vec)) return

    if (associated(vec%DData)) then
      deallocate(vec%DData)
    end if
    nullify(vec%DData)

    deallocate(vec)
    nullify(vec)

  end subroutine 

  !---------------------------------------------------------------------------
  function DoubleVectorGetSize(vec) result(n)
    implicit none
    type(DoubleVector) vec
    integer*8 n

    n=vec%DDataAt-1

  end function

  !---------------------------------------------------------------------------
  subroutine DoubleVectorTruncate(vec)
    implicit none
    type(DoubleVector) vec

    vec%DDataAt=1

  end subroutine

  !----------------------------------------------------------------------------
  function DoubleVectorGetAllocatedSize(vec) result(n)
    implicit none
    type(DoubleVector) vec
    integer*8 n

    n=vec%DDataEnd

  end function

  !----------------------------------------------------------------------------
  function DoubleVectorGetFreeSpace(vec) result(n)
    implicit none
    type(DoubleVector) vec
    integer*8 n

    n=vec%DDataEnd-vec%DDataAt+1

  end function

  !----------------------------------------------------------------------------
  subroutine DoubleVectorRealloc(vec,n)
    implicit none
    type(DoubleVector) vec
    real*8,pointer,dimension(:) :: newData
    integer*8 n,m,i
    integer iErr

    if (n.lt.1) then
      write(0,*)"Error: Request reallocation to 0 doubles."
      stop
    end if
    if (n.gt.67108864) then
      write(0,*)"Error: Request reallocation greater than 64M doubles."
      stop
    end if

    ! allocte the new array
    allocate(newData(n),stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate newData."
      stop
    end if

    ! copy
    m=DoubleVectorGetSize(vec)
    do i=1,m
      newData(i)=vec%DData(i)
    end do

    ! initialize
    do i=m+1,n
      newData(i)=0.0
    end do

    !  free the old array
    if (associated(vec%DData)) then
      deallocate(vec%DData)
    end if

    ! asign
    vec%DData=>newData
    vec%DDataEnd=n

  end subroutine

  !----------------------------------------------------------------------------
  subroutine DoubleVectorPush(vec,val)
    implicit none
    type(DoubleVector) vec
    real*8 val
    real*8 ddata(1)

    ddata(1)=val
    call DoubleVectorAppendRange(vec,ddata,1_8,1_8)

  end subroutine

  !----------------------------------------------------------------------------
  function DoubleVectorPop(vec) result(ddata)
    implicit none
    type(DoubleVector) vec
    real*8 ddata

    vec%DDataAt=vec%DDataAt-1

    ddata=vec%DData(vec%DDataAt)

  end function

  !----------------------------------------------------------------------------
  subroutine DoubleVectorAppendN(vec,ddata,n)
    implicit none
    type(DoubleVector) vec
    real*8 ddata(:)
    integer*8 n

    call DoubleVectorAppendRange(vec,ddata,1_8,n)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine DoubleVectorAppendRange(vec,ddata,startId,endId)
    implicit none
    type(DoubleVector) vec
    real*8 ddata(:)
    integer*8 i,j
    integer*8 reqSize,curSize,newSize,startId,endId,ddataSize

    ddataSize=endId-startId+1

    ! resize
    reqSize=DoubleVectorGetSize(vec)+ddataSize
    curSize=DoubleVectorGetAllocatedSize(vec)

    if (reqSize.gt.curSize) then
      newSize=curSize
      do
        if (reqSize.le.newSize) exit
        newSize=2*newSize
      end do

      call DoubleVectorRealloc(vec,newSize)

    end if

    ! append
    i=vec%DDataAt
    do j=startId,endId
      vec%DData(i)=ddata(j)
      i=i+1
    end do

    vec%DDataAt=vec%DDataAt+ddataSize

  end subroutine

  !----------------------------------------------------------------------------
  subroutine DoubleVectorWrite(vec,unitNo,iErr)
    implicit none
    type(DoubleVector) vec
    integer unitNo
    integer n
    integer iErr
    character(len=256) eStr

    n=DoubleVectorGetSize(vec)

    ! write(unit=unitNo,fmt='(A,I5)')'DataAt=',vec%DDataAt
    ! write(unit=unitNo,fmt='(A,I5)')'DataEnd=',vec%DDataEnd
    ! write(unit=unitNo,fmt='(A,I5)')'Size=',n

    write(unit=unitNo,iostat=iErr,iomsg=eStr)vec%DData(1:n)
    if (iErr.ne.0) then
      write(0,*)"Error: ",trim(eStr)
      write(0,*)"Error: Failed to write file."
      return
    end if

  end subroutine


end module
