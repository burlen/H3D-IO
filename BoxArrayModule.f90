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

module BoxArrayModule
  use BoxModule

interface NewBoxArray
  module procedure NewBoxArray4
  module procedure NewBoxArray8
end interface

interface DeleteBoxArray
  module procedure DeleteBoxArray4
  module procedure DeleteBoxArray8
end interface

interface BoxArrayInit
  module procedure BoxArrayInit4
  module procedure BoxArrayInit8
end interface

contains
  !----------------------------------------------------------------------------
  subroutine NewBoxArray4(bbox,A)
    implicit none
    type(Box) bbox
    real*4, pointer, dimension(:,:,:) :: A
    integer iErr

    allocate(                   &
        A(bbox%I(1):bbox%I(2),  &
          bbox%I(3):bbox%I(4),  &
          bbox%I(5):bbox%I(6)), &
          stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Failed to allocate array."
      stop
    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine NewBoxArray8(bbox,A)
    implicit none
    type(Box) bbox
    real*8, pointer, dimension(:,:,:) :: A
    integer iErr

    allocate(                   &
        A(bbox%I(1):bbox%I(2),  &
          bbox%I(3):bbox%I(4),  &
          bbox%I(5):bbox%I(6)), &
          stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Failed to allocate array."
      stop
    end if

  end subroutine

  !----------------------------------------------------------------------------
  subroutine DeleteBoxArray4(A)
    implicit none
    real*4, pointer, dimension(:,:,:) :: A

    if (.not.associated(A)) return

    deallocate(A)
    nullify(A)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine DeleteBoxArray8(A)
    implicit none
    real*8, pointer, dimension(:,:,:) :: A

    if (.not.associated(A)) return

    deallocate(A)
    nullify(A)

  end subroutine

  !------------------------------------------------------------------------------
  subroutine BoxArrayInit4(bbox,A,val)
    implicit none
    type(Box) bbox
    real*4 A(                 &
        bbox%I(1):bbox%I(2),  &
        bbox%I(3):bbox%I(4),  &
        bbox%I(5):bbox%I(6))
    real*4 val
    integer i,j,k

    do k=bbox%I(5),bbox%I(6)
      do j=bbox%I(3),bbox%I(4)
        do i=bbox%I(1),bbox%I(2)
          A(i,j,k)=val
        enddo
      enddo
    enddo

    return
  end subroutine

  !------------------------------------------------------------------------------
  subroutine BoxArrayInit8(bbox,A,val)
    implicit none
    type(Box) bbox
    real*8 A(                 &
        bbox%I(1):bbox%I(2),  &
        bbox%I(3):bbox%I(4),  &
        bbox%I(5):bbox%I(6))
    real*4 val
    integer i,j,k

    do k=bbox%I(5),bbox%I(6)
      do j=bbox%I(3),bbox%I(4)
        do i=bbox%I(1),bbox%I(2)
          A(i,j,k)=val
        enddo
      enddo
    enddo

    return
  end subroutine

end module
