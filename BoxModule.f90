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

module BoxModule
  !============================================================================
  ! multidimensional cartesian index space.
  type Box
    integer I(6)
  end type Box

interface NewBox
  module procedure NewBox0
  module procedure NewBox6
end interface

interface BoxSetExtents
  module procedure BoxSetExtents1
  module procedure BoxSetExtents6
end interface

interface BoxPrintSelf
  module procedure BoxPrintSelfUnit
  module procedure BoxPrintSelfString
end interface

contains


  !----------------------------------------------------------------------------
  function NewBox0() result(b)
    implicit none
    type(Box), pointer :: b

    b => NewBox(1,-1,1,-1,1,-1)

  end function

  !----------------------------------------------------------------------------
  function NewBox6(ilo,ihi,jlo,jhi,klo,khi) result(b)
    implicit none
    type(Box), pointer :: b
    integer, intent(in) :: ilo,ihi,jlo,jhi,klo,khi
    integer iErr

    allocate(b,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: failed to allocate Box."
      stop
    end if

    call BoxSetExtents(b,ilo,ihi,jlo,jhi,klo,khi)

  end function


  !----------------------------------------------------------------------------
  subroutine DeleteBox(b)
    implicit none
    type(Box), pointer :: b

    if (associated(b)) then
      deallocate(b)
    end if

    nullify(b)

  end subroutine

  !----------------------------------------------------------------------------
  ! Set the box extents (the index space in the simulation grid)
  !interface SetExtents
  subroutine BoxSetExtents6(b,ilo,ihi,jlo,jhi,klo,khi)
    implicit none
    type(Box) b
    integer, intent(in) :: ilo,ihi,jlo,jhi,klo,khi

    b%I(1)=ilo; b%I(2)=ihi;
    b%I(3)=jlo; b%I(4)=jhi;
    b%I(5)=klo; b%I(6)=khi;

  end subroutine

  !----------------------------------------------------------------------------
  subroutine BoxSetExtents1(b,I)
    implicit none
    type(Box) b
    integer, intent(in) :: I(6)

    b%I(1)=I(1); b%I(2)=I(2);
    b%I(3)=I(3); b%I(4)=I(4);
    b%I(5)=I(5); b%I(6)=I(6);

  end subroutine


  !----------------------------------------------------------------------------
  ! write the box to the prescribed unit
  subroutine BoxPrintSelfString(b,string)
    implicit none
    type(Box), intent(in) :: b
    character(len=*) string

    write(string,'(A,3(I5,A,I5),A)')'(' &
        ,b%I(1),', ',b%I(2)             &
        ,b%I(3),', ',b%I(4)             &
        ,b%I(5),', ',b%I(6),')'

  end subroutine

  !----------------------------------------------------------------------------
  ! write the box to the prescribed unit
  subroutine BoxPrintSelfUnit(b,unitNo)
    implicit none
    type(Box) b
    integer unitNo
    character(len=512) buffer

    call BoxPrintSelf(b,buffer)

    write(unitNo,*)trim(buffer)

  end subroutine

  !----------------------------------------------------------------------------
  ! Computes the box's size in each dimension.
  subroutine BoxGetDimensions(b, dims)
    implicit none
    type(Box) b
    integer dims(3)

    dims(1)=b%I(2)-b%I(1)+1
    dims(2)=b%I(4)-b%I(3)+1
    dims(3)=b%I(6)-b%I(5)+1

  end subroutine

  !----------------------------------------------------------------------------
  ! Computes the number of cells,(box size) in all dimensions.
  function BoxGetSize(b) result(n)
    implicit none
    type(Box) b
    integer n
    integer dims(3)

    call BoxGetDimensions(b,dims)

    n=dims(1)*dims(2)*dims(3)

  end function

end module
