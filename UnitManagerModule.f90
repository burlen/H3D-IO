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

module UnitManagerModule

  !============================================================================
  type UnitManager
    integer FirstUnit
    integer LastUnit
  end type

contains
  !----------------------------------------------------------------------------
  function NewUnitManager() result(um)
    implicit none
    type(UnitManager), pointer :: um
    integer iErr

    allocate(um,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate the UnitManager."
      stop
    end if

    um%FirstUnit=10
    um%LastUnit=99

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteUnitManager(um)
    implicit none
    type(UnitManager), pointer :: um

    if (.not.associated(um)) return

    deallocate(um)
    nullify(um)

  end subroutine

  !----------------------------------------------------------------------------
  ! This funtion returns the first unopened unit number in the specified
  ! range. If none are unopened then the error flag is set.
  function UnitManagerGetUnit(um,iErr) result(unitNo)
    implicit none
    type(UnitManager) :: um
    integer unitNo
    logical unitValid,unitOpen
    integer iErr

    iErr=0

    ! linear search through the potentially available
    ! unit numbers
    do unitNo=um%FirstUnit,um%LastUnit
      inquire(unit=unitNo,exist=unitValid,opened=unitOpen)
      if (unitValid.and.(.not.unitOpen)) return
    end do

    write(0,*)"Error: All available units have been exhausted."
    iErr=1

  end function

end module
