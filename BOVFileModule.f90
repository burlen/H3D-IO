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

module BOVFileModule
  ! !============================================================================
  ! type BOVFile
  !   character(len=*) FileName
  !   integer UnitNo
  ! end type

contains
  ! !----------------------------------------------------------------------------
  ! function NewBOVFile()
  !   implicit none
  !   type(BOVFile), pointer :: fh
  !   integer iErr
  ! 
  !   allocate(fh,stat=iErr)
  !   if (iErr.ne.0) then
  !     write(0,*)"Error: Failed to allocate BOVFile."
  !     stop
  !   end if
  ! 
  !   fh%FileName="/dev/null"
  !   fh%UnitNo=-1
  ! 
  ! end function

  ! !----------------------------------------------------------------------------
  ! subroutine WriteBOVFile(fileName, nX, iErr)
  !   use UnitManagerModule
  ! 
  !   implicit none
  !   character(len=*) fileName
  !   integer nX(3)
  !   integer iErr
  !   integer unitNo
  !   type(UnitManager),pointer :: unitMan
  ! 
  ! 
  !   unitMan => NewUnitManager()
  !   unitNo=UnitManagerGetUnit(unitMan,iErr)
  !   call DeleteUnitManager(unitMan)
  !   if (iErr.ne.0) return
  ! 
  !   open(unit=unitNo,file=fileName,status="replace",iostat=iErr)
  !   if (iErr.ne.0) then
  !     write(0,*)"Error: Failed to create the BOV file at ",trim(fileName),"."
  !     return
  !   end if
  ! 
  !   write(unitNo,'(A)')"# gda data file header"
  !   write(unitNo,'(3(A,I5))')"nx=",nX(1)," ny=",nX(2)," nz=",nX(3)
  !   write(unitNo,'(A)')""
  !   close(unit=unitNo)
  ! 
  ! end subroutine

end module
