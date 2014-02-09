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

!------------------------------------------------------------------------------
subroutine ExpectSuccess(name,iErr,nUnexpected)
  implicit none
  integer iErr
  integer nUnexpected
  character*(*) name

  if (iErr.ne.0) then
    write(0,*)name,"...Failed"
    nUnexpected=nUnexpected+1
  else
    write(0,*)name,"...OK."
  end if

end subroutine

!------------------------------------------------------------------------------
subroutine ExpectError(name,iErr,nUnexpected)
  implicit none
  integer iErr
  integer nUnexpected
  character*(*) name

  if (iErr.ne.0) then
    write(0,*)name,"...OK."
  else
    write(0,*)name,"...Failed"
    nUnexpected=nUnexpected+1
  end if

end subroutine

!------------------------------------------------------------------------------
program main
  implicit none
  integer iErr,nFailed,haveLfs
  integer stripe1,stripe2

  nFailed=0

  call usinglustrefs(haveLfs)


  stripe1=1024
  stripe2=524288
  call lfpagealign(stripe1)
  call lfpagealign(stripe2)

  write(0,*)"Page alignment 1024=",stripe1
  write(0,*)"Page alignment 524288=",stripe2
	

  ! split files
  write(0,*)"Testing file create."
  call lfcreatesplit("test_1"//char(0),5,stripe1,16,iErr)
  call ExpectSuccess("Creating test_1",iErr,nFailed)

  call lfcreatesplit("test_2"//char(0),5,stripe2,16,iErr)
  call ExpectSuccess("Creating test_2",iErr,nFailed)

  call lfcreatesplit("test_3"//char(0),5,stripe1,32,iErr)
  call ExpectSuccess("Creating test_3",iErr,nFailed)

  ! validate them
  write(0,*)"Testing validate."
  call lfvalidatesplit("test_1"//char(0),5,stripe1,16,iErr)
  call ExpectSuccess("Validating test_1",iErr,nFailed)

  call lfvalidatesplit("test_2"//char(0),5,stripe1,16,iErr)
  if (haveLfs.eq.0) then
    call ExpectSuccess("Validating test_2",iErr,nFailed)
  else
    call ExpectError("Validating test_2",iErr,nFailed)
  end if

  call lfvalidatesplit("test_3"//char(0),5,stripe1,16,iErr)
  if (haveLfs.eq.0) then
    call ExpectSuccess("Validating test_3",iErr,nFailed)
  else
    call ExpectError("Validating test_3",iErr,nFailed)
  end if

  ! clean up
  write(0,*)"Testing file delete."
  call lfdeletesplit("test_1"//char(0),iErr)
  call ExpectSuccess("Deleting test_1",iErr,nFailed)

  call lfdeletesplit("test_2"//char(0),iErr)
  call ExpectSuccess("Deleting test_2",iErr,nFailed)

  call lfdeletesplit("test_3"//char(0),iErr)
  call ExpectSuccess("Deleting test_3",iErr,nFailed)

  write(0,*)nFailed," tests failed."

end program
