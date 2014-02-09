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

!==============================================================================
! Codes for manipulating MPI_Info
module MPIFileHintModule
  use mpi
  use StringModule

  !----------------------------------------------------------------------------
  ! generic type conversion for mpi hints (always strings)
  interface MPIFileHintSet
    module procedure MPIFileHintSetChar
    module procedure MPIFileHintSetInt
    module procedure MPIFileHintSetBool
    module procedure MPIFileHintSetBoolTriState
  end interface

  interface MPIFileHintGet
    module procedure MPIFileHintGetInt
    module procedure MPIFileHintGetStr
  end interface

  interface MPIFileHintStream
    module procedure MPIFileHintStreamChar
    module procedure MPIFileHintStreamString
  end interface

  ! !----------------------------------------------------------------------------
  ! ! generic interface printing file hints
  ! interface MPIFileHintWrite
  !   procedure MPIFileHintWriteString
  !   procedure MPIFileHintWriteUnit
  ! end interface

contains
  !------------------------------------------------------------------------------
  ! Sets the hint to tString if lvalue is .true., otherwise fString
  subroutine MPIFileHintSetBoolTriState( &
      info,                              &
      key,                               &
      value,                             &
      tString,                           &
      fString,                           &
      oString,                           &
      iErr)
    implicit none
    integer info
    character(len=*) :: key
    character(len=*) :: tString,fString,oString
    integer value
    integer iErr

    iErr=0

    select case (value)

      case (0)
        call MPIFileHintSet(info,key,fString,iErr)

      case (1)
        call MPIFileHintSet(info,key,tString,iErr)

      case (2)
        call MPIFileHintSet(info,key,oString,iErr)

      case default
        write(0,*)"Error: invalid value in FileHintSetBoolTriState."
        iErr=1
    end select

  end subroutine

  !------------------------------------------------------------------------------
  ! Sets the hint to tString if lvalue is .true., otherwise fString
  subroutine MPIFileHintSetBool(info,key,lvalue,tString,fString,iErr)
    implicit none
    integer info
    character(len=*) :: key
    character(len=*) :: tString,fString
    logical lvalue
    integer iErr

    iErr=0

    if (lvalue.eqv..true.) then
      call MPIFileHintSet(info,key,tString,iErr)
    else
      call MPIFileHintSet(info,key,fString,iErr)
    end if

  end subroutine

  !------------------------------------------------------------------------------
  ! Sets the hint from aan integer value
  subroutine MPIFileHintSetInt(info,key,iValue,iErr)
    implicit none
    integer info
    character(len=*) key
    character(len=15) value
    integer iValue
    integer iErr

    iErr=0

    write(value,'(I15)')iValue
    call MPIFileHintSet(info,key,trim(value),iErr)

  end subroutine

  !------------------------------------------------------------------------------
  ! Set hint name,value pair on info. If info is set to MPI_INFO_NULL
  ! upon entry then a new info object is created.
  subroutine MPIFileHintSetChar(info,key,value,iErr)
    implicit none
    integer info
    character(len=*) key,value
    integer iErr

    iErr=0

    if (info.eq.MPI_INFO_NULL) then
      call MPI_Info_create(info,iErr)
    end if

    call MPI_Info_set(info,key,value,iErr)

  end subroutine

  ! !------------------------------------------------------------------------------
  ! ! Write the hints to a the given open file unit.
  ! subroutine MPIFileHintWrite(info,unitNo,iErr)
  !   implicit none
  !   integer info
  !   integer unitNo
  !   integer iErr
  !   character(len=256) eStr
  !   character(len=1024) buffer
  ! 
  !   iErr=0
  ! 
  !   call MPIFileHintWriteString(info,buffer,iErr)
  !   if (iErr.ne.0) return
  ! 
  !   write(
  !     unit=unitNo,
  !     fmt='(A)',
  !     iostat=iErr,
  !     iomsg=eStr)
  !     MPIFileHintStream(
  !   if (iErr.ne.0) then
  !     write(0,*)"Error: ",trim(eStr)
  !     return
  !   end if
  ! 
  ! end subroutine

  !------------------------------------------------------------------------------
  ! serializes the given hints into a string one per line.
  function MPIFileHintStreamChar(info) result(oBuffer)
    implicit none
    character(len=4096) oBuffer
    integer info, nKeys
    character(len=128) key
    character(len=256) value
    character(len=512) keyValueBuf
    character(len=512) iBuffer
    logical keyDefined
    integer i
    integer iErr

    iErr=0

    oBuffer=""

    call MPI_Info_get_nkeys(info,nKeys,iErr)

    do i=1,nKeys

      call MPI_Info_get_nthkey(info,i-1,key,iErr)
      call MPI_Info_get(info,key,256,value,keyDefined,iErr)

      if (iErr.eq.0) then
        keyValueBuf=StringNameValue(key,value)
      else
        write(keyValueBuf,'(4A)')"Error: Buffer too small. ",trim(key),char(10)
        write(0,*)keyValueBuf
      endif

      iBuffer=""
      iBuffer=trim(oBuffer)//trim(keyValueBuf)
      oBuffer=trim(iBuffer)

    end do

    ! call MPI_Info_free(info,iErr)

  end function

  !------------------------------------------------------------------------------
  ! serializes the given hints intoa string one per line.
  subroutine MPIFileHintStreamString(info,str)
    implicit none
    integer info, nKeys
    type(String) str
    character(len=128) key
    character(len=256) value
    character(len=512) keyValueBuf
    logical keyDefined
    integer i
    integer iErr

    iErr=0

    call MPI_Info_get_nkeys(info,nKeys,iErr)

    do i=1,nKeys

      call MPI_Info_get_nthkey(info,i-1,key,iErr)
      call MPI_Info_get(info,key,256,value,keyDefined,iErr)

      if (iErr.eq.0) then
        keyValueBuf=StringNameValue(key,value)
      else
        write(keyValueBuf,'(4A)')"Error: Buffer too small. ",trim(key),char(10)
        write(0,*)keyValueBuf
      endif

      call StringAppend(str,keyValueBuf)

    end do

    ! call MPI_Info_free(info,iErr)

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileHintGetInt(info,key,value,iErr)
    integer info
    character(*) key
    integer value
    character(256) valueStr
    integer iErr
    logical keyDefined

    call MPI_Info_get(info,key,256,valueStr,keyDefined,iErr)
    if ((.not.keyDefined).or.(iErr.ne.0)) then
      write(0,*)"Error: Failed to get value for key ",trim(key),"."
      return
    endif

    read(valueStr,'(i10)')value

  end subroutine

  !----------------------------------------------------------------------------
  subroutine MPIFileHintGetStr(info,key,value,iErr)
    integer info
    character(*) key
    character(*) value
    integer valueLen
    integer iErr
    logical keyDefined

    valueLen=len(value)
    call MPI_Info_get(info,key,valueLen,value,keyDefined,iErr)
    if (.not.keyDefined) then
      write(0,*)"Error: Key ",trim(key)," is undefined."
    endif

  end subroutine


end module
