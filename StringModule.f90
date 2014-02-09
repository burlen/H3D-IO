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


module StringModule

  !=============================================================================
  ! dynamic sized string container
  type String
    character,pointer,dimension(:) :: DData
    integer DDataEnd
    integer DDataAt
  end type

  interface StringAppend
    module procedure StringAppendChars
    module procedure StringAppendString
  end interface

  !----------------------------------------------------------------------------
  ! generic returns "value"
  ! inputs:
  !   value
  ! optional:
  !   convFtmSpec, fmt spec w/o parens or rep sepc, applied to value
  interface Convert
    module procedure ConvertBool
    module procedure ConvertInt4
    module procedure ConvertInt4Array
    module procedure ConvertInt8
    module procedure ConvertReal4
    module procedure ConvertReal4Array
    module procedure ConvertReal8
  end interface


  !----------------------------------------------------------------------------
  ! generic returns "value" with no padding
  ! inputs:
  !   value
  interface ConvertTight
    ! procedure ConvertTightBool
    !
    module procedure ConvertTightInt4
    ! procedure ConvertTightInt4Array
    ! 
    ! procedure ConvertTightInt8
    ! 
    ! procedure ConvertTightReal4
    ! procedure ConvertTightReal4Array
    ! 
    ! procedure ConvertTightReal8
  end interface

  !----------------------------------------------------------------------------
  ! generic returns "\rname=value\n"
  ! inputs:
  !   name ,value
  ! optional:
  !   convFtmSpec, fmt spec w/o parens or rep sepc, applied to value
  !   outFmtSpec, fmt spec w/o parens, applied to name,value
  interface StringNameValue
    module procedure StringNameValueBool
    module procedure StringNameValueInt4
    module procedure StringNameValueInt4Array
    module procedure StringNameValueInt8
    module procedure StringNameValueReal4
    module procedure StringNameValueReal4Array
    module procedure StringNameValueReal8
    module procedure StringNameValueChar
  end interface

contains

  !----------------------------------------------------------------------------
  function NewString() result(str)
    implicit none
    type(String), pointer :: str
    integer iErr

    allocate(str,stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate Bufffer."
      stop
    end if

    str%DData => null()
    str%DDataEnd=0
    str%DDataAt=1

    call StringRealloc(str,16)

  end function

  !----------------------------------------------------------------------------
  subroutine DeleteString(str)
    implicit none
    type(String), pointer :: str

    if (.not.associated(str)) return

    if (associated(str%DData)) then
      deallocate(str%DData)
    end if
    nullify(str%DData)

    deallocate(str)
    nullify(str)

  end subroutine 

  !---------------------------------------------------------------------------
  function StringGetSize(str) result(n)
    implicit none
    type(String) str
    integer n

    n=str%DDataAt-1

  end function

  !---------------------------------------------------------------------------
  subroutine StringTruncate(str)
    implicit none
    type(String) str

    str%DDataAt=1

  end subroutine

  !----------------------------------------------------------------------------
  function StringGetAllocatedSize(str) result(n)
    implicit none
    type(String) str
    integer n

    n=str%DDataEnd

  end function

  !----------------------------------------------------------------------------
  function StringGetFreeSpace(str) result(n)
    implicit none
    type(String) str
    integer n

    n=str%DDataEnd-str%DDataAt+1

  end function

  !----------------------------------------------------------------------------
  subroutine StringRealloc(str,n)
    implicit none
    type(String) str
    character,pointer,dimension(:) :: newDData
    integer n,m,i
    integer iErr

    ! sanity -- validate requested size
    if (n.lt.1) then
      write(0,*)"Error: Request reallocation to 0 bytes."
      stop
    end if
    if (n.gt.536870912) then
      write(0,*)"Error: Request reallocation greater than 500M bytes."
      stop
    end if

    ! allocte the new array
    allocate(newDData(n),stat=iErr)
    if (iErr.ne.0) then
      write(0,*)"Error: Failed to allocate newDData."
      stop
    end if

    ! copy
    m=StringGetSize(str)
    do i=1,m
      newDData(i)=str%DData(i)
    end do

    ! initialize
    do i=m+1,n
      newDData(i)='?'
    end do

    !  free the old array
    if (associated(str%DData)) then
      deallocate(str%DData)
    end if

    ! asign
    str%DData=>newDData
    str%DDataEnd=n

  end subroutine

  !----------------------------------------------------------------------------
  subroutine StringAppendString(str,other)
    implicit none
    type(String) str
    type(String) other
    integer i,j
    integer reqSize,curSize,newSize,otherSize

    otherSize=StringGetSize(other)

    ! resize
    reqSize=StringGetSize(str)+otherSize
    curSize=StringGetAllocatedSize(str)

    if (reqSize.gt.curSize) then
      newSize=curSize
      do
        if (reqSize.le.newSize) exit
        newSize=2*newSize
      end do

      call StringRealloc(str,newSize)

    end if

    ! append
    i=str%DDataAt
    do j=1,otherSize
      str%DData(i)=other%DData(j)
      i=i+1
    end do

    str%DDataAt=str%DDataAt+otherSize

  end subroutine

  !----------------------------------------------------------------------------
  subroutine StringAppendChars(str,ddata)
    implicit none
    type(String) str
    character(len=*) ddata
    integer i,j
    integer reqSize,curSize,newSize,ddataSize

    ddataSize=len_trim(ddata)


    ! resize
    reqSize=StringGetSize(str)+ddataSize
    curSize=StringGetAllocatedSize(str)

    if (reqSize.gt.curSize) then
      newSize=curSize
      do
        if (reqSize.le.newSize) exit
        newSize=2*newSize
      end do

      call StringRealloc(str,newSize)

    end if

    ! append
    i=str%DDataAt
    do j=1,ddataSize
      str%DData(i)=ddata(j:j)
      i=i+1
    end do

    str%DDataAt=str%DDataAt+ddataSize

  end subroutine

  !----------------------------------------------------------------------------
  subroutine StringWrite(str,unitNo,iErr)
    implicit none
    type(String) str
    integer unitNo
    integer n
    integer iErr
    character(len=256) eStr

    n=StringGetSize(str)

    write(unit=unitNo,iostat=iErr,iomsg=eStr)str%DData(1:n)
    if (iErr.ne.0) then
      write(0,*)"Error: ",trim(eStr)
      write(0,*)"Error: Failed to write file."
      return
    end if

  end subroutine



  !----------------------------------------------------------------------------
  subroutine ToLower(word)
    implicit none
    character (len=*) , intent(in out) :: word
    integer :: i,ic,nlen

    nlen = len(word)

    do i=1,nlen

      ic = ichar(word(i:i))

      if (ic >= 65 .and. ic < 90) word(i:i) = char(ic+32)

    end do

  end subroutine








  !----------------------------------------------------------------------------
  ! convert a fortran string into a c-string (ie null terminated)
  function CString(fstr) result(cstr)
    implicit none
    character(len=*) fstr
    character(len=len(fstr)) cstr

    cstr=trim(fstr)//char(0)

  end function


  !----------------------------------------------------------------------------
  function ConvertTightInt4(value) result(charValue)
    implicit none
    integer value
    character(len=32) fmtSpec
    character(len=64) charValue

    fmtSpec=""
    charValue=""

    if (value.lt.10) then
      fmtSpec="(I1)"

    else if (value.lt.100) then
      fmtSpec="(I2)"

    else if (value.lt.1000) then
      fmtSpec="(I3)"

    else if (value.lt.10000) then
      fmtSpec="(I4)"

    else if (value.lt.100000) then
      fmtSpec="(I5)"

    else if (value.lt.1000000) then
      fmtSpec="(I6)"

    else if (value.lt.10000000) then
      fmtSpec="(I7)"

    else if (value.lt.100000000) then
      fmtSpec="(I8)"

    else if (value.lt.1000000000) then
      fmtSpec="(I9)"

    else
      fmtSpec="(I10)"

    end if

    write(charValue,fmt=fmtSpec),value

  end function

  !----------------------------------------------------------------------------
  function ConvertBool(value,convFmtSpec) result(charValue)
    implicit none
    logical value
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    iConvFmtSpec="(L17)"

    if (present(convFmtSpec)) then
      iConvFmtSpec="("//trim(convFmtSpec)//")"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function

  !----------------------------------------------------------------------------
  function ConvertInt4(value,convFmtSpec) result(charValue)
    implicit none
    integer value
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    if (present(convFmtSpec)) then
      iConvFmtSpec="("//trim(convFmtSpec)//")"
    else
      iConvFmtSpec="(I17)"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function

  !----------------------------------------------------------------------------
  function ConvertInt4Array(value,convFmtSpec) result(charValue)
    implicit none
    integer value(:)
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    if (present(convFmtSpec)) then
      iConvFmtSpec="(3"//trim(convFmtSpec)//")"
    else
      iConvFmtSpec="(2X,3I5)"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function


  !----------------------------------------------------------------------------
  function ConvertInt8(value,convFmtSpec) result(charValue)
    implicit none
    integer*8 value
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    iConvFmtSpec="(I17)"

    if (present(convFmtSpec)) then
      iConvFmtSpec="("//trim(convFmtSpec)//")"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function

  !----------------------------------------------------------------------------
  function ConvertReal4(value,convFmtSpec) result(charValue)
    implicit none
    real*4 value
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    iConvFmtSpec="(E24.15)"

    if (present(convFmtSpec)) then
      iConvFmtSpec="("//trim(convFmtSpec)//")"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function

  !----------------------------------------------------------------------------
  function ConvertReal4Array(value,convFmtSpec) result(charValue)
    implicit none
    real*4 value(:)
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    iConvFmtSpec="(3E24.15)"

    if (present(convFmtSpec)) then
      iConvFmtSpec="(3"//trim(convFmtSpec)//")"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function

  !----------------------------------------------------------------------------
  function ConvertReal8(value,convFmtSpec) result(charValue)
    implicit none
    real*8 value
    character(len=*),optional :: convFmtSpec
    character(len=32) iConvFmtSpec
    character(len=64) charValue

    iConvFmtSpec="(E24.15)"

    if (present(convFmtSpec)) then
      iConvFmtSpec="("//trim(convFmtSpec)//")"
    end if

    write(charValue,fmt=iConvFmtSpec),value

  end function







  !----------------------------------------------------------------------------
  function StringNameValueBool(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    logical value
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if


  end function

  !----------------------------------------------------------------------------
  function StringNameValueInt4(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    integer value
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if

  end function

  !----------------------------------------------------------------------------
  function StringNameValueInt4Array(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    integer value(:)
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if

  end function

  !----------------------------------------------------------------------------
  function StringNameValueInt8(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    integer*8 value
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if

  end function

  !----------------------------------------------------------------------------
  function StringNameValueReal4(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    real*4 value
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if

  end function

  !----------------------------------------------------------------------------
  function StringNameValueReal4Array(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    real*4 value(:)
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if

  end function

  !----------------------------------------------------------------------------
  function StringNameValueReal8(name,value,convFmtSpec,outFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    real*8 value
    character(len=*),optional :: convFmtSpec
    character(len=*),optional :: outFmtSpec
    character(len=512) buffer
    character(len=64) charValue

    if (present(convFmtSpec)) then
      charValue=Convert(value,convFmtSpec)
    else
      charValue=Convert(value)
    end if

    if (present(outFmtSpec)) then
      buffer=StringNameValueChar(name,charValue,outFmtSpec)
    else
      buffer=StringNameValueChar(name,charValue)
    end if

  end function

  !----------------------------------------------------------------------------
  function StringNameValueChar(name,value,convFmtSpec) result(buffer)
    implicit none
    character(len=*) name
    character(len=*) value
    character(len=*),optional :: convFmtSpec
    character(len=512) buffer
    character(len=128) iFmtSpec

    iFmtSpec="(2A24,A)"

    if (present(convFmtSpec)) then
      iFmtSpec="("//convFmtSpec//",A)"
    end if

    write(buffer,fmt=iFmtSpec)trim(name),trim(value),char(10)

  end function

end module