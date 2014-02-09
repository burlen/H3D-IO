/*
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
*/
//#define _XOPEN_SOURCE 600

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <ftw.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#ifdef LUSTRE_FS
#include <lustre/liblustreapi.h>
#include <lustre/lustre_user.h>
#else
#define O_LOV_DELAY_CREATE 0x0
#endif

// currently no prgramatic access to this.
#define LUSTRE_STRIPE_UNIT 65536

#ifndef FTW_CONTINUE
  #define FTW_CONTINUE 0
#endif

#ifndef FTW_STOP
  #define FTW_STOP 1
#endif

//******************************************************************************
int FileExists(char *fileName)
{
  struct stat ss;

  int iErr=stat(fileName,&ss);
  if (iErr && errno==ENOENT)
    {
    return 0;
    }

  return 1;
}

//******************************************************************************
int DirectoryExists(char *fileName)
{
  struct stat ss;

  int iErr=stat(fileName,&ss);
  if ((iErr && errno==ENOENT) || !S_ISDIR(ss.st_mode))
    {
    return 0;
    }

  return 1;
}

//******************************************************************************
void usinglustrefs_(int *lfs)
{
  #ifdef LUSTRE_FS
  (*lfs)=1;
  #else
  (*lfs)=0;
  #endif
}

//*****************************************************************************
void lfmaxstripecount_(int *maxCount)
{
  (*maxCount)=160;
}

//*****************************************************************************
void lfpagealign_(int *blockSize)
{
  const int pageSize=65536; // This is Lustre page size as of 07/2010
  int nPages=(*blockSize)/pageSize+(((*blockSize)%pageSize)?1:0);
  (*blockSize)=nPages*pageSize;
}

//*****************************************************************************
void lfpagesize_(int *pageSize)
{
  (*pageSize)=getpagesize();
}

//*****************************************************************************
void lfsetenv_(char *var, int *val, int *iErr)
{
  char valStr[32];
  snprintf(valStr,32,"%i",*val);
  (*iErr)=setenv(var,valStr,1);
}

//*****************************************************************************
void lfcreate_(char *fileName, int *stripeSize, int *stripeCount, int *iErr)
{
  #ifdef LUSTRE_FS
  // lustre expects divisibility by page size
  // int pageSize=getpagesize();
  // int nPages=(*stripeSize)/pageSize;
  // stripeSize=nPages*pageSize;

  (*iErr)=llapi_file_create(fileName,*stripeSize,-1,*stripeCount,0);
  #else
  int fd=open(fileName,O_WRONLY|O_CREAT|O_TRUNC,(S_IRWXU&~S_IXUSR)|S_IRGRP|S_IROTH);
  if (fd!=-1)
    {
    close(fd);
    (*iErr)=0;
    }
  else
    {
    perror(" Error");
    (*iErr)=1;
    }
  #endif

  if ((*iErr))
    {
    fprintf(stderr,"Error: Failed to create file \"%s\"\n",fileName);
    }
}

//******************************************************************************
void lfvalidate_(char *fileName, int *stripeSize, int *stripeCount, int *iErr)
{
  #ifdef LUSTRE_FS
  struct lov_user_md umd;
  
  (*iErr)=llapi_file_get_stripe(fileName,&umd);
  if (((*iErr)==0)
    && (umd.lmm_stripe_size==(*stripeSize))
    && (umd.lmm_stripe_count==(*stripeCount)))
    {
    return;
    }
  else
  if (iErr)
    {
    fprintf(
      stderr,
      "Error: Failed to get Lustre meta-data for \"%s\".\n",
      fileName);
    return;
    }
  else
  if (umd.lmm_stripe_size!=(*stripeSize))
    {
    (*iErr)=1;
    fprintf(
      stderr,
      "Error: Stripe size mismatch for \"%s\", %i requested, %i found.\n",
      fileName,
      *stripeSize,
      umd.lmm_stripe_size);
    return;
    }
  else
  if (umd.lmm_stripe_count!=(*stripeCount))
    {
    (*iErr)=1;
    fprintf(
      stderr,
      "Error: Stripe count mismatch for \"%s\", %i requested, %i found.\n",
      fileName,
      *stripeCount,
      umd.lmm_stripe_count);
    return;
    }
  #else
  (*iErr)=!FileExists(fileName);
  #endif
}

//*****************************************************************************
void lfdelete_(char *fileName, int *iErr)
{
  (*iErr)=remove(fileName);
  if (*iErr)
    {
    perror("Error deleting file");
    fprintf(stderr," Error: Failed to delete \"%s\"\n",fileName);
    return;
    }
}

//*****************************************************************************
void lfcreatedir_(char *dirName, int *iErr)
{
  // Create the directory containing the split files.
  (*iErr)=mkdir(dirName,S_IRWXU|S_IRWXG|S_IROTH|S_IXOTH);
  if (*iErr)
    {
    perror("Error creating directory");
    }
}

//*****************************************************************************
void lfcreatesplit_(
        char *fileName,
        int *nSubFiles,
        int *stripeSize,
        int *stripeCount,
        int *iErr)
{
  // Create the directory containing the split files.
  (*iErr)=mkdir(fileName,S_IRWXU|S_IRWXG|S_IROTH|S_IXOTH);
  if (*iErr)
    {
    perror("Error creating directory");
    return;
    }

  // create the component files
  for (int i=0; i<(*nSubFiles); ++i)
    {
    char cfn[1024];
    snprintf(cfn,1024,"%s/%05i",fileName,i);

    lfcreate_(cfn,stripeSize,stripeCount,iErr);
    if ((*iErr))
      {
      perror("Error:");
      fprintf(stderr,"Error: Failed to create component file \"%s\"\n",fileName);
      return;
      }
    }
}

//*****************************************************************************
void lfvalidatesplit_(char *fileName, int *nSubFiles, int *stripeSize, int *stripeCount, int *iErr)
{
  // the file passed should be a directory
  struct stat ss;
  (*iErr)=stat(fileName,&ss);
  if ((*iErr) || !S_ISDIR(ss.st_mode))
    {
    (*iErr)=1;
    return;
    }

  // in the directory there should be a number of component files.
  for (int i=0; i<(*nSubFiles); ++i)
    {
    char cfn[1024];
    snprintf(cfn,1024,"%s/%05i",fileName,i);
    lfvalidate_(cfn,stripeSize,stripeCount,iErr);
    if (*iErr)
      {
      return;
      }
    }
}

//*****************************************************************************
static int nftw_rmSubFile(
    const char *path,
    const struct stat *ss,
    int etype,
    struct FTW *sf)
{
  // rather than "rm -rf *" when removing existing split
  // files, we'll check that each file is named following
  // split file convention, all digits. if we find one that
  // doesn't follow the convention we'll return an error.
  if (etype==FTW_F)
    {
    int valideSplitFileName=1;
    int pathLen=strlen(path);
    for (int i=1; i<6; ++i)
      {
      // expect file name to contain 5 digits.
      if (!isdigit(path[pathLen-i]))
        {
        fprintf(stderr," Error: expected digit got %s\n",path);
        return FTW_STOP;
        }
      }
    // The pattern matches, this file is a valid sub file
    // remove it.
    int iErr=remove(path);
    if (iErr)
      {
      perror(" Error");
      fprintf(stderr,"nftw_rmSubFile(%s)",path);
      return FTW_STOP;
      }
    }
  // Only made it here if the path points to a valid
  // subfile, in which case it was removed, or if path
  // is a directory, in which case we skip it. In the case
  // that this is not the split file root directory removing
  // the split file root directory will later fail.
  return FTW_CONTINUE;
}


//*****************************************************************************
void lfdeletesplit_(char *fileName, int *iErr)
{
  (*iErr)=nftw(fileName,nftw_rmSubFile,128,FTW_PHYS|FTW_DEPTH);
  if (*iErr)
    {
    fprintf(
      stderr,
      " Error: Failed to delete split file rooted at \"%s\"\n",
      fileName);
    return;
    }

  // delete the directory
  (*iErr)=remove(fileName);
  if (*iErr)
    {
    perror(" Error");
    fprintf(
      stderr,
      " Error: Failed to delete split file rooted at \"%s\"\n",
      fileName);
    return;
    }
}

//*****************************************************************************
static int nftw_rm(
    const char *path,
    const struct stat *ss,
    int etype,
    struct FTW *sf)
{
  int iErr=remove(path);
  if (iErr)
    {
    perror("nftw_rm");
    return FTW_STOP;
    }
  return FTW_CONTINUE;
}

//*****************************************************************************
void lfforcedeletesplit_(char *fileName, int *iErr)
{
  (*iErr)=nftw(fileName,nftw_rm,128,FTW_PHYS|FTW_DEPTH);
  if (*iErr)
    {
    fprintf(
      stderr,
      "Error: Failed to delete split file, \"%s\"\n",
      fileName);
    return;
    }
}

//*****************************************************************************
void lfopen_(
      char *a_fileName,     // name of file
      int *a_flags,         // POSIX flags, -1 means (O_WRONLY|O_CREAT|O_TRUNC)
      int *a_mode,          // POSIX ownership flags, -1 means ((S_IRWXU&~S_IXUSR)|S_IRGRP|S_IROTH)
      int *a_stripeSize,    // Lustre stripe size (must be in lustre page units)
      int *a_stripeCount,   // Lustre stripe count
      int *a_stripeOffset,  // Lustre OST id
      int *fd,              // return, POSIX file descriptor
      int *iErr)            // set if an error occured
{
  #ifdef LUSTRE_FS
  // use lustre api for file creation.
  (*iErr)=llapi_file_create(
        a_fileName,
        *a_stripeSize,
        *a_stripeOffset,
        *a_stripeCount,
        0);
  if ((*iErr))
    {
    fprintf(
      stderr,
      "%i Error: llapi_file_create failed.\n"
      "fileName=\"%s\"\n"
      "stripeSize=%li\n"
      "stripeCount=%i\n"
      "stripeOffset=%i\n",
      __LINE__,
      a_fileName,
      *a_stripeSize,
      *a_stripeCount,
      *a_stripeOffset);
    return;
    }
  #endif

  // as a convineince provide defaults.
  int flags;
  if ((*a_flags)<0)
    {
    flags=O_WRONLY|O_CREAT|O_TRUNC|O_LOV_DELAY_CREATE;
    }
  else
    {
    flags=*a_flags;
    }
  int mode;
  if ((*a_mode)<0)
    {
    mode=(S_IRWXU&~S_IXUSR)|S_IRGRP|S_IROTH;
    }
  else
    {
    mode=*a_mode;
    }

  // POSIX open 
  (*fd)=open(a_fileName,flags,mode);
  if ((*fd)==-1)
    {
    fprintf(
      stderr,
      "%i Error: open failed.\n"
      "fileName=\"%s\"\n"
      "flags=%i\n"
      "mode=%i\n",
      __LINE__,
      a_fileName,
      flags,
      mode);
    perror("Error");
    (*iErr)=1;
    }
}

//*****************************************************************************
void lfwrite_(int *fd, void *data, size_t *count, int *iErr)
{
  ssize_t n=write(*fd,data,*count);
  if (n==-1)
    {
    fprintf(
        stderr,
        "%i Error: write failed.\n"
        "fd=%i\n"
        "count=%zi\n",
        __LINE__,
        *fd,
        *count);
    perror("Error");
    (*iErr)=1;
    }
  if (n!=(*count))
    {
    fprintf(
        stderr,
        "%i Error: write failed.\n"
        "fd=%i\n"
        "count=%zi\n"
        "wrote=%zi\n",
        __LINE__,
        *fd,
        *count,
        n);
    (*iErr)=1;
    }
}

//*****************************************************************************
void lffdatasync_(int *fd, int *iErr)
{
  (*iErr)=fdatasync(*fd);
  if ((*iErr)==-1)
    {
    fprintf(
        stderr,
        "%i Error: fdatasync failed.\n"
        "fd=%i",
        __LINE__,
        *fd);
    perror("Error");
    }
}

//*****************************************************************************
void lfclose_(int *fd, int *iErr)
{
  (*iErr)=close(*fd);
  if ((*iErr)==-1)
    {
    fprintf(
        stderr,
        "%i Error: close failed.\n"
        "fd=%i",
        __LINE__,
        *fd);
    perror("Error");
    }
}

