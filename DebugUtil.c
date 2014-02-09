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
#include <mpi.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

//*****************************************************************************
static
int min(int a, int b)
{
  return a<b?a:b;
}

//*****************************************************************************
static
int parseRanksStr(char *cRanks, int **iRanks, int *n)
{
  /*
    cRanks : in  : comma delimited list of ranks to attach to , -1 means all
    iRanks : out : array of ranks, caller must free
    n      : out : length of array.
    return : non-zero if an error occured.
  */

  if ( cRanks[0]!='-'
    && cRanks[0]!='0'
    && cRanks[0]!='1'
    && cRanks[0]!='2'
    && cRanks[0]!='3'
    && cRanks[0]!='4'
    && cRanks[0]!='5'
    && cRanks[0]!='6'
    && cRanks[0]!='7'
    && cRanks[0]!='8'
    && cRanks[0]!='9')
    {
    return 1;
    }

  (*iRanks)=0;
  int q=0;
  char *cur=cRanks;
  char *end=cRanks+strlen(cRanks);

  while(cur && cur<=end)
    {
    (*iRanks)=realloc((*iRanks),(q+1)*sizeof(int));
    (*iRanks)[q]=atoi(cur);
    cur=strchr(cur,',');
    if (cur) ++cur;
    ++q;
    }

  (*n)=q;

  return 0;
}

//*****************************************************************************
static
void StartX11GdbSession(char *iface, char *pid, char *display, char *title)
{
  if (strncmp("konsole",iface,7)==0)
    {
    const char *argv[] = { "konsole",
                            "--display",display,
                            "--title",title,
                            "-e",
                            "gdb","--pid",pid,
                            (char *)0 };

    execvp("konsole",(char **)argv);

    fprintf(stderr,"Error could not launch konsole. Falling back to xterm...\n");
    }
  else
  if (strncmp("emacs",iface,5)==0)
    {
    char emacsCmd[1024];
    snprintf(emacsCmd,1024,"(progn (gdb \"gdb -q --pid %s \") )",pid);
    const char *argv[] = { "emacs",
                            "-display",
                            display,
                            "--eval",
                            emacsCmd,
                            (char *)0 };

    execvp("emacs",(char **)argv);

    fprintf(stderr,"Error could not launch emacs. Falling back to xterm...\n");
    }

  // default is xterm, if any of the above fail this will save us
  const char* argv[] = { "xterm",
                         "-title",title,
                         "-geometry","200x80",
                         "-rightbar",
                         "-fg","white",
                         "-bg","black",
                         "-display",display,
                         "-e",
                         "gdb","--pid",pid,
                         (char *)0 };

  execvp("xterm",(char **)argv);


  perror("StartX11GdbSession");

  // execution should never get here
  fprintf(stderr,"%i Failed to exec x11 session for gdb.",__LINE__);
  return;
}

//*****************************************************************************
void gdbattachranks_(char *iface, char *cRanks)
{
  int nRanks;
  int *iRanks;
  int iErr=parseRanksStr(cRanks,&iRanks,&nRanks);
  if (iErr!=0)
    {
    fprintf(stderr,"Error: Invalid rank list: %s\n.",cRanks);
    return;
    }

  int myRank;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  int masterRank=INT_MAX;
  int rankActive=0;

  if ((*iRanks)==-1)
    {
    // all ranks are active
    rankActive=1;
    masterRank=0;
    }
  else
    {
    // list of ranks which are active. Determine if this rank
    // is listed and which rank is the master.
    for (int i=0; i<nRanks; ++i)
      {
      masterRank=min(masterRank,iRanks[i]);

      if (myRank==iRanks[i])
        {
        rankActive=1;
        }
      }
    }

  if (rankActive)
    {
    // Get iformation about this processs.
    char *display=getenv("DISPLAY");
    if (display==0)
      {
      display="0:0";
      }

    char pid[15]={'\0'};
    snprintf(pid,15,"%i",getpid());

    char host[64]={'\0'};
    gethostname(host,64);

    char title[128]={'\0'};
    snprintf(title,128,"%i:%s:%s",myRank,host,pid);

    // redirect gdb's stdout to stderr
    if ( dup2(STDERR_FILENO,STDOUT_FILENO)<0 )
      {
      fprintf(stderr,"%i Failed to redirect the standard error device.",__LINE__);
      return;
      }

    // fork & launch a gdb attached to us
    int gdbPid=fork();
    if (gdbPid<0)
      {
      fprintf(stderr,"%i Failed to fork.",__LINE__);
      return;
      }
    else
    if (gdbPid==0)
      {
      // start gdb...
      StartX11GdbSession(iface,pid,display,title);
      fprintf(stderr,"%i Failed to start gdb.",__LINE__);
      return;
      }

    // if we are here gdb is up and running in a child
    // process. We wait for input from the user indicating
    // he's ready to continue (otherwise the app runs away
    // while gdb starts).
    if (myRank==masterRank)
      {
      fprintf(
          stderr,
          "Pausing while debugger starts.\n"
          "Press enter to continue.\n");
      char input[256];
      fgets(input,256,stdin);
      }
    }

  MPI_Barrier(MPI_COMM_WORLD);

  // User's ready to continue , yeild back to the caller.
  return;
}

//*****************************************************************************
void gdbattachall_(char *iface)
{
  gdbattachranks_(iface,"-1");
}
