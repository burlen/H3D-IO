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
#include<iostream>
#include<fstream>
#include<cstdlib>
using namespace std;

int main(int argc, char **argv)
{
  if (argc!=3)
    {
    cerr << "Usage:" << endl
         << argv[0] << "/path/to/subarray/file nProcs" << endl
         << endl;
    return 1;
    }
  
  const char *fileName=argv[1];
  int nProcs=atoi(argv[2]);

  // open the file.
  ifstream f(fileName,ifstream::in|ifstream::binary);
  if (!f.good())
    {
    cerr << "Error opeing file " << fileName << "." << endl;
    return 1;
    }
  // get length of file:
  f.seekg (0, ios::end);
  int n = f.tellg();
  f.seekg (0, ios::beg);
  // allocate memory:
  float *data=(float *)malloc(n);
  // read data as a block:
  f.read ((char *)data,n);
  f.close();

  // Dump, we expect a nProcs*2 x 2 x 2 array
  // we'll format the dump that way.
  int q=0;
  int ihi=nProcs*2;
  for (int k=0; k<2; ++k)
    {
    for (int j=0; j<2; ++j)
      {
      cerr << data[q];
      ++q;
      for (int i=1; i<ihi; ++i)
        {
        cerr << ", " << data[q];
        ++q;
        }
      cerr << endl;
      }
    cerr << endl;
    }

  // clean up
  free(data);
  return 0;
}
