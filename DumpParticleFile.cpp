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
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sstream>
using namespace std;

int main(int argc, char **argv)
{
  if (argc!=3)
    {
    cerr << "Usage:" << endl
         << argv[0] << "/path/to/particle/file NumberOfParticles" << endl
         << endl;
    return 1;
    }
  // open the file.
  ifstream f(argv[1],ifstream::in|ifstream::binary);
  if (!f.good())
    {
    cerr << "Error opeing file " << argv[1] << "." << endl;
    return 1;
    }
  // get length of file:
  f.seekg (0, ios::end);
  int n = f.tellg();
  f.seekg (0, ios::beg);
  // allocate memory:
  double *data=(double *)malloc(n);
  // read data as a block:
  f.read ((char *)data,n);
  f.close();

  istringstream is(argv[2]);
  int np;
  is >> np;

  int ps=np*sizeof(double);
  double *x=(double *)malloc(ps);
  double *y=(double *)malloc(ps);
  double *z=(double *)malloc(ps);
  double *vx=(double *)malloc(ps);
  double *vy=(double *)malloc(ps);
  double *vz=(double *)malloc(ps);

  int q=0;
  for (int i=0; i<np; ++i)
    {
    x[i]=data[q];
    ++q;
    }
  for (int i=0; i<np; ++i)
    {
    y[i]=data[q];
    ++q;
    }
  for (int i=0; i<np; ++i)
    {
    z[i]=data[q];
    ++q;
    }
  for (int i=0; i<np; ++i)
    {
    vx[i]=data[q];
    ++q;
    }
  for (int i=0; i<np; ++i)
    {
    vy[i]=data[q];
    ++q;
    }
  for (int i=0; i<np; ++i)
    {
    vz[i]=data[q];
    ++q;
    }

  for (int i=0; i<np; ++i)
    {
    cerr
      << i << ", "
      << x[i] << ", "
      << y[i] << ", "
      << z[i] << ", "
      << vx[i] << ", "
      << vy[i] << ", "
      << vz[i] << endl;
    }

/*

  // We expect each particle to have 6 components
  // display as such
  for(unsigned int i=0; i<n/sizeof(float); i+=6)
    {
    cerr << data[i  ] << ", "
         << data[i+1] << ", "
         << data[i+2] << ", "
         << data[i+3] << ", "
         << data[i+4] << ", "
         << data[i+5] << endl;
    } */

  free(data);
  free(x);
  free(y);
  free(z);
  free(vx);
  free(vy);
  free(vz);

  return 0;
}
