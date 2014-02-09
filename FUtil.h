/*
   ____    _ __           ____               __    ____
  / __/___(_) /  ___ ____/ __ \__ _____ ___ / /_  /  _/__  ____
 _\ \/ __/ / _ \/ -_) __/ /_/ / // / -_|_-</ __/ _/ // _ \/ __/
/___/\__/_/_.__/\__/_/  \___\_\_,_/\__/___/\__/ /___/_//_/\__(_) 

Copyright 2008 SciberQuest Inc.

*/
#ifndef LustreFUtil_h
#define LustreFUtil_h

// Utilities for setting parameters of files on a Lsutre fs.
// When LUSTRE_FS is not defined the operations depending on
// Lustre specific features become no-ops.


/// Test wether the application was built for use with Lustre fs.
/**
Test wether the application was built for use with Lustre fs. Return
true if it was.
*/
void usinglustrefs_(int *lfs);

/// Find the closest page aligned size.
void lfpagealign_(int *blockSize);

/// Get the system page size
void lfpagesize_(int *pageSize);


/// Make a directory.
void lfcreatedir_(char *dirName, int *iErr);

// Data is stored in a single file

/// Create a file and assign the specified stripe count and size.
/**
Create a file and assign the specified stripe count and size.
If the file exists, it's current stripe count and size are
compared against the specified values, if they differ the
operation fails.
The operations status is returned through the last value. A non-zero
return indicates that the operation failed.
*/
void lfcreate_(char *fileName, int *stripeSize, int *stripeCount, int *iErr);

/// Validate that an existing file has the desired stripe size and count.
/**
If the specified  stripe size and count match the files settings. If the
tests fail then non-0 value is returned.
*/
void lfvalidate_(char *fileName, int *stripeSize, int *stripeCount, int *iErr);

/// Delete a file created with lfcreate.
/**
Delete a file created with lfscreate. There is nothing special about this,
it's included for convinience, any other method fo deletion should be
fine.
The operations status is returned through the last value. A non-zero
return indicates that the operation failed.
*/
void lfdelete_(char *fileName, int *iErr);




// Data is stored in multiple files.

/// Create a split file and assign the specified stripe count and size.
/**
Create a split file and assign the specified stripe count and size.
A split file consists of a directory, and a collection of files
therein.
If the directory and or files exists, their current stripe count and
size are compared against the specified values, if they differ the
operation fails.
The operations status is returned through the last value. A non-zero
return indicates that the operation failed.
*/
void lfcreatesplit_(char *dirName, int *nSubFiles, int *stripeSize, int *stripeCount, int *iErr);

/// Validate that an existing split file has the desired stripe size and count.
/**
Verify that the specified  stripe size and count match the file's settings.
The operations status is returned through the last value. A non-zero
return indicates that the operation failed.
*/
void lfvalidatesplit_(char *fileName, int *nSubFiles, int *stripeSize, int *stripeCount, int *iErr);


/// Delete a split file created with lfscreatesplit.
/**
Deletes the specified split file created with slfscreatesplit. It only deletes 
nSubFiles number of files, and they have to be named the same as those created by
lfcreatsplit. The operation will fail if other files are present.
The operations status is returned through the last value. A non-zero
return indicates that the operation failed.
*/
void lfdeletesplit_(char *fileName, int *iErr);

/// Delete a split file created with lfscreatesplit.
/**
Deletes the specified split file created with slfscreatesplit. It deletes
all files in the heirarchy even those that could not have been created
by lfcreatesplit.
The operations status is returned through the last value. A non-zero
return indicates that the operation failed.
*/
void lfforcedeletesplit_(char *fileName, int *iErr);



/// Tests for a file's existence.
/**
Tests for a file's existence ,if so , a non-zero value is returned.
*/
int FileExists(char *fileName);

/// Tests for a directory's existence.
/**
Tests for a directory's existence ,if so , a non-zero value is returned.
*/
int DirectoryExists(char *fileName);

#endif
