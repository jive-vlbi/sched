C$Procedure ZZDDHF2H ( Private --- DDH Filename to Handle )
 
      SUBROUTINE ZZDDHF2H ( FNAME,  FTABS,  FTAMH, FTARC, FTBFF,
     .                      FTHAN,  FTNAM,  FTRTM, FTMNM, NFT,   UTCST,
     .                      UTHAN,  UTLCK,  UTLUN, NUT,   EXISTS,
     .                      OPENED, HANDLE, FOUND, MNM                 )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Convert filename to a handle.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      CHARACTER*(*)         FNAME
 
      INTEGER               FTABS ( * )
      INTEGER               FTAMH ( * )
      INTEGER               FTARC ( * )
      INTEGER               FTBFF ( * )
      INTEGER               FTHAN ( * )
      CHARACTER*(*)         FTNAM ( * )
      INTEGER               FTRTM ( * )
      DOUBLE PRECISION      FTMNM ( * )
      INTEGER               NFT
 
      INTEGER               UTCST ( * )
      INTEGER               UTHAN ( * )
      LOGICAL               UTLCK ( * )
      INTEGER               UTLUN ( * )
      INTEGER               NUT
 
      LOGICAL               EXISTS
      LOGICAL               OPENED
      INTEGER               HANDLE
      LOGICAL               FOUND
      DOUBLE PRECISION      MNM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of the file to convert to a handle.
C     FTABS,
C     FTAMH,
C     FTARC,
C     FTBFF,
C     FTHAN,
C     FTNAM,
C     FTRTM,
C     FTMNM      I   File table.
C     NFT        I   Number of entries in the file table.
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN     I/O  Unit table.
C     NUT       I/O  Number of entries in the unit table.
C     EXISTS     O   Logical indicating if FNAME exists.
C     OPENED     O   Logical indicating if FNAME is opened.
C     HANDLE     O   Handle associated with FNAME.
C     FOUND      O   Logical indicating if FNAME's HANDLE was found.
C     MNM        O   Unique DP (Magic NuMber) associated with FNAME.
C
C$ Detailed_Input
C
C     FNAME      is the name of the file to locate in the file table.
C
C     FTABS,
C     FTAMH,
C     FTARC,
C     FTBFF,
C     FTHAN,
C     FTNAM,
C     FTRTM,
C     FTMNM      are the arrays respectively containing the absolute
C                value of the handle, access method, architecture,
C                binary file format, handle, name, RTRIM and
C                magic number columns of the file table.
C
C     NFT        is the number of entries in the file table.
C
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN      are the arrays respectively containing the cost,
C                handle, locked, and logical unit columns of the unit
C                table.
C
C     NUT        is the number of entries in the unit table.
C
C$ Detailed_Output
C
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN      are the arrays respectively containing the cost,
C                handle, locked, and logical unit columns of the unit
C                table.  If ZZDDHF2H requires a logical unit, then
C                it will borrow one from the unit table.  Depending
C                on the state of the table passed in from the caller
C                one of three possible scenarios may occur (Recall
C                that 'zero-cost' rows are ones whose units are
C                reserved with RESLUN and not currently connected
C                to any file.)
C
C                   A 'zero-cost' row exists in the table, in
C                   which case the row is used temporarily and
C                   may be removed depending on the number of entries
C                   in the file table (NFT).
C
C                   The unit table is full (NUT=UTSIZE), in which
C                   case the unit with the lowest cost that is not
C                   locked to its handle will be disconnected, used,
C                   and then returned to the table as a 'zero-cost'
C                   row before returning to the caller.
C
C                   The unit table is not full (NUT<UTSIZE) and there
C                   are no 'zero-cost' rows.  In this case NUT is
C                   temporarily increased by one, and the new row
C                   is used.  After this routine no longer requires
C                   the unit, depending on the number of entries in
C                   the file table (NFT) the row may be left in the
C                   table as a 'zero-handle' row or removed entirely.
C
C                In the event an error is signaled, the contents of the
C                unit table are placed into a usable state before
C                returning to the caller.
C
C     NUT        is the number of entries in the unit table. Since
C                this routine borrows a unit from the unit table, which
C                may involve allocation of a new unit, this value may
C                change.
C
C     EXISTS     is a logical if set to TRUE, indicates that FNAME
C                exists.  If FALSE, FNAME does not exist.  In the event
C                an exception is signaled the value is undefined.
C
C     OPENED     is a logical if set to TRUE, indicates that FNAME
C                is opened and attached to a logical unit.  If FALSE,
C                FNAME is not attached to a unit.  In the event an
C                exception is signaled the value is undefined.
C
C     HANDLE     is the handle in the file table associated with
C                FNAME.  If FOUND is FALSE, then HANDLE is returned as
C                0.
C
C     FOUND      is a logical if TRUE indicates that FNAME was found
C                in the file table.  If FALSE indicates that it was not
C                located.
C
C     MNM        is a unique (enough) DP number -- the Magic NuMber --
C                associated with FNAME computed by this examining the
C                file contents.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If any of the INQUIRE statments this routine performs fail,
C        the error SPICE(INQUIREFAILED) is signaled. FOUND is set to
C        FALSE and HANDLE to 0.
C
C     2) If the attempt to open FNAME fails, then SPICE(FILEOPENFAILED)
C        is signaled. FOUND is set to FALSE, and HANDLE to 0.
C
C     3) If FNAME is determined not to be loaded into the file table
C        then FOUND is set to FALSE and HANDLE is set to 0.
C
C$ Files
C
C     If the file named by FNAME is not connected to a logical unit,
C     this routine will open it for direct access to complete its
C     examination.
C
C$ Particulars
C
C     This routine encapsulates the logic necessary to determine if
C     a particular filename names a file already loaded into the
C     DAF/DAS handle manager.  If it discovers the file is loaded,
C     the routine returns the handle to the caller.
C
C$ Examples
C
C     See ZZDDHFNH for sample usage.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 26-APR-2012 (BVS)
C
C        Changed calling sequence to include FTMNM and MNM. Change
C        algorithm to compute MNM and use it to bypass n^2 INQUIREs
C        for files opened for READ access, if possible.
C
C-    SPICELIB Version 2.0.1, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 2.0.0, 05-AUG-2002 (FST)
C
C        Bug fix: this module was updated to allow proper loading
C        of read-only files on VAX environments.
C
C-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
C
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 05-AUG-2002 (FST)
C
C        An OPEN statement that is exercised by this module under
C        certain circumstances, failed to pass the non-standard
C        READONLY option for the VAX environments.  This had the
C        undesirable side-effect of not permitting files available
C        only for READ access to be opened.
C
C        This file was promoted from a standard portable module
C        to a master file.
C
C-&
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      ZZDDHMNM
 
      INTEGER               ISRCHI
      INTEGER               RTRIM
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               RCHAR
      INTEGER               UINDEX
      INTEGER               UNIT
 
      LOGICAL               LOCEXS
      LOGICAL               LOCOPN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHF2H' )
      END IF
 
C
C     First check to see if FNAME is blank.  If so, set FOUND to .FALSE.
C     and return.  ZZDDHOPN prevents any blank filenames from being
C     loaded into the file table.
C
      IF ( FNAME .EQ. ' ' ) THEN
 
         FOUND  = .FALSE.
         HANDLE = 0
         OPENED = .FALSE.
         EXISTS = .FALSE.
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN
 
      END IF
 
C
C     Start by trimming the file name in preparation for the INQUIRE.
C
      RCHAR = RTRIM ( FNAME )
 
C
C     Now INQUIRE on the input file FNAME.
C
      INQUIRE ( FILE   = FNAME( 1:RCHAR ),
     .          EXIST  = LOCEXS,
     .          OPENED = LOCOPN,
     .          NUMBER = UNIT,
     .          IOSTAT = IOSTAT            )
 
C
C     Check IOSTAT for failure.
C
      IF ( IOSTAT .NE. 0 ) THEN
         FOUND  = .FALSE.
         HANDLE = 0
         CALL SETMSG ( 'INQUIRE failed. Value of IOSTAT was #.' )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
         CALL CHKOUT ( 'ZZDDHF2H'                               )
         RETURN
      END IF
 
C
C     First, set some of the output arguments.  Remember, some
C     systems consider non-existant files as open.  Compensate for
C     this unusual behavior.
C
      EXISTS = LOCEXS
      OPENED = LOCOPN .AND. EXISTS
 
C
C     Now check to see if the file exists.  If it does not, then
C     set FOUND to false and HANDLE to 0 as non-existant files
C     can not possibly be present in the file table.
C
      IF ( .NOT. EXISTS ) THEN
 
         FOUND  = .FALSE.
         HANDLE = 0
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN
 
      END IF
 
C
C     Now check to see if the file is opened.  If it is, we need to
C     determine whether or not the logical unit to which it is
C     attached is present in the unit table.
C
      IF ( OPENED ) THEN
 
C
C        Since the file is opened, see if we can find its unit
C        in the unit table.
C
         UINDEX = ISRCHI ( UNIT, NUT, UTLUN )
 
C
C        When UINDEX is 0, the file is opened, but not by
C        the DAF/DAS handle manager.  Set FOUND to FALSE, HANDLE
C        to 0, and return to the caller.
C
         IF ( UINDEX .EQ. 0 ) THEN
            HANDLE = 0
            FOUND  = .FALSE.
            CALL CHKOUT ( 'ZZDDHF2H' )
            RETURN
         END IF
 
C
C        If we end up here, then we found UNIT in the unit table.
C        Set FOUND to TRUE if the handle associated with UNIT is
C        non-zero.
C
         HANDLE = UTHAN(UINDEX)
         FOUND  = HANDLE .NE. 0
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN
 
      END IF
 
C
C     At this point, we took action for all simple cases.  Now
C     we need to find out if FNAME is one of the files in the
C     file table that isn't open.  To determine this, we open FNAME,
C     and then INQUIRE on every file in the table.  To do this, we
C     need a unit. Get one.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
      IF ( FAILED() ) THEN
         HANDLE = 0
         FOUND  = .FALSE.
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN
      END IF
 
C
C     Now open the file (which we know exists and isn't open). Since
C     we effectively are just borrowing this unit, we are not going to
C     set UTHAN or UTCST from the defaults that ZZDDHGTU sets up.
C
      OPEN ( UNIT   = UTLUN(UINDEX),
     .       FILE   = FNAME( 1:RCHAR ),
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT            )
 
C
C     Check IOSTAT.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
C
C        Since an error has occurred, set FOUND to false and HANDLE
C        to 0.
C
         FOUND  = .FALSE.
         HANDLE = 0
 
C
C        Close the unit and remove it from the unit table.
C
         CLOSE ( UNIT = UTLUN(UINDEX) )
 
         CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK, UTLUN, NUT )
 
C
C        Signal the error and return.
C
         CALL SETMSG ( 'Attempt to open file ''#'' failed. Value of '
     .   //            'IOSTAT was #.'                                )
         CALL ERRCH  ( '#', FNAME                                     )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                        )
         CALL CHKOUT ( 'ZZDDHF2H'                                     )
         RETURN
 
      END IF
 
C
C     Get a unique enough DP number -- the Magic NuMber (MNM) ;) -- for
C     this file.
C
      MNM = ZZDDHMNM ( UTLUN(UINDEX) )
 
C
C     Now loop through all the files in the file table. Unfortunately
C     we have no other choice.
C
      I     = 1
      FOUND = .FALSE.
 
      DO WHILE ( ( I .LE. NFT ) .AND. ( .NOT. FOUND ) )
 
C
C        If this file's magic number is non-zero and is different from
C        the magic number of the currently checked, opened-for-READ
C        file, we will declare that these files are not the same file
C        and will skip INQUIRE. In all other cases we will do INQUIRE
C        and check UNITs.
C
         IF (   MNM .NE. 0.D0                               .AND.
     .        ( MNM .NE. FTMNM(I) .AND. FTAMH(I) .EQ. READ )     ) THEN
 
C
C           These files are not the same file. Clear IOSTAT and set
C           UNIT to not match the UNIT of the input file.
C
            IOSTAT = 0
            UNIT   = UTLUN(UINDEX) + 1
 
         ELSE
 
C
C           Do the INQUIRE. ;(
C
            INQUIRE ( FILE   = FTNAM(I) ( 1 : FTRTM(I) ),
     .                EXIST  = LOCEXS,
     .                OPENED = LOCOPN,
     .                NUMBER = UNIT,
     .                IOSTAT = IOSTAT               )
 
         END IF
 
C
C        Check IOSTAT.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
C
C           Since we have an error condition, set FOUND to FALSE
C           and HANDLE to 0.
C
            FOUND  = .FALSE.
            HANDLE = 0
 
C
C           Close the unit and clean up the unit table.
C
            CLOSE ( UNIT = UTLUN(UINDEX) )
 
            CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                      UTLCK,  UTLUN, NUT           )
 
 
C
C           Signal the error and return.
C
            CALL SETMSG ( 'INQUIRE failed. Value of IOSTAT was #.' )
            CALL ERRINT ( '#', IOSTAT                              )
            CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
            CALL CHKOUT ( 'ZZDDHF2H'                               )
            RETURN
 
         END IF
 
C
C        Now check to see if FILE exists, is currently open. and
C        its UNIT matches UTLUN(UINDEX).
C
         IF (       ( LOCEXS .AND. LOCOPN )
     .        .AND. (   UNIT .EQ.  UTLUN(UINDEX) ) ) THEN
 
               HANDLE = FTHAN(I)
               FOUND  = .TRUE.
 
C
C        Otherwise, continue searching.
C
         ELSE
            I = I + 1
         END IF
 
      END DO
 
C
C     Check to see if we found the file in the file table.
C
      IF ( .NOT. FOUND ) THEN
         HANDLE = 0
      END IF
 
C
C     Close the unit and clean up the unit table.
C
      CLOSE ( UNIT=UTLUN(UINDEX) )
 
      CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK,  UTLUN, NUT )
 
      CALL CHKOUT ( 'ZZDDHF2H' )
      RETURN
 
      END
