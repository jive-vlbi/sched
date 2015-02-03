C$Procedure      DASCUD ( DAS, create or update directories )
 
      SUBROUTINE DASCUD ( HANDLE, TYPE, NWORDS )
 
C$ Abstract
C
C     Create or update directories in a DAS file to reflect addition
C     of a specified number of words of a specified data type.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               NWORDS
 
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )
 
      INTEGER               DP
      PARAMETER           ( DP     =  2  )
 
      INTEGER               INT
      PARAMETER           ( INT    =  3  )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     TYPE       I   Data type specifier.
C     NWORDS     I   Number of words of data being added.
C     CHAR       P   Parameter indicating character data type.
C     DP         P   Parameter indicating double precision data type.
C     INT        P   Parameter indicating integer data type.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of a DAS file open for writing.
C
C     TYPE           is a data type specifier.  TYPE may be any of
C                    the parameters
C
C                       CHAR
C                       DP
C                       INT
C
C                    which indicate `character', `double precision',
C                    and `integer' respectively.
C
C     NWORDS         is the number of words of data of the data type
C                    indicated by TYPE whose addition to the indicated
C                    DAS file is to be accounted for.
C
C$ Detailed_Output
C
C     None.          See $Particulars for a description of the action
C                    of this routine.
C
C$ Parameters
C
C     CHAR,
C     DP,
C     INT            are data type specifiers which indicate
C                    `character', `double precision', and `integer'
C                    respectively.  These parameters are used in
C                    all DAS routines that require a data type
C                    specifier as input.
C
C$ Exceptions
C
C     1)  If the input handle is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C     2)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
C         will be signalled.
C
C     3)  If NWORDS is negative, the error SPICE(VALUEOUTOFRANGE) will
C         be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine operates by side effects:  the directories in the
C     indicated DAS file will be updated to reflect the addition of
C     the indicated number of words of the specified data type.
C     If necessary, a new directory record will be added to the file
C     to hold a new cluster descriptor.
C
C     In addition, the file summary for the indicated DAS file will be
C     updated with the new values of the descriptor location and last
C     logical address of the indicated type, as well as with the new
C     value of the free record pointer.
C
C     This routine is used by the DASADx routines:  after each data
C     addition, they call this routine to update the directories of the
C     affected DAS file.
C
C     Normally, there will be no need for routines outside of SPICELIB
C     to call this routine directly.  To add data to or update a DAS
C     file, the DASADx and DASUDx routines should be used; these
C     routines take care of directory creation and updates.
C
C$ Examples
C
C     1)  Update directories after writing N integer words to a
C         DAS file designated by HANDLE:
C
C             CALL DASCUD ( HANDLE, INT, N )
C
C$ Restrictions
C
C     1)  This routine is intended for use by the SPICELIB DAS routines.
C         Non-SPICELIB software normally will not need to call this
C         routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0 07-AUG-2006 (NJB)
C
C        Bug fix:  added intialization of variable LTYPE to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms.
C
C-    SPICELIB Version 1.3.0 16-JAN-2003 (NJB)
C
C        Bug fix:  fixed previous bug fix.  
C
C-    SPICELIB Version 1.2.0 10-DEC-2002 (NJB)
C
C        Bug fix:  now a new, empty directory record with valid
C        backward and forward pointers is written immediately
C        when it is created.
C
C-    SPICELIB Version 1.1.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C        Removed an unused variable.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     update DAS cluster directories
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.4.0 07-AUG-2006 (NJB)
C
C        Bug fix:  added intialization of variable LTYPE to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms. The
C                  statement referencing the uninitialized variable
C                  was:
C
C           ELSE IF (       ( TYPE   .EQ. LTYPE )
C          .          .AND. ( DSCREC .GT. 0     )
C          .          .AND. ( LWORD  .LT. NWI   )  ) THEN
C
C        
C        In the previous version of the code, LTYPE is uninitialized
C        when the DAS file is empty, which implies DSCREC is 0.
C        Otherwise LTYPE is initialized.  So the value of the logical
C        expression is not affected by the uninitialized value of
C        LTYPE.
C
C        However, the Intel Fortran compiler for the Mac flags a runtime
C        error when the above code is exercised.  So LTYPE is now 
C        initialized to an invalid value prior to execution of this 
C        code.  If the invalid value is ever used, a runtime error
C        should result.
C
C
C-    SPICELIB Version 1.3.0 16-JAN-2003 (NJB)
C
C        Bug fix:  fixed previous bug fix.  
C
C
C        The offending line (#778) in previous version) of code is:
C
C           CALL DASWRI ( HANDLE, RECNO, DIRREC )
C
C        The correct line of code is:
C
C          CALL DASWRI ( HANDLE, FREE, DIRREC )
C
C
C-    SPICELIB Version 1.2.0 10-DEC-2002 (NJB)
C
C        Bug fix:  now a new, empty directory record with valid
C        backward and forward pointers is written immediately
C        when it is created.  This prevents an unsegregated file
C        from being left with an invalid forward pointer.
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C        Removed an unused variable, PREV.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
 
C
C     Words per data record, for each data type:
C
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )
 
      INTEGER               NWD
      PARAMETER           ( NWD =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI =  256 )
 
C
C     Directory pointer locations (backward and forward):
C
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC = 1 )
 
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )
 
C
C     Directory address range locations
C
      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )
 
      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )
 
      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )
 
C
C     Location of first type descriptor
C
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )
 
 
 
 
C
C     Local variables
C
      INTEGER               DESCR
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCREC
      INTEGER               FREE
      INTEGER               I
      INTEGER               LAST
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LREC
      INTEGER               LOC
      INTEGER               LTYPE
      INTEGER               LWORD
      INTEGER               MAXADR
      INTEGER               MINADR
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NEEDED
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NW
      INTEGER               RECNO
      INTEGER               RNGLOC
      INTEGER               ROOM
 
 
 
C
C     Saved variables
C
C
 
C
C     NEXT maps the DAS data type codes to their successors.
C
      INTEGER               NEXT   ( 3 )
      SAVE                  NEXT
 
C
C     Initial values
C
      DATA                  NEXT   /  2,   3,   1  /
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASCUD' )
      END IF
 
C
C     Here's a preview of coming attractions:
C
C        We're going to update the directories in the indicated
C        DAS file to reflect the addition of NWORDS new data words.
C        This data is supposed to have been added to the file BEFORE
C        this routine is called.  There are several possible states
C        the file can be in at the point this routine is called.
C
C
C           1)  There is already a descriptor of TYPE in the file, and
C               the addition of data does not require this descriptor
C               to be modified.
C
C               We can tell that we have this case when the file
C               summary indicates that, before the addition of data,
C               there was room for NWORDS of data in the last data
C               record in the file.  Since no new data records were
C               required to accommodate the new data, the descriptor
C               for TYPE does not have to be updated.
C
C               However, even though the descriptor need not be
C               modified, the address range for TYPE covered by the
C               directory record containing this last descriptor must be
C               updated, as must be the file summary.
C
C
C           2)  There is already a descriptor of TYPE in the file, and
C               in order to describe the new data added to the file,
C               it suffices to update this descriptor and the address
C               range in the directory containing it.
C
C               This happens when case (1) doesn't apply, and the
C               descriptor of TYPE is the last descriptor in the last
C               directory, and the descriptor is not in the last
C               position (index NWI) of the directory.
C
C               Note that we never update the last descriptor in a
C               directory record.  The reason for this is that after
C               this descriptor is written, we build a new directory
C               record.  All subsequent additions of data are made to
C               records that follow this new directory record;
C               otherwise, the new directory would get overwritten
C               with data.
C
C
C           3)  A new descriptor of TYPE is needed.
C
C               This can happen in several ways:
C
C               a)  There are no directories in the file yet, in which
C                   case space has been reserved for the first
C                   directory.
C
C                   This can happen only when the file had no data at
C                   all in it before the last addition of data.
C
C                   In this case, we must fill in the first descriptor
C                   and the address range for TYPE.  We must also update
C                   the file summary, because the descriptor location,
C                   last logical address of TYPE, and the free pointer
C                   have changed.
C
C               b)  The conditions for cases (1) and (2) are not
C                   satisfied, and the current last directory record
C                   has room for a new descriptor.  In this case, if
C                   the data addition filled in the last data record
C                   described by the current last descriptor of type,
C                   (which will usually be the case), we must update
C                   the appropriate address range in the directory
C                   record containing that descriptor.  We will then
C                   add a new descriptor to the last directory record
C                   and update the address range for TYPE in that
C                   record.  The file summary must be updated as well.
C
C                   If the new descriptor we've added went into the
C                   last slot in a directory record (index NWI), we
C                   also create a new, empty directory record and
C                   update the forward pointer of the current directory
C                   to point to it.  We also update the file summary
C                   so that the free pointer points to the record
C                   following the empty directory record.
C
C
C               c)  The conditions for cases (1) and (2) are not
C                   satisfied, and the current last directory record
C                   has no room for a new descriptor.
C
C                   In this case, if the data addition filled in the
C                   last data record described by the current last
C                   descriptor of TYPE, (which will usually be the
C                   case), we must update the appropriate address range
C                   in the directory record containing that descriptor.
C                   We will then add a new descriptor to the empty
C                   directory record and initialize the address range
C                   for TYPE in that record.  The file summary must be
C                   updated as well.
C
C
C     To start out, we'll need to find out how the file is currently
C     disposed.  We'll need the location of the last descriptor of
C     TYPE, the last logical address of TYPE, and the location of
C     the last descriptor of any type.
C
C     Get the file summary.
C
      CALL DASHFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
 
C
C     Now do all of the data-type-dependent work:
C
C        -- Set the last address of the indicated data type LAST.
C
C        -- Set the physical record of the last descriptor of TYPE.
C
C        -- Set the number of words of data of the specified type per
C           physical record NW.
C
C        -- Set the address range location used to pick address ranges
C           out of directory records.
C
C
C     Note that the address and descriptor location information from
C     the file summary is assumed NOT to take into account the latest
C     data addition.
C
C
      LAST    =  LASTLA( TYPE )
      DSCREC  =  LASTRC( TYPE )
 
      IF ( TYPE .EQ. DP ) THEN
 
         NW      =  NWD
         RNGLOC  =  DPRNG
 
      ELSE IF ( TYPE .EQ. INT ) THEN
 
         NW      =  NWI
         RNGLOC  =  INTRNG
 
      ELSE IF ( TYPE .EQ. CHAR ) THEN
 
         NW      =  NWC
         RNGLOC  =  CHRRNG
 
      ELSE
 
         CALL SETMSG ( 'Invalid data type: #. ' )
         CALL ERRINT ( '#',  TYPE               )
         CALL SIGERR ( 'SPICE(DASINVALIDTYPE)'  )
         CALL CHKOUT ( 'DASCUD'                 )
         RETURN
 
      END IF
 
C
C     Make sure that NWORDS is something sensible.
C
      IF ( NWORDS .LT. 0 ) THEN
 
         CALL SETMSG ( 'NWORDS was #; should be non-negative.' )
         CALL ERRINT ( '#',  NWORDS                            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
         CALL CHKOUT ( 'DASCUD'                                )
         RETURN
 
      END IF
 
C
C     Find the record and word positions LREC and LWORD of the last
C     descriptor in the file, and also find the type of the descriptor
C     LTYPE.
C
      CALL MAXAI  ( LASTRC,  3,  LREC,  LOC )
      LWORD  =  0
      LTYPE  =  0

      DO I = 1, 3
 
         IF (       ( LASTRC(I) .EQ. LREC  )
     .        .AND. ( LASTWD(I) .GT. LWORD )  ) THEN
 
            LWORD = LASTWD(I)
            LTYPE = I
 
         END IF
 
      END DO
 
C
C     LREC, LWORD, and LTYPE are now the record, word, and data type
C     of the last descriptor in the file.  If LREC is zero, there are
C     no directories in the file yet.  In this case, LWORD and
C     LTYPE are both zero.
C
 
C
C     Compute the number of words we have room for in the current
C     last data record of the indicated type.
C
      IF ( LAST .GT. 0 ) THEN
         ROOM  =  NW   -   (  LAST  -  ( (LAST-1) / NW ) * NW  )
      ELSE
         ROOM  =  0
      END IF
 
C
C     Compute the number of additional data records needed to
C     accommodate (NWORDS - ROOM) additional words of data of type
C     TYPE.
C
      NEEDED  =  ( NWORDS - ROOM + NW - 1 ) / NW
 
 
C
C     Now, update the descriptor directories.
C
 
      IF (  ( ROOM .GE. NWORDS ) .AND. ( DSCREC .GT. 0 )  ) THEN
C
C        This is case (1).
C
C        There is already a descriptor of TYPE in the file.  The data
C        fits in the current record, so no descriptors have to change.
C
C        Update the address range in the directory record containing
C        the last descriptor of TYPE.
C
         MAXADR  =  LAST + NWORDS
 
         CALL DASURI ( HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )
 
C
C        The last logical address of TYPE is now MAXADR.
C
         LASTLA( TYPE )  =  MAXADR
 
C
C        Write out the updated file summary.
C
         CALL DASUFS ( HANDLE,
     .                 NRESVR,
     .                 NRESVC,
     .                 NCOMR,
     .                 NCOMC,
     .                 FREE,
     .                 LASTLA,
     .                 LASTRC,
     .                 LASTWD )
 
 
 
      ELSE IF (       ( TYPE   .EQ. LTYPE )
     .          .AND. ( DSCREC .GT. 0     )
     .          .AND. ( LWORD  .LT. NWI   )  ) THEN
C
C
C        This is case (2).
C
C        The descriptor of TYPE is the last descriptor in the
C        file but is not in the last location (index NWI) of a
C        directory record.  All we have to do is update this last
C        descriptor to reflect the addition of the number of needed
C        data records.
C
C        Get the old descriptor, since we're going to update it.
C
C
         CALL DASRRI ( HANDLE, DSCREC, LWORD, LWORD, DESCR )
 
C
C        Update the descriptor and write it back into the file.
C
         IF ( DESCR .LT. 0 ) THEN
            DESCR = DESCR - NEEDED
         ELSE
            DESCR = DESCR + NEEDED
         END IF
 
         CALL DASURI ( HANDLE, DSCREC, LWORD, LWORD, DESCR )
 
C
C        Update the address range for this type.
C
         MAXADR = LAST + NWORDS
 
         CALL DASURI ( HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )
 
C
C        The last logical address of TYPE is now MAXADR.  The first
C        free record follows the last data record in use.
C
         LASTLA( TYPE )  =  MAXADR
         FREE            =  FREE  +  NEEDED
 
C
C        Write out the updated file summary.
C
         CALL DASUFS ( HANDLE,
     .                 NRESVR,
     .                 NRESVC,
     .                 NCOMR,
     .                 NCOMC,
     .                 FREE,
     .                 LASTLA,
     .                 LASTRC,
     .                 LASTWD )
 
 
      ELSE
C
C        This is case (3).  We need a new descriptor.
C
 
         IF ( LREC .EQ. 0 ) THEN
C
C           This is case (3a).  We have a virgin directory record.
C           Set the number of this record.
C
            RECNO  =  NRESVR + NCOMR + 2
 
C
C           Start with an empty directory record.
C
            CALL CLEARI ( NWI, DIRREC )
 
C
C           Add a new descriptor to the directory.  The record
C           count is the number of new records required:  NEEDED.
C
            DIRREC ( BEGDSC     )  =  TYPE
            DIRREC ( BEGDSC + 1 )  =  NEEDED
 
C
C           Fill in the address range for TYPE covered by this
C           directory.
C
            DIRREC ( RNGLOC     )  =  1
            DIRREC ( RNGLOC + 1 )  =  NWORDS
 
C
C           Write out this directory.
C
            CALL DASWRI ( HANDLE, RECNO, DIRREC )
 
C
C           Update the file summary:  the location of the descriptor
C           and the last logical address for this type must be set.
C           The count portion of the descriptor goes after the initial
C           data type indicator; this data type indicator is not
C           considered to be part of the descriptor.
C
C           The first free record follows the last data record in use.
C
            FREE            =  RECNO  + NEEDED + 1
            LASTLA( TYPE )  =  NWORDS
            LASTRC( TYPE )  =  RECNO
            LASTWD( TYPE )  =  BEGDSC + 1
 
            CALL DASUFS ( HANDLE,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC,
     .                    FREE,
     .                    LASTLA,
     .                    LASTRC,
     .                    LASTWD )
 
 
         ELSE IF ( LWORD .LT. NWI ) THEN
C
C           This is case (3b).  We have room for another descriptor
C           in the current directory record.
C
C           Before adding the new descriptor, we must update the
C           directory containing the current last descriptor of TYPE,
C           if the range of addresses covered by the cluster it
C           describes was increased by the last data addition.  Of
C           course, this update is required only if there IS such a
C           descriptor, and if it is in a record that precedes LREC.
C
            IF (       ( DSCREC .GT. 0    )
     .           .AND. ( DSCREC .LT. LREC )
     .           .AND. ( ROOM   .GT. 0    )   )  THEN
C
C              Update the address range for TYPE in record DSCREC.
C              The upper bound is increased by ROOM, since that many
C              words of TYPE were added to the last record in the
C              last cluster of TYPE described by that directory.
C
               MAXADR = LAST + ROOM
 
               CALL DASURI (HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )
 
            END IF
 
C
C           Make up the new descriptor and write it to the last
C           directory, following the current last descriptor.  The
C           sign of the new descriptor is a function of the type of
C           the current last descriptor.
C
            IF (  TYPE  .EQ.  NEXT(LTYPE)  ) THEN
C
C              TYPE is the successor in the type sequence of the type
C              of the previous descriptor; use a positive count.
C
               DESCR  =   NEEDED
 
            ELSE
               DESCR  =  -NEEDED
 
            END IF
 
            CALL DASURI ( HANDLE, LREC, LWORD+1,  LWORD+1,  DESCR  )
 
C
C           Update the address range for this type.  Some care is needed
C           when updating the minimum address:  this value should be
C           assigned only if this is the first descriptor of TYPE in
C           this directory record.
C
            IF ( DSCREC .LT. LREC ) THEN
 
               MINADR = LAST + ROOM + 1
 
               CALL DASURI ( HANDLE, LREC, RNGLOC, RNGLOC, MINADR )
 
            END IF
 
 
            MAXADR = LAST + NWORDS
 
            CALL DASURI ( HANDLE, LREC, RNGLOC+1, RNGLOC+1, MAXADR )
 
C
C           Update the file summary:  the location of the descriptor
C           and the last logical address for this type must be set.
C
C           The first free record follows the last data record in use.
C
            FREE            =  FREE  +  NEEDED
            LASTLA( TYPE )  =  LAST  +  NWORDS
            LASTRC( TYPE )  =  LREC
            LASTWD( TYPE )  =  LWORD +  1
 
C
C           Before writing out the summary, see whether we'll need
C           a new directory; this will decide whether the first free
C           record changes.
C
C           If we just filled in the last descriptor in a directory,
C           it's time to add a new directory record to the file.
C           All we have to do at the moment is make room for it, and
C           set the forward pointer of the current directory record
C           to point to the saved record.  Initialize the pointers
C           of the new directory record to make the linked list valid.
C
 
            IF ( LWORD+1 .EQ. NWI ) THEN
C
C              Update the previous directory to point forward to the
C              next one.
C
               CALL DASURI ( HANDLE, LREC, FWDLOC, FWDLOC, FREE )

 
C
C              Prepare the new directory record: clear it, set the 
C              backward pointer, and write the record.
C
               CALL CLEARI ( NWI, DIRREC )

               DIRREC ( BWDLOC )  =  LREC

               CALL DASWRI ( HANDLE, FREE, DIRREC )

C
C              Update the free record number.
C
               FREE = FREE + 1
 
            END IF
 
C
C           Now write out the file summary.
C
            CALL DASUFS ( HANDLE,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC,
     .                    FREE,
     .                    LASTLA,
     .                    LASTRC,
     .                    LASTWD )
 
 
         ELSE
C
C           This is case (3c).  We must put the new descriptor in
C           the last directory record, which is currently empty.
C
C           As in case (3b), we may have to update the directory
C           containing the current last descriptor of TYPE, if the
C           range of addresses covered by the cluster it describes was
C           increased by the last data addition.  Of course, this
C           update is required only if there IS such a descriptor.
C
            IF (  ( DSCREC .GT. 0 ) .AND. ( ROOM .GT. 0 )  )  THEN
C
C              Update the address range for TYPE in record DSCREC.
C              The upper bound is increased by ROOM, since that many
C              words of TYPE were added to the last record in the
C              last cluster of TYPE described by that directory.
C
               MAXADR = LAST + ROOM
 
               CALL DASURI (HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )
 
            END IF
 
C
C           Obtain the record number for this directory.
C
            CALL DASRRI ( HANDLE, LREC, FWDLOC, FWDLOC, RECNO )
 
C
C           Now fill in the new directory record.  Start with a clean
C           record.
C
            CALL CLEARI ( NWI, DIRREC )
 
C
C           Set the backward pointer, the address range for TYPE,
C           initial data type, and record count.
C
            DIRREC ( BWDLOC     )  =  LREC
            DIRREC ( RNGLOC     )  =  LAST   +  ROOM    +  1
            DIRREC ( RNGLOC + 1 )  =  LAST   +  NWORDS
            DIRREC ( BEGDSC     )  =  TYPE
            DIRREC ( BEGDSC + 1 )  =  NEEDED
 
C
C           Write out the record.
C
            CALL DASWRI ( HANDLE, RECNO, DIRREC )
 
 
C
C           Update the file summary to reflect the new record and word
C           offsets of the last descriptor of the indicated type.  The
C           last address of TYPE has increased also.  The first free
C           record lies after the added data records.
C
            FREE            =  FREE   +  NEEDED
            LASTLA( TYPE )  =  LAST   +  NWORDS
            LASTRC( TYPE )  =  RECNO
            LASTWD( TYPE )  =  BEGDSC +  1
 
            CALL DASUFS ( HANDLE,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC,
     .                    FREE,
     .                    LASTLA,
     .                    LASTRC,
     .                    LASTWD )
 
         END IF
 
 
      END IF
 
 
      CALL CHKOUT ( 'DASCUD' )
      RETURN
      END
