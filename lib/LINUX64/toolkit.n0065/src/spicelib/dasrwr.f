C$Procedure      DASRWR ( DAS, read/write records )
 
      SUBROUTINE DASRWR ( HANDLE,
     .                    RECNO,
     .                    RECC,
     .                    RECD,
     .                    RECI,
     .                    FIRST,
     .                    LAST,
     .                    DATAD,
     .                    DATAI,
     .                    DATAC   )
 
C$ Abstract
C
C     Read and write DAS physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
 
      INTEGER               NWD
      PARAMETER           ( NWD    =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )
 
      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )
 
      INTEGER               HANDLE
      INTEGER               RECNO
      CHARACTER*(*)         RECC
      DOUBLE PRECISION      RECD    ( NWD )
      INTEGER               RECI    ( NWI )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      DATAD   ( * )
      INTEGER               DATAI   ( * )
      CHARACTER*(*)         DATAC
 
      INTEGER               BUFSZD
      PARAMETER           ( BUFSZD = 10 )
 
      INTEGER               BUFSZI
      PARAMETER           ( BUFSZI = 10 )
 
      INTEGER               BUFSZC
      PARAMETER           ( BUFSZC = 10 )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     HANDLE     I   RRD, RRI, RRC, WRD, WRI, WRC, URD, URI, URC
C     RECNO      I   RRD, RRI, RRC, WRD, WRI, WRC, URD, URI, URC
C     RECC       I   WRC
C     RECD       I   WRD
C     RECI       I   WRI
C     FIRST      I   RRD, RRI, RRC, URD, URI, URC
C     LAST       I   RRD, RRI, RRC, URD, URI, URC
C     DATAD      O   RRD, URD
C     DATAI      O   RRI, URI
C     DATAC      O   RRC, URC
C     NWD        P   RRD, WRD, URD
C     NWI        P   RRI, WRI, URI
C     NWC        P   RRC, WRC, URC
C     BUFSZD     P   RRD, WRD, URD
C     BUFSZI     P   RRI, WRI, URI
C     BUFSZC     P   RRC, WRC, URC
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their inputs.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their outputs.
C
C$ Parameters
C
C     NWD           is the number of DPs in a single DAS record
C                   containing DPs.
C
C     NWI           is the number of integers in a single DAS record
C                   containing integers.
C
C     NWC           is the number of characters in a single DAS record
C                   containing characters.
C
C     BUFSZD,
C     BUFSZI,
C     BUFSZC        are, respectively, the number of records in the
C                   data buffers for double precision, integer, and
C                   character records.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) will be signaled.
C
C     See the entry points for discussions of their exceptions.
C
C$ Files
C
C     See the description of the argument HANDLE in the headers of
C     the entry points for a description of files accessed by this
C     set of routines.
C
C$ Particulars
C
C     This suite of routines provides buffered read and write access to
C     DAS files.  The purpose of this feature is to increase the
C     performance of application programs that access DAS files:  in
C     particular, repeated reads from or writes to a given record
C     should be relatively fast, because the contents of the most
C     recently accessed records are buffered in memory.  Thus DASRWR
C     and its entry points act as a miniature virtual memory system for
C     DAS files.
C
C     These routines are intended primarily for use by other SPICELIB
C     routines; users' application programs will not normally need to
C     call these routines.  Writing to a DAS file with these routines
C     demands a particularly circumspect approach:  it's quite easy to
C     end up with something other than a DAS file if one misuses the
C     routines.
C
C     The entry points of DASRWR support writing, reading, and updating
C     the records in a DAS file.  The distinction between writing and
C     updating is that any record may be written (as long as the record
C     belongs to a file open for writing), but only existing records
C     may be updated.  `Writing' a record sets the values of all of
C     the elements of the record, while a subrange of the elements of an
C     existing record may be `updated'.
C
C     For each of these three operations, there are three DAS routines,
C     one for each supported data type.  The names of the routines are
C
C        -- For writing:     DASWRC,  DASWRD,  DASWRI
C        -- For updating:    DASURC,  DASURD,  DASURI
C        -- For reading:     DASRRC,  DASRRD,  DASRRI
C
C     Users should note that, unlike in the case of SPICELIB's DAF
C     routines, the DAS routines buffer data that is written as well
C     as data that is read.  Consequently a DAS file does not
C     necessarily yet contain, at any moment, all of the data that
C     has been written to it by the DASWRx or DASURx routines.  The
C     written data that is buffered is written out when the need
C     to buffer additional data requires it, and also when the user
C     commands the closure of a file that has been written.  So, at
C     the time a DAS file is closed, the contents of the physical file
C     do reflect what has been `written' to the file by the DASWRx and
C     DASURx entry points.
C
C     At any time, an application program can force the DAS system to
C     write to a DAS file any buffered records maintained for that
C     file.  The entry point DASWBR (DAS, write buffered records)
C     provides this capability.
C
C     DASRWR contains three record buffers:  one of character type,
C     one of double precision type, and one of integer type.  Each
C     buffer has enough room for an integer number of records.  The
C     sizes of the buffers are parameterized and can be increased if
C     necessary.  When contemplating the revision of the buffer
C     sizes selected by NAIF, SPICELIB users should take note of the
C     following points:
C
C        -- Changing values of parameters in NAIF subroutines may cause
C           a maintenance burden for the users of the modified NAIF
C           code, since any changes made to a SPICELIB routine will have
C           to be made to any new version of that routine released by
C           NAIF in a later version of SPICELIB.
C
C        -- The effect of buffer size on the speed with which an
C           application executes is highly dependent on the specific
C           application.  In some cases, increasing the buffer sizes
C           may slow the application down.
C
C$ Examples
C
C     See the entry points for examples specific to those routines.
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
C
C        Added description of NWD, NWI, and NWC to the Parameters
C        and Brief_I/O sections of the header.
C
C-    SPICELIB Version 1.1.0, 17-NOV-1995 (NJB)
C
C        Made modifications to the DASRRx routines to enhance
C        efficiency.  Removed references to the function RETURN.
C
C        Removed weird spaces from ENTRY statements.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header for each entry point.
C        This was done in order to minimize documentation changes if the
C        DAS open routines ever change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     read and write DAS physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 17-NOV-1995 (NJB)
C
C        Made modifications to the DASRRx routines to enhance
C        efficiency.  Removed references to the function RETURN.
C
C        Removed weird spaces from ENTRY statements.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header for each entry point.
C        This was done in order to minimize documentation changes if the
C        DAS open routines ever change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LNKTL
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL  =   -5 )
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL  =   -5 )
 
      INTEGER               FORWRD
      PARAMETER           ( FORWRD  =    1 )
 
C
C     Local variables
C
 
C
C     The data structure maintained by this set of routines consists
C     of three record buffers, one each for use with records of double
C     precision, integer, and character data types.
C
C     Each buffer consists of five parallel arrays; the arrays contain:
C
C        -- data records
C        -- Fortran record numbers
C        -- file handles
C        -- Fortran logical unit numbers
C        -- Update flags
C
C     In addition, for each buffer there is a doubly linked list that
C     points to the buffer and keeps track of the order in which the
C     records in the buffer were accessed.  The three linked lists are
C     maintained in a doubly linked list pool structure.  The logical
C     structure of each buffer is illustrated below.  All of the array
C     elements in the same row are associated with the data record in
C     that row.
C
C
C
C     Linked          Record       Record   Handles   Unit     Update
C      List           buffer       Numbers           Numbers   Flags
C
C      +---+      +------------+    +---+    +---+    +---+    +---+
C      |   | ---> |            |    |   |    |   |    |   |    |   |
C      +---+      +------------+    +---+    +---+    +---+    +---+
C      |   | ---> |            |    |   |    |   |    |   |    |   |
C      +---+      +------------+    +---+    +---+    +---+    +---+
C        .              .             .        .        .        .
C        .              .             .        .        .        .
C        .              .             .        .        .        .
C      +---+      +------------+    +---+    +---+    +---+    +---+
C      |   | ---> |            |    |   |    |   |    |   |    |   |
C      +---+      +------------+    +---+    +---+    +---+    +---+
C
C
      CHARACTER*(NWC)       RCBUFC (      BUFSZC )
      DOUBLE PRECISION      RCBUFD ( NWD, BUFSZD )
      INTEGER               RCBUFI ( NWI, BUFSZI )
 
      INTEGER               RNBUFC (      BUFSZC )
      INTEGER               RNBUFD (      BUFSZD )
      INTEGER               RNBUFI (      BUFSZI )
 
      INTEGER               HNBUFC (      BUFSZC )
      INTEGER               HNBUFD (      BUFSZD )
      INTEGER               HNBUFI (      BUFSZI )
 
      INTEGER               LUBUFC (      BUFSZC )
      INTEGER               LUBUFD (      BUFSZD )
      INTEGER               LUBUFI (      BUFSZI )
 
      LOGICAL               UPBUFC (      BUFSZC )
      LOGICAL               UPBUFD (      BUFSZD )
      LOGICAL               UPBUFI (      BUFSZI )
 
      INTEGER               POOLC  ( 2, LBPOOL : BUFSZC )
      INTEGER               POOLD  ( 2, LBPOOL : BUFSZD )
      INTEGER               POOLI  ( 2, LBPOOL : BUFSZI )
 
      INTEGER               HEADC
      INTEGER               HEADD
      INTEGER               HEADI
 
      INTEGER               USEDC
      INTEGER               USEDD
      INTEGER               USEDI
 
C
C     Other local variables
C
      INTEGER               NEXT
      INTEGER               NODE
      INTEGER               UNIT
 
      LOGICAL               PASS1
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                  PASS1   / .TRUE.           /
 
      DATA                  USEDC   /  0               /
      DATA                  USEDD   /  0               /
      DATA                  USEDI   /  0               /
 
      DATA                  RNBUFC  / BUFSZC * 0       /
      DATA                  RNBUFD  / BUFSZD * 0       /
      DATA                  RNBUFI  / BUFSZI * 0       /
 
      DATA                  HNBUFC  / BUFSZC * 0       /
      DATA                  HNBUFD  / BUFSZD * 0       /
      DATA                  HNBUFI  / BUFSZI * 0       /
 
      DATA                  LUBUFC  / BUFSZC * 0       /
      DATA                  LUBUFD  / BUFSZD * 0       /
      DATA                  LUBUFI  / BUFSZI * 0       /
 
      DATA                  UPBUFC  / BUFSZC * .FALSE. /
      DATA                  UPBUFD  / BUFSZD * .FALSE. /
      DATA                  UPBUFI  / BUFSZI * .FALSE. /
 
      DATA                  HEADC   / 0                /
      DATA                  HEADD   / 0                /
      DATA                  HEADI   / 0                /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASRWR' )
      END IF
 
C
C     Never come here.
C
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
 
      CALL CHKOUT ( 'DASRWR' )
      RETURN
 
 
 
 
 
C$Procedure DASRRD ( DAS, read record, double precision )
 
      ENTRY DASRRD ( HANDLE, RECNO, FIRST, LAST, DATAD )
 
C$ Abstract
C
C     Read DAS double precision physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               FIRST
C     INTEGER               LAST
C     DOUBLE PRECISION      DATAD   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     FIRST,
C     LAST       I   First and last indices of range within record.
C     DATAD      O   Double precision data read from record.
C     BUFSZD     P   Number of records in the DP record buffer.
C     NWD        P   Number of DP in a single DAS DP record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an open DAS file.
C
C     RECNO          is the number of a record in a DAS file.
C
C     FIRST,
C     LAST           are the first and last indices of a range of
C                    double precision numbers to be read from the
C                    indicated record.  The record contains NWD
C                    double precision numbers; these have indices
C                    ranging from 1 to NWD.
C
C$ Detailed_Output
C
C     DATAD          is a double precision array containing the
C                    elements FIRST through LAST of the specified
C                    record.  The record element FIRST is placed
C                    in DATAD(1), the record element FIRST+1 is placed
C                    in DATAD(2), and so on; the record element LAST is
C                    placed in DATAD(LAST-FIRST+1).
C
C$ Parameters
C
C     NWD            is the number of DPs in a single DAS record
C                    containing DPs.
C 
C     BUFSZD         is the number of records in the double precision
C                    record buffer.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The
C         output argument DATAD will not be modified.
C
C     2)  If a read operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The output argument DATAD will not be modified.
C
C     3)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The output argument DATAD will not be modified.  This routine
C         may write out updated, buffered records in order to make
C         room in the double precision buffer for a newly read record.
C         Note that the file written to may be different than the file
C         designated by HANDLE if multiple DAS files are open for
C         writing.
C
C     4)  If FIRST or LAST is not in the range [1, NWD], the error
C         SPICE(INDEXOUTOFRANGE) will be signaled.  The output argument
C         DATAD will not be modified.
C
C     5)  If FIRST > LAST, this routine will return without modifying
C         the output argument DATAD.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to read from a DAS file that is open for
C     reading or for writing.  Any buffered double precision record
C     can be read with this routine.  In particular, records that have
C     been written to the DAS double precision record buffer but have
C     not yet been written out to the DAS file they're intended to go
C     to ARE visible to this routine.
C
C     This routine should be used to read only records that contain
C     double precision data.
C
C$ Examples
C
C     1)  Read the 10th through 100th d.p. numbers from record number 9
C         in a DAS file designated by HANDLE.
C
C             CALL DASRRD ( HANDLE, 9, 10, 100, DATAD )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
C
C        Added description of NWD to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Made modifications to enhance efficiency.  Removed references
C        to the function RETURN.
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     read DAS double precision physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Made modifications to enhance efficiency.  Removed references
C        to the function RETURN.  For buffered reads, MOVED is not
C        called when a single word is to be read.
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     Check FIRST and LAST.  Use discovery check-in.
C
      IF (        ( FIRST .LT. 1   )
     .      .OR.  ( FIRST .GT. NWD )
     .      .OR.  ( LAST  .LT. 1   )
     .      .OR.  ( LAST  .GT. NWD )    )  THEN
 
         CALL CHKIN  ( 'DASRRD'      )
         CALL DASHLU (  HANDLE, UNIT )
 
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    //
     .                 'allowed range for both is [#, #]. File was '  //
     .                 '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWD                                       )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASRRD' )
         RETURN
 
      END IF
 
C
C     There's nothing to do if LAST < FIRST.  (We're not checked in at
C     this point.)
C
      IF ( LAST .LT. FIRST ) THEN
         RETURN
      END IF
 
C
C     See whether record number RECNO in file HANDLE is buffered.  We'll
C     search through the list of buffered records starting at the head
C     of the list.  If we find the desired record, transfer the
C     requested data to the array DATAD and return without further ado.
C
      NODE = HEADD
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFD(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFD(NODE)  )    )  THEN
C
C           Found it.  Move this record to the head of the list.
C           Update our head pointer as required.
C
            IF ( NODE .NE. HEADD ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLD )
               CALL LNKILB ( NODE, HEADD, POOLD )
 
               HEADD  =  NODE
 
            END IF
 
C
C           Don't forget to return the requested data.
C
            IF ( FIRST .EQ. LAST ) THEN
 
               DATAD(1) = RCBUFD( FIRST, NODE )
 
            ELSE
 
               CALL MOVED ( RCBUFD(FIRST, NODE),  LAST-FIRST+1,  DATAD )
 
            END IF
 
C
C           We haven't checked in, so don't check out.
C
            RETURN
 
         END IF
 
         NODE = POOLD ( FORWRD, NODE )
 
      END DO
 
C
C     The record wasn't buffered.  We need to allocate entries to
C     hold the record contents.  If the buffer isn't full, just
C     select a free set of entries.  If the buffer is full, use
C     the set of entries at the tail of the list.
C
C     Since we're now going to do a file read, it doesn't slow
C     us down much to check in, comparatively speaking.
C
      CALL CHKIN ( 'DASRRD' )
 
      IF (  USEDD .EQ. BUFSZD ) THEN
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADD, POOLD )
 
         CALL LNKXSL ( NODE, NODE, POOLD )
 
C
C        If the allocated buffer entry was updated, write it out.
C
         IF (  UPBUFD(NODE)  ) THEN
 
            CALL DASIOD ( 'WRITE',
     .                     LUBUFD(    NODE ),
     .                     RNBUFD(    NODE ),
     .                     RCBUFD( 1, NODE )  )
 
         END IF
 
 
      ELSE
C
C        Allocate a new set of buffer entries, but don't link
C        them into the list yet.
C
         CALL LNKAN  ( POOLD, NODE )
         USEDD = USEDD + 1
 
      END IF
 
C
C     Try to read the record.
C
      CALL DASHLU ( HANDLE, UNIT )
      CALL DASIOD ( 'READ', UNIT, RECNO, RCBUFD(1,NODE) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRRD' )
         RETURN
      END IF
 
C
C     The read was successful.  Link the node pointing to the buffer
C     entries for this record in before the current head of the
C     list, thus putting them at the head.
C
C     Set the file handle, record number, unit, and update flag for
C     this record.
C
      CALL LNKILB ( NODE, HEADD, POOLD )
 
      HNBUFD ( NODE )  =  HANDLE
      RNBUFD ( NODE )  =  RECNO
      LUBUFD ( NODE )  =  UNIT
      UPBUFD ( NODE )  =  .FALSE.
      HEADD            =  NODE
 
C
C     Don't forget to return the requested data.
C
      CALL MOVED (  RCBUFD ( FIRST,  NODE ),
     .              LAST  -  FIRST + 1,
     .              DATAD                     )
 
 
      CALL CHKOUT ( 'DASRRD' )
      RETURN
 
 
 
 
 
 
C$Procedure DASRRI ( DAS, read record, integer )
 
      ENTRY DASRRI ( HANDLE, RECNO, FIRST, LAST, DATAI )
 
C$ Abstract
C
C     Read DAS integer physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               FIRST
C     INTEGER               LAST
C     INTEGER               DATAI   ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     FIRST,
C     LAST       I   First and last indices of range within record.
C     DATAI      O   Integer data read from record.
C     BUFSZI     P   Number of records in the integer record buffer.
C     NWI        P   Number of integers in a single DAS integer record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an open DAS file.
C
C     RECNO          is the number of a record in a DAS file.
C
C     FIRST,
C     LAST           are the first and last indices of a range of
C                    integers to be read from the indicated record.
C                    The record contains NWI integers; these have
C                    indices ranging from 1 to NWI.
C
C$ Detailed_Output
C
C     DATAI          is an integer array containing the elements FIRST
C                    through LAST of the specified record.  The record
C                    element FIRST is placed in DATAI(1), the record
C                    element FIRST+1 is placed in DATAI(2), and so on;
C                    the record element LAST is placed in
C                    DATAI(LAST-FIRST+1).
C
C$ Parameters
C
C     NWI           is the number of integers in a single DAS record
C                   containing integers.
C
C     BUFSZI        is the number of records in the integer record
C                   buffer.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The
C         output argument DATAI will not be modified.
C
C     2)  If a read operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The output argument DATAI will not be modified.
C
C     3)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The output argument DATAI will not be modified.  This routine
C         may write out updated, buffered records in order to make room
C         in the integer buffer for a newly read record.  Note that the
C         file written to may be different than the file designated by
C         HANDLE if multiple DAS files are open for writing.
C
C     4)  If FIRST or LAST is not in the range [1, NWI], the error
C         SPICE(INDEXOUTOFRANGE) will be signaled.  The output argument
C         DATAI will not be modified.
C
C     5)  If FIRST > LAST, this routine will return without modifying
C         the output argument DATAI.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to read from a DAS file that is open for
C     reading or writing.  Any buffered integer record can be read with
C     this routine.  In particular, records that have been written to
C     the DAS integer record buffer but have not yet been written out
C     to the DAS file they're intended to go to ARE visible to this
C     routine.
C
C     This routine should be used to read only records that contain
C     integer data.
C
C$ Examples
C
C     1)  Read the 10th through 100th integers from record number 9
C         in a DAS file designated by HANDLE.
C
C             CALL DASRRI ( HANDLE, 9, 10, 100, DATAI )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
C
C        Added description of NWI to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Made modifications to enhance efficiency.  Removed references
C        to the function RETURN.
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     read DAS integer physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Made modifications to enhance efficiency.  Removed references
C        to the function RETURN.  For buffered reads, MOVEI is not
C        called when a single word is to be read.
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Non-standard SPICE error handling.
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     Check FIRST and LAST.  Use discovery check-in.
C
      IF (        ( FIRST .LT. 1   )
     .      .OR.  ( FIRST .GT. NWI )
     .      .OR.  ( LAST  .LT. 1   )
     .      .OR.  ( LAST  .GT. NWI )    )  THEN
 
         CALL CHKIN  ( 'DASRRI'      )
         CALL DASHLU (  HANDLE, UNIT )
 
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    //
     .                 'allowed range for both is [#, #]. File was '  //
     .                 '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWI                                       )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASRRI' )
         RETURN
 
      END IF
 
C
C     There's nothing to do if LAST < FIRST.  (We're not checked in at
C     this point.)
C
      IF ( LAST .LT. FIRST ) THEN
         RETURN
      END IF
 
C
C     See whether record number RECNO in file HANDLE is buffered.  We'll
C     search through the list of buffered records starting at the head
C     of the list.  If we find the desired record, transfer the
C     requested data to the array DATAI and return without further ado.
C
      NODE = HEADI
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFI(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFI(NODE)  )    )  THEN
C
C
C           Found it.  Move this record to the head of the list.
C           Update our head pointer as required.
C
            IF ( NODE .NE. HEADI ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLI )
               CALL LNKILB ( NODE, HEADI, POOLI )
 
               HEADI  =  NODE
 
            END IF
 
C
C           Don't forget to return the requested data.
C
            IF ( FIRST .EQ. LAST ) THEN
 
               DATAI(1) = RCBUFI( FIRST, NODE )
 
            ELSE
 
               CALL MOVEI ( RCBUFI(FIRST, NODE),  LAST-FIRST+1,  DATAI )
 
            END IF
 
C
C           We haven't checked in, so don't check out.
C
            RETURN
 
         END IF
 
         NODE = POOLI ( FORWRD, NODE )
 
      END DO
 
C
C     The record wasn't buffered.  We need to allocate entries to
C     hold the record contents.  If the buffer isn't full, just
C     select a free set of entries.  If the buffer is full, use
C     the set of entries at the tail of the list.
C
C     Since we're now going to do a file read, it doesn't slow
C     us down much to check in, comparatively speaking.
C
      CALL CHKIN ( 'DASRRI' )
 
      IF (  USEDI .EQ. BUFSZI ) THEN
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADI, POOLI )
 
         CALL LNKXSL ( NODE, NODE, POOLI )
 
C
C        If the allocated buffer entry was updated, write it out.
C
         IF (  UPBUFI(NODE)  ) THEN
 
            CALL DASIOI (  'WRITE',
     .                      LUBUFI(    NODE ),
     .                      RNBUFI(    NODE ),
     .                      RCBUFI( 1, NODE )   )
         END IF
 
 
      ELSE
C
C        Allocate a new set of buffer entries, but don't link
C        them into the list yet.
C
         CALL LNKAN ( POOLI, NODE )
         USEDI = USEDI + 1
 
      END IF
 
C
C     Try to read the record.
C
      CALL DASHLU ( HANDLE,  UNIT )
      CALL DASIOI ( 'READ',  UNIT,  RECNO,  RCBUFI(1,NODE)  )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRRI' )
         RETURN
      END IF
 
C
C     The read was successful.  Link the node pointing to the buffer
C     entries for this record in before the current head of the
C     list, thus putting them at the head.
C
C     Set the file handle, record number, unit, and update flag for
C     this record.
C
      CALL LNKILB ( NODE, HEADI, POOLI )
 
      HNBUFI ( NODE )  =  HANDLE
      RNBUFI ( NODE )  =  RECNO
      LUBUFI ( NODE )  =  UNIT
      UPBUFI ( NODE )  =  .FALSE.
      HEADI            =  NODE
 
C
C     Don't forget to return the requested data.
C
      CALL MOVEI (  RCBUFI ( FIRST,  NODE ),
     .              LAST  -  FIRST + 1,
     .              DATAI                     )
 
 
      CALL CHKOUT ( 'DASRRI' )
      RETURN
 
 
 
 
 
 
 
C$Procedure DASRRC ( DAS, read record, character )
 
      ENTRY DASRRC ( HANDLE, RECNO, FIRST, LAST, DATAC )
 
C$ Abstract
C
C     Read DAS character physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               FIRST
C     INTEGER               LAST
C     CHARACTER*(*)         DATAC
C
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     FIRST,
C     LAST       I   First and last indices of range within record.
C     DATAC      O   Character data read from record.
C     BUFSZC     P   Number of records in the character record buffer.
C     NWC        P   Number of characters in a single DAS char. record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an open DAS file.
C
C     RECNO          is the number of a record in a DAS file.
C
C     FIRST,
C     LAST           are the first and last indices of a range of
C                    characters to be read from the indicated record.
C                    The record contains NWC characters; these have
C                    indices ranging from 1 to NWC.
C
C$ Detailed_Output
C
C     DATAC          is a character string containing the elements
C                    FIRST through LAST of the specified record.  The
C                    record element FIRST is placed in DATAC(1:1), the
C                    record element FIRST+1 is placed in DATAC(2:2),
C                    and so on; the record element LAST is placed in
C                    DATAC( LAST-FIRST+1 : LAST-FIRST+1 ).
C
C$ Parameters
C
C     NWC           is the number of characters in a single DAS record
C                   containing characters.
C
C     BUFSZC        is the number of records in the character record
C                   buffer.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The
C         output argument DATAC will not be modified.
C
C     2)  If a read operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The output argument DATAC will not be modified.
C
C     3)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The output argument DATAC will not be modified.  This routine
C         may write out updated, buffered records in order to make room
C         in the character buffer for a newly read record.  Note that
C         the file written to may be different than the file
C         designated by HANDLE if multiple DAS files are open for
C         writing.
C
C     4)  If FIRST or LAST is not in the range [1, NWC], the error
C         SPICE(INDEXOUTOFRANGE) will be signaled.  The output argument
C         DATAC will not be modified.
C
C     5)  If FIRST > LAST, this routine will return without modifying
C         the output argument DATAC.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to read from a DAS file that is open for
C     reading or writing.  Any buffered character record can be read
C     with this routine.  In particular, records that have been
C     written to the DAS character record buffer but have not yet been
C     written out to the DAS file they're intended to go to ARE
C     visible to this routine.
C
C     This routine should be used to read only records that contain
C     character data.
C
C$ Examples
C
C     1)  Read the 10th through 100th characters from record number 9
C         in a DAS file designated by HANDLE.
C
C             CALL DASRRC ( HANDLE, 9, 10, 100, DATAC )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS)
C
C        Added description of NWC to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.1.0, 09-NOV-1995 (NJB)
C
C        Made modifications to enhance efficiency.  Removed references
C        to the function RETURN.
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     read DAS character physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 09-NOV-1995 (NJB)
C
C        Made modifications to enhance efficiency.  Removed references
C        to the function RETURN.
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     Check FIRST and LAST.  Use discovery check-in.
C
      IF (        ( FIRST .LT. 1   )
     .      .OR.  ( FIRST .GT. NWC )
     .      .OR.  ( LAST  .LT. 1   )
     .      .OR.  ( LAST  .GT. NWC )    )  THEN
 
         CALL CHKIN  ( 'DASRRC'      )
         CALL DASHLU (  HANDLE, UNIT )
 
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    //
     .                 'allowed range for both is [#, #]. File was '  //
     .                 '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWC                                       )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASRRC' )
         RETURN
 
      END IF
 
C
C     There's nothing to do if LAST < FIRST.  (We're not checked in at
C     this point.)
C
      IF ( LAST .LT. FIRST ) THEN
         RETURN
      END IF
 
C
C     See whether record number RECNO in file HANDLE is buffered.  We'll
C     search through the list of buffered records starting at the head
C     of the list.  If we find the desired record, transfer the
C     requested data to the array DATAC and return without further ado.
C
      NODE = HEADC
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFC(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFC(NODE)  )    )  THEN
C
C
C           Found it.  Move this record to the head of the list.
C           Update our head pointer as required.
C
            IF ( NODE .NE. HEADC ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLC )
               CALL LNKILB ( NODE, HEADC, POOLC )
 
               HEADC  =  NODE
 
            END IF
 
C
C           Don't forget to return the requested data.
C
            DATAC  =  RCBUFC( NODE ) ( FIRST : LAST )
 
C
C           We haven't checked in, so don't check out.
C
            RETURN
 
         END IF
 
         NODE = POOLC ( FORWRD, NODE )
 
      END DO
 
C
C     The record wasn't buffered.  We need to allocate entries to
C     hold the record contents.  If the buffer isn't full, just
C     select a free set of entries.  If the buffer is full, use
C     the set of entries at the tail of the list.
C
C     Since we're now going to do a file read, it doesn't slow
C     us down much to check in, comparatively speaking.
C
      CALL CHKIN ( 'DASRRC' )
 
      IF (  USEDC .EQ. BUFSZC ) THEN
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADC, POOLC )
 
         CALL LNKXSL ( NODE, NODE, POOLC )
 
C
C        If the allocated buffer entry was updated, write it out.
C
         IF (  UPBUFC(NODE)  ) THEN
 
            CALL DASIOC (  'WRITE',
     .                      LUBUFC( NODE ),
     .                      RNBUFC( NODE ),
     .                      RCBUFC( NODE )  )
 
         END IF
 
 
      ELSE
C
C        Allocate a new set of buffer entries, but don't link
C        them into the list yet.
C
         CALL LNKAN ( POOLC, NODE )
         USEDC = USEDC + 1
 
      END IF
 
C
C     Try to read the record.
C
      CALL DASHLU ( HANDLE,  UNIT )
      CALL DASIOC ( 'READ',  UNIT,  RECNO, RCBUFC(NODE) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRRC' )
         RETURN
      END IF
 
C
C     The read was successful.  Link the node pointing to the buffer
C     entries for this record in before the current head of the
C     list, thus putting them at the head.
C
C     Set the file handle, record number, unit, and update flag for
C     this record.
C
      CALL LNKILB ( NODE, HEADC, POOLC )
 
      HNBUFC ( NODE )  =  HANDLE
      RNBUFC ( NODE )  =  RECNO
      LUBUFC ( NODE )  =  UNIT
      UPBUFC ( NODE )  =  .FALSE.
      HEADC            =  NODE
 
C
C     Don't forget to return the requested data.
C
      DATAC  =  RCBUFC( NODE ) ( FIRST : LAST )
 
      CALL CHKOUT ( 'DASRRC' )
      RETURN
 
 
 
 
 
 
C$Procedure DASWRD ( DAS, write record, double precision )
 
      ENTRY DASWRD ( HANDLE, RECNO, RECD )
 
C$ Abstract
C
C     Write DAS double precision physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     DOUBLE PRECISION      RECD   ( NWD )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     RECD       I   Double precision data to be written to record.
C     BUFSZD     P   Number of records in the DP record buffer.
C     NWD        P   Number of DP in a single DAS DP record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C     RECNO          is the number of a record in a DAS file.
C
C     RECD           is an array of NWD double precision numbers.  The
C                    contents of this array are to be written to the
C                    physical file record having number RECNO.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     NWD           is the number of DPs in a single DAS record
C                   containing DPs.
C
C     BUFSZD        is the number of records in the double precision
C                   record buffer.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The DAS file
C         designated by HANDLE will not be modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.  Note that the file written to may be different than
C         the file designated by HANDLE if multiple DAS files are open
C         for writing.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to write to only DAS files that are open
C     for writing.  Records written via this routine will always be
C     buffered immediately, but may not be written to the file until
C     they are cleared from the double precision buffer to make room
C     for other records, or until they are explicitly forced to to be
C     written via a call to DASWBR.  In any case, at the moment this
C     routine returns, the data supplied on input may be read back by
C     DASRRD or updated by DASURD.
C
C     Closing a DAS file via DASCLS forces any remaining updated data
C     records buffered by this routine to be written to the file.
C
C$ Examples
C
C     1)  Write an array of NWD double precision numbers to the 9th
C         record in a DAS file designated by HANDLE.
C
C            DOUBLE PRECISION        RECD
C
C                         .
C                         .
C                         .
C
C            DO I = 1, NWD
C               RECD(I) = DBLE(I)
C            END DO
C
C            CALL DASWRD ( HANDLE, 9, RECD )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
C
C        Added description of NWD to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     write DAS double precision physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASWRD' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWRD' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     See whether double precision record number RECNO from file HANDLE
C     is buffered.  We'll search through the list of buffered records
C     starting at the head of the list.  If the record is already
C     buffered, we'll update the buffer entry, but we'll defer writing
C     the record out until we need to free a record, or until the
C     d.p. buffer is flushed, whichever comes first.
C
      NODE = HEADD
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFD(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFD(NODE)  )    )  THEN
C
C           Found it.  Update the buffered record.
C
            CALL MOVED (  RECD,  NWD,  RCBUFD(1, NODE)  )
 
C
C           Set the update flag, indicating that this buffer entry
C           has been modified.
C
            UPBUFD ( NODE ) = .TRUE.
 
C
C           Put the information about this record at the head of the
C           active list, if it is not already there.
C
            IF ( NODE .NE. HEADD ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLD )
               CALL LNKILB ( NODE, HEADD, POOLD )
               HEADD = NODE
 
            END IF
 
            CALL CHKOUT ( 'DASWRD' )
            RETURN
 
         END IF
 
         NODE = POOLD ( FORWRD, NODE )
 
      END DO
 
C
C     The record we're writing to is not buffered.  We'll allocate
C     a buffer entry.  If the record buffer is full, we'll
C     commandeer the least recently accessed record.  Before using
C     this record, we'll write its contents out to the corresponding
C     file, if the record has been updated.
C
      IF ( USEDD .LT. BUFSZD ) THEN
C
C        There's a free buffer entry available.  Just allocate it.
C
         CALL LNKAN ( POOLD, NODE )
 
         USEDD = USEDD + 1
 
      ELSE
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADD, POOLD )
 
         CALL LNKXSL ( NODE, NODE, POOLD )
 
C
C        If the allocated record was updated, write it out.
C
         IF (  UPBUFD(NODE)  ) THEN
 
            CALL DASIOD (  'WRITE',
     .                      LUBUFD(    NODE ),
     .                      RNBUFD(    NODE ),
     .                      RCBUFD( 1, NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWRD' )
               RETURN
            END IF
 
         END IF
 
      END IF
 
C
C     Now update the allocated buffer entry with the input data.
C
      CALL MOVED (  RECD,  NWD,  RCBUFD( 1, NODE )  )
 
C
C     Set the update flag, indicating that this buffer entry
C     has been modified.  Also set the handle, unit, and record number
C     entries.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      UPBUFD ( NODE ) = .TRUE.
      HNBUFD ( NODE ) =  HANDLE
      LUBUFD ( NODE ) =  UNIT
      RNBUFD ( NODE ) =  RECNO
 
C
C     Link this buffer entry to the head of the list.
C
      CALL LNKILB ( NODE, HEADD, POOLD )
 
      HEADD = NODE
 
      CALL CHKOUT ( 'DASWRD' )
      RETURN
 
 
 
 
 
 
C$Procedure DASWRI ( DAS, write record, integer )
 
      ENTRY DASWRI ( HANDLE, RECNO, RECI )
 
C$ Abstract
C
C     Write DAS integer physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               RECI   ( NWI )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     RECI       I   Integer data to be written to record.
C     BUFSZI     P   Number of records in the integer record buffer.
C     NWI        P   Number of integers in a single DAS integer record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C     RECNO          is the number of a record in a DAS file.
C
C     RECI           is an array of NWI integers.  The contents of this
C                    array are to be written to the physical file
C                    record having number RECNO.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     NWI           is the number of integers in a single DAS record
C                   containing integers.
C
C     BUFSZI        is the number of records in the integer record
C                   buffer.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The DAS file
C         designated by HANDLE will not be modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.  Note that the file written to may be different than
C         the file designated by HANDLE if multiple DAS files are open
C         for writing.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to write to only DAS files that are open
C     for writing.  Records written via this routine will always be
C     buffered immediately, but may not be written to the file until
C     they are cleared from the integer buffer to make room for other
C     records, or until they are explicitly forced to to be written via
C     a call to DASWBR.  In any case, at the moment this routine
C     returns, the data supplied on input may be read back by DASRRI
C     or updated by DASURI.
C
C     Closing a DAS file via DASCLS forces any remaining updated data
C     records buffered by this routine to be written to the file.
C
C$ Examples
C
C     1)  Write an array of NWI integers to the 9th record in a DAS
C         file designated by HANDLE.
C
C            INTEGER                RECI ( NWI )
C                         .
C                         .
C                         .
C
C            DO I = 1, NWI
C               RECI(I) = I
C            END DO
C
C            CALL DASWRI ( HANDLE, 9, RECI )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
C
C        Added description of NWI to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     write DAS integer physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASWRI' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWRI' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     See whether integer record number RECNO from file HANDLE is
C     buffered.  We'll search through the list of buffered records
C     starting at the head of the list.  If the record is already
C     buffered, we'll update the buffer entry, but we'll defer writing
C     the record out until we need to free a record, or until the
C     integer buffer is flushed, whichever comes first.
C
      NODE = HEADI
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFI(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFI(NODE)  )    )  THEN
C
C           Found it.  Update the buffered record.
C
            CALL MOVEI (  RECI,  NWI,  RCBUFI( 1, NODE )  )
 
C
C           Set the update flag, indicating that this buffer entry
C           has been modified.
C
            UPBUFI ( NODE ) = .TRUE.
 
C
C           Put the information about this record at the head of the
C           active list, if it is not already there.
C
            IF ( NODE .NE. HEADI ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLI )
               CALL LNKILB ( NODE, HEADI, POOLI )
               HEADI = NODE
 
            END IF
 
            CALL CHKOUT ( 'DASWRI' )
            RETURN
 
         END IF
 
         NODE = POOLI ( FORWRD, NODE )
 
      END DO
 
C
C     The record we're writing to is not buffered.  We'll allocate
C     a buffer entry.  If the record buffer is full, we'll
C     commandeer the least recently accessed record.  Before using
C     this record, we'll write its contents out to the corresponding
C     file, if the record has been updated.
C
      IF ( USEDI .LT. BUFSZI ) THEN
C
C        There's a free buffer entry available.  Just allocate it.
C
         CALL LNKAN ( POOLI, NODE )
         USEDI = USEDI + 1
 
      ELSE
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADI, POOLI )
 
         CALL LNKXSL ( NODE, NODE, POOLI )
 
C
C        If the allocated record was updated, write it out.
C
         IF (  UPBUFI(NODE)  ) THEN
 
            CALL DASIOI (  'WRITE',
     .                      LUBUFI(    NODE ),
     .                      RNBUFI(    NODE ),
     .                      RCBUFI( 1, NODE )  )
 
         END IF
 
      END IF
 
C
C     Now update the allocated buffer entry with the input data.
C
      CALL MOVEI (  RECI,  NWI,  RCBUFI( 1, NODE )  )
 
C
C     Set the update flag, indicating that this buffer entry
C     has been modified.  Also set the handle, unit, and record number
C     entries.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      UPBUFI ( NODE ) = .TRUE.
      HNBUFI ( NODE ) =  HANDLE
      LUBUFI ( NODE ) =  UNIT
      RNBUFI ( NODE ) =  RECNO
 
C
C     Link this buffer entry to the head of the list.
C
      CALL LNKILB ( NODE, HEADI, POOLI )
 
      HEADI = NODE
 
      CALL CHKOUT ( 'DASWRI' )
      RETURN
 
 
 
 
 
 
C$Procedure DASWRC ( DAS, write record, character )
 
      ENTRY DASWRC ( HANDLE, RECNO, RECC )
 
C$ Abstract
C
C     Write DAS character physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     CHARACTER*(*)         RECC
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     RECC       I   Character data to be written to record.
C     BUFSZC     P   Number of records in the character record buffer.
C     NWC        P   Number of characters in a single DAS char. record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C     RECNO          is the number of a record in a DAS file.
C
C     RECC           is a string of length NWC.  The contents of this
C                    string are to be written to the physical file
C                    record having number RECNO.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     NWC           is the number of characters in a single DAS record
C                   containing characters.
C
C     BUFSZC        is the number of records in the character record
C                   buffer.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The DAS file
C         designated by HANDLE will not be modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.  Note that the file written to may be different than
C         the file designated by HANDLE if multiple DAS files are open
C         for writing.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to write to only DAS files that are open
C     for writing.  Records written via this routine will always be
C     buffered immediately, but may not be written to the file until
C     they are cleared from the character buffer to make room for other
C     records, or until they are explicitly forced to to be written via
C     a call to DASWBR.  In any case, at the moment this routine
C     returns, the data supplied on input may be read back by DASRRC
C     or updated by DASURC.
C
C     Closing a DAS file via DASCLS forces any remaining updated data
C     records buffered by this routine to be written to the file.
C
C$ Examples
C
C     1)  Write a string of NWC characters to the 9th record in a DAS
C         file designated by HANDLE.
C
C            CHARACTER*(NWC)           RECC
C
C                         .
C                         .
C                         .
C
C            RECC = 'This example string is blank-padded on the '    //
C           .       'right.  All of the trailing blanks will be '    //
C           .       'written to the DAS file by the following call.'
C
C            CALL DASWRC ( HANDLE, 9, RECC )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
C
C        Added description of NWC to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     write DAS character physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASWRC' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWRC' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     See whether character record number RECNO from file HANDLE is
C     buffered.  We'll search through the list of buffered records
C     starting at the head of the list.  If the record is already
C     buffered, we'll update the buffer entry, but we'll defer writing
C     the record out until we need to free a record, or until the
C     character buffer is flushed, whichever comes first.
C
      NODE = HEADC
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFC(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFC(NODE)  )    )  THEN
C
C           Found it.  Update the buffered record.
C
            RCBUFC (NODE)  =  RECC
 
C
C           Set the update flag, indicating that this buffer entry
C           has been modified.
C
            UPBUFC ( NODE ) = .TRUE.
 
C
C           Put the information about this record at the head of the
C           active list, if it is not already there.
C
            IF ( NODE .NE. HEADC ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLC )
               CALL LNKILB ( NODE, HEADC, POOLC )
               HEADC = NODE
 
            END IF
 
            CALL CHKOUT ( 'DASWRC' )
            RETURN
 
         END IF
 
         NODE = POOLC ( FORWRD, NODE )
 
      END DO
 
C
C     The record we're writing to is not buffered.  We'll allocate
C     a buffer entry.  If the record buffer is full, we'll
C     commandeer the least recently accessed record.  Before using
C     this record, we'll write its contents out to the corresponding
C     file, if the record has been updated.
C
      IF ( USEDC .LT. BUFSZC ) THEN
C
C        There's a free buffer entry available.  Just allocate it.
C
         CALL LNKAN ( POOLC, NODE )
         USEDC = USEDC + 1
 
      ELSE
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  = LNKTL ( HEADC, POOLC )
 
         CALL LNKXSL ( NODE, NODE, POOLC )
 
C
C        If the allocated record was updated, write it out.
C
         IF (  UPBUFC(NODE)  ) THEN
 
            CALL DASIOC ( 'WRITE',
     .                     LUBUFC( NODE ),
     .                     RNBUFC( NODE ),
     .                     RCBUFC( NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWRC' )
               RETURN
            END IF
 
         END IF
 
      END IF
 
C
C     Now update the allocated buffer entry with the input data.
C
      RCBUFC (NODE)  =  RECC
 
C
C     Set the update flag, indicating that this buffer entry
C     has been modified.  Also set the handle, unit, and record number
C     entries.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      UPBUFC ( NODE ) = .TRUE.
      HNBUFC ( NODE ) =  HANDLE
      LUBUFC ( NODE ) =  UNIT
      RNBUFC ( NODE ) =  RECNO
 
C
C     Link this buffer entry to the head of the list.
C
      CALL LNKILB ( NODE, HEADC, POOLC )
 
      HEADC = NODE
 
      CALL CHKOUT ( 'DASWRC' )
      RETURN
 
 
 
 
 
 
 
 
C$Procedure DASURD ( DAS, update record, double precision )
 
      ENTRY DASURD ( HANDLE, RECNO, FIRST, LAST, DATAD )
 
C$ Abstract
C
C     Update DAS double precision physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               FIRST
C     INTEGER               LAST
C     DOUBLE PRECISION      DATAD  ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     FIRST,
C     LAST       I   First and last indices of range within record.
C     DATAD      I   Double precision data to write to record.
C     BUFSZD     P   Number of records in the DP record buffer.
C     NWD        P   Number of DPs in a single DAS DP record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C     RECNO          is the number of a record in a DAS file.
C
C     FIRST,
C     LAST           are the first and last indices of a range of
C                    elements to be updated in the indicated record.
C                    The record contains NWD double precision numbers;
C                    these have indices ranging from 1 to NWD.
C
C     DATAD          is a double precision array to be written to
C                    elements FIRST through LAST of the specified
C                    record.  The array element DATAD(1) is placed in
C                    record element FIRST, the array element DATAD(2)
C                    is placed in record element FIRST+1, and so on;
C                    the array element DATAD(LAST-FIRST+1) is placed in
C                    the record element LAST.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     NWD           is the number of DPs in a single DAS record
C                   containing DPs.
C
C     BUFSZD        is the number of records in the double precision
C                   record buffer.
C
C$ Exceptions
C
C     1)  This routine may be used to update only records that have
C         already been written by DASWRD or that already exist in the
C         file designated by HANDLE.  Attempting to update a record
C         that hasn't yet been written will cause the read operation
C         performed by this routine to fail.
C
C         If a read operation attempted by this routine fails for this
C         or any other reason, the error will be diagnosed by routines
C         called by this routine.  The indicated record will not be
C         modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.  Note that the file written to may be different than
C         the file designated by HANDLE if multiple DAS files are open
C         for writing.
C
C     3)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The indicated
C         record will not be modified.
C
C     4)  If FIRST or LAST is not in the range [1, NWD], the error
C         SPICE(INDEXOUTOFRANGE) will be signaled.  The indicated
C         record will not be modified.
C
C     5)  If FIRST > LAST, this routine will return without modifying
C         the indicated record.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to update any existing record in a DAS
C     file that is open for writing, or any record that has been
C     `written' by DASWRD, whether or not that record has yet been
C     physically written to the file it belongs to.  Records that have
C     never been written cannot be updated.
C
C     Because the DAS system buffers records that are written, multiple
C     updates of parts of a record can be made without incurring a
C     large number of file reads and writes.
C
C     This routine should be used to update only records that contain
C     double precision data.
C
C$ Examples
C
C     1)  Update the 10th through 100th d.p. numbers in record number 9
C         in a DAS file designated by HANDLE.
C
C             DOUBLE PRECISION      DATAD ( 100 )
C
C                         .
C                         .
C                         .
C
C             DO I = 1, 91
C                DATAD  =  DBLE(I)
C             END DO
C
C             CALL DASURD ( HANDLE, 9, 10, 100, DATAD )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
C
C        Added description of NWD to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     update DAS double precision physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASURD' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURD' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     If FIRST or LAST are out of range, no dice.
C
      IF (        ( FIRST .LT. 1   )
     .      .OR.  ( FIRST .GT. NWD )
     .      .OR.  ( LAST  .LT. 1   )
     .      .OR.  ( LAST  .GT. NWD )    )  THEN
 
         CALL DASHLU (  HANDLE, UNIT )
 
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    //
     .                 'allowed range for both is [#, #]. File was '  //
     .                 '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWD                                       )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASURD' )
         RETURN
 
      END IF
 
C
C     There's nothing to do if LAST < FIRST.
C
      IF ( LAST .LT. FIRST ) THEN
         CALL CHKOUT ( 'DASURD' )
         RETURN
      END IF
 
C
C     See whether double precision record number RECNO from file HANDLE
C     is buffered.  We'll search through the list of buffered records
C     starting at the head of the list.  If the record is already
C     buffered, we'll update the buffer entry, but we'll defer writing
C     the record out until we need to free a record, or until the
C     d.p. buffer is flushed, whichever comes first.
C
      NODE = HEADD
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFD(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFD(NODE)  )    )  THEN
C
C           Found it.  Update the buffered record.
C
            CALL MOVED ( DATAD,   LAST-FIRST+1,   RCBUFD(FIRST, NODE)  )
 
C
C           Set the update flag, indicating that this buffer entry
C           has been modified.
C
            UPBUFD ( NODE ) = .TRUE.
 
C
C           Put the information about this record at the head of the
C           active list, if it is not already there.
C
            IF ( NODE .NE. HEADD ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLD )
               CALL LNKILB ( NODE, HEADD, POOLD )
               HEADD = NODE
 
            END IF
 
            CALL CHKOUT ( 'DASURD' )
            RETURN
 
         END IF
 
         NODE  =  POOLD ( FORWRD, NODE )
 
      END DO
 
C
C     The record we're writing to is not buffered.  In order to
C     update this record, we'll need to read it first.  But before
C     we do that, we'll need to allocate a buffer entry.  If the record
C     buffer is full, we'll commandeer the least recently accessed
C     record.  Before using this record, we'll write its contents out
C     to the corresponding file, if the record has been updated.
C
      IF ( USEDD .LT. BUFSZD ) THEN
C
C        There's a free buffer entry available.  Just allocate it.
C
         CALL LNKAN ( POOLD, NODE )
 
         USEDD = USEDD + 1
 
      ELSE
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADD, POOLD )
 
         CALL LNKXSL ( NODE, NODE, POOLD )
 
C
C        If the allocated record was updated, write it out.
C
         IF (  UPBUFD(NODE)  ) THEN
 
            CALL DASIOD ( 'WRITE',
     .                     LUBUFD(    NODE ),
     .                     RNBUFD(    NODE ),
     .                     RCBUFD( 1, NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASURD' )
               RETURN
            END IF
 
         END IF
 
      END IF
 
 
C
C     Now try to read the record we're going to update.
C
      CALL DASHLU ( HANDLE,  UNIT )
      CALL DASIOD ( 'READ',  UNIT,  RECNO, RCBUFD(1,NODE) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURD' )
         RETURN
      END IF
 
C
C     The read was successful, so set the record number, handle, unit,
C     and update flag for this buffer entry, and link these buffer
C     entries in before the current head of the list, thus putting
C     them at the head.
C
C     Update the head pointer.
C
      CALL LNKILB ( NODE, HEADD, POOLD )
 
      HNBUFD ( NODE )  =  HANDLE
      RNBUFD ( NODE )  =  RECNO
      LUBUFD ( NODE )  =  UNIT
      UPBUFD ( NODE )  = .TRUE.
      HEADD            =  NODE
 
C
C     At long last, make the requested update.  Note that we don't
C     have to write the record back to the file; that will get done
C     automatically before or at the time the file is closed.
C
      CALL MOVED (  DATAD,  LAST-FIRST+1,  RCBUFD( FIRST, NODE )  )
 
 
      CALL CHKOUT ( 'DASURD' )
      RETURN
 
 
 
 
 
 
 
C$Procedure DASURI ( DAS, update record, integer )
 
      ENTRY DASURI ( HANDLE, RECNO, FIRST, LAST, DATAI )
 
C$ Abstract
C
C     Update DAS integer physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               FIRST
C     INTEGER               LAST
C     INTEGER               DATAI  ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     FIRST,
C     LAST       I   First and last indices of range within record.
C     DATAI      I   Integer data to write to record.
C     BUFSZI     P   Number of records in the integer record buffer.
C     NWI        P   Number of integers in a single DAS integer record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C     RECNO          is the number of a record in a DAS file.
C
C     FIRST,
C     LAST           are the first and last indices of a range of
C                    elements to be updated in the indicated record.
C                    The record contains NWI integers; these have
C                    indices ranging from 1 to NWI.
C
C     DATAI          is an integer array to be written to elements FIRST
C                    through LAST of the specified record.  The array
C                    element DATAI(1) is placed in record element FIRST,
C                    the array element DATAI(2) is placed in record
C                    element FIRST+1, and so on; the array element
C                    DATAI(LAST-FIRST+1) is placed in the record element
C                    LAST.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     NWI           is the number of integers in a single DAS record
C                   containing integers.
C
C     BUFSZI        is the number of records in the integer record
C                   buffer.
C
C$ Exceptions
C
C     1)  This routine may be used to update only records that have
C         already been written by DASWRI or that already exist in the
C         file designated by HANDLE.  Attempting to update a record
C         that hasn't yet been written will cause the read operation
C         performed by this routine to fail.
C
C         If a read operation attempted by this routine fails for this
C         or any other reason, the error will be diagnosed by routines
C         called by this routine.  The indicated record will not be
C         modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.  Note that the file written to may be different than
C         the file designated by HANDLE if multiple DAS files are open
C         for writing.
C
C     3)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The indicated
C         record will not be modified.
C
C     4)  If FIRST or LAST is not in the range [1, NWI], the error
C         SPICE(INDEXOUTOFRANGE) will be signaled.  The indicated
C         record will not be modified.
C
C     5)  If FIRST > LAST, this routine will return without modifying
C         the indicated record.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to update any existing record in a DAS
C     file that is open for writing, or any record that has been
C     `written' by DASWRI, whether or not that record has yet been
C     physically written to the file it belongs to.  Records that have
C     never been written cannot be updated.
C
C     Because the DAS system buffers records that are written, multiple
C     updates of parts of a record can be made without incurring a
C     large number of file reads and writes.
C
C     This routine should be used to update only records that contain
C     integer data.
C
C$ Examples
C
C     1)  Update the 10th through 100th integers in record number 9
C         in a DAS file designated by HANDLE.
C
C             INTEGER               DATAI ( 100 )
C
C                         .
C                         .
C                         .
C
C             DO I = 1, 91
C                DATAI  =  I
C             END DO
C
C             CALL DASURI ( HANDLE, 9, 10, 100, DATAI )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
C
C        Added description of NWI to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     update DAS integer physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASURI' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURI' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     If FIRST or LAST are out of range, no dice.
C
      IF (        ( FIRST .LT. 1   )
     .      .OR.  ( FIRST .GT. NWI )
     .      .OR.  ( LAST  .LT. 1   )
     .      .OR.  ( LAST  .GT. NWI )    )  THEN
 
         CALL DASHLU (  HANDLE, UNIT )
 
         CALL SETMSG ( 'Array indices FIRST and LAST were #,  #; '    //
     .                 'allowed range for both is [#, #]. File was '  //
     .                 '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWI                                       )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASURI' )
         RETURN
 
      END IF
 
C
C     There's nothing to do if LAST < FIRST.
C
      IF ( LAST .LT. FIRST ) THEN
         CALL CHKOUT ( 'DASURI' )
         RETURN
      END IF
 
C
C     See whether integer record number RECNO from file HANDLE is
C     buffered.  We'll search through the list of buffered records
C     starting at the head of the list.  If the record is already
C     buffered, we'll update the buffer entry, but we'll defer writing
C     the record out until we need to free a record, or until the
C     integer buffer is flushed, whichever comes first.
C
      NODE = HEADI
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFI(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFI(NODE)  )    )  THEN
C
C           Found it.  Update the buffered record.
C
            CALL MOVEI ( DATAI,   LAST-FIRST+1,   RCBUFI(FIRST, NODE) )
 
C
C           Set the update flag, indicating that this buffer entry
C           has been modified.
C
            UPBUFI ( NODE ) = .TRUE.
 
C
C           Put the information about this record at the head of the
C           active list, if it is not already there.
C
            IF ( NODE .NE. HEADI ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLI )
               CALL LNKILB ( NODE, HEADI, POOLI )
               HEADI = NODE
 
            END IF
 
            CALL CHKOUT ( 'DASURI' )
            RETURN
 
         END IF
 
         NODE  =  POOLI ( FORWRD, NODE )
 
      END DO
 
C
C     The record we're writing to is not buffered.  We'll allocate
C     a buffer entry.  If the record buffer is full, we'll
C     commandeer the least recently accessed record.  Before using
C     this record, we'll write its contents out to the corresponding
C     file, if the record has been updated.
C
      IF ( USEDI .LT. BUFSZI ) THEN
C
C        There's a free buffer entry available.  Just allocate it.
C
         CALL LNKAN ( POOLI, NODE )
         USEDI = USEDI + 1
 
      ELSE
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  =  LNKTL ( HEADI, POOLI )
 
         CALL LNKXSL ( NODE, NODE, POOLI )
 
C
C        If the allocated record was updated, write it out.
C
         IF (  UPBUFI(NODE)  ) THEN
 
            CALL DASIOI ( 'WRITE',
     .                     LUBUFI(    NODE ),
     .                     RNBUFI(    NODE ),
     .                     RCBUFI( 1, NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASURI' )
               RETURN
            END IF
 
         END IF
 
      END IF
 
C
C     Now try to read the record we're going to update.
C
      CALL DASHLU ( HANDLE,  UNIT )
      CALL DASIOI ( 'READ',  UNIT,  RECNO, RCBUFI(1,NODE) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURI' )
         RETURN
      END IF
 
C
C     The read was successful, so set the record number, handle, unit,
C     and update flag for this buffer entry, and link these buffer
C     entries in before the current head of the list, thus putting
C     them at the head.
C
C     Update the head pointer.
C
      CALL LNKILB ( NODE, HEADI, POOLI )
 
      HNBUFI ( NODE )  =  HANDLE
      RNBUFI ( NODE )  =  RECNO
      LUBUFI ( NODE )  =  UNIT
      UPBUFI ( NODE )  = .TRUE.
      HEADI            =  NODE
 
C
C     At long last, make the requested update.  Note that we don't
C     have to write the record back to the file; that will get done
C     automatically before or at the time the file is closed.
C
      CALL MOVEI (  DATAI,  LAST-FIRST+1,  RCBUFI( FIRST, NODE )  )
 
 
      CALL CHKOUT ( 'DASURI' )
      RETURN
 
 
 
 
 
 
 
C$Procedure DASURC ( DAS, update record, character )
 
      ENTRY DASURC ( HANDLE, RECNO, FIRST, LAST, DATAC )
 
C$ Abstract
C
C     Update DAS character physical records.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               FIRST
C     INTEGER               LAST
C     CHARACTER*(*)         DATAC
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C     RECNO      I   Record number.
C     FIRST,
C     LAST       I   First and last indices of range within record.
C     DATAC      I   Character data to write to record.
C     BUFSZC     P   Number of records in the character record buffer.
C     NWC        P   Number of characters in a single DAS char. record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C     RECNO          is the number of a record in a DAS file.
C
C     FIRST,
C     LAST           are the first and last indices of a range of
C                    elements to be updated in the indicated record.
C                    The record contains NWC characters; these have
C                    indices ranging from 1 to NWC.
C
C     DATAC          is a character string to be written to elements
C                    FIRST through LAST of the specified record.  The
C                    character DATAC(1:1) is placed in record element
C                    FIRST, the character DATAC(2) is placed in record
C                    element FIRST+1, and so on; the character
C                    DATAC(LAST-FIRST+1) is placed in the record element
C                    LAST.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     NWC           is the number of characters in a single DAS record
C                   containing characters.
C
C     BUFSZC        is the number of records in the character record
C                   buffer.
C
C$ Exceptions
C
C     1)  This routine may be used to update only records that have
C         already been written by DASWRC or that already exist in the
C         file designated by HANDLE.  Attempting to update a record
C         that hasn't yet been written will cause the read operation
C         performed by this routine to fail.
C
C         If a read operation attempted by this routine fails for this
C         or any other reason, the error will be diagnosed by routines
C         called by this routine.  The indicated record will not be
C         modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.  Note that the file written to may be different than
C         the file designated by HANDLE if multiple DAS files are open
C         for writing.
C
C     3)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The indicated
C         record will not be modified.
C
C     4)  If FIRST or LAST is not in the range [1, NWC], the error
C         SPICE(INDEXOUTOFRANGE) will be signaled.  The indicated
C         record will not be modified.
C
C     5)  If FIRST > LAST, this routine will return without modifying
C         the indicated record.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Routines outside of SPICELIB will normally have no need to call
C     this routine.
C
C     This routine can be used to update any existing record in a DAS
C     file that is open for writing, or any record that has been
C     `written' by DASWRC, whether or not that record has yet been
C     physically written to the file it belongs to.  Records that have
C     never been written cannot be updated.
C
C     Because the DAS system buffers records that are written, multiple
C     updates of parts of a record can be made without incurring a
C     large number of file reads and writes.
C
C     Any buffered character record can be updated with this routine.
C     In particular, records that have been written to the DAS character
C     record buffer but have not yet been written out to the DAS file
C     they're intended to go to ARE visible to this routine.
C
C     This routine should be used to update only records that contain
C     character data.
C
C$ Examples
C
C     1)  Update the 10th through 100th characters in record number 9
C         in a DAS file designated by HANDLE.
C
C             CHARACTER*(100)       DATAC
C
C                         .
C                         .
C                         .
C
C             DATAC = 'The first 91 characters of this string, '      //
C            .        'including trailing blanks, will be written '   //
C            .        'to the indicated DAS file.'
C
C             CALL DASURC ( HANDLE, 9, 10, 100, DATAC )
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS)
C
C        Added description of NWC to the Parameters and Brief_I/O
C        sections of the header.
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     update DAS character physical records
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASURC' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURC' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
 
C
C     If FIRST or LAST are out of range, no dice.
C
      IF (        ( FIRST .LT. 1   )
     .      .OR.  ( FIRST .GT. NWC )
     .      .OR.  ( LAST  .LT. 1   )
     .      .OR.  ( LAST  .GT. NWC )    )  THEN
 
         CALL DASHLU (  HANDLE, UNIT )
 
         CALL SETMSG ( 'String indices FIRST and LAST were #,  #; '   //
     .                 'allowed range for both is [#, #]. File was '  //
     .                 '#, record number was #.'                       )
         CALL ERRINT ( '#',  FIRST                                     )
         CALL ERRINT ( '#',  LAST                                      )
         CALL ERRINT ( '#',  1                                         )
         CALL ERRINT ( '#',  NWC                                       )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'DASURC' )
         RETURN
 
      END IF
 
C
C     There's nothing to do if LAST < FIRST.
C
      IF ( LAST .LT. FIRST ) THEN
         CALL CHKOUT ( 'DASURC' )
         RETURN
      END IF
 
C
C     See whether character record number RECNO from file HANDLE is
C     buffered.  We'll search through the list of buffered records
C     starting at the head of the list.  If the record is already
C     buffered, we'll update the buffer entry, but we'll defer writing
C     the record out until we need to free a record, or until the
C     character buffer is flushed, whichever comes first.
C
      NODE = HEADC
 
      DO WHILE ( NODE .GT. 0 )
 
         IF (         (  HANDLE .EQ. HNBUFC(NODE)  )
     .         .AND.  (  RECNO  .EQ. RNBUFC(NODE)  )    )  THEN
C
C           Found it.  Update the buffered record.
C
            RCBUFC (NODE) ( FIRST : LAST )  =  DATAC
 
C
C           Set the update flag, indicating that this buffer entry
C           has been modified.
C
            UPBUFC ( NODE ) = .TRUE.
 
C
C           Put the information about this record at the head of the
C           active list, if it is not already there.
C
            IF ( NODE .NE. HEADC ) THEN
 
               CALL LNKXSL ( NODE, NODE,  POOLC )
               CALL LNKILB ( NODE, HEADC, POOLC )
               HEADC = NODE
 
            END IF
 
            CALL CHKOUT ( 'DASURC' )
            RETURN
 
         END IF
 
         NODE  =  POOLC ( FORWRD, NODE )
 
      END DO
 
C
C     The record we're writing to is not buffered.  We'll allocate
C     a buffer entry.  If the record buffer is full, we'll
C     commandeer the least recently accessed record.  Before using
C     this record, we'll write its contents out to the corresponding
C     file, if the record has been updated.
C
      IF ( USEDC .LT. BUFSZC ) THEN
C
C        There's a free buffer entry available.  Just allocate it.
C
         CALL LNKAN ( POOLC, NODE )
         USEDC = USEDC + 1
 
      ELSE
C
C        Grab the buffer entry at the tail end of the list.
C
         NODE  = LNKTL ( HEADC, POOLC )
 
         CALL LNKXSL ( NODE, NODE, POOLC )
 
C
C        If the allocated record was updated, write it out.
C
         IF (  UPBUFC(NODE)  ) THEN
 
            CALL DASIOC ( 'WRITE',
     .                     LUBUFC( NODE ),
     .                     RNBUFC( NODE ),
     .                     RCBUFC( NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASURC' )
               RETURN
            END IF
 
         END IF
 
      END IF
 
C
C     Now try to read the record we're going to update.
C
      CALL DASHLU ( HANDLE,  UNIT )
      CALL DASIOC ( 'READ',  UNIT,  RECNO, RCBUFC(NODE) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASURC' )
         RETURN
      END IF
 
C
C     The read was successful, so set the record number, handle, unit,
C     and update flag for this buffer entry, and link these buffer
C     entries in before the current head of the list, thus putting
C     them at the head.
C
C     Update the head pointer.
C
      CALL LNKILB ( NODE, HEADC, POOLC )
 
      HNBUFC ( NODE )  =  HANDLE
      RNBUFC ( NODE )  =  RECNO
      LUBUFC ( NODE )  =  UNIT
      UPBUFC ( NODE )  = .TRUE.
      HEADC            =  NODE
 
C
C     At long last, make the requested update.  Note that we don't
C     have to write the record back to the file; that will get done
C     automatically before or at the time the file is closed.
C
      RCBUFC ( NODE )  ( FIRST : LAST )   =   DATAC
 
      CALL CHKOUT ( 'DASURC' )
      RETURN
 
 
 
 
 
 
 
C$Procedure DASWBR ( DAS, write buffered records )
 
      ENTRY DASWBR ( HANDLE )
 
C$ Abstract
C
C     Write out all buffered records of a specified file.
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
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAS file.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAS file opened for writing.
C
C$ Detailed_Output
C
C     None.   See $Particulars for a description of the action of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  The indicated
C         file will not be modified.
C
C     2)  If a write operation attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The status of the DAS file written to is uncertain in this
C         case.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine writes buffered records out to the DAS file to which
C     they correspond.  After the records are written, the buffer
C     elements used to store them are deallocated.
C
C     Because the DAS system buffers records that are written as well
C     as those that are read, data supplied to the DASWRx and DASURx
C     routines on input has not necessarily been physically written to
C     the DAS file specified by the caller of those routines, at the
C     time those routines return.  Before closing a DAS file that has
C     been opened for writing, the DAS system must write out to the
C     file any updated records present in the DAS buffers.  The SPICELIB
C     routine DASCLS uses this routine to perform this function.  The
C     SPICELIB routines DASACR and DASRCR, which respectively add
C     comment records to or delete comment records from a DAS file, use
C     this routine to ensure that the DASRWR record buffers don't
C     become out of sync with the file they operate upon.
C
C     In addition, this routine can be used by application programs
C     that create or update DAS files.  The reason for calling this
C     routine directly would be to provide a measure of safety when
C     writing a very large file:  if the file creation or update were
C     interrupted, the amount of work lost due to the loss of buffered,
C     unwritten records could be reduced.
C
C     However, routines outside of SPICELIB will generally not need to
C     call this routine directly.
C
C$ Examples
C
C     1)  Supply a series of double precision records to DASWRD,
C         then force a physical write of those records to the file.
C
C            DO RECNO = 77, 100
C
C               CALL FILLD  ( DBLE(RECNO), NWD,      RECD )
C               CALL DASWRD ( HANDLE,      RECNO,    RECD )
C
C            END DO
C
C            CALL DASWBR ( HANDLE )
C
C
C     2)  This is the same as example (1), except we force a physical
C         write by closing the file.
C
C            DO RECNO = 77, 100
C
C               CALL FILLD  ( DBLE(RECNO), NWD,      RECD )
C               CALL DASWRD ( HANDLE,      RECNO,    RECD )
C
C            END DO
C
C            CALL DASCLS ( HANDLE )
C
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB)
C
C        Removed weird spaces from ENTRY statement.
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     write buffered records to a DAS file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASWBR' )
      END IF
 
C
C     Check that the file is open for writing.  Signal an error if not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASWBR' )
         RETURN
      END IF
 
C
C     If it hasn't been done yet, initialize the pointer list pools.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( BUFSZD, POOLD )
         CALL LNKINI ( BUFSZI, POOLI )
         CALL LNKINI ( BUFSZC, POOLC )
 
         PASS1  = .FALSE.
 
      END IF
 
C
C     For each buffer, find the records belonging to this file, and
C     write them out to the file.
C
C     Double precision records first.
C
      NODE = HEADD
 
      DO WHILE ( NODE .GT. 0 )
 
         IF ( HANDLE .EQ. HNBUFD(NODE) ) THEN
C
C           This record belongs to the file of interest, so write the
C           the record out.
C
            CALL DASIOD ( 'WRITE',
     .                     LUBUFD(    NODE ),
     .                     RNBUFD(    NODE ),
     .                     RCBUFD( 1, NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWBR' )
               RETURN
            END IF
 
C
C           The record is no longer in use; return it to the
C           free list.  But grab the successor first.  Update
C           the head of the list, if the node we're freeing is
C           the head node.  Decrement the number of used d.p.
C           buffer elements.
C
            NEXT = POOLD ( FORWRD, NODE )
 
            IF ( NODE .EQ. HEADD ) THEN
               HEADD = NEXT
            END IF
 
            CALL LNKFSL ( NODE, NODE, POOLD )
 
            NODE   =  NEXT
            USEDD  =  USEDD - 1
 
         ELSE
C
C           Just get the next node.
C
            NODE = POOLD ( FORWRD, NODE )
 
         END IF
 
      END DO
 
 
 
C
C     Next, integer records.
C
      NODE = HEADI
 
      DO WHILE ( NODE .GT. 0 )
 
         IF ( HANDLE .EQ. HNBUFI(NODE) ) THEN
C
C           This record belongs to the file of interest, so write the
C           the record out.
C
            CALL DASIOI ( 'WRITE',
     .                     LUBUFI(    NODE ),
     .                     RNBUFI(    NODE ),
     .                     RCBUFI( 1, NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWBR' )
               RETURN
            END IF
 
C
C           The record is no longer in use; return it to the
C           free list.  But grab the successor first.  Update
C           the head of the list, if the node we're freeing is
C           the head node.  Decrement the number of used integer
C           buffer elements.
C
            NEXT = POOLI ( FORWRD, NODE )
 
            IF ( NODE .EQ. HEADI ) THEN
               HEADI = NEXT
            END IF
 
            CALL LNKFSL ( NODE, NODE, POOLI )
 
            NODE   =  NEXT
            USEDI  =  USEDI - 1
 
         ELSE
C
C           Just get the next node.
C
            NODE = POOLI ( FORWRD, NODE )
 
         END IF
 
      END DO
 
 
 
C
C     And last, character records.
C
      NODE = HEADC
 
      DO WHILE ( NODE .GT. 0 )
 
         IF ( HANDLE .EQ. HNBUFC(NODE) ) THEN
C
C           This record belongs to the file of interest, so write the
C           the record out.
C
            CALL DASIOC ( 'WRITE',
     .                     LUBUFC( NODE ),
     .                     RNBUFC( NODE ),
     .                     RCBUFC( NODE )  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASWBR' )
               RETURN
            END IF
 
C
C           The record is no longer in use; return it to the
C           free list.  But grab the successor first.  Update
C           the head of the list, if the node we're freeing is
C           the head node.  Decrement the number of used character
C           buffer elements.
C
            NEXT = POOLC ( FORWRD, NODE )
 
            IF ( NODE .EQ. HEADC ) THEN
               HEADC = NEXT
            END IF
 
            CALL LNKFSL ( NODE, NODE, POOLC )
 
            NODE   =  NEXT
            USEDC  =  USEDC - 1
 
         ELSE
C
C           Just get the next node.
C
            NODE = POOLC ( FORWRD, NODE )
 
         END IF
 
      END DO
 
      CALL CHKOUT ( 'DASWBR' )
      RETURN
      END
