C$Procedure DAFRWD ( DAF, read, write double precision )
 
      SUBROUTINE DAFRWD ( HANDLE,
     .                    RECNO,
     .                    BEGIN,
     .                    END,
     .                    DREC,
     .                    DATA,
     .                    FOUND,
     .                    READS,
     .                    REQS     )
 
C$ Abstract
C
C     Read, write, and rewrite double precision records to and
C     from DAFs.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               HANDLE
      INTEGER               RECNO
      INTEGER               BEGIN
      INTEGER               END
      DOUBLE PRECISION      DREC     ( 128 )
      DOUBLE PRECISION      DATA     (  *  )
      LOGICAL               FOUND
      INTEGER               READS
      INTEGER               REQS
 
      INTEGER               RBSIZE
      PARAMETER           ( RBSIZE = 100 )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAFGDR. DAFGSR, DAFRDR (Obsolete), DAFWDR
C     RECNO      I   DAFGDR. DAFGSR, DAFRDR (Obsolete), DAFWDR
C     BEGIN      I   DAFGDR. DAFGSR, DAFRDR (Obsolete)
C     END        I   DAFGDR. DAFGSR, DAFRDR (Obsolete)
C     DREC       I   DAFWDR
C     DATA       O   DAFGDR. DAFGSR, DAFRDR (Obsolete)
C     FOUND      O   DAFGDR. DAFGSR, DAFRDR (Obsolete)
C     READS      O   DAFNRR
C     REQS       O   DAFNRR
C     RBSIZE     P   DAFGDR. DAFGSR, DAFRDR (Obsolete), DAFWDR, DAFNRR
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a double precision record
C                 within a DAF to be read or written.
C
C     BEGIN       is the first in word in a double precision record
C                 to be read.
C
C     END         is the last in word in a double precision record
C                 to be read.
C
C     DREC        contains a single double precision record, to be
C                 written to the specified DAF.
C
C$ Detailed_Output
C
C     DATA        contains a portion of a single double precision
C                 record, read from the specified DAF.
C
C     FOUND       is true when the specified record is found, and is
C                 false otherwise.
C
C     READS,
C     REQS        are the number of physical reads and the number
C                 of requests processed by DAFRDR during the current
C                 execution of the calling program.
C
C
C$ Parameters
C
C     RBSIZE      is the size of the record buffer maintained by
C                 DAFRWD. In effect, RBSIZE is the maximum number
C                 of records that can be stored (buffered) at any
C                 one time. Higher values of RBSIZE reduce the
C                 amount of time spent reading from disk at the
C                 cost of increasing the amount of space required
C                 by the calling program. The optimal value of
C                 RBSIZE may differ from environment to environment,
C                 and may even vary from application to application.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If DAFRWD is called directly, the error SPICE(BOGUSENTRY)
C        is signalled.
C
C     2) See entry points DAFGDR, DAFGSR, DAFRDR, DAFWDR, and DAFNRR
C        for exceptions specific to those entry points.
C
C$ Particulars
C
C     DAFRWD serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        DAFGDR         Read double precision record.
C
C        DAFGSR         Read summary/descriptor record.
C
C        DAFRDR         Read double precision record. (Obsolete, use
C                       DAFGDR)
C
C        DAFWDR         Write double precision record.
C
C        DAFNRR         Number of reads, requests.
C
C     DAFGDR, DAFGSR, and DAFWDR are the only approved means for
C     reading and writing double precision records to and from DAFs.
C     DAFRDR continues to function, but only on files of the native
C     binary format.  They keep track of which records have been read
C     most recently, and of which records have been requested most
C     often, in order to minimize the amount of time spent actually
C     reading from external storage.
C
C     DAFNRR may be used at any time during the execution of a
C     program to determine the number of requests that have been
C     processed, and the number of actual read operations needed
C     to fulfill those requests. Ideally, the ratio of reads to
C     requests should approach zero. In the worst case, the ratio
C     approaches one. The ratio is related to the size of the
C     record buffer, which controlled by parameter RBSIZE. The
C     results returned by DAFNRR may be used to determine the
C     optimal value of RBSIZE empirically.
C
C     All data records in a DAF can be treated as an undifferentiated
C     collection of double precision numbers.  Summary records must
C     be read using the DAFGSR interface, but their contents are
C     properly buffered in a single buffer with the data records.
C     No special buffers are required for each new data type, or to
C     keep summary records separate from data records.
C
C$ Examples
C
C     See entry points DAFGDR, DAFGSR, DAFRDR, DAFWDR, and DAFNRR
C     for examples specific to those entry points.
C
C$ Restrictions
C
C     1) An integer overflow may occur if the number of requests
C        by a single program exceeds the maximum number that can
C        be stored in an integer variable.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Added DAFGDR and DAFGSR entry points to allow read access
C        to DAFs utilizing non-native, but supported, binary file
C        formats.
C
C        DAFRDR was phased into obsolescence.
C
C        The umbrella no longer suffers from integer overflow if
C        a sufficient number of successful read requests are made.
C
C        DAFWDR no longer uses DAFHLU to retrieve a logical unit
C        for HANDLE.  This call has been replaced with the handle
C        manager interface, which does not lock handles to their
C        logical units.
C
C-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
C
C        The loop in DAFRDR that moved buffered d.p.s into the output
C        array DATA was modified to use the routine MOVED.
C
C-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
C
C        Unnecessary CHKIN and CHKOUT calls were removed from entry
C        point DAFRDR.
C
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        1) In DAFRDR, the found flag is now set to false if the
C           call to DAFHLU fails.
C
C        2) In the example code fragment in DAFRDR and DAFWDR, the
C           calling sequence to MOVED was corrected.
C
C        3) In DAFRDR a variable name was changed.
C
C        4) In DAFNRR a cut and paste error in the header was fixed.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read write d.p. daf
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this umbrella and its entry points in preparation
C        for DAF's utilization of the handle manager.  DAFRDR is
C        obsolete, and will now signal errors when used to read
C        records from DAFs using non-native, binary file formats.
C
C        Two new entry points were added: DAFGDR and DAFGDR.  These
C        are the translation-aware 'get data record' and 'get
C        summary record' routines that all new software developed
C        should utilize.
C
C-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
C
C        The loop in DAFRDR that moved buffered d.p.s into the output
C        array DATA was modified to use the routine MOVED.
C
C-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
C
C        Unnecessary CHKIN and CHKOUT calls were removed from entry
C        point DAFRDR.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               RDSIZE
      PARAMETER           ( RDSIZE = 128 * RBSIZE )
 
      INTEGER               UNIT
      INTEGER               IOSTAT
 
      INTEGER               BUFLOC
      LOGICAL               DONE
      LOGICAL               STORED
 
      INTEGER               B
      INTEGER               COUNT
      INTEGER               E
      INTEGER               RBHAN    (      RBSIZE )
      INTEGER               RBREC    (      RBSIZE )
      INTEGER               RBREQ    (      RBSIZE )
      DOUBLE PRECISION      RBDAT    ( 128, RBSIZE )
      INTEGER               RBNBR
      INTEGER               NREAD
      INTEGER               NREQ
      INTEGER               MINVAL
 
      INTEGER               ND
      INTEGER               NI
 
      LOGICAL               LOCFND
      LOGICAL               NATIVE
 
C
C     Saved variables
C
      SAVE                  RBHAN
      SAVE                  RBREC
      SAVE                  RBREQ
      SAVE                  RBDAT
      SAVE                  RBNBR
 
      SAVE                  NREAD
      SAVE                  NREQ
 
C
C     Initial values
C
      DATA                  RBHAN   / RBSIZE * 0    /
      DATA                  RBREC   / RBSIZE * 0    /
      DATA                  RBREQ   / RBSIZE * 0    /
      DATA                  RBDAT   / RDSIZE * 0.D0 /
      DATA                  RBNBR   /          1    /
 
      DATA                  NREAD   /          0    /
      DATA                  NREQ    /          0    /
 
 
C
C     As double precision records are processed, they are stored in a
C     record buffer. (File and character records are not buffered.)
C     The user controls the number of records that may be stored at
C     any one time by setting the value of the paramater RBSIZE before
C     compiling the routine.
C
C     The record buffer contains one entry for each record that has
C     been read.
C
C        +----------+----------+----------+----------+
C        | File       Record     Request    Contents |
C        | Handle     Number     Number              |
C        +----------+----------+----------+----------+
C        | INT        INT        INT        DP(128)  |
C        +----------+----------+----------+----------+
C
C     The request number is a counter that is incremented every time
C     a record is requested. When all the slots in the record buffer are
C     full, the least recently requested record (the one with the lowest
C     request number) is replaced by the new record.
C
C     In addition, a separate counter is used to keep track of the
C     number of actual file reads performed. It is possible to tune
C     the entire package by checking the read/request ratio for
C     any specific buffer configuration.
C
C     Note also that whenever a write operation fails, the affected
C     buffers (if any) should NOT be updated.
C
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DAFRWD' )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'DAFRWD' )
      END IF
 
      RETURN
 
 
 
 
C$Procedure DAFGDR ( DAF, get double precision record )
 
      ENTRY DAFGDR ( HANDLE, RECNO, BEGIN, END, DATA, FOUND )
 
C$ Abstract
C
C     Read a portion of the contents of a double precision record in a
C     DAF file.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               BEGIN
C     INTEGER               END
C     DOUBLE PRECISION      DATA    ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     RECNO      I   Record number.
C     BEGIN      I   First word to read from record.
C     END        I   Last word to read from record.
C     DATA       O   Contents of record.
C     FOUND      O   True if record is found.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a particular double precision
C                 record within the DAF, whose contents are to be read.
C
C     BEGIN       is the first word in the specified record to be
C                 returned.
C
C     END         is the final word in the specified record to be
C                 returned.
C
C$ Detailed_Output
C
C     DATA        contains the specified portion (from BEGIN to END,
C                 inclusize) of the specified record from the specified
C                 file, specifically.
C
C     FOUND       is true when the specified record is found, and is
C                 false otherwise.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     DAFGDR checks the record buffer to see if the requested
C     record can be returned without actually reading it from
C     external storage. If not, it reads the record and stores
C     it in the buffer, typically removing another record from
C     the buffer as a result.
C
C     Once in the buffer, the specified portion of the record is
C     returned, using the following control loop.
C
C        J = 1
C        DO I = MAX( 1, BEGIN ), MIN( 128, END )
C           DATA( J ) = Buffered record ( I )
C           J = J + 1
C        END DO
C
C     Therefore bad values for BEGIN and END (BEGIN < 1, END < BEGIN,
C     etc.) are not signaled as errors, but result in the actions
C     implied by the above.
C
C$ Examples
C
C     The following code fragment illustrates one way that DAFGDR
C     and DAFWDR can be used to update part of a double precision
C     record. If the record does not yet exist, we can assume that
C     it is filled with zeros.
C
C        CALL DAFGDR ( HANDLE, RECNO, 1, 128, DREC, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C           CALL MOVED ( 0.D0, 128, DREC )
C        END IF
C
C        DO I = FIRST, LAST
C           DREC(I) = NEW_VALUE(I)
C        END DO
C
C        CALL DAFWDR ( HANDLE, RECNO, DREC )
C
C     Note that since only entire records may be written using DAFWDR,
C     the entire record needs to be read also.
C
C$ Restrictions
C
C     1) Bad values for BEGIN and END ( BEGIN < 1, END > 128,
C        END < BEGIN ) are not signalled as errors. The effects of
C        such assignments on the returned data are defined by the
C        following control structure:
C
C           J = 1
C           DO I = MAX( 1, BEGIN ), MIN( 128, END )
C              DATA( J ) = Buffered record ( I )
C              J = J + 1
C           END DO
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C-&
 
C$ Index_Entries
C
C     read daf d.p. record
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Assume that the record will be found until proven otherwise.
C
      FOUND  = .TRUE.
 
C
C     First, find the record.
C
C     If the specified handle and record number match those of
C     a buffered record, determine the location of that record
C     within the buffer.
C
      BUFLOC = 0
      DONE   = .FALSE.
      STORED = .FALSE.
 
      DO WHILE ( .NOT. DONE )
         BUFLOC = BUFLOC + 1
 
         STORED = (       HANDLE .EQ. RBHAN(BUFLOC)
     .              .AND. RECNO  .EQ. RBREC(BUFLOC) )
 
         DONE   = (       STORED
     .              .OR.  BUFLOC .EQ. RBNBR )
      END DO
 
C
C     If not, determine the location of the least recently requested
C     record (the one with the smallest request number). Get the unit
C     number for the file, and read the record into this location.
C
C     If an error occurs while reading the record, clear the entire
C     buffer entry in case the entry was corrupted by a partial read.
C     Otherwise, increment the number of reads performed so far.
C
      IF ( .NOT. STORED ) THEN
 
         CALL MINAI ( RBREQ, RBNBR, MINVAL, BUFLOC )
 
         CALL ZZDAFGDR ( HANDLE, RECNO, RBDAT(1,BUFLOC), LOCFND )
 
C
C        If the call to ZZDAFGDR failed, or the record was not found,
C        then clean up.
C
         IF ( ( FAILED() ) .OR. ( .NOT. LOCFND ) ) THEN
            FOUND         = .FALSE.
            RBHAN(BUFLOC) = 0
            RBREC(BUFLOC) = 0
            RBREQ(BUFLOC) = 0
         ELSE
            NREAD         = NREAD + 1
            RBHAN(BUFLOC) = HANDLE
            RBREC(BUFLOC) = RECNO
 
            IF ( RBNBR .LT. RBSIZE ) THEN
               RBNBR = RBNBR + 1
            END IF
 
         END IF
      END IF
 
C
C     Whether previously stored or just read, the record is now in
C     the buffer. Return the specified portion directly, and increment
C     the corresponding request number.
C
      IF ( FOUND ) THEN
 
         B     = MAX(1,BEGIN)
         E     = MIN(128,END)
         COUNT = E-B+1
 
         CALL MOVED ( RBDAT(B,BUFLOC), COUNT, DATA )
 
C
C        Increment the request counter in such a way that integer
C        overflow will not occur.  This private module from the
C        handle manager halves RBREQ if adding 1 to NREQ would
C        cause its value to exceed INTMAX.
C
         CALL ZZDDHRCM ( RBNBR, RBREQ, NREQ )
         RBREQ(BUFLOC) = NREQ
 
      END IF
 
      RETURN
 
 
 
C$Procedure DAFGSR ( DAF, get summary/descriptor record )
 
      ENTRY DAFGSR ( HANDLE, RECNO, BEGIN, END, DATA, FOUND )
 
C$ Abstract
C
C     Read a portion of the contents of a summary record in a
C     DAF file.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               BEGIN
C     INTEGER               END
C     DOUBLE PRECISION      DATA    ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     RECNO      I   Record number.
C     BEGIN      I   First word to read from record.
C     END        I   Last word to read from record.
C     DATA       O   Contents of record.
C     FOUND      O   True if record is found.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a particular double precision
C                 record within the DAF, whose contents are to be read.
C
C     BEGIN       is the first word in the specified record to be
C                 returned.
C
C     END         is the final word in the specified record to be
C                 returned.
C
C$ Detailed_Output
C
C     DATA        contains the specified portion (from BEGIN to END,
C                 inclusize) of the specified record from the specified
C                 file, specifically.
C
C     FOUND       is true when the specified record is found, and is
C                 false otherwise.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     DAFGSR checks the record buffer to see if the requested
C     record can be returned without actually reading it from
C     external storage. If not, it reads the record and stores
C     it in the buffer, typically removing another record from
C     the buffer as a result.
C
C     Once in the buffer, the specified portion of the record is
C     returned, using the following control loop.
C
C        J = 1
C        DO I = MAX( 1, BEGIN ), MIN( 128, END )
C           DATA( J ) = Buffered record ( I )
C           J = J + 1
C        END DO
C
C     Therefore bad values for BEGIN and END (BEGIN < 1, END < BEGIN,
C     etc.) are not signalled as errors, but result in the actions
C     implied by the above.
C
C$ Examples
C
C     The following code fragment illustrates one way that DAFGSR
C     and DAFWDR can be used to update part of a summary record.
C     If the record does not yet exist, we can assume that it is
C     filled with zeros.
C
C        CALL DAFGSR ( HANDLE, RECNO, 1, 128, DREC, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C           CALL MOVED ( 0.D0, 128, DREC )
C        END IF
C
C        DO I = FIRST, LAST
C           DREC(I) = NEW_VALUE(I)
C        END DO
C
C        CALL DAFWDR ( HANDLE, RECNO, DREC )
C
C     Note that since only entire records may be written using DAFWDR,
C     the entire record needs to be read also.
C
C$ Restrictions
C
C     1) Bad values for BEGIN and END ( BEGIN < 1, END > 128,
C        END < BEGIN ) are not signalled as errors. The effects of
C        such assignments on the returned data are defined by the
C        following control structure:
C
C           J = 1
C           DO I = MAX( 1, BEGIN ), MIN( 128, END )
C              DATA( J ) = Buffered record ( I )
C              J = J + 1
C           END DO
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C-&
 
C$ Index_Entries
C
C     read daf summary record
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Assume that the record will be found until proven otherwise.
C
      FOUND  = .TRUE.
 
C
C     First, find the record.
C
C     If the specified handle and record number match those of
C     a buffered record, determine the location of that record
C     within the buffer.
C
      BUFLOC = 0
      DONE   = .FALSE.
      STORED = .FALSE.
 
      DO WHILE ( .NOT. DONE )
         BUFLOC = BUFLOC + 1
 
         STORED = (       HANDLE .EQ. RBHAN(BUFLOC)
     .              .AND. RECNO  .EQ. RBREC(BUFLOC) )
 
         DONE   = (       STORED
     .              .OR.  BUFLOC .EQ. RBNBR )
      END DO
 
C
C     If not, determine the location of the least recently requested
C     record (the one with the smallest request number). Get the unit
C     number for the file, and read the record into this location.
C
C     If an error occurs while reading the record, clear the entire
C     buffer entry in case the entry was corrupted by a partial read.
C     Otherwise, increment the number of reads performed so far.
C
      IF ( .NOT. STORED ) THEN
 
         CALL MINAI ( RBREQ, RBNBR, MINVAL, BUFLOC )
 
         CALL DAFHSF ( HANDLE, ND, NI )
 
         CALL ZZDAFGSR ( HANDLE,          RECNO, ND, NI,
     .                   RBDAT(1,BUFLOC), LOCFND         )
 
C
C        If the call to ZZDAFGSR or DAFHSF failed, or the record
C        was not found, then clean up.
C
         IF ( ( FAILED() ) .OR. ( .NOT. LOCFND ) ) THEN
            FOUND         = .FALSE.
            RBHAN(BUFLOC) = 0
            RBREC(BUFLOC) = 0
            RBREQ(BUFLOC) = 0
         ELSE
            NREAD         = NREAD + 1
            RBHAN(BUFLOC) = HANDLE
            RBREC(BUFLOC) = RECNO
 
            IF ( RBNBR .LT. RBSIZE ) THEN
               RBNBR = RBNBR + 1
            END IF
 
         END IF
      END IF
 
C
C     Whether previously stored or just read, the record is now in
C     the buffer. Return the specified portion directly, and increment
C     the corresponding request number.
C
      IF ( FOUND ) THEN
 
         B     = MAX(1,BEGIN)
         E     = MIN(128,END)
         COUNT = E-B+1
 
         CALL MOVED ( RBDAT(B,BUFLOC), COUNT, DATA )
 
C
C        Increment the request counter in such a way that integer
C        overflow will not occur.  This private module from the
C        handle manager halves RBREQ if adding 1 to NREQ would
C        cause its value to exceed INTMAX.
C
         CALL ZZDDHRCM ( RBNBR, RBREQ, NREQ )
         RBREQ(BUFLOC) = NREQ
 
      END IF
 
      RETURN
 
 
 
 
C$Procedure DAFRDR ( DAF, read double precision record )
 
      ENTRY DAFRDR ( HANDLE, RECNO, BEGIN, END, DATA, FOUND )
 
C$ Abstract
C
C     Read a portion of the contents of a double precision record in a
C     DAF file.
C     Obsolete: This routine has been superceded by DAFGDR, and it is
C     supported for purposes of backwards compatibility only.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     INTEGER               BEGIN
C     INTEGER               END
C     DOUBLE PRECISION      DATA    ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     RECNO      I   Record number.
C     BEGIN      I   First word to read from record.
C     END        I   Last word to read from record.
C     DATA       O   Contents of record.
C     FOUND      O   True if record is found.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a particular double precision
C                 record within the DAF, whose contents are to be read.
C
C     BEGIN       is the first word in the specified record to be
C                 returned.
C
C     END         is the final word in the specified record to be
C                 returned.
C
C$ Detailed_Output
C
C     DATA        contains the specified portion (from BEGIN to END,
C                 inclusize) of the specified record from the specified
C                 file, specifically.
C
C     FOUND       is true when the specified record is found, and is
C                 false otherwise.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the file associated with HANDLE is not of the native
C        binary file format, this routine signals the error
C        SPICE(UNSUPPORTEDBFF).
C
C$ Particulars
C
C     DAFRDR checks the record buffer to see if the requested
C     record can be returned without actually reading it from
C     external storage. If not, it reads the record and stores
C     it in the buffer, typically removing another record from
C     the buffer as a result.
C
C     Once in the buffer, the specified portion of the record is
C     returned, using the following control loop.
C
C        J = 1
C        DO I = MAX( 1, BEGIN ), MIN( 128, END )
C           DATA( J ) = Buffered record ( I )
C           J = J + 1
C        END DO
C
C     Therefore bad values for BEGIN and END (BEGIN < 1, END < BEGIN,
C     etc.) are not signalled as errors, but result in the actions
C     implied by the above.
C
C     This routine has been made obsolete by the routine DAFGDR,
C     and it is supported for reasons of backwards compatibility
C     only.  New software development should utilize DAFGDA.
C
C$ Examples
C
C     The following code fragment illustrates one way that DAFRDR
C     and DAFWDR can be used to update part of a double precision
C     record. If the record does not yet exist, we can assume that
C     it is filled with zeros.
C
C        CALL DAFRDR ( HANDLE, RECNO, 1, 128, DREC, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C           CALL MOVED ( 0.D0, 128, DREC )
C        END IF
C
C        DO I = FIRST, LAST
C           DREC(I) = NEW_VALUE(I)
C        END DO
C
C        CALL DAFWDR ( HANDLE, RECNO, DREC )
C
C     Note that since only entire records may be written using DAFWDR,
C     the entire record needs to be read also.
C
C$ Restrictions
C
C     1) An integer overflow may occur if the number of requests
C        by a single program exceeds the maximum number that can
C        be stored in an integer variable.
C
C     2) Bad values for BEGIN and END ( BEGIN < 1, END > 128,
C        END < BEGIN ) are not signalled as errors. The effects of
C        such assignments on the returned data are defined by the
C        following control structure:
C
C           J = 1
C           DO I = MAX( 1, BEGIN ), MIN( 128, END )
C              DATA( J ) = Buffered record ( I )
C              J = J + 1
C           END DO
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Added SPICE(UNSUPPORTEDBFF) exception to the routine.
C
C-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
C
C        The loop in DAFRDR that moved buffered d.p.s into the output
C        array DATA was modified to use the routine MOVED.
C
C-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
C
C        Unnecessary CHKIN and CHKOUT calls were removed from entry
C        point DAFRDR.
C
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        1) In DAFRDR, the found flag is now set to false if the
C           call to DAFHLU fails.
C
C        2) In the example code fragment in DAFRDR and DAFWDR, the
C           calling sequence to MOVED was corrected.
C
C        3) In the call to MINAI the argument for the minimum value
C           was changed from I to MINVAL.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read daf d.p. record
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        The exception SPICE(UNSUPPORTEDBFF) was added to guarantee
C        this routine's functionality remains unchanged as a result
C        of the updates to the underlying DAF software's utilization of
C        the handle manager.  In versions of the toolkit prior to this,
C        all DAFs loaded were of the native binary file format.
C        Previously, this routine was used to read the contents of
C        summary records in addition to the usual data records.
C        The non-native to native translation process for these two
C        different types of records in general are not the same.
C        Rather than attempt to interpret the caller's intent, this
C        routine is obsolete and restricted to functioning only on
C        DAFs of the native binary file format.
C
C-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
C
C        The loop in DAFRDR that moved buffered d.p.s into the output
C        array DATA was modified to use the routine MOVED.
C
C-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
C
C        Unnecessary CHKIN and CHKOUT calls were removed from entry
C        point DAFRDR.  These calls were placed together prior to
C        a RETURN statement.  It's unclear why they were there in the
C        first place.
C
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        1) In DAFRDR, the found flag is now set to false if the
C           call to DAFHLU fails.
C
C        2) In the example code fragment in DAFRDR and DAFWDR, the
C           calling sequence to MOVED was corrected.
C
C        3) In the call to MINAI the argument for the minimum value
C           was changed from I to MINVAL.
C
C-     Beta Version 2.0.0, 1-NOV-1989 (RET)
C
C        The function of DAFRDR was changed so that it returns only
C        a specified portion of the record. The calling sequence there-
C        fore changed from
C
C           DAFRDR ( HANDLE, RECNO, DREC, FOUND ) to
C           DAFRDR ( HANDLE, RECNO, BEGIN, END, DATA, FOUND )
C
C        The change was made to cut down on the shuffling of unneeded
C        data.
C
C        Also, DAFRDR now only checks in and checks out if DAFHLU has
C        failed (the only routine called by DAFRDR that could possibly
C        signal an error). The purpose of this change was to help
C        speed up a routine that gets called constantly by higher level
C        DAF routines.
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Assume that the record will be found until proven otherwise.
C
      FOUND  = .TRUE.
 
C
C     First check to see if HANDLE is associated with a DAF of the
C     native binary file format.
C
      CALL ZZDDHISN ( HANDLE, NATIVE, LOCFND )
 
      IF ( ( LOCFND ) .AND. ( .NOT. NATIVE ) ) THEN
 
         FOUND = .FALSE.
 
         CALL CHKIN  ( 'DAFRDR'                                 )
         CALL SETMSG ( 'The binary file format for file ''#'' '
     .   //            'is not native. This routine operates '
     .   //            'only on files of the native format.'    )
         CALL ERRHAN ( '#', HANDLE                              )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'                  )
         CALL CHKOUT ( 'DAFRDR'                                 )
         RETURN
      END IF
 
C
C     Now, find the record.
C
C     If the specified handle and record number match those of
C     a buffered record, determine the location of that record
C     within the buffer.
C
      BUFLOC = 0
      DONE   = .FALSE.
      STORED = .FALSE.
 
      DO WHILE ( .NOT. DONE )
         BUFLOC = BUFLOC + 1
 
         STORED = (       HANDLE .EQ. RBHAN(BUFLOC)
     .              .AND. RECNO  .EQ. RBREC(BUFLOC) )
 
         DONE   = (       STORED
     .              .OR.  BUFLOC .EQ. RBNBR )
      END DO
 
C
C     If not, determine the location of the least recently requested
C     record (the one with the smallest request number). Get the unit
C     number for the file, and read the record into this location.
C
C     If an error occurs while reading the record, clear the entire
C     buffer entry in case the entry was corrupted by a partial read.
C     Otherwise, increment the number of reads performed so far.
C
      IF ( .NOT. STORED ) THEN
 
         CALL MINAI ( RBREQ, RBNBR, MINVAL, BUFLOC )
 
         CALL ZZDAFGDR ( HANDLE, RECNO, RBDAT(1, BUFLOC), LOCFND )
 
C
C        If the call to ZZDAFGDR failed, or the record was not found,
C        then clean up.
C
         IF ( ( FAILED() ) .OR. ( .NOT. LOCFND ) ) THEN
            FOUND         = .FALSE.
            RBHAN(BUFLOC) = 0
            RBREC(BUFLOC) = 0
            RBREQ(BUFLOC) = 0
         ELSE
            NREAD         = NREAD + 1
            RBHAN(BUFLOC) = HANDLE
            RBREC(BUFLOC) = RECNO
 
            IF ( RBNBR .LT. RBSIZE ) THEN
               RBNBR = RBNBR + 1
            END IF
 
         END IF
      END IF
 
C
C     Whether previously stored or just read, the record is now in
C     the buffer. Return the specified portion directly, and increment
C     the corresponding request number.
C
      IF ( FOUND ) THEN
 
         B     = MAX(1,BEGIN)
         E     = MIN(128,END)
         COUNT = E-B+1
 
         CALL MOVED ( RBDAT(B,BUFLOC), COUNT, DATA )
 
C
C        Increment the request counter in such a way that integer
C        overflow will not occur.  This private module from the
C        handle manager halves RBREQ if adding 1 to NREQ would
C        cause its value to exceed INTMAX.
C
         CALL ZZDDHRCM ( RBNBR, RBREQ, NREQ )
         RBREQ(BUFLOC) = NREQ
 
      END IF
 
      RETURN
 
 
 
 
C$Procedure DAFWDR ( DAF, write double precision record )
 
      ENTRY DAFWDR ( HANDLE, RECNO, DREC )
 
C$ Abstract
C
C     Write or rewrite the contents of a double precision record in
C     a DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               RECNO
C     DOUBLE PRECISION      DREC     ( 128 )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     RECNO      I   Record number.
C     DREC       I   Contents of record.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a DAF.
C
C     RECNO       is the record number of a particular double
C                 precision record within the file, whose
C                 contents are to be written (if the record does
C                 not yet exist) or overwritten (if it does).
C
C     DREC        contains the new contents of the record.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the file is not open for write access, the error
C        SPICE(DAFILLEGWRITE) is signalled.
C
C     2) If (for some reason) the record cannot be written the
C        error SPICE(DAFDPWRITEFAIL) is signalled.
C
C$ Particulars
C
C     Like DAFRDR, DAFWDR checks the record buffer to see if the
C     requested record is in the buffer. If so, the buffer is
C     updated along with the file. This prevents the buffer from
C     becoming outdated.
C
C$ Examples
C
C     The following code fragment illustrates one way that DAFRDR
C     and DAFWDR can be used to update part of a double precision
C     record. If the record does not yet exist, we can assume that
C     it is filled with zeros.
C
C        CALL DAFRDR ( HANDLE, RECNO, DREC, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C           CALL MOVED ( 0.D0, 128, DREC )
C        END IF
C
C        DO I = FIRST, LAST
C           DREC(I) = NEW_VALUE(I)
C        END DO
C
C        CALL DAFWDR ( HANDLE, RECNO, DREC )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Replaced the call to DAFHLU to ZZDDHHLU.  This prevents
C        DAFWDR from tying up resources in the handle manager.
C
C-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
C
C        The loop in DAFRDR that moved buffered d.p.s into the output
C        array DATA was modified to use the routine MOVED.
C
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        In the example code fragment in DAFRDR and DAFWDR, the
C        calling sequence to MOVED was corrected.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     write daf d.p. record
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFWDR' )
      END IF
 
C
C     No fair writing to a read-only file!
C
      IF ( HANDLE .GE. 0 ) THEN
         CALL SETMSG ( 'Attempt was made to write to a read-only file.')
         CALL SIGERR ( 'SPICE(DAFILLEGWRITE)' )
 
         CALL CHKOUT ( 'DAFWDR' )
         RETURN
      END IF
 
C
C     If the specified handle and record number match those of
C     a buffered record, determine the location of that record
C     within the buffer.
C
      BUFLOC = 0
      DONE   = .FALSE.
      STORED = .FALSE.
 
      DO WHILE ( .NOT. DONE )
         BUFLOC = BUFLOC + 1
 
         STORED = (       HANDLE .EQ. RBHAN(BUFLOC)
     .              .AND. RECNO  .EQ. RBREC(BUFLOC) )
 
         DONE   = (       STORED
     .              .OR.  BUFLOC .EQ. RBSIZE )
      END DO
 
C
C     Get the unit number for the file, and write the record.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., UNIT )
 
      WRITE (UNIT,
     .       REC=RECNO,
     .       IOSTAT=IOSTAT) DREC
 
C
C     If the record was buffered, replace it---with the input
C     record if the write was successful, or with zeros if it
C     was not.
C
      IF ( STORED ) THEN
         IF ( IOSTAT .EQ. 0 ) THEN
            CALL MOVED  ( DREC, 128, RBDAT(1,BUFLOC) )
         ELSE
            RBHAN(BUFLOC) = 0
            RBREC(BUFLOC) = 0
            RBREQ(BUFLOC) = 0
         END IF
      END IF
 
C
C     Declare an error if the write failed.
C
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Double precision write failed. Value of '     //
     .                 'IOSTAT was #' )
         CALL ERRINT ( '#', IOSTAT )
         CALL SIGERR ( 'SPICE(DAFDPWRITEFAIL)' )
      END IF
 
      CALL CHKOUT ( 'DAFWDR' )
      RETURN
 
 
 
C$Procedure DAFNRR ( DAF number of reads, requests )
 
      ENTRY DAFNRR ( READS, REQS )
 
C$ Abstract
C
C     Return the number of reads and requests fielded by DAFRDR.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               READS
C     INTEGER               REQS
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     READS,
C     REQS       O   Reads, requests in this execution.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     READS,
C     REQS        are the number of physical reads and the number
C                 of requests processed by DAFRDR during the current
C                 execution of the calling program.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     The ratio of reads to requests tells you something about
C     the effectiveness with which the record buffer is preventing
C     unwanted disk access. In the ideal case, most of the records
C     needed by the calling program can be returned directly from
C     the buffer, and the ratio of reads to requests approaches zero.
C     More realistically, it should be be somewhere between 1/10
C     and 1/2.
C
C     If the ratio is greater than 1/2, you should consider increasing
C     the size of the record buffer (which is controlled by parameter
C     RBSIZE) in order to improve the performance of the DAF package,
C     unless your application is strapped for space.
C
C$ Examples
C
C     In the following code fragment, the ratio of reads to requests
C     is determined following a series of calls to the reader DAFEZ.
C
C        DO I = 1, N
C           CALL DAFEZ ( ..., STATES(1,I), ... )
C        END DO
C
C        CALL DAFNRR ( READS, REQS )
C
C        WRITE (*,*) 'Reads/requests = ', FLOAT( READS ) / FLOAT( REQS )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
C
C        The loop in DAFRDR that moved buffered d.p.s into the output
C        array DATA was modified to use the routine MOVED.
C
C-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
C
C        A cut and paste error in the literature references
C        section of the header was fixed.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     number of daf read requests
C
C-&
 
 
      READS = NREAD
      REQS  = NREQ
 
      RETURN
      END
