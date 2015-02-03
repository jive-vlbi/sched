C$Procedure      DASRCR ( DAS, remove comment records )
 
      SUBROUTINE DASRCR ( HANDLE, N )
 
C$ Abstract
C
C     Decrease the size of the comment area in a DAS file to reclaim
C     space freed by the removal of a specified number of comment
C     records.
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
      INTEGER               N
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   A DAS file handle.
C     N          I   Number of comment records to remove.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an existing DAS file opened for
C                    comment area modification by DASOPC.
C
C     N              is the number of records to remove from the end of
C                    the comment area.  of the specified file.  If NCOMR
C                    is the number of comment records present in the
C                    file on input, then on output the number of comment
C                    records will be MAX ( 0,  NCOMR - N ).
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input handle is invalid, the error will be diagnosed by
C         routines called by this routine.
C
C     2)  If an I/O error occurs during the removal process, the error
C         will be diagnosed by routines called by this routine.  The
C         DAS file will probably be corrupted in this case.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine is used to reclaim freed space in the comment area
C     of a DAS file subsequent to removal of comments from the file.
C     Any existing directory records and data records will be shifted
C     up by N records.
C
C     This routine updates the file record of the specified DAS file
C     to reflect the addition of records to the file's comment area.
C     Also, the file summary obtainable from DASHFS will be updated to
C     reflect the addition of comment records.
C
C     The disk space occupied by the specified DAS file will not
C     decrease as a result of calling this routine, but the number of
C     records occupied by meaningful data will decrease.  The useful
C     records in the file can be copied by DAS routines to create a
C     new, smaller file which contains only the meaningful data.
C
C     This routine may be used only on existing DAS files opened by
C     DASOPC.
C
C     The association of DAS logical addresses and data within the
C     specified file will remain unaffected by use of this routine.
C
C     Normally, SPICELIB applications will not call this routine
C     directly, but will remove comments by calling DASRC.
C
C     This routine has an inverse DASACR, which appends a specified
C     number of records to the end of the comment area.
C
C$ Examples
C
C
C            C
C            C     Open an existing DAS file for modification of
C            C     the comment area.  We'll presume that the file
C            C     contains 20 comment records.
C            C
C                  CALL DASOPC ( DAS, HANDLE )
C
C            C
C            C     Remove the last 10 comment records from the file.
C            C
C                  CALL DASRCR ( HANDLE, 10  )
C
C            C
C            C     Close the file.
C            C
C                  CALL DASCLS ( HANDLE )
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
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     remove comment records from a DAS file
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
 
C
C     Local parameters
C
 
C
C     Words per data record, for each data type:
C
      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )
 
      INTEGER               NWD
      PARAMETER           ( NWD    =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )
 
C
C     Data type parameters
C
      INTEGER               CHAR
      PARAMETER           ( CHAR   =   1  )
 
      INTEGER               DP
      PARAMETER           ( DP     =   2  )
 
      INTEGER               INT
      PARAMETER           ( INT    =   3  )
 
C
C     Directory pointer locations (backward and forward):
C
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC =   1  )
 
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC =   2  )
 
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
      CHARACTER*(NWC)       RECC
 
      DOUBLE PRECISION      RECD   ( NWD )
 
      INTEGER               BASE
      INTEGER               DIRREC ( NWI )
      INTEGER               FREE
      INTEGER               I
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LOC
      INTEGER               LREC
      INTEGER               LINDEX
      INTEGER               LTYPE
      INTEGER               LWORD
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NSHIFT
      INTEGER               POS
      INTEGER               RECI   ( NWI    )
      INTEGER               TYPE
      INTEGER               UNIT
 
 
C
C     Saved variables
C
 
C
C     NEXT and PREV map the DAS data type codes to their
C     successors and predecessors, respectively.
C
      INTEGER               NEXT   ( 3 )
      SAVE                  NEXT
 
      INTEGER               PREV   ( 3 )
      SAVE                  PREV
 
C
C     Initial values
C
      DATA                  NEXT   /  2,   3,   1  /
      DATA                  PREV   /  3,   1,   2  /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASRCR' )
      END IF
 
C
C     Make sure this DAS file is open for writing.  Signal an error if
C     not.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
C
C     Get the logical unit for this DAS file.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRCR' )
         RETURN
      END IF
 
C
C     It's a mistake to use a negative value of N.
C
      IF ( N .LT. 0 ) THEN
 
         CALL SETMSG ( 'Number of comment records to remove must be ' //
     .                 'non-negative.  Actual number requested was #.' )
         CALL ERRINT ( '#',  N                                         )
         CALL SIGERR ( 'SPICE(DASINVALIDCOUNT)'                        )
         CALL CHKOUT ( 'DASRCR'                                        )
         RETURN
 
      END IF
 
C
C     Before doing anything to the file, make sure that the DASRWR
C     data buffers do not contain any updated records for this file.
C     All of the record numbers that pertain to this file and remain
C     in the DASRWR buffers will be invalidated after this routine
C     returns.
C
C     DASWBR flushes buffered records to the file.
C
      CALL DASWBR ( HANDLE )
 
C
C     Grab the file summary for this DAS file.  Find the number of
C     reserved records and the number of the first free record.
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
C     Determine the size of the record shift we'll actually perform.
C
      NSHIFT  =  MIN ( N, NCOMR )
 
C
C     Find the record and word positions LREC and LWORD of the last
C     descriptor in the file, and also find the type of the descriptor
C     LTYPE.
C
      CALL MAXAI  ( LASTRC,  3,  LREC,  LOC )
      LWORD  =  0
 
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
C     no directories in the file yet.   However, even DAS files that
C     don't contain any data have their first directory records
C     zeroed out, and this should remain true after the removal of
C     the comment records.
C
      IF ( LREC .EQ. 0 ) THEN
C
C        Just write the zero-filled record to record number
C
C           NRESVR + NCOMR + 2 - NSHIFT
C
         CALL CLEARI (  NWI,                                 DIRREC )
         CALL DASIOI ( 'WRITE', UNIT, NRESVR+NCOMR+2-NSHIFT, DIRREC )
 
      ELSE
C
C        There really is stuff to move.  For each directory record,
C        move the record and then all of the records described by that
C        record.  We start at the beginning of the data area and move
C        downwards in the file as we go.
C
         NREC  =  NRESVR + NCOMR + 2
 
         DO WHILE ( ( NREC .LE. LREC ) .AND. ( NREC .NE. 0 ) )
C
C           Read the current directory record and move it.
C
            CALL DASIOI ( 'READ',   UNIT,  NREC,         DIRREC )
            CALL DASIOI ( 'WRITE',  UNIT,  NREC-NSHIFT,  DIRREC )
 
C
C           For each descriptor in the current directory, move the
C           cluster of data records it refers to.
C
C           Find the data type, size, and base record number of the
C           first cluster described by the current directory.  Also
C           find the index within the directory of the directory's
C           last descriptor.
C
            TYPE  =  DIRREC( BEGDSC )
            BASE  =  NREC + 1
 
            IF ( NREC .EQ. LREC ) THEN
               LINDEX = LWORD
            ELSE
               LINDEX = NWI
            END IF
 
C
C           We'll now traverse the directory in forward order, keeping
C           track of cluster sizes and types as we go.
C
C           POS will be the index of the descriptor of the current
C           cluster.
C
            POS  =  BEGDSC + 1
 
            DO WHILE ( POS .LE. LINDEX )
 
               IF ( POS .GT. BEGDSC+1 ) THEN
C
C                 We'll need to determine the type of the current
C                 cluster.  If the descriptor contains a positive
C                 value, the data type of the cluster it refers to is
C                 the successor of the previous type, according to our
C                 ordering of types.
C
                  IF (  DIRREC(POS)  .GT.  0  ) THEN
                     TYPE  =  NEXT( TYPE )
                  ELSE
                     TYPE  =  PREV( TYPE )
                  END IF
 
C
C                 Update the cluster base record number.
C
                  BASE  =  BASE + ABS ( DIRREC(POS-1) )
 
               END IF
 
C
C              BASE and TYPE now are correctly set for the current
C              cluster.  Move the cluster.
C
               DO I  =  BASE,   BASE + ABS( DIRREC(POS) ) - 1
 
                  IF ( TYPE .EQ. CHAR ) THEN
 
                     CALL DASIOC ( 'READ',   UNIT,  I,         RECC )
                     CALL DASIOC ( 'WRITE',  UNIT,  I-NSHIFT,  RECC )
 
                  ELSE IF ( TYPE .EQ. DP ) THEN
 
                     CALL DASIOD ( 'READ',   UNIT,  I,         RECD )
                     CALL DASIOD ( 'WRITE',  UNIT,  I-NSHIFT,  RECD )
 
                  ELSE
 
                     CALL DASIOI ( 'READ',   UNIT,  I,         RECI )
                     CALL DASIOI ( 'WRITE',  UNIT,  I-NSHIFT,  RECI )
 
                  END IF
 
               END DO
 
C
C              The next descriptor to look at is the next one in the
C              current directory.
C
               POS     =  POS + 1
 
            END DO
 
C
C           Find the next directory record.
C
            NREC = DIRREC ( FWDLOC )
 
         END DO
 
      END IF
 
C
C     Update the file summary.  The number of comment records and the
C     number of the first free record have been decremented by NSHIFT.
C     The numbers of the records containing the last descriptor of each
C     type have been decremented by NSHIFT only if they were non-zero.
C
C
C     The call to DASUFS will update the file record as well as the
C     file summary.
C
      NCOMR  =  NCOMR - NSHIFT
      FREE   =  FREE  - NSHIFT
 
      DO I = 1, 3
 
         IF ( LASTRC(I) .NE. 0 ) THEN
            LASTRC(I)  =  LASTRC(I) - NSHIFT
         END IF
 
      END DO
 
      CALL DASUFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
 
      CALL CHKOUT ( 'DASRCR' )
      RETURN
      END
