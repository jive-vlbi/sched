C$Procedure      CKW04E ( CK type 04: End a segment )

      SUBROUTINE CKW04E ( HANDLE, ENDTIM )

C$ Abstract
C
C     End the type 04 CK segment currently being written to the DAF
C     file associated with HANDLE. See also CKW04B and CKW04E.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      ENDTIM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an CK file open for writing.
C     ENDTIM     I   The segment coverage end encoded SCLK time.
C
C$ Detailed_Input
C
C     HANDLE     is the file handle of an CK file that has been
C                opened for writing, and to which a type 4 CK segment 
C                is being written.
C
C     ENDTIM     is the encoded SCLK time for the end of the segment
C                coverage.
C
C$ Detailed_Output
C
C     None.       The type 4 segment in the DAF file associated with
C                 HANDLE will be ended, making the addition of the
C                 data to the file permanent.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Errors reading or writing the file indicated by HANDLE will
C         be diagnosed by routine in the call tree of this routine.
C
C$ Files
C
C     See the argument HANDLE.
C
C$ Particulars
C
C     This routine ends a type 4 CK segment which is being written to
C     the DAF file associated with HANDLE. Ending the DAF segment is a
C     necessary step in the process of making the data a permanent part
C     of the DAF file.
C
C     This routine is one of a set of three routines for creating and
C     adding data to type 4 CK segments. These routines are:
C
C        CKW04B: Begin a type 4 CK segment. This routine must be
C                called before any data may be added to a type 4
C                segment.
C
C        CKW04A: Add data to a type 4 CK segment. This routine may be
C                called any number of times after a call to CKW04B to
C                add type 4 records to the CK segment that was
C                started.
C
C        CKW04E: End a type 4 CK segment. This routine is called to
C                make the type 4 segment a permanent addition to the
C                DAF file. Once this routine is called, no further type
C                4 records may be added to the segment. A new segment
C                must be started.
C
C     A type 4 CK segment consists of coefficient sets for variable
C     order Chebyshev polynomials over consecutive time intervals of 
C     a variable length. The gaps between intervals are allowed.
C     The Chebyshev polynomials represent individual quaternion 
C     components q0, q1, q2 and q3 and individual angular velocities 
C     AV1, AV2 and AV3 if they are included with the data.
C
C     The pointing data supplied to the type 4 CK writer (CKW04A) 
C     is packed into an array as a sequence of records,
C
C        ----------------------------------------------------
C        | Record 1 | Record 2 | .. | Record N-1 | Record N |
C        ----------------------------------------------------
C
C     with each record in data packets has the following format.
C
C        ----------------------------------------------------
C        | The midpoint of the approximation interval       |
C        ----------------------------------------------------
C        | The radius of the approximation interval         |
C        ----------------------------------------------------
C        | Number of coefficients for q0                    |
C        ----------------------------------------------------
C        | Number of coefficients for q1                    |
C        ----------------------------------------------------
C        | Number of coefficients for q2                    |
C        ----------------------------------------------------
C        | Number of coefficients for q3                    |
C        ----------------------------------------------------
C        | Number of coefficients for AV1                   |
C        ----------------------------------------------------
C        | Number of coefficients for AV2                   |
C        ----------------------------------------------------
C        | Number of coefficients for AV3                   |
C        ----------------------------------------------------
C        | q0 Cheby coefficients                            |
C        ----------------------------------------------------
C        | q1 Cheby coefficients                            |
C        ----------------------------------------------------
C        | q2 Cheby coefficients                            |
C        ----------------------------------------------------
C        | q3 Cheby coefficients                            |
C        ----------------------------------------------------
C        | AV1 Cheby coefficients (optional)                |
C        ----------------------------------------------------
C        | AV2 Cheby coefficients (optional)                |
C        ----------------------------------------------------
C        | AV3 Cheby coefficients (optional)                |
C        ----------------------------------------------------
C
C$ Examples
C
C     Assume that we have:
C
C        HANDLE   is the handle of an CK file opened with write
C                 access.
C
C        SEGID    is a character string of no more than 40 characters
C                 which provides a pedigree for the data in the CK
C                 segment we will create.
C
C        INST     is the SPICE ID code for the instrument whose
C                 pointing data is to be placed into the file.
C
C        AVFLAG   angular rates flag.
C
C        REFFRM   is the name of the SPICE reference frame for the
C                 pointing data.
C
C        BEGTIM   is the starting encoded SCLK time for which the
C                 segment is valid.
C
C        ENDTIM   is the ending encoded SCLK time for which the segment
C                 is valid.
C
C        N        is the number of type 4 records that we want to
C                 put into a segment in an CK file.
C
C        NPKTS    is integer array which contains the lengths of
C                 variable size data packets 
C
C        RECRDS   contains N type 4 records packaged for the CK
C                 file.
C
C        SCSTRT   contains the initial encoded SC time for each of
C                 the records contained in RECRDS, where
C
C                    SCSTRT(I) < SCSTRT(I+1), I = 1, N-1
C
C                    SCSTRT(1) <= FIRST, SCSTRT(N) < LAST
C
C     Then the following code fragment demonstrates how to create
C     a type 4 CK segment if all of the data for the segment is
C     available at one time.
C
C     C
C     C     Begin the segment.
C     C
C           CALL CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID )
C     C
C     C     Add the data to the segment all at once.
C     C
C           CALL CKW04A ( HANDLE, N, NPKTS, RECRDS, SCSTRT )
C     C
C     C     End the segment, making the segment a permanent
C     C     addition to the CK file.
C     C
C           CALL CKW04E ( HANDLE, ENDTIM )
C
C$ Restrictions
C
C     1) The type 4 CK segment being closed must have been started by
C        the routine CKW04B, the routine which begins a type 4 CK
C        segment.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Y.K. Zaiko     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
C
C-&

C$ Index_Entries
C
C     end a type_4 ck segment
C
C-&

C
C     SPICELIB functions.
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters.
C

C
C     DAF ND and NI values for CK files and length of a DAF descriptor.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )
      
      INTEGER               NDESCR
      PARAMETER           ( NDESCR =  ND + ( NI + 1 ) / 2 )

C
C     Local variables.
C
      DOUBLE PRECISION      DCD    ( ND )
      DOUBLE PRECISION      DESCR  ( NDESCR )
      
      INTEGER               ICD    ( NI )

      LOGICAL               FOUND

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW04E' )
      END IF

C
C     This is simple, just call the routine which ends a generic
C     segment.
C
      CALL SGWES ( HANDLE )

      IF ( FAILED() ) THEN
      
         CALL CHKOUT ( 'CKW04E' )
         RETURN
         
      END IF

C
C     Now update the descriptor with the end time. Locate the segment
C     with a backward search.
C
      CALL DAFBBS ( HANDLE )
      CALL DAFFPA ( FOUND  )

      IF ( .NOT. FOUND ) THEN

C
C        We have a bug.
C
         CALL SETMSG ( 'The segment which was just written could '   //
     .                 'not be found by a DAF search. This  '        //
     .                 'indicates a serious error.  Contact NAIF.'   )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'CKW04E'                                      )
         RETURN

      END IF

C
C     Get the descriptor, set the end time, and update the descriptor
C     in the file.
C
      CALL DAFGS( DESCR )

      CALL DAFUS( DESCR, ND, NI, DCD, ICD )

      DCD( 2 ) = ENDTIM

      CALL DAFPS( ND, NI, DCD, ICD, DESCR )

      CALL DAFRS( DESCR )

C
C     All done.
C
      CALL CHKOUT( 'CKW04E' )
      
      RETURN

      END
