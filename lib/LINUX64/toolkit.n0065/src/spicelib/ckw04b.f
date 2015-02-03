C$Procedure      CKW04B ( CK type 04: Begin a segment )

      SUBROUTINE CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID )

C$ Abstract
C
C     Begin a type CK04 segment in the DAF file associated with
C     HANDLE. See also CKW04A and CKW04E.
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

      IMPLICIT              NONE

      INCLUDE               'sgparam.inc'
      INCLUDE               'ckparam.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         SEGID
      INTEGER               INST
      LOGICAL               AVFLAG
      CHARACTER*(*)         REF
      DOUBLE PRECISION      BEGTIM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an DAF file open for writing.
C     SEGID      I   The string to use for segment identifier.
C     INST       I   The NAIF ID code for the SC or instrument.
C     AVFLAG     I   The angular rates flag.
C     REF        I   The reference frame for this segment.
C     BEGTIM     I   The segment coverage start encoded SCLK time
C
C$ Detailed_Input
C
C     HANDLE     is the file handle of a CK file that has been
C                opened for writing.
C
C     SEGID      is the segment identifier. CK segment identifier
C                may contain up to 40 printing ASCII characters.
C
C     INST       is the SPICE ID for the SC structure or instrument 
C                whose orientation are to be recorded in a CK file.
C
C     AVFLAG     angular rates flag indicates whether segment will 
C                contain angular rate information.
C
C     REF        is the name of a reference frame that pointing is  
C                given with respect to, for example 'J2000'.
C
C     BEGTIM     is the encoded SCLK time for the start of the segment
C                coverage.
C
C$ Detailed_Output
C
C     None.      The input data is used to create the segment summary 
C                for the segment being started in the DAF file 
C                associated with HANDLE.
C
C                See the $ Particulars section for details about the
C                structure of a type 4 CK segment.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the files
C     'sgparam.inc' and 'ckparam.inc'.
C
C$ Exceptions
C
C     1) File access errors are diagnosed by routines in the call tree
C        of this routine.
C
C     2) If numeric ID for given reference frame cannot be resolved 
C        from it's name SPICE(INVALIDREFFRAME) is signalled.
C
C     2) If SEGID is more than 40 characters long, the error
C        SPICE(SEGIDTOOLONG) is signalled.
C
C     3) If SEGID contains any nonprintable characters, the error
C        SPICE(NONPRINTABLECHARS) is signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     This routine begins writing a type 4 CK segment to the open DAF
C     file that is associated with HANDLE. The file must have been
C     opened with write access.
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
C     1) The file containing the segment should be opened for read
C        or write access either by CKOPN or DAFOPW.
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
C     begin writing a type_4 CK segment
C
C-&

C
C     Spicelib functions
C
      INTEGER               LASTNB
      LOGICAL               RETURN
      
C
C     Local Parameters
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
C     The number of generic segment constants in a type 4 CK segment.
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 0 )

C
C     The integer codes of the first and last printable ASCII 
C     characters.
C
      INTEGER               FPRINT
      PARAMETER           ( FPRINT = 32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT = 126 )

C
C     The maximum number of characters allowed in a CK segment 
C     identifier.
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

C
C     Local variables
C
      DOUBLE PRECISION      DCD    ( ND )
      DOUBLE PRECISION      DCOEFF
      DOUBLE PRECISION      DESCR  ( NDESCR )
      
      INTEGER               I
      INTEGER               ICD    ( NI )
      INTEGER               REFCOD
      INTEGER               VALUE
      
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW04B' )
      END IF

C
C     Create a descriptor for the segment we are about to write. First
C     assign start and stop times.
C
      DCD( 1 ) = BEGTIM
      DCD( 2 ) = 0.D0

C
C     Second, resolve reference frame ID code from its name and 
C     assign it to the corresponding descriptor component. Signal
C     an error if frame is not recognized.
C
      CALL NAMFRM ( REF, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN
         CALL SETMSG ( 'The reference frame # is not supported.' )
         CALL ERRCH  ( '#', REF                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                  )
         CALL CHKOUT ( 'CKW04B'                                  )
         RETURN
      END IF

      ICD ( 2 ) = REFCOD

C
C     Third, assign values to the rest of the integer components of
C     the segment descriptor.
C
      ICD ( 1 ) = INST
      ICD ( 3 ) = CK4DTP

      IF ( AVFLAG ) THEN
         ICD ( 4 ) = 1
      ELSE
         ICD ( 4 ) = 0
      END IF

C
C     Now pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Check that all characters in the SEGID are printable.
C
      DO I = 1, LASTNB(SEGID)

         VALUE = ICHAR( SEGID(I:I) )

         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '      //
     .                    'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'CKW04B'                                )
            RETURN

         END IF

      END DO

C
C     Also check if the segment identifier is too long.
C
      IF ( LASTNB( SEGID ) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than '   //
     .                 '40 characters.'                           )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                      )
         CALL CHKOUT ( 'CKW04B'                                   )
         RETURN

      END IF

C
C     We've got a valid descriptor and identifier and can begin
C     the segment. For this data type, we want to use an explicit
C     reference value index where the reference epochs are in
C     increasing order. We also want the index returned for a
C     particular request epoch to be the index of the greatest
C     reference epoch less than or equal to the request epoch. These
C     characteristics are prescribed by the mnemonic EXPLE. See the
C     include file 'sgparam.inc' for more details.
C
      CALL SGBWVS ( HANDLE, DESCR, SEGID, NCONST, DCOEFF, EXPLE )

C
C     No need to check FAILED() here, since all we do after this 
C     point is checking out.
C
      CALL CHKOUT ( 'CKW04B' )
      
      RETURN

      END

