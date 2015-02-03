 
C$Procedure      SPKS14 ( S/P Kernel, subset, type 14 )
 
      SUBROUTINE SPKS14 ( SRCHAN, SRCDSC, DSTHAN, DSTDSC, DSTSID )
 
C$ Abstract
C
C     Extract a subset of the data in a type 14 SPK segment into a new
C     type 14 segment.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               SRCHAN
      DOUBLE PRECISION      SRCDSC(*)
      INTEGER               DSTHAN
      DOUBLE PRECISION      DSTDSC(*)
      CHARACTER*(*)         DSTSID
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SRCHAN     I   Handle of the SPK file with the source segment.
C     SRCDSC     I   Descriptor for the source segment.
C     DSTHAN     I   Handle of the SPK file for the destination segment.
C     DSTDSC     I   Descriptor for the destination segment.
C     DSTSID     I   Segment identifier for the new segment.
C
C$ Detailed_Input
C
C     SRCHAN   The handle of the SPK file containing the source segment.
C
C     SRCDSC   The SPK descriptor for the source segment.
C
C     DSTHAN   The handle of the SPK file containing the new segment.
C
C     DSTDSC   The SPK descriptor for the destination segment. It
C              contains the desired start and stop times for the
C              requested subset.
C
C     DSTSID   The segment identifier for the destination segment.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the length of the SPK record that is to be moved is larger
C        than MAXREC, the error 'SPICE(SPKRECTOOLARGE)' will be
C        signalled.
C
C$ Files
C
C     See arguments SRCHAN, DSTHAN.
C
C$ Particulars
C
C     This subroutine copies a subset of the data form one SPK segment
C     to another.
C
C     The exact structure of a segment of SPK type 14 is detailed in
C     the SPK Required Reading. Please see this document for details.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) We assume that the source descriptor actually describes a
C        segment in the source SPK file containing the time coverage
C        that is desired for the subsetting operation.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 08-MAR-1995 (KRG)
C
C-&
 
C$ Index_Entries
C
C     subset type_14 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     This is the maximum size type 14 record that we can move. This
C     allows a 20th degree Chebyshev Polynomial, which should be more
C     than sufficient. This should be the same as the value in SPKPV.
C
      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 128 )
C
C     Reference frame name size. See CHGIRF.
C
      INTEGER               FRMSIZ
      PARAMETER           ( FRMSIZ = 16 )
C
C     DAF ND and NI values for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
C
C     Length of a state.
C
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )
C
C     Local Variables
C
      CHARACTER*(FRMSIZ)    MYFRAM
 
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      DTEMP  ( ND     )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      MYREF
      DOUBLE PRECISION      RECORD ( MAXREC )
 
      INTEGER               BEGIDX
      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               CHBDEG
      INTEGER               DUMMY
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               IFRAME
      INTEGER               ITEMP  ( NI )
      INTEGER               RECSIZ
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS14' )
      END IF
C
C     First, unpack the destination segment descriptor and set some
C     local variables.
C
      CALL DAFUS( DSTDSC, ND, NI, DTEMP, ITEMP )
 
      BEGTIM = DTEMP(1)
      ENDTIM = DTEMP(2)
      BODY   = ITEMP(1)
      CENTER = ITEMP(2)
      IFRAME = ITEMP(3)
 
      CALL IRFNAM ( IFRAME, MYFRAM )
C
C     If we can't find the code, it can't be an SPK file.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS14' )
         RETURN
      END IF
C
C     Get the constants for this segment. There is only one.
C
      CALL SGFCON ( SRCHAN, SRCDSC, 1, 1, DTEMP )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS14' )
         RETURN
      END IF
C
C     The first element of DTEMP now contains the number of coefficients
C     used for the Chebyshev polynomials. We need the degree of the
C     polynomial which is one less than the number of coefficients.
C
      CHBDEG = INT ( DTEMP(1) ) - 1
C
C     Compute the size of the SPK record and signal an error if there is
C     not enough room in the variable RECORD to hold it.
C
      RECSIZ = (CHBDEG + 1) * NSTATE + 2
 
      IF ( RECSIZ .GT. MAXREC ) THEN
         CALL SETMSG ( 'Storage for # double precision numbers'
     .   //            ' is needed for an SPK data record and'
     .   //            ' only # locations were available. Update'
     .   //            ' the parameter MAXREC in the subroutine'
     .   //            ' SPKS14 and notify the NAIF group of this'
     .   //            ' problem.'                                )
         CALL ERRINT ( '#', RECSIZ                                )
         CALL ERRINT ( '#', MAXREC                                )
         CALL SIGERR ( 'SPICE(SPKRECTOOLARGE)'                    )
         CALL CHKOUT ( 'SPKS14'                                   )
         RETURN
      END IF
 
C
C     Get the beginning and ending indices for the packets we need for
C     the destination segment.
C
      CALL SGFRVI ( SRCHAN, SRCDSC, BEGTIM, MYREF, BEGIDX, FOUND )
      CALL SGFRVI ( SRCHAN, SRCDSC, ENDTIM, MYREF, ENDIDX, FOUND )
C
C     Begin the destination segment.
C
      CALL SPK14B ( DSTHAN, DSTSID, BODY, CENTER, MYFRAM,
     .                      BEGTIM, ENDTIM, CHBDEG        )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS14' )
         RETURN
      END IF
C
C     Now we get the data one record at a time from the source segment
C     and write it out to the destination segment.
C
      DO I = BEGIDX, ENDIDX
 
         CALL SGFPKT ( SRCHAN, SRCDSC, I, I, RECORD, DUMMY )
         CALL SGFREF ( SRCHAN, SRCDSC, I, I, MYREF         )
         CALL SPK14A ( DSTHAN, 1, RECORD, MYREF            )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKS14' )
            RETURN
         END IF
 
      END DO
C
C     Now all we need to do is end the segment.
C
      CALL SPK14E ( DSTHAN )
 
      CALL CHKOUT ( 'SPKS14' )
      RETURN
 
      END
