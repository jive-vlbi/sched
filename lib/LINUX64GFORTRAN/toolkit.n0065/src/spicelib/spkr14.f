 
C$Procedure      SPKR14 ( Read SPK record from segment, type 14 )
 
      SUBROUTINE SPKR14 ( HANDLE, DESCR, ET, RECORD )
 
C$ Abstract
C
C     Read a single data record from a type 14 SPK segment.
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
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( * )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of the open SPK file.
C     DESCR      I   Descriptor of the segment with the desired record.
C     ET         I   Epoch used to identify the desired record.
C     RECORD     O   The desired type 14 SPK record.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of the open SPK file which contains the
C              segment of interest.
C
C     DESCR    is the descriptor for a type 14 SPK segment that contains
C              the record of interest.
C
C     ET       is the target epoch used to determine the particular
C              record to be obtained from the SPK segment.
C
C$ Detailed_Output
C
C     RECORD   is the record from the specified segment which,
C              when evaluated at epoch ET, will give the state
C              (position and velocity) of some body, relative
C              to some center, in some inertial reference frame.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) It is assumed that the descriptor and handle supplied are
C        for a properly constructed type 14 segment. No checks are
C        performed to ensure this.
C
C     2) If the input ET value is not within the range specified
C        in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
C        is signalled.
C
C     3) All other errors are diagnosed by routines in the call tree
C        of this routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     This subroutine will read a single record from a type 14 SPK
C     segment. The record read will provide the data necessary to
C     compute the state for a some body in some inertial frame at epoch
C     ET.
C
C     See the SPK Required Reading file for a description of the
C     structure of a type 14 SPK segment.
C
C$ Examples
C
C     The data returned by the SPKRnn routine is in a raw form, taken
C     directly from the segment.  As such, it will be not be directly
C     useful to a user unless they have a complete understanding of the
C     structure of the data type.  Given that understanding, however,
C     the SPKRnn routines could be used to "dump" and check segment data
C     for a particular epoch, as in the example which follows.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 14 ) THEN
C              CALL SPKR14 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C           END IF
C
C$ Restrictions
C
C     This subroutine should not be called directly by a casual user. It
C     is intended for use by the subroutine SPKPV, and certain tests for
C     error conditions are not performed here, as SPKPV will have
C     already performed them.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 10-MAR-1995 (KRG)
C
C-&
 
C$ Index_Entries
C
C     read record from type_14 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local variables
C
C     The number of constant values stored with a type 14 segment
C     segment.
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 1  )
C
C     The beginning location in the output record for the non constant
C     segment data.
C
      INTEGER               BEGEL1
      PARAMETER           ( BEGEL1 = NCONST + 1      )
 
      DOUBLE PRECISION      VALUE
 
      INTEGER               ENDS
      INTEGER               INDX
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKR14' )
      END IF
C
C     Check the request time against the time bounds in the segment
C     descriptor. In order to get the right data back from the generic
C     segment calls below, we need to be sure that the desired epoch
C     falls within the bounds of the segment, as specified by the
C     descriptor. The first two elements of the descriptor are the start
C     time for the segment and the stop time for the segment,
C     respectively.
C
      IF ( ( ET .LT. DESCR(1) )  .OR.  ( ET .GT. DESCR(2) ) )  THEN
 
         CALL SETMSG( 'Request time # is outside of descriptor '   //
     .                'bounds # : #.'                               )
         CALL ERRDP ( '#',  ET                                      )
         CALL ERRDP ( '#',  DESCR(1)                                )
         CALL ERRDP ( '#',  DESCR(2)                                )
         CALL SIGERR( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         CALL CHKOUT( 'SPKR14'                                      )
         RETURN
 
      END IF
C
C     Fetch the constants and store them in the first part of
C     the output RECORD.
C
      CALL SGFCON ( HANDLE, DESCR, 1, NCONST, RECORD(1) )
C
C     Locate the time in the file less than or equal to the input ET,
C     obtaining its index. This will allow us to retrieve the proper
C     record.
C
      CALL SGFRVI ( HANDLE, DESCR, ET, VALUE, INDX, FOUND )
C
C     Fetch the appropriate record from the segment.
C
      CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD(BEGEL1), ENDS )
 
      CALL CHKOUT ( 'SPKR14' )
      RETURN
 
      END
