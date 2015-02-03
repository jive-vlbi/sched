C$Procedure      SPKR12 ( Read SPK record from segment, type 12 )
 
      SUBROUTINE SPKR12 ( HANDLE, DESCR, ET, RECORD )
 
C$ Abstract
C
C     Read a single data record from a type 12 SPK segment.
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
      
      INTEGER               MAXREC
      PARAMETER           ( MAXREC  = 129 )
      
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
C     RECORD     O   The desired type 12 SPK record.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of the open SPK file which contains 
C                    the segment of interest.
C
C     DESCR          is the descriptor for a type 12 SPK segment that
C                    contains the record of interest.
C
C     ET             is the target epoch used to determine the 
C                    particular record to be obtained from the SPK 
C                    segment.
C
C$ Detailed_Output
C
C     RECORD         is the record from the specified segment which,
C                    when evaluated at epoch ET, will give the state
C                    (position and velocity) of some body, relative
C                    to some center, in some inertial reference frame.
C
C                    The structure of the record is as follows:
C
C                       +----------------------+
C                       | number of states (n) |
C                       +----------------------+
C                       | start epoch          |
C                       +----------------------+
C                       | step size            |
C                       +----------------------+
C                       | state 1 (6 elts.)    |
C                       +----------------------+
C                       | state 2 (6 elts.)    |
C                       +----------------------+
C                                   .
C                                   .
C                                   .
C                       +----------------------+
C                       | state n (6 elts.)    |
C                       +----------------------+
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) All errors are diagnosed by routines in the call tree
C        of this routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     This subroutine will read a single record from a type 12 SPK
C     segment. The record read will provide the data necessary to
C     compute the state for the body designated by DESCR at epoch
C     ET.
C
C     The exact format and structure of a type 12 SPK segment is
C     described in the SPK Required Reading.
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
C           IF ( TYPE .EQ. 12 ) THEN
C              CALL SPKR12 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C           END IF
C
C$ Restrictions
C
C     This subroutine should not be called directly by a casual user. It
C     is intended for use by the subroutine SPKPVN, and certain tests
C     for error conditions are not performed here, as SPKPVN will have
C     already performed them.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
C
C-&
 
C$ Index_Entries
C
C     read record from type_12 spk segment
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'SPKR12' )

C
C     The type 8 reader knows how to obtain a type 12 record.
C 
      CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
 
      CALL CHKOUT ( 'SPKR12' )
      RETURN
      END
 
