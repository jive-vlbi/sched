C$Procedure      SPKE19 ( SPK, evaluate record, type 19 )
 
      SUBROUTINE SPKE19 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single data record from a type 19 SPK segment.
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
C     SPK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'spk19.inc'
      INCLUDE 'spkrec.inc'
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXREC     P   Maximum size of SPK record.  See SPKPVN.
C     ET         I   Epoch for which a state is desired.
C     RECORD     I   Record from a type 19 SPK segment valid for ET.
C     STATE      O   State (position and velocity) at epoch ET.
C
C$ Detailed_Input
C
C     ET             is the epoch for which a state vector is desired,
C                    expressed as seconds past J2000 TDB.
C                    
C
C     RECORD         is a record from a type 19 SPK segment which, when
C                    evaluated at epoch ET, will give the state
C                    (position and velocity) of some body, relative to
C                    some center, in the reference frame associated
C                    with the data packets. Usually the body, center
C                    and frame are those of the SPK segment from which
C                    the packets were read.
C
C                    The structure of the record is as follows:
C
C                       +----------------------+
C                       | subtype code         |
C                       +----------------------+
C                       | number of packets (n)|
C                       +----------------------+
C                       | packet 1             |
C                       +----------------------+
C                                .
C                                .
C                                .
C                       +----------------------+
C                       | packet n             |
C                       +----------------------+
C                       | epoch 1              |
C                       +----------------------+
C                                .
C                                .
C                                .
C                       +----------------------+
C                       | epoch n              |
C                       +----------------------+
C
C$ Detailed_Output
C
C     STATE    is the state vector at epoch ET. Its contents are, in
C              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
C
C$ Parameters
C
C     MAXREC   is the maximum size of SPK record. See the SPICELIB 
C              routine SPKPVN for details.
C
C$ Exceptions
C
C     1)  Most types of errors in the input record cannot be diagnosed
C         by this routine. This routine assumes that the input record
C         is valid.
C
C     2)  If the subtype code in the input record is invalid, the error
C         will be diagnosed by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 19 (ESOC/DDID piecewise
C     interpolation) SPK segments is described in the SPK Required
C     Reading.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in a raw form, taken
C     directly from the segment. As such, it will be not be directly
C     useful to a user unless they have a complete understanding of the
C     structure of the data type.  Given that understanding, however,
C     the SPKRnn routines could be used to "dump" and check segment data
C     for a particular epoch before evaluating the record to obtain a
C     state vector, as in the example that follows.
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
C
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 19 ) THEN
C
C              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE19 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     1)  This routine assumes that the input record is valid.  Any
C         checking of the input data is assumed to have been performed
C         when the source SPK file was created.
C
C$ Literature_References
C
C     None.
C     
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
C
C-&


C$ Index_Entries
C
C     evaluate type_19 spk_segment
C
C-&

C$ Revisions
C
C     None.
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
      
      CALL CHKIN ( 'SPKE19' )
 
C
C     Given that our nominally type 19 input record is actually a 
C     valid type 18 record, we let the type 18 evaluator do the 
C     work.
C     
      CALL SPKE18 ( ET, RECORD, STATE )

      CALL CHKOUT ( 'SPKE19' )
      RETURN
      END
 
