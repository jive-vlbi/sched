C$Procedure      SPKE18 ( S/P Kernel, evaluate, type 18 )
 
      SUBROUTINE SPKE18 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single data record from a type 18 SPK segment.
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

      INCLUDE 'spk18.inc'
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
C     RECORD     I   Record from a type 18 SPK segment valid for ET.
C     STATE      O   State (position and velocity) at epoch ET.
C
C$ Detailed_Input
C
C     ET             is the epoch for which a state vector is desired.
C
C     RECORD         is a record from a type 18 SPK segment which, when
C                    evaluated at epoch ET, will give the state
C                    (position and velocity) of some body, relative to
C                    some center, in some inertial reference frame.
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
C                       | packet 2             |
C                       +----------------------+
C                                .
C                                .
C                                .
C                       +----------------------+
C                       | packet n             |
C                       +----------------------+
C                       | epochs 1--n          |
C                       +----------------------+
C
C$ Detailed_Output
C
C     STATE    is the state vector at epoch ET. Its contents are, in
C              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
C
C$ Parameters
C
C     MAXREC   is the maximum size of SPK record.  See the SPICELIB 
C              routine SPKPVN for details.
C
C$ Exceptions
C
C     None.  This routine assumes that the input record is valid.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 18 (MEX/Rosetta Orbit 
C     file interpolation) SPK segments is described in the SPK 
C     Required Reading.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in a raw form, taken
C     directly from the segment.  As such, it will be not be directly
C     useful to a user unless they have a complete understanding of the
C     structure of the data type.  Given that understanding, however,
C     the SPKRnn routines could be used to "dump" and check segment data
C     for a particular epoch before evaluating the record to obtain a
C     state vector, as in the example which follows.
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
C           IF ( TYPE .EQ. 18 ) THEN
C
C              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE18 ( ET, RECORD, STATE )
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 05-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in XPOSEG and LGRINT calls.
C
C-    SPICELIB Version 1.0.0, 17-AUG-2002 (NJB)
C
C-&


C$ Index_Entries
C
C     evaluate type_18 spk segment
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 05-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in XPOSEG and LGRINT calls.
C
C-& 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      LGRINT
      LOGICAL               RETURN
 
C
C     Local parameters
C

C
C     Index of subtype code in record:
C
      INTEGER               SBTIDX
      PARAMETER           ( SBTIDX = 1 )

C
C     Index of packet count in record:
C
      INTEGER               CNTIDX
      PARAMETER           ( CNTIDX = 2 )

C
C     Index at which packets start:
C
      INTEGER               PKTIDX
      PARAMETER           ( PKTIDX = 3 )

C
C     Maximum polynomial degree:
C     
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )


C
C     Local variables
C
      DOUBLE PRECISION      LOCREC ( MAXREC )
      DOUBLE PRECISION      WORK   ( MAXREC * 2,  2 )
      DOUBLE PRECISION      VBUFF  ( 6 )
      
      INTEGER               FROM
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               PACKSZ
      INTEGER               SUBTYP
      INTEGER               TO
      INTEGER               XSTART
      INTEGER               YSTART

      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'SPKE18' )
 
C
C     Capture the subtype from the record and set the packet size
C     accordingly.
C
      SUBTYP =  NINT( RECORD(SBTIDX) )

      IF ( SUBTYP .EQ. S18TP0 ) THEN

         PACKSZ = S18PS0

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         PACKSZ = S18PS1

      ELSE
         
         CALL SETMSG ( 'Unexpected SPK type 18 subtype found in ' //
     .                 'type 18 record.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                      )
         CALL CHKOUT ( 'SPKE18'                                   )
         RETURN
      
      END IF
C
C     Get the packet count.
C
      N = NINT( RECORD(CNTIDX) )

      IF ( SUBTYP .EQ. S18TP1 ) THEN
C
C        This is the easy case:  we perform Lagrange interpolation
C        on each state component.
C
C        We'll transpose the state information in the input record so
C        that contiguous pieces of it can be shoved directly into the
C        interpolation routine LGRINT.  
C
         N   =  NINT( RECORD(CNTIDX) )
 
         CALL XPSGIP ( PACKSZ, N, RECORD(PKTIDX) )
 
C
C        We interpolate each state component in turn.
C
         XSTART   =   3   +  N * PACKSZ
 
         DO I = 1, PACKSZ
 
            YSTART    =   3   +  N * (I-1)
 
            STATE(I)  =   LGRINT ( N,
     .                             RECORD(XSTART),
     .                             RECORD(YSTART),
     .                             LOCREC,
     .                             ET              )
         END DO  

      ELSE

C
C        We interpolate each state component in turn.  Position and
C        velocity are interpolated separately.
C
         XSTART   =   3  +  PACKSZ * N 
   
         DO I = 1, 3
 
            DO J = 1, N
C
C              For the Jth input packet, copy the Ith position and
C              velocity components into the local record buffer LOCREC.
C
               FROM         = 2 + PACKSZ*(J-1) + I
               TO           =     2 * J        - 1
            
               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 3 )
            
            END DO

C
C           Interpolate the Ith position and velocity components of the
C           state.  We'll keep the position and overwrite the velocity.
C        
            CALL HRMINT ( N, 
     .                    RECORD(XSTART),
     .                    LOCREC,
     .                    ET,           
     .                    WORK,
     .                    STATE(I  ),
     .                    STATE(I+3)      ) 

         END DO
 
C
C        Now interpolate velocity, using separate velocity data and 
C        acceleration.
C
         DO I = 1, 3
  
            DO J = 1, N
C
C              For the Jth input packet, copy the Ith position and
C              velocity components into the local record buffer LOCREC.
C
               FROM         = 2  +  PACKSZ*(J-1)  +  PACKSZ/2  +  I
               TO           =       2 * J         -  1
            
               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 3 )
            
            END DO

C
C           Interpolate the Ith velocity and acceleration components of 
C           the state.  We'll capture the result in a temporary buffer,
C           then transfer the velocity to the output state array.
C        
            CALL HRMINT ( N, 
     .                    RECORD(XSTART),
     .                    LOCREC,
     .                    ET,           
     .                    WORK,
     .                    VBUFF(I  ),
     .                    VBUFF(I+3)     ) 

         END DO
 
C
C        Fill in the velocity in the output state using the results of
C        interpolating velocity and acceleration.
C
         CALL VEQU ( VBUFF, STATE(4) )
   
      END IF
   
      CALL CHKOUT ( 'SPKE18' )
      RETURN
      END
 
