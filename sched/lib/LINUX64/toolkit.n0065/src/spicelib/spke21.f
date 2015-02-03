C$Procedure      SPKE21 ( S/P Kernel, evaluate, type 21 )
 
      SUBROUTINE SPKE21 ( ET, RECORD, STATE )
      IMPLICIT NONE
 
C$ Abstract
C
C     Evaluate a single SPK data record from a segment of type 21
C     (Extended Difference Lines).
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
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      INCLUDE 'spk21.inc'

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Evaluation epoch.
C     RECORD     I   Data record.
C     STATE      O   State (position and velocity).
C     MAXTRM     P   Maximum number of terms per difference table
C                    component.
C
C$ Detailed_Input
C
C     ET          is an epoch at which a state vector is to be
C                 computed. The epoch is represented as seconds past
C                 J2000 TDB.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of an
C                 ephemeris object, relative to its center of motion,
C                 in an inertial reference frame.
C
C                 The contents of RECORD are as follows:
C
C                    RECORD(1):         The difference table size per
C                                       Cartesian component. Call this
C                                       size MAXDIM; then the difference
C                                       line (MDA) size DLSIZE is
C
C                                         ( 4 * MAXDIM ) + 11
C                                    
C                    RECORD(2)
C                       ...
C                    RECORD(1+DLSIZE):  An extended difference line.
C                                       The contents are:
C
C                       Dimension  Description
C                       ---------  ----------------------------------
C                       1          Reference epoch of difference line
C                       MAXDIM     Stepsize function vector
C                       1          Reference position vector,  x
C                       1          Reference velocity vector,  x
C                       1          Reference position vector,  y
C                       1          Reference velocity vector,  y
C                       1          Reference position vector,  z
C                       1          Reference velocity vector,  z
C                       MAXDIM,3   Modified divided difference
C                                  arrays (MDAs)
C                       1          Maximum integration order plus 1
C                       3          Integration order array
C
C$ Detailed_Output
C
C     STATE       is the state resulting from evaluation of the input
C                 record at ET. Units are km and km/sec.
C
C$ Parameters
C
C     MAXTRM      is the maximum number of terms allowed in
C                 each component of the difference table 
C                 contained in the input argument RECORD.
C                 See the INCLUDE file spk21.inc for the value
C                 of MAXTRM.
C                  
C$ Exceptions
C
C     1) If the maximum table size of the input record exceeds 
C        MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 21 (difference lines)
C     segments are described in the SPK Required Reading file.
C
C     SPKE21 is a modified version of SPKE01. The routine has been
C     generalized to support variable size difference lines.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Unknown.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     F.T. Krogh      (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB) (FTK) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_21 spk segment
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C

C
C     Local variables
C
C     The names below are original to the routine. They correspond
C     roughly to the original memos written by Fred Krogh to explain
C     how all this stuff really works.
C
      DOUBLE PRECISION      TL
      DOUBLE PRECISION      G        ( MAXTRM )
      
      DOUBLE PRECISION      REFPOS   (    3 )
      DOUBLE PRECISION      REFVEL   (    3 )
      
      DOUBLE PRECISION      DT       ( MAXTRM, 3 )
      INTEGER               KQMAX1
      INTEGER               KQ       (    3 )

      DOUBLE PRECISION      FC       ( MAXTRM )


      DOUBLE PRECISION      SUM
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      TP
      DOUBLE PRECISION      WC       ( MAXTRM-1 )
      DOUBLE PRECISION      W        ( MAXTRM+2 )

      INTEGER               MAXDIM
      INTEGER               MQ2
      INTEGER               KS1
      INTEGER               KS
      INTEGER               KQQ
      INTEGER               I
      INTEGER               J
      INTEGER               JX

C
C     Save everything between calls.
C 
      SAVE

C
C     Initial values
C
      DATA                  FC (  1 )    / 1.D0 /

C
C     Use discovery check-in.
C
C     If the RETURN function is set, don't even bother with this.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
C
C     The first element of the input record is the dimension
C     of the difference table MAXDIM. 
C
      MAXDIM = NINT ( RECORD(1) )

      IF ( MAXDIM .GT. MAXTRM ) THEN

         CALL CHKIN  ( 'SPKE21'                                )
         CALL SETMSG ( 'The input record has a maximum table '
     .   //            'dimension of #, while the maximum '
     .   //            'supported by this routine is #. It is '
     .   //            'possible that this problem is due to '
     .   //            'your SPICE Toolkit being out of date.' )
         CALL ERRINT ( '#',  MAXDIM                            )
         CALL ERRINT ( '#',  MAXTRM                            )
         CALL SIGERR ( 'SPICE(DIFFLINETOOLARGE)'               )
         CALL CHKOUT ( 'SPKE21'                                )
         RETURN

      END IF

C
C     Unpack the contents of the MDA array.
C
C        Name     Dimension  Description
C        ------   ---------  -------------------------------
C        TL               1  Reference epoch of record
C        G           MAXDIM  Stepsize function vector
C        REFPOS           3  Reference position vector
C        REFVEL           3  Reference velocity vector
C        DT      MAXDIM,NTE  Modified divided difference arrays
C        KQMAX1           1  Maximum integration order plus 1
C        KQ             NTE  Integration order array
C
C     For our purposes, NTE is always 3.
C
    
      CALL MOVED ( RECORD( 2),      1, TL )
      CALL MOVED ( RECORD( 3), MAXDIM, G  )

C     
C     Collect the reference position and velocity.
C     
      REFPOS(1) =  RECORD(MAXDIM+3)
      REFVEL(1) =  RECORD(MAXDIM+4)
       
      REFPOS(2) =  RECORD(MAXDIM+5)
      REFVEL(2) =  RECORD(MAXDIM+6)
      
      REFPOS(3) =  RECORD(MAXDIM+7)
      REFVEL(3) =  RECORD(MAXDIM+8)

C
C     Initializing the difference table is one aspect of this routine
C     that's a bit different from SPKE01. Here the first dimension of
C     the table in the input record can be smaller than MAXTRM. So, we
C     must transfer separately the portions of the table corresponding
C     to each component.
C
      DO I = 1, 3
         CALL MOVED ( RECORD(I*MAXDIM+9), MAXDIM, DT(1,I) )
      END DO

      KQMAX1    = INT ( RECORD(4*MAXDIM + 9 ) )
      KQ(1)     = INT ( RECORD(4*MAXDIM + 10) )
      KQ(2)     = INT ( RECORD(4*MAXDIM + 11) )
      KQ(3)     = INT ( RECORD(4*MAXDIM + 12) )

C     
C     Next we set up for the computation of the various differences
C     
      DELTA     = ET     - TL
      TP        = DELTA 
      MQ2       = KQMAX1 - 2
      KS        = KQMAX1 - 1
   
C
C     This is clearly collecting some kind of coefficients.  
C     The problem is that we have no idea what they are...
C     
C     The G coefficients are supposed to be some kind of step size 
C     vector. 
C     
C     TP starts out as the delta t between the request time and the
C     difference line's reference epoch. We then change it from DELTA
C     by the components of the stepsize vector G.
C
      DO J = 1, MQ2
C
C        Make sure we're not about to attempt division by zero.
C
         IF ( G(J) .EQ. 0.D0 ) THEN

            CALL CHKIN  ( 'SPKE21'                             )
            CALL SETMSG ( 'A  value of zero was found at '
     .      //            'index # of the step size vector.'   )
            CALL ERRINT ( '#',  J                              )
            CALL SIGERR ( 'SPICE(ZEROSTEP)'                    )
            CALL CHKOUT ( 'SPKE21'                             )
            RETURN

         END IF

         FC(J+1) = TP    / G(J)
         WC(J)   = DELTA / G(J)
         TP      = DELTA + G(J)

      END DO
   
C
C     Collect KQMAX1 reciprocals. 
C   
      DO J = 1, KQMAX1
         W(J) = 1.0D0/DBLE(J)
      END DO
      
C
C     Compute the W(K) terms needed for the position interpolation
C     (Note,  it is assumed throughout this routine that KS, which 
C     starts out as KQMAX1-1 (the ``maximum integration'') 
C     is at least 2.
C
      JX  = 0
      KS1 = KS - 1
      
      DO WHILE ( KS .GE. 2 )

         JX  = JX+1

         DO  J = 1, JX
            W(J+KS) = FC(J+1)*W(J+KS1) - WC(J)*W(J+KS)
         END DO
         
         KS  = KS1
         KS1 = KS1 - 1
         
      END DO
      
C
C     Perform position interpolation: (Note that KS = 1 right now.
C     We don't know much more than that.)
C
      DO  I = 1, 3
      
         KQQ = KQ(I)
         SUM = 0.0D0
         
         DO J = KQQ, 1, -1 
            SUM = SUM + DT(J,I)*W(J+KS)
         END DO
         
         STATE(I) = REFPOS(I)  +  DELTA * ( REFVEL(I) + DELTA*SUM )
         
      END DO
   
C
C     Again we need to compute the W(K) coefficients that are 
C     going to be used in the velocity interpolation. 
C     (Note, at this point, KS = 1, KS1 = 0.)
C      
      DO  J = 1, JX
         W(J+KS) = FC(J+1)*W(J+KS1) - WC(J)*W(J+KS)
      END DO   

      KS = KS - 1

C
C     Perform velocity interpolation:
C
      DO  I = 1, 3
      
         KQQ = KQ(I)
         SUM = 0.0D0
         
         DO J = KQQ, 1, -1
            SUM = SUM + DT(J,I)*W(J+KS)
         END DO
         
         STATE(I+3) = REFVEL(I) + DELTA*SUM
         
      END DO
 
      RETURN
      END


