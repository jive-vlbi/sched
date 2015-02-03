C$Procedure ZZDDHRCM ( Private --- DDH Request Count )
 
      SUBROUTINE ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Manage augmentation of the handle to logical unit request counter
C     and the cost column in the unit table.
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
C     None.
C
C$ Keywords
C
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               NUT
      INTEGER               UTCST  ( * )
      INTEGER               REQCNT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUT        I   Number of entries in the unit table.
C     UTCST     I/O  Cost column of the unit table.
C     REQCNT    I/O  Value of the HLU request counter.
C
C$ Detailed_Input
C
C     NUT        is the number of entries in the unit table.
C
C     UTCST      is the current cost column of the unit table.
C
C     REQCNT     is the current value of the HLU request counter to
C                adjust.
C
C$ Detailed_Output
C
C     UTCST      is the updated cost column of the unit table.  In
C                the nominal case, UTCST will not be adjusted, but
C                if REQCNT overflows, then adjustments will be made
C                to approximately preserve the priority.
C
C     REQCNT     is the updated value of the request counter.
C                Nominally this will be 1 more than the input
C                value.  However, in the case where REQCNT will
C                exceed INTMAX it will be assigned to INTMAX()/2 + 1.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If REQCNT on input is INTMAX(), then REQCNT on output will
C        be assigned INTMAX()/2 + 1 and the UTCST column will be
C        recomputed.
C
C$ Particulars
C
C     This module manages the request counter and the cost column of
C     the unit table which is used to determine the expense of
C     disconnecting a handle from its logical unit.
C
C     In the nominal mode of operation, the request counter is simply
C     incremented by one and the cost column remains untouched.
C     However, when the request counter passed into the routine is
C     INTMAX, then REQCNT is not incremented.  In an attempt to preserve
C     the relationships between costs, all entries in the cost column
C     are halved and REQCNT is set to INTMAX()/2 + 1.  This has the
C     effect of preserving the cost relationships between rows, except
C     in half the cases where subsequent cost values are present.
C
C     The occurrence of rollover is rare, and thus the destruction of
C     relative cost relationships as well.
C
C$ Examples
C
C     See ZZDDHHLU for sample usage.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               INTMAX
 
C
C     Local Variables
C
      INTEGER               I
 
C
C     Check to see if REQCNT is INTMAX, otherwise just increment
C     REQCNT.
C
      IF ( REQCNT .EQ. INTMAX() ) THEN
 
         REQCNT = INTMAX()/2 + 1
 
         DO I = 1,NUT
            UTCST(I) = MAX(1, UTCST(I)/2 )
         END DO
 
      ELSE
 
         REQCNT = REQCNT + 1
 
      END IF
 
      RETURN
      END
