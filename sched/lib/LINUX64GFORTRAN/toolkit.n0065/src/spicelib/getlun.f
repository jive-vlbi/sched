C$Procedure      GETLUN ( Get a free logical unit )
 
      SUBROUTINE GETLUN ( UNIT )
 
C$ Abstract
C
C     Return the number of a free logical unit.
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
C     FILES
C
C$ Declarations
 
      INTEGER          UNIT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       O   The number of a free logical unit.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     UNIT        is the number of a free logical unit (also called
C                 an "external unit"). If no free units are available,
C                 the value of UNIT is 0.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If there are no free logical units available, UNIT is assigned
C         the value 0, and the error SPICE(NOFREELOGICALUNIT) is
C         signalled.
C
C     2)  This routine obtains a logical unit number from FNDLUN.
C         FNDLUN executes a Fortran INQUIRE statement; if that statement
C         fails to execute properly, FNDLUN returns a negative unit
C         number.  In this case, GETLUN assigns the value 0 to UNIT,
C         and the error SPICE(INQUIREFAILED) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     GETLUN returns the number of the first (unreserved) unit not
C     currently connected to a file.  It thus frees the user from
C     having to maintain an accounting of which units are open, which
C     are closed, and which are available.
C
C     This routine is related to the routines FNDLUN, RESLUN, and
C     FRELUN.  Together, these routines support coordinated usage of
C     Fortran logical units.  GETLUN (Get a free logical unit) and
C     FNDLUN (Find a free logical unit) both have the function of
C     returning a logical unit number that is not reserved or already
C     in use.  The principal difference between the functionality of
C     these routines is that GETLUN both returns a status code and
C     signals an error if a free unit is not found, while FNDLUN
C     merely returns a status code.
C
C     RESLUN is used to reserve logical unit numbers, so that they will
C     not be returned by GETLUN or FNDLUN; FRELUN frees logical units
C     previously reserved via calls to RESLUN.
C
C     Logical units 5-7 are reserved by default. Other units may be
C     reserved by calling RESLUN. Once reserved, units (except 5-7) may
C     be unreserved by calling FRELUN.
C
C     To reserve logical unit numbers for special use, refer to
C     RESLUN. To make reserved units available to GETLUN or FNDLUN,
C     refer to FRELUN.
C
C     A unit returned by GETLUN does NOT automatically become a
C     reserved unit.  If the user wishes to reserve a unit found by
C     GETLUN, the call to GETLUN must be followed by a call to RESLUN.
C
C     This routine obtains a logical unit number via a call to FNDLUN.
C     FNDLUN uses an INQUIRE statement; if that statement doesn't
C     execute properly, GETLUN will signal the error.  This arrangement
C     allows FNDLUN to be error free.
C
C     The range of possible unit numbers returned by GETLUN is dependent
C     on the parameters MINLUN and MAXLUN, which are defined in FNDLUN.
C
C     Note that although 0 is a valid logical unit number on some
C     systems, a value of 0 returned by GETLUN indicates that no free
C     logical unit was available, rather than that logical unit 0 is
C     available.
C
C$ Examples
C
C     The following code fragment illustrates the use of GETLUN.
C
C        CALL GETLUN ( UNIT )
C
C        IF ( UNIT .EQ. 0 ) THEN
C           RETURN
C        END IF
C
C$ Restrictions
C
C     This routine never returns a logical unit number of 0.  The
C     value 0 is used to indicate that no free logical unit was
C     found.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     C.A. Curzon     (JPL)
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (IMU)
C
C-&
 
C$ Index_Entries
C
C     get a free logical unit
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 24-FEB-1989 (HAN) (NJB)
C
C         This routine has been substantially re-written so as to
C         obtain a free logical unit number via a call to FNDLUN.
C
C         If there are no free logical units available, UNIT
C         is assigned the value 0, and an error is signalled.
C
C         The "Parameters" section was added to the header.
C
C-&
 
 
 
C
C     Spicelib functions
C
      LOGICAL          RETURN
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETLUN' )
      END IF
 
 
 
C
C     Find a free logical unit, if there's one to be had.
C
      CALL FNDLUN ( UNIT )
 
 
 
      IF ( UNIT .EQ. 0 ) THEN
C
C        There are no free units to be had.  C'est la vie.  Signal an
C        error.
C
         CALL SETMSG ( 'No free logical units are available.' )
         CALL SIGERR ( 'SPICE(NOFREELOGICALUNIT)' )
         CALL CHKOUT ( 'GETLUN' )
         RETURN
 
      ELSE IF ( UNIT .LT. 0 ) THEN
C
C        There are no free units to be had.  In this case, we know the
C        "INQUIRE" attempted by FNDLUN failed.  Assign 0 to the unit
C        number, and signal an error.
C
 
         CALL SETMSG ( 'INQUIRE iostat was #.' )
         CALL ERRINT ( '#', - UNIT )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)' )
 
         UNIT = 0
 
         CALL CHKOUT ( 'GETLUN' )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'GETLUN' )
      RETURN
      END
