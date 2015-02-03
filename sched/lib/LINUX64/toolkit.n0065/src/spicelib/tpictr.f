C$Procedure      TPICTR ( Create a Time Format Picture )
 
      SUBROUTINE TPICTR ( SAMPLE, PICTUR, OK, ERROR )
 
C$ Abstract
C
C     Given a sample time string, create a time format picture
C     suitable for use by the routine TIMOUT.
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
C      None.
C
C$ Keywords
C
C      TIME
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         SAMPLE
      CHARACTER*(*)         PICTUR
      LOGICAL               OK
      CHARACTER*(*)         ERROR
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SAMPLE     I   is a sample date time string
C     PICTUR     O   is a format picture that describes SAMPLE
C     OK         O   indicates success or failure to parse SAMPLE
C     ERROR      O   a diagnostic returned if SAMPLE cannot be parsed
C
C$ Detailed_Input
C
C     SAMPLE     is a representative time string that to use
C                as a model to format time strings.
C
C$ Detailed_Output
C
C     PICTUR     is a format picture suitable for use with the SPICE
C                routine TIMOUT.  This picture when used to format
C                the appropriate  epoch via TIMOUT will yield the same
C                time components in the same order as the components
C                in SAMPLE.
C
C                Picture should be declared to be at least 80 characters
C                in length.  If Picture is not sufficiently large
C                to contain the format picture, the picture will
C                be truncated on the right.
C
C     OK         is a logical flag.  If all of the components of SAMPLE
C                are recognizable, OK will be returned with the value
C                TRUE.  If some part of PICTUR cannot be parsed,
C                OK will be returned with the value FALSE.
C
C     ERROR      is a diagnostic message  that indicates what part of
C                SAMPLE was not recognizable.  If SAMPLE can be
C                successfully parsed, OK will be TRUE and ERROR will
C                be returned as a blank string.  If ERROR does not
C                have sufficient room (up to 400 characters) to 
C                contain the full message, the message will be truncated
C                on the right.
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
C     1) All problems with the inputs are diagnosed via OK and ERROR.
C
C     2) If a format picture can not be created from the sample
C        time string, PICTUR is returned as a blank string.
C
C$ Particulars
C
C     Although the routine TIMOUT provides SPICE users with a great
C     deal of flexibility in formatting time strings, users must
C     master the means by which a time picture is constructed
C     suitable for use by TIMOUT.
C
C     This routine allows SPICE users to supply a sample time string
C     from which a corresponding time format picture can be created,
C     freeing users from the task of mastering the intricacies of
C     the routine TIMOUT.
C
C     Note that TIMOUT can produce many time strings whose patterns
C     can not be discerned by this routine.  When such outputs are
C     called for, the user must consult TIMOUT and construct the
C     appropriate format picture "by hand".  However, these exceptional
C     formats are not widely used and are not generally recognizable
C     to an uninitiated reader.
C
C$ Examples
C
C     Suppose you need to print epochs corresponding to some
C     events and you wish the epochs to have the same arrangement
C     of components as in the string '10:23 P.M. PDT January 3, 1993'
C
C     The following subroutine call will construct the appropriate
C     format picture for use with TIMOUT.
C
C     CALL TPICTR ( '10:23 P.M. PDT January 3, 1993', PICTUR, OK, ERROR)
C
C     The resulting picture is:
C
C        'AP:MN AMPM PDT Month DD, YYYY ::UTC-7'
C
C     This picture can be used with TIMOUT to format a sequence
C     of epochs, ET(1),...,ET(N) (given as ephemeris seconds past J2000)
C     as shown in the loop below:
C
C        DO I = 1, N
C           CALL TIMOUT ( ET(I), PICTUR, STRING )
C           WRITE (*,*) 'Epoch: ', I, ' --- ', STRING
C        END DO
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 16-MAR-1999 (WLT)
C
C        Corrected a minor spelling error in the header comments.
C
C-    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Use a sample time string to produce a time format picture
C
C-&
 
      CHARACTER*(5)         TYPE
      CHARACTER*(8)         MODIFY   ( 5 )
 
      DOUBLE PRECISION      TVEC     ( 10 )
 
      INTEGER               NTVEC
 
      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV
C
C     This routine is really just a front for one aspect of
C     the routine TPARTV.
C
      ERROR  = ' '
 
      CALL TPARTV ( SAMPLE,
     .              TVEC,   NTVEC, TYPE,
     .              MODIFY, MODS,  YABBRV, SUCCES,
     .              PICTUR, ERROR )
 
 
      IF ( PICTUR .EQ. ' ' ) THEN
         OK = .FALSE.
      ELSE
         OK    = .TRUE.
         ERROR = ' '
      END IF
 
      RETURN
      END
