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
C
C     This is the routine needed by NSPFLG to create the reports
C     that are produced for flagged preserved or flagged format.
C     It returns either a column alias, a separator (right now
C     a colon) or the current value of the column.  To get back
C     the column alias send the opposite of the column ID to
C     NSPFRP, to get the colon, send the ID of zero, to get the
C     current print value for the column send the ID for the
C     column.
C
      SUBROUTINE NSPFRP ( ID, COMPNT, STRING, WIDTH )
 
      INTEGER               ID
      INTEGER               COMPNT
      CHARACTER*(*)         STRING
      INTEGER               WIDTH
 
      INTEGER               MYID
      INTEGER               PRESET
      SAVE
 
      DATA                  PRESET / 8 /
 
 
 
      MYID = ID
 
      IF ( MYID .LT. 0 ) THEN
 
         IF ( COMPNT .EQ. 1 ) THEN
            CALL FETCHA ( -MYID, COMPNT, STRING, WIDTH )
            WIDTH = PRESET
         ELSE
            STRING = ' '
            WIDTH  = PRESET
         END IF
 
      ELSE IF ( ID .EQ. 0 ) THEN
 
         IF ( COMPNT .EQ. 1 ) THEN
            WIDTH  =  2
            STRING = ':'
         ELSE
            WIDTH  =  2
            STRING = ' '
         END IF
 
      ELSE
 
         CALL CLPVAL ( ID, COMPNT, STRING, WIDTH )
 
      END IF
      RETURN
 
C
C     The entry point here allows NSPFLG to set the width
C     for all column aliases returned by NSPFRP.
C
      ENTRY NSPFRW   ( WIDTH )
 
      PRESET = WIDTH
      RETURN
 
      END
 
 
