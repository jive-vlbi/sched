C$Procedure  REPMCW  ( Replace marker with character string, width )
 
      SUBROUTINE REPMCW ( IN, MARKER, VALUE, WIDTH, OUT )
 
C$ Abstract
C
C     Replace a marker with a character string of specified width.
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
C     CHARACTER
C     CONVERSION
C     STRING
C
C$ Declarations
 
      CHARACTER*(*)         IN
      CHARACTER*(*)         MARKER
      CHARACTER*(*)         VALUE
      INTEGER               WIDTH
      CHARACTER*(*)         OUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     MARKER     I   Marker to be replaced.
C     VALUE      I   Replacement string.
C     WIDTH      I   Width of replacement string to appear in OUT.
C     OUT        O   Output string.
C
C$ Detailed_Input
C
C     IN             is an arbitrary character string.
C
C     MARKER         is an arbitrary character string. The first
C                    occurrence of MARKER in the input string is
C                    to be replaced by VALUE.
C
C                    Leading and trailing blanks in MARKER are NOT
C                    significant. In particular, no substitution is
C                    performed if MARKER is blank.
C
C     VALUE          is an arbitrary character string. Leading and 
C                    trailing blanks in VALUE are significant.
C
C     WIDTH          Number of characters from VALUE that must be
C                    inserted in place of MARKER. If WIDTH is greater
C                    than actual length of the VALUE, blanks are 
C                    appended to the end of the VALUE when it is 
C                    inserted. Zero or negative widths are allowed,
C                    if such specified, marker is simply deleted 
C                    from input string.
C
C$ Detailed_Output
C
C     OUT            is the string obtained by substituting VALUE
C                    of specified width for the first occurrence 
C                    of MARKER in the input string.
C
C                    OUT and IN must be identical or disjoint.
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
C     1) If OUT does not have sufficient length to accommodate the
C        result of the substitution, the result will be truncated on
C        the right.
C
C     2) If MARKER is blank, or if MARKER is not a substring of IN,
C        no substitution is performed. (OUT and IN are identical.)
C
C     3) If VALUE is blank, a WIDTH number of blanks is substituted 
C        for the first occurrence of MARKER.
C
C$ Particulars
C
C     This is routine was created by modifying SPICELIB's REPMC 
C     behavior a little bit to make it more suitable for 
C     generation fixed width tables.
C
C$ Examples
C
C     This program using REPMCW:
C
C              INTEGER               WORDLN
C              PARAMETER           ( WORDLN = 10 )
C              
C              INTEGER               ACWDTH
C              PARAMETER           ( ACWDTH = 13 )
C        
C              INTEGER               NMWDTH
C              PARAMETER           ( NMWDTH = 22 )
C        
C              INTEGER               IDWDTH
C              PARAMETER           ( IDWDTH = 13 )
C        
C              CHARACTER*(WORDLN  )  ACRNYM
C              CHARACTER*(WORDLN*2)  SCNAME
C              CHARACTER*(WORDLN*4)  SCID
C              CHARACTER*(WORDLN*8)  TRECRD
C              CHARACTER*(WORDLN*8)  HLINE
C              CHARACTER*(WORDLN*8)  HTITLE
C              CHARACTER*(WORDLN*8)  DASHLN      
C        C
C        C     Table line templates.
C        C
C              HLINE  = '      +#+#+#+'
C              HTITLE = '      |#|#|#|'
C              TRECRD = '      |#|#|#|'
C              DASHLN = '--------------------------------------'
C        C
C        C     Create table separator line.
C        C
C              CALL REPMCW ( HLINE, '#', DASHLN,  ACWDTH, HLINE  )
C              CALL REPMCW ( HLINE, '#', DASHLN,  NMWDTH, HLINE  )
C              CALL REPMCW ( HLINE, '#', DASHLN,  IDWDTH, HLINE  )
C        C
C        C     Create table title line.
C        C
C              CALL REPMCW ( HTITLE, '#', ' S/C ACRONYM', 
C             .                                   ACWDTH, HTITLE )
C              CALL REPMCW ( HTITLE, '#', '   SPACECRAFT NAME  ', 
C             .                                   NMWDTH, HTITLE )
C              CALL REPMCW ( HTITLE, '#', ' S/C NAIF ID ',        
C             .                                   IDWDTH, HTITLE )
C        C
C        C     Create table record for MPF.
C        C
C              ACRNYM = '  MPF'
C              SCNAME = ' Mars Pathfinder'
C              SCID   = '     -53'
C              CALL REPMCW ( TRECRD, '#', ACRNYM, ACWDTH, TRECRD )
C              CALL REPMCW ( TRECRD, '#', SCNAME, NMWDTH, TRECRD )
C              CALL REPMCW ( TRECRD, '#', SCID,   IDWDTH, TRECRD )
C        C
C        C     Print table. 
C        C
C              WRITE (*,*) HLINE
C              WRITE (*,*) HTITLE
C              WRITE (*,*) HLINE
C              WRITE (*,*) TRECRD
C              WRITE (*,*) HLINE
C        
C              END 
C     
C        print the following output:
C
C              +-------------+----------------------+-------------+
C              | S/C ACRONYM |   SPACECRAFT NAME    | S/C NAIF ID |
C              +-------------+----------------------+-------------+
C              |  MPF        | Mars Pathfinder      |     -53     |
C              +-------------+----------------------+-------------+
C   
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V.Semenov   (JPL)
C
C$ Version
C
C-    Alpha Version 1.0.0, 29-JUL-1998 (BVS)
C
C-&
 
C$ Index_Entries
C
C     replace marker with character_string of a specified width
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               MRKPOS
      INTEGER               VLNGTH
      INTEGER               I 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REPMCW' )
      END IF
 
C
C     If MARKER is blank, no substitution is possible.
C
      IF ( MARKER .EQ. ' ' ) THEN
 
         OUT = IN
 
         CALL CHKOUT ( 'REPMCW')
         RETURN
 
      END IF
 
C
C     Locate the leftmost occurrence of MARKER, if there is one
C     (ignoring leading and trailing blanks). If MARKER is not
C     a substring of IN, no substitution can be performed.
C
      MRKPOS = INDEX (  IN,
     .                  MARKER ( FRSTNB(MARKER):LASTNB(MARKER) )  )
 
      IF  ( MRKPOS .EQ. 0 ) THEN
 
         OUT = IN
 
         CALL CHKOUT ( 'REPMCW' )
         RETURN
 
      END IF
 
C
C     Okay, MARKER is non-blank and has been found. Substitute string 
C     of required width.
C
      VLNGTH = LEN( VALUE ) 
      
      IF      ( WIDTH .LE. 0 ) THEN
   
C
C        In this case we just delete marker from input string.
C  
         CALL REMSUB ( IN,
     .                 MRKPOS,
     .                 MRKPOS + LASTNB(MARKER) - FRSTNB(MARKER),
     .                 OUT  )
         
      ELSE IF ( WIDTH .LE. VLNGTH ) THEN
      
C
C        Here we simply insert as many characters as needed into 
C        our output string.
C  
         CALL REPSUB ( IN,
     .                 MRKPOS,
     .                 MRKPOS + LASTNB(MARKER) - FRSTNB(MARKER),
     .                 VALUE(:WIDTH),
     .                 OUT  )
     
      ELSE 
                  
C
C        Here we insert all characters from VALUE and as many spaces 
C        as needed to make in our output string WIDTH wide.
C  
         CALL REPSUB ( IN,
     .                 MRKPOS,
     .                 MRKPOS + LASTNB(MARKER) - FRSTNB(MARKER),
     .                 VALUE(:VLNGTH),
     .                 OUT )
         
         IF ( MRKPOS + VLNGTH - 1 .LT. LEN(OUT) ) THEN
     
            DO I = 1, WIDTH - VLNGTH
            
               IF ( ( MRKPOS + VLNGTH - 1 ) .LT. LEN(OUT) ) THEN
            
                  CALL REPSUB ( OUT,
     .                          MRKPOS + VLNGTH,
     .                          MRKPOS + VLNGTH - 1,
     .                          ' ',
     .                          OUT  )
     
               END IF
     
            END DO
            
         END IF
      
      END IF
      
      CALL CHKOUT ( 'REPMCW' )
      RETURN
      END
