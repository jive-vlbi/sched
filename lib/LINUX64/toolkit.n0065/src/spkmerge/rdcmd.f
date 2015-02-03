C$Procedure      RDCMD (Read command file)

      SUBROUTINE RDCMD (CMDFIL, CMDSYM, CMDPTR, CMDVAL)

C$ Abstract
C
C     Parse the command file.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INTEGER          LBCELL
      PARAMETER       (LBCELL = -5)

      CHARACTER*(*)    CMDFIL
      CHARACTER*(*)    CMDSYM (LBCELL: *)
      INTEGER          CMDPTR (LBCELL: *)
      CHARACTER*(*)    CMDVAL (LBCELL: *)

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CMDFIL     I   Name of command file.
C     CMDSYM,
C     CMDPTR,
C     CMDVAL     O   Command symbol table.
C
C$ Detailed_Input
C
C     CMDFIL     is the name of the command file.
C
C$ Detailed_Output
C
C     CMDSYM,
C     CMDPTR,
C     CMDVAL     is the command symbol table.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) An error is signaled if the file cannot be parsed
C        successfully.
C     
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    Beta Version 1.1.0, 17-JAN-2014 (BVS)
C
C        Increased LINLEN from 120 to 350 (350 = 300 characters for
C        value consistent with VALLEN in CPARSE_2 and the main program
C        + 50 more characters for the keyword name, =, and blanks.)
C
C        Increased maximum counts of child values in KWDS* from 300 to
C        1000 for all values.
C
C        Saved all variables.
C
C-    Beta Version 1.0.0, 26-JAN-1994 (MJS)
C
C-&
      
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Other functions
C

C
C     Local parameters
C
      INTEGER              SYMLEN
      PARAMETER           (SYMLEN = 32)

      INTEGER              MAXSYM
      PARAMETER           (MAXSYM = 20)
      
      INTEGER              MAXVAL
      PARAMETER           (MAXVAL = 20)
      
      INTEGER              VALLEN
      PARAMETER           (VALLEN = 32)

      INTEGER              LINLEN
      PARAMETER           (LINLEN = 350)

      INTEGER              ERRLEN
      PARAMETER           (ERRLEN = 160)
      
C
C     Local variables
C
      CHARACTER*(SYMLEN)   KWDS1  (2)
      CHARACTER*(SYMLEN)   KWDS2  (5)
      CHARACTER*(SYMLEN)   KWDS3  (3)
      CHARACTER*(SYMLEN)   KWDS4  (1)
      CHARACTER*(LINLEN)   LINE
      CHARACTER*(ERRLEN)   REASON

      CHARACTER*(SYMLEN)   TABSYM (LBCELL: MAXSYM)
      INTEGER              TABPTR (LBCELL: MAXSYM)
      CHARACTER*(VALLEN)   TABVAL (LBCELL: MAXVAL)

      INTEGER              IOSTAT
      INTEGER              UNIT
      INTEGER              LINNUM
      
      LOGICAL              EOF
      LOGICAL              ERR

C
C     Save all.
C
      SAVE

C
C     Initial values
C
      DATA KWDS1        / 'LEAPSECONDS_KERNEL  1  1',
     .                    'SPK_KERNEL          1  1000' /

      DATA KWDS2        / 'SOURCE_SPK_KERNEL   1  1000',
     .                    'LOG_FILE            0  1',
     .                    'BODIES              0  1',
     .                    'BEGIN_TIME          0  1000',
     .                    'INCLUDE_TEXT_FILE   0  1000' /
      
      DATA KWDS3        / 'BODIES              0  1',
     .                    'BEGIN_TIME          0  1000',
     .                    'INCLUDE_COMMENTS    0  1'    /

      DATA KWDS4        / 'END_TIME            1  1'    /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ('RDCMD')
      END IF

C
C     Initialize the parser.
C     
      CALL SSIZEC (MAXSYM, TABSYM)
      CALL SSIZEI (MAXSYM, TABPTR)
      CALL SSIZEC (MAXVAL, TABVAL)
      
      CALL SYPUTC ('HEAD',              KWDS1,  2,
     .                                  TABSYM, TABPTR, TABVAL)
      CALL SYPUTC ('SPK_KERNEL',        KWDS2,  5,
     .                                  TABSYM, TABPTR, TABVAL)
      CALL SYPUTC ('SOURCE_SPK_KERNEL', KWDS3,  3,
     .                                  TABSYM, TABPTR, TABVAL)
      CALL SYPUTC ('BEGIN_TIME',        KWDS4,  1,
     .                                  TABSYM, TABPTR, TABVAL)
      
      CALL INITCP (TABSYM, TABPTR, TABVAL, 'HEAD')

C
C     Open the command file, and parse its contents
C     
      CALL TXTOPR (CMDFIL, UNIT)

      EOF = .FALSE.
      ERR = .FALSE.
      
      DO WHILE (.NOT. EOF .AND. .NOT. ERR) 
      
         READ (UNIT, FMT='(A)', IOSTAT=IOSTAT) LINE
         EOF = IOSTAT .NE. 0
         CALL EVALCP (LINE, EOF, CMDSYM, CMDPTR, CMDVAL, ERR)
         
      END DO

      IF (ERR) THEN
         CALL CPERR  (REASON,      LINNUM)
         CALL REPMI  (REASON, '#', LINNUM, REASON)
         CALL PREFIX (':',     1,  REASON)
         CALL PREFIX (CMDFIL,  0,  REASON)
         
         CALL SETMSG (REASON                )
         CALL SIGERR ('SPICE(CMDPARSEERROR)')
         CALL CHKOUT ('RDCMD'               )
         RETURN
      END IF

      CALL CHKOUT ('RDCMD')
      RETURN
      END
