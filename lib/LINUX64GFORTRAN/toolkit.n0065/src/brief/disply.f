C$Procedure DISPLY ( BRIEF Display Summary )
 
      SUBROUTINE DISPLY ( FMTPIC, TDSP,   GDSP, SDSP, OBNAM, OBJLIS,
     .                    WINSYM, WINPTR, WINVAL, TIMTYP, KERTYP )

C$ Abstract
C
C     Display BRIEF summary.
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

      INCLUDE              'brief.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         FMTPIC
      LOGICAL               TDSP
      LOGICAL               GDSP
      LOGICAL               SDSP
      LOGICAL               OBNAM
      INTEGER               OBJLIS ( LBCELL : * )
 
      CHARACTER*(*)         WINSYM ( LBCELL : * )
      INTEGER               WINPTR ( LBCELL : * )
      DOUBLE PRECISION      WINVAL ( LBCELL : * )
 
      CHARACTER*(*)         TIMTYP
      CHARACTER*(*)         KERTYP

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FMTPIC     I   Body name/ID format picture (see BRIEF.PGM)
C     TDSP       I   Tabular display flag
C     GDSP       I   Grouping display flag
C     SDSP       I   Time-sorted tabular display flag
C     OBNAM      I   Name ordering flag
C     OBJLIS     I   List of object (?)
C     WINSYM     I   Symbol table with object attributes (?)
C     WINPTR     I   Symbol table with object attributes (?)
C     WINVAL     I   Symbol table with object attributes (?)
C     TIMTYP     I   Output time type (see DISTIM.FOR)
C     KERTYP     I   Kernel type (SPK, PCK)
C
C$ Detailed_Input
C
C     See Brief_I/O.
C
C$ Detailed_Output
C
C     This routine return no outputs. Instead it prints summary of 
C     provided input information to STDOUT.
C
C$ Parameters
C
C     LBCELL.
C
C$ Exceptions
C
C     1) Errors may be signaled by routines in the calling tree of 
C        this routine.
C
C$ Files
C
C     TBD.
C
C$ Particulars
C
C     TBD.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This routine must not be called by any routines except BRIEF's
C     main program.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    BRIEF Version 3.0.0, 08-SEP-2010 (BVS)
C
C        Added sorted-by-time tabular output (-s). Changed calling
C        sequence: add sorted-by-time flag (SDSP).
C
C-    BRIEF Version 2.0.0, 22-OCT-2007 (BVS)
C
C        Added output time type to the argument list. Changed to 
C        call DISTIM to format output time and provide time system
C        label for the summary table header.
C
C-    BRIEF Version 1.0.0, 14-MAR-1996 (WLT)
C
C        Bill's initial version.
C
C-&
 
 
C$ Index_Entries
C
C     display summary by BRIEF
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               OBJACT
      INTEGER               OBJSIZ
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )
 
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 8 )
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 64 )
 
C
C     Local Variables.
C
      CHARACTER*(LNSIZE)    HEADER ( 2 )
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(LNSIZE)    REST
      CHARACTER*(SMWDSZ)    P1
      CHARACTER*(SMWDSZ)    P2
      CHARACTER*(SMWDSZ)    WD
      CHARACTER*(SMWDSZ)    TIMLBL
      CHARACTER*(WDSIZE)    TIMSTR
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    NAMES  ( LBCELL : MAXBOD )

      CHARACTER*(WDSIZE)    SNAME
 
      DOUBLE PRECISION      SSTART ( WINRM / 2 )
      DOUBLE PRECISION      SSTOP  ( WINRM / 2 )

      INTEGER               OBJ1   ( WINRM / 2 )
      INTEGER               OBJ2   ( WINRM / 2 )
      INTEGER               IORDER ( WINRM / 2 )
 
      INTEGER               STOTAL 

      DOUBLE PRECISION      FILWIN ( LBCELL : LRGWIN )
      DOUBLE PRECISION      LSTWIN ( LBCELL : LRGWIN )
 
      INTEGER               B
      INTEGER               E
      INTEGER               I
      INTEGER               J
      INTEGER               N1
      INTEGER               N2
      INTEGER               NGROUP
      INTEGER               NLINES
      INTEGER               NOBJ
      INTEGER               NPLINE
      INTEGER               OBJ    ( 2 )
      INTEGER               OBJTMP ( 2 )
      INTEGER               OBJECT ( 3 )
      INTEGER               OBJCT2 ( 3 )
      INTEGER               OBJN   ( 2 )
      INTEGER               REMAIN
      INTEGER               S
      INTEGER               SIZE
      INTEGER               SOBJ
      INTEGER               START
      INTEGER               WIDEST
 
      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               GROUP
      LOGICAL               SAME

C
C     SPICELIB Calls
C
      INTEGER               TOUCHI



C
C     Saved variables
C
C     The SAVE statement that appears here causes f2c to create
C     local variables with static duration.  This enables the CSPICE 
C     version of brief to run under cygwin.
C
      SAVE

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DISPLY' )
      END IF
 
C
C     Get time system label for the table header.
C
      CALL DISTIM( TIMTYP, 0.D0, TIMLBL, TIMSTR )

C
C     Set local grouping flag.
C     
      GROUP = .NOT. TDSP .OR. GDSP

C
C     First take apart the format picture to see what
C     the various components are.
C
      CALL NEXTWD ( FMTPIC, P1, REST )
      CALL NEXTWD ( REST,   WD, REST )
      CALL NEXTWD ( REST,   P2, REST )
 
      SIZE = 1
      IF ( P2 .NE. ' ' ) THEN
         SIZE = 3
      END IF
 
 
 
 
 
C
C     Find out the width of the widest name.
C
      NOBJ = OBJACT(OBJLIS)
      SOBJ = OBJSIZ(OBJLIS)
C
C     If we don't have any objects to display then
C     we just return.
C
      IF ( NOBJ .EQ. 0 ) THEN
         CALL CHKOUT ( 'DISPLY' )
         RETURN
      END IF
 
 
      CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
      WIDEST = 0
      DO WHILE ( FOUND )
 
         CALL OBJGET ( OBJ,    OBJLIS,   OBJECT              )
         CALL OBJNXT ( OBJ,    OBJLIS,   OBJN,         FOUND )
         CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, NAME )
 
         WIDEST = MAX ( WIDEST, RTRIM(NAME))
         OBJ(1) = OBJN( 1 )
         OBJ(2) = OBJN( 2 )
 
      END DO
 
C
C     Are we going to group by or sort by time window? If not, this is
C     pretty easy. Just display tabular output.
C
      IF ( TDSP .AND. .NOT. GDSP .AND. .NOT. SDSP ) THEN
 
         S = WIDEST + 3
         E = S + 32
 
         IF      ( KERTYP .EQ. 'SPK' ) THEN
            LINE(1:) = 'Bodies'
         ELSE IF ( KERTYP .EQ. 'PCK' ) THEN
            LINE(1:) = 'Frames'
         ELSE
            LINE(1:) = 'IDs'
         END IF
         LINE(S:) = 'Start of Interval (' 
     .              // TIMLBL(:RTRIM(TIMLBL)) // ')'
         LINE(E:) = 'End of Interval ('
     .              // TIMLBL(:RTRIM(TIMLBL)) // ')'
 
         CALL WRITIT ( LINE )
 
         LINE(1:) = '-------'
         LINE(S:) = '-----------------------------'
         LINE(E:) = '-----------------------------'
 
         CALL WRITIT ( LINE )
 
         CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
         N1 = 0
 
         DO WHILE ( FOUND )
 
            LINE = ' '
            CALL OBJGET ( OBJ,    OBJLIS,   OBJECT              )
            CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, LINE )
            CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME )
            CALL SYGETD ( NAME,   WINSYM,
     .                            WINPTR,
     .                            WINVAL, N2,   FILWIN(1), FOUND )
 
            IF ( N2 .EQ. N1 ) THEN
 
               SAME = .TRUE.
               I    =  1
               DO WHILE ( SAME .AND. I .LE. N1 )
                  SAME = FILWIN(I) .EQ. LSTWIN(I)
                  I    = I + 1
               END DO
 
            ELSE
               SAME = .FALSE.
            END IF
 
 
            IF ( .NOT. SAME ) THEN
 
               DO I = 1, N2, 2
                  CALL DISTIM ( TIMTYP, FILWIN(I  ), TIMLBL, LINE(S:) )
                  CALL DISTIM ( TIMTYP, FILWIN(I+1), TIMLBL, LINE(E:) )
                  CALL WRITIT ( LINE )
                  LINE = ' '
                  LSTWIN(I)   = FILWIN(I)
                  LSTWIN(I+1) = FILWIN(I+1)
                  N1          = N2
               END DO
 
            ELSE
 
               LINE(S+12:) = 'Same coverage as previous object '
               CALL WRITIT ( LINE )
 
            END IF
 
            CALL OBJNXT ( OBJ, OBJLIS, OBJTMP, FOUND )

            OBJ(1) = TOUCHI( OBJTMP(1) )
            OBJ(2) = TOUCHI( OBJTMP(2) )
 
 
         END DO

C
C     Were we asked to do tabular output sorted by start time for each 
C     SPK body or PCK frame?
C
      ELSE IF ( TDSP .AND. SDSP ) THEN
 
         S = WIDEST + 3
         E = S + 32
 
         IF      ( KERTYP .EQ. 'SPK' ) THEN
            LINE(1:) = 'Bodies'
         ELSE IF ( KERTYP .EQ. 'PCK' ) THEN
            LINE(1:) = 'Frames'
         ELSE
            LINE(1:) = 'IDs'
         END IF
         LINE(S:) = 'Start of Interval (' 
     .              // TIMLBL(:RTRIM(TIMLBL)) // ')'
         LINE(E:) = 'End of Interval ('
     .              // TIMLBL(:RTRIM(TIMLBL)) // ')'
 
         CALL WRITIT ( LINE )
 
         LINE(1:) = '-------'
         LINE(S:) = '-----------------------------'
         LINE(E:) = '-----------------------------'
 
         CALL WRITIT ( LINE )
 
         CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
         N1 = 0
 
         DO WHILE ( FOUND )
 
C
C           Get and buffer individual coverage intervals for this
C           object.
C
            CALL OBJGET ( OBJ,    OBJLIS,   OBJECT              )
            CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME )
            CALL SYGETD ( NAME,   WINSYM,
     .                            WINPTR,
     .                            WINVAL, N2,   FILWIN(1), FOUND )

            STOTAL = 0
            DO I = 1, N2, 2
               STOTAL = STOTAL + 1
               OBJ1  (STOTAL) = OBJ(1)
               OBJ2  (STOTAL) = OBJ(2)
               SSTART(STOTAL) = FILWIN(I  )
               SSTOP (STOTAL) = FILWIN(I+1)
            END DO

C
C           Buffer coverage intervals for subsequent objects as long as
C           these objects have the same ID or we run out of objects.
C
            CALL OBJNXT ( OBJ, OBJLIS, OBJTMP, FOUND )

            DO WHILE ( FOUND )

               CALL OBJGET ( OBJTMP, OBJLIS, OBJCT2 )

               IF ( OBJECT(1) .EQ. OBJCT2(1) ) THEN

                  OBJ(1) = OBJTMP(1)
                  OBJ(2) = OBJTMP(2)

                  CALL OBJGET ( OBJ,    OBJLIS,   OBJECT               )
                  CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME )
                  CALL SYGETD ( NAME,   WINSYM,
     .                                  WINPTR,
     .                                  WINVAL, N2,   FILWIN(1), FOUND )

                  DO I = 1, N2, 2
                     STOTAL = STOTAL + 1
                     OBJ1  (STOTAL) = OBJ(1)
                     OBJ2  (STOTAL) = OBJ(2)
                     SSTART(STOTAL) = FILWIN(I  )
                     SSTOP (STOTAL) = FILWIN(I+1)
                  END DO

                  CALL OBJNXT ( OBJ, OBJLIS, OBJTMP, FOUND )

               ELSE
                  FOUND = .FALSE.
               END IF
               
            END DO

C
C           Re-order buffered information by start time.
C
            CALL ORDERD ( SSTART, STOTAL, IORDER )

            CALL REORDI ( IORDER, STOTAL, OBJ1 )
            CALL REORDI ( IORDER, STOTAL, OBJ2 )
            CALL REORDD ( IORDER, STOTAL, SSTART )
            CALL REORDD ( IORDER, STOTAL, SSTOP  )

C
C           Loop through the buffer and print its contents.
C
            J         = 1
            OBJTMP(1) = OBJ1(J)
            OBJTMP(2) = OBJ2(J)
            CALL OBJGET ( OBJTMP, OBJLIS, OBJECT )
            CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, LINE )
            FILWIN(1) = SSTART(J)
            FILWIN(2) = SSTOP (J)
            N2        = 2

            J = 2
            DO WHILE ( J .LE. STOTAL )

               OBJTMP(1) = OBJ1(J)
               OBJTMP(2) = OBJ2(J)
               CALL OBJGET ( OBJTMP, OBJLIS, OBJECT )
               CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, 
     .                                                          SNAME )

               IF ( LINE .NE. SNAME ) THEN

                  IF ( N2 .EQ. N1 ) THEN

                     SAME = .TRUE.
                     I    =  1
                     DO WHILE ( SAME .AND. I .LE. N1 )
                        SAME = FILWIN(I) .EQ. LSTWIN(I)
                        I    = I + 1
                     END DO

                  ELSE
                     SAME = .FALSE.
                  END IF

                  IF ( .NOT. SAME ) THEN

                     DO I = 1, N2, 2
                        CALL DISTIM ( TIMTYP, FILWIN(I  ), TIMLBL, 
     .                                                     LINE(S:) )
                        CALL DISTIM ( TIMTYP, FILWIN(I+1), TIMLBL, 
     .                                                     LINE(E:) )
                        CALL WRITIT ( LINE )
                        LINE = ' '
                        LSTWIN(I)   = FILWIN(I)
                        LSTWIN(I+1) = FILWIN(I+1)
                        N1          = N2
                     END DO

                  ELSE

                     LINE(S+12:) = 'Same coverage as previous object '
                     CALL WRITIT ( LINE )
                     
                  END IF

                  LINE      = SNAME
                  FILWIN(1) = SSTART(J)
                  FILWIN(2) = SSTOP (J)
                  N2        = 2

               ELSE

                  FILWIN(N2+1) = SSTART(J)
                  FILWIN(N2+2) = SSTOP (J)
                  N2           = N2 + 2

               END IF

               J = J + 1

            END DO

C
C           Print information for the last object from the buffered
C           set.
C
            IF ( N2 .EQ. N1 ) THEN

               SAME = .TRUE.
               I    =  1
               DO WHILE ( SAME .AND. I .LE. N1 )
                  SAME = FILWIN(I) .EQ. LSTWIN(I)
                  I    = I + 1
               END DO

            ELSE
               SAME = .FALSE.
            END IF

            IF ( .NOT. SAME ) THEN

               DO I = 1, N2, 2
                  CALL DISTIM ( TIMTYP, FILWIN(I  ), TIMLBL, LINE(S:) )
                  CALL DISTIM ( TIMTYP, FILWIN(I+1), TIMLBL, LINE(E:) )
                  CALL WRITIT ( LINE )
                  LINE = ' '
                  LSTWIN(I)   = FILWIN(I)
                  LSTWIN(I+1) = FILWIN(I+1)
                  N1          = N2
               END DO

            ELSE

               LINE(S+12:) = 'Same coverage as previous object '
               CALL WRITIT ( LINE )

            END IF

C
C           Move onto the next object.
C
            CALL OBJNXT ( OBJ, OBJLIS, OBJTMP, FOUND )

            OBJ(1) = TOUCHI( OBJTMP(1) )
            OBJ(2) = TOUCHI( OBJTMP(2) )
  
         END DO
 
C
C     Were we asked to do tabular output grouped by similar coverages?
C
      ELSE IF ( TDSP .AND. GDSP ) THEN
 
         S = WIDEST + 3
         E = S + 32
 
         IF      ( KERTYP .EQ. 'SPK' ) THEN
            LINE(1:) = 'Bodies'
         ELSE IF ( KERTYP .EQ. 'PCK' ) THEN
            LINE(1:) = 'Frames'
         ELSE
            LINE(1:) = 'IDs'
         END IF
         LINE(S:) = 'Start of Interval ('
     .              // TIMLBL(:RTRIM(TIMLBL)) // ')'
         LINE(E:) = 'End of Interval ('
     .              // TIMLBL(:RTRIM(TIMLBL)) // ')'

 
         CALL WRITIT ( LINE )
 
         LINE(1:) = '-------'
         LINE(S:) = '-----------------------------'
         LINE(E:) = '-----------------------------'
 
         CALL WRITIT ( LINE )
 
         CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
 
         DO WHILE ( FOUND )
 
            LINE = ' '
            CALL OBJGET ( OBJ,    OBJLIS,   OBJECT              )
            CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, LINE )
            CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME )
            CALL SYGETD ( NAME,   WINSYM,
     .                            WINPTR,
     .                            WINVAL, N1,   FILWIN(1), FOUND )
 
            DO I = 1, N1, 2
               CALL DISTIM ( TIMTYP, FILWIN(I  ), TIMLBL, LINE(S:) )
               CALL DISTIM ( TIMTYP, FILWIN(I+1), TIMLBL, LINE(E:) )
               CALL WRITIT ( LINE )
               LINE = ' '
               LSTWIN(I)   = FILWIN(I)
               LSTWIN(I+1) = FILWIN(I+1)
            END DO
 
            CALL OBJNXT ( OBJ, OBJLIS, OBJN, FND )
            CALL OBJREM ( OBJ, OBJLIS )
            OBJ(1) = OBJN(1)
            OBJ(2) = OBJN(2)
 
            DO WHILE ( FND )
 
               LINE = ' '
               CALL OBJGET ( OBJ,    OBJLIS,   OBJECT              )
               CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, 
     .                       LINE )
               CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME )
               CALL SYGETD ( NAME,   WINSYM,
     .                               WINPTR,
     .                               WINVAL, N2,   FILWIN(1), FOUND )
 
               IF ( N2 .EQ. N1 ) THEN
 
                  SAME = .TRUE.
                  I    =  1
                  DO WHILE ( SAME .AND. I .LE. N1 )
                     SAME = FILWIN(I) .EQ. LSTWIN(I)
                     I    = I + 1
                  END DO
 
               ELSE
                  SAME = .FALSE.
               END IF
 
 
               IF ( SAME ) THEN
 
                  LINE(S+12:) = 'Same coverage as previous object '
                  CALL WRITIT ( LINE )
 
               END IF
 
               CALL OBJNXT ( OBJ, OBJLIS, OBJN, FND )
 
               IF ( SAME ) THEN
                  CALL OBJREM ( OBJ, OBJLIS )
               END IF

               OBJ(1) = OBJN(1)
               OBJ(2) = OBJN(2)
 
            END DO
 
            CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
         END DO
 
 
C
C     We were not asked to do tabular output. So do a regular one.
C
      ELSE
 
         CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
         DO WHILE ( FOUND )
 
            CALL SSIZEC ( MAXBOD, NAMES )
            CALL OBJGET ( OBJ,    OBJLIS,   OBJECT )
            CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, NAME )
            CALL APPNDC ( NAME,   NAMES )
C
C           Look up the window associated with this object.
C
            CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME )
            CALL SYGETD ( NAME,   WINSYM,
     .                            WINPTR,
     .                            WINVAL, N1,   LSTWIN(1), FND   )
C
C           Fetch the next object.
C
            CALL OBJNXT ( OBJ, OBJLIS, OBJN, FND )
            CALL OBJREM ( OBJ, OBJLIS )
 
            OBJ(1) = OBJN(1)
            OBJ(2) = OBJN(2)
 
            DO WHILE ( FND )
 
               CALL OBJGET ( OBJ,    OBJLIS, OBJECT )
 
               CALL MAKNAM ( OBJECT, SOBJ, OBNAM, KERTYP, NAME  )
               CALL SYGETD ( NAME,   WINSYM,
     .                               WINPTR,
     .                               WINVAL, N2,   FILWIN(1),   FND   )
C
C              See if this window is the same as the current
C              window under considerations.
C
               IF ( N1 .EQ. N2 ) THEN
                  SAME = .TRUE.
                  I    =  1
 
                  DO WHILE ( SAME .AND. I .LE. N1 )
                     SAME = FILWIN(I) .EQ. LSTWIN(I)
                     I    = I+1
                  END DO
               ELSE
                  SAME = .FALSE.
               END IF
 
               CALL OBJNXT ( OBJ, OBJLIS, OBJN, FND )
 
               IF ( SAME ) THEN
                  CALL OBJREM ( OBJ,    OBJLIS )
                  CALL PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, 
     .                          NAME )
                  CALL APPNDC ( NAME,   NAMES )
               END IF
 
               OBJ(1) = OBJN(1)
               OBJ(2) = OBJN(2)
 
            END DO
 
            NGROUP = CARDC ( NAMES )
 
            IF ( NGROUP .EQ. 1 ) THEN
               IF      ( KERTYP .EQ. 'SPK' ) THEN
                  LINE  = 'Body: '
                  START =  7
               ELSE IF ( KERTYP .EQ. 'PCK' ) THEN
                  LINE  = 'Frame: '
                  START =  8
               ELSE
                  LINE  = 'ID: '
                  START =  5
               END IF
            ELSE
               IF      ( KERTYP .EQ. 'SPK' ) THEN
                  LINE  = 'Bodies: '
                  START =  9
               ELSE IF ( KERTYP .EQ. 'PCK' ) THEN
                  LINE  = 'Frames: '
                  START =  9
               ELSE
                  LINE  = 'IDs: '
                  START =  6
               END IF
            END IF
 
            NPLINE = 1 + (( 80 - WIDEST - START )/( WIDEST + 2 ))
 
            CALL RMAINI  ( NGROUP, NPLINE, NLINES, REMAIN )
 
            IF ( REMAIN .NE. 0 ) THEN
               NLINES = NLINES + 1
            END IF
 
            DO J = 1, NLINES
 
               B = START
 
               DO I = J, NGROUP, NLINES
                  LINE(B:) = NAMES(I)
                  B        = B + WIDEST + 2
               END DO
 
               CALL WRITIT ( LINE )
               LINE = ' '
 
            END DO
 
            S = START
            E = START + 36
 
            HEADER(1) = ' '
            HEADER(2) = ' '

            HEADER(1)(S:) = 'Start of Interval (' 
     .                      // TIMLBL(:RTRIM(TIMLBL)) // ')'
            HEADER(1)(E:) = 'End of Interval ('
     .                      // TIMLBL(:RTRIM(TIMLBL)) // ')'

            HEADER(2)(S:) = '-----------------------------'
            HEADER(2)(E:) = '-----------------------------'
 
            CALL WRITIT ( HEADER(1) )
            CALL WRITIT ( HEADER(2) )
 
            DO I = 1, N1, 2
               LINE = ' '
               CALL DISTIM ( TIMTYP, LSTWIN(I),   TIMLBL, LINE(S:) )
               CALL DISTIM ( TIMTYP, LSTWIN(I+1), TIMLBL, LINE(E:) )
               CALL WRITIT ( LINE )
            END DO
 
            CALL WRITIT ( ' ' )
 
            CALL OBJNTH ( OBJLIS, 1, OBJ, FOUND )
 
         END DO
 
      END IF
 
C
C     All done.
C
      CALL CHKOUT ( 'DISPLY' )
      RETURN

      END
