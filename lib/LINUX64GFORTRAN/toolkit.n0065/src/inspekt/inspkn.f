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
 
      SUBROUTINE INSPKN ( COMMND,  ERROR )
 
      IMPLICIT NONE
 
      CHARACTER*(*)         COMMND
      CHARACTER*(*)         ERROR ( 2 )

C     Nov 21, 1995 (WLT)
C
C     Removed SAVE and DISCARD commands as these are handled
C     by the generic Command loop code.
C
C     Oct 18, 1995 (WLT)
C
C     Added the ability to turn tracing off.
C
C     Aug 21, 1995 (WLT)
C
C     Modified the syntax of HELP to allow for multiple word help
C     requests.
C
C     This routine handles the delegation of tasks within the
C     the INSPEKT program.  It makes a preliminary check of the
C     syntax to see if it remotely resembles a recognizable
C     command.  It then passes the command (or the untested
C     portion of it on to a routine that is set up to deal
C     with the details of the command.
C
C     Spicelib functions
C
      INTEGER               RTRIM
      INTEGER               LTRIM
      INTEGER               ISRCHC
C
C     Error handling interface routines.
C
      LOGICAL               HAVE
C
C     META/2 Functions
C
      LOGICAL               M2XIST
 
C
C     Variables needed for syntax declarations.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               NUPPER
      PARAMETER           ( NUPPER =  9 )
 
      INTEGER               NSYN
      PARAMETER           ( NSYN   = NUPPER )
 
      INTEGER               SYNLEN
      PARAMETER           ( SYNLEN = 80 )
 
      CHARACTER*(WDSIZE)    SYNKEY ( LBCELL : NSYN )
      INTEGER               SYNPTR ( LBCELL : NSYN )
      CHARACTER*(SYNLEN)    SYNVAL ( LBCELL : NSYN )
 
C
C     The following are for special commands that will not be
C     processed by INSPKTN.
C
      INTEGER               NSP
      PARAMETER           ( NSP = 2 )
 
      CHARACTER*(8)         SPCIAL ( NSP )
 
C
C     Other Local Variables
C
      CHARACTER*(1)         BS
      INTEGER               E
      INTEGER               I
      INTEGER               L
      INTEGER               REST
 
      LOGICAL               FOUND
      LOGICAL               FIRST
      LOGICAL               OFF
 
 
C
C     Save everything
C
      SAVE
 
      DATA     FIRST     /.TRUE./
 
      DATA (   SYNVAL(I), I = 1, NUPPER )
     .     /   'SET[set]        (1:)#word[rest]',
     .         'SHOW[show]      (1:)#word[rest]',
     .         'LOAD[load]      (1:)#word[rest]',
     .         'UNLOAD[unload]  (1:)#word[rest]',
     .         'HELP[help] ',
     .         'HELP[help]      (1:)#word[rest]',
     .         'SELECT[find]    (1:)#word[rest]',
     .         'SAMPLE[find]    (1:)#word[rest]',
     .         'TRACEOFF[traceoff]' /
 
      DATA  SPCIAL / ' ', '?' /
      DATA  OFF    / .FALSE.  /
 
C
C     On the first pass set up the collection of recognized
C     syntax statements.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         BS    =  '@'
 
         DO I = 1, NUPPER
            CALL REPLCH ( SYNVAL(I), '#', BS, SYNVAL(I)        )
         END DO
 
         CALL M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
 
      END IF
 
C
C     Special commands are stopped here.  Right now there are
C     only two such commands: '?' and ' '.  Note, the error values
C     are NOT reset.
C
      L    = LTRIM ( COMMND)
      REST = RTRIM ( COMMND ) + 1
 
      IF ( ISRCHC( COMMND(L:REST), NSP, SPCIAL ) .GT. 0 ) THEN
         RETURN
      END IF
C
C     There are no errors yet.
C
      ERROR(1) = ' '
      ERROR(2) = ' '
 
C
C     Check the input command to see if it is recognizable
C
      CALL M2CHCK ( COMMND, SYNKEY, SYNPTR, SYNVAL, ERROR )
 
      IF ( HAVE(ERROR) ) THEN
         CALL PREFIX ( 'INSPKN:', 1, ERROR )
         RETURN
      END IF
 
C
C     If we get this far, we have a potentially legitimate
C     command.  We simply pass the command to someone responsible.
C
      CALL M2VGET ( 'rest', 1, FOUND, REST, E )
 
 
      IF      ( M2XIST('set') ) THEN
 
         CALL NSPSET ( COMMND(REST:), ERROR )
 
      ELSE IF ( M2XIST('show')      ) THEN
 
         CALL NSPSHO ( COMMND(REST:), ERROR )
 
      ELSE IF ( M2XIST('load')      ) THEN
 
         CALL NSPLD ( COMMND(REST:), ERROR )
 
      ELSE IF ( M2XIST('unload')    ) THEN
 
         CALL NSPULD ( COMMND(REST:), ERROR )
 
      ELSE IF ( M2XIST('help')      ) THEN
 
         CALL NSPHLP ( COMMND(REST:), ERROR )
 
      ELSE IF ( M2XIST('find')      ) THEN
 
         CALL NSPFND ( COMMND, ERROR )
 
      ELSE IF ( M2XIST('traceoff') ) THEN
 
         IF ( .NOT. OFF ) THEN
            OFF = .TRUE.
            CALL TRCOFF()
         END IF
 
      ELSE
 
         WRITE (*,*) 'Unrecognized or unimplemented'
         ERROR (1) = 'The input command is not yet supported.'
 
      END IF
 
      FOUND =  HAVE(ERROR)
 
      RETURN
 
      END
