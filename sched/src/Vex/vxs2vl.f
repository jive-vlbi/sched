      SUBROUTINE VXS2VL( NDATCH, BBC, BBCSD, MBITSC, BSAMPL, NGROUP, 
     1    S2MODE, VALID, NS2USD, IS2USD )
C 
C     Routine specific for the VEX extension of SCHED. 
C     Figures out S2 mode name and checks validity
C     This subroutine is specific for S2 with VLBA DAR,
C     it uses the table supplied by Novikov.
C     By H.J. van Langevelde, JIVE, 250996
C     If TAPEMODE is set then checked for consitency
C     else set on return.
C 
      INTEGER NDATCH, BSAMPL, NGROUP, NS2USD, BBC(NDATCH)
      REAL MBITSC
      CHARACTER S2MODE*7, IS2USD(16)*4, BBCSD(NDATCH)*1, MSGTXT*80
      LOGICAL VALID
C
      INTEGER IMODE, ICH, JCH, MODBBC
      CHARACTER ACTIVE*3, MODSB*1, MODBIT*1
      LOGICAL ALLOK, FOUND, OKRATE
C      
      INTEGER MS2, MUCH
      PARAMETER (MS2 = 31, MUCH = 16)
      CHARACTER VLBCH(MUCH)*4, S2M(MS2)*7
      INTEGER CHBW(MS2), NBIT(MS2), GRP(MS2)
      INTEGER NCH(MS2), UCH(MUCH,MS2)
      DATA VLBCH / '1us', '1um', '1ls', '1lm', 
     1    '3us', '3um', '3ls', '3lm',
     2    '2us', '2um', '2ls', '2lm', 
     3    '4us', '4um', '4ls', '4lm' /
C
C     VLBA DAR specific cabling
C     ignore all special modes, do all for J01
C     max 8 data channels, max 16 str, from e-mail by Novikov
C
C     ---------mode----------------------------
      DATA NCH(1), CHBW(1), NBIT(1)               / 1, 16, 1 /
      DATA S2M(1), GRP(1)                         / '32x1-1', 4 /
      DATA UCH(1,1)                               / 0 /
C     ---------mode----------------------------
      DATA NCH(2), CHBW(2), NBIT(2)               / 1, 8, 1 /
      DATA S2M(2), GRP(2)                         / '16x1-1', 8  /
      DATA UCH(1,2)                               / 0 /
C     ---------mode----------------------------
      DATA NCH(3), CHBW(3), NBIT(3)               / 1, 16, 2 /
      DATA S2M(3), GRP(3)                         / '32x2-2', 2  /
      DATA UCH(1,3),UCH(2,3)                      / 0, 1 /
C     ---------mode----------------------------
      DATA NCH(4), CHBW(4), NBIT(4)               / 1, 8, 2 /
      DATA S2M(4), GRP(4)                         / '16x2-2', 4  /
      DATA UCH(1,4),UCH(2,4)                      / 0, 1 /
C     ---------mode----------------------------
      DATA NCH(5), CHBW(5), NBIT(5)               / 1, 4, 2 /
      DATA S2M(5), GRP(5)                         / '8x2-2', 8  /
      DATA UCH(1,5),UCH(2,5)                      / 0, 1 /
C     ---------mode----------------------------
      DATA NCH(6), CHBW(6), NBIT(6)               / 2, 16, 2 /
      DATA S2M(6), GRP(6)                         / '32x4-2', 1  /
      DATA UCH(1,6),UCH(2,6),UCH(3,6),UCH(4,6)    / 0, 1, 2, 3 /
C     ---------mode----------------------------
      DATA NCH(7), CHBW(7), NBIT(7)               / 2, 8, 2 /
      DATA S2M(7), GRP(7)                         / '16x4-2', 2  /
      DATA UCH(1,7),UCH(2,7),UCH(3,7),UCH(4,7)    / 0, 1, 2, 3 /
C     ---------mode----------------------------
      DATA NCH(8), CHBW(8), NBIT(8)               / 2, 4, 2 /
      DATA S2M(8), GRP(8)                         / '8x4-2', 4  /
      DATA UCH(1,8),UCH(2,8),UCH(3,8),UCH(4,8)    / 0, 1, 2, 3 /
C     ---------mode----------------------------
      DATA NCH(9), CHBW(9), NBIT(9)               / 2, 2, 2 /
      DATA S2M(9), GRP(9)                         / '4x4-2', 8  /
      DATA UCH(1,9),UCH(2,9),UCH(3,9),UCH(4,9)    / 0, 1, 2, 3 /
C     ---------mode----------------------------
      DATA NCH(10), CHBW(10), NBIT(10)            / 2, 16, 1 /
      DATA S2M(10), GRP(10)                       / '32s2-1', 2  /
      DATA UCH(1,10),UCH(2,10)                    / 0, 2 /
C     ---------mode----------------------------
      DATA NCH(11), CHBW(11), NBIT(11)            / 2, 8, 1 /
      DATA S2M(11), GRP(11)                       / '16s2-1', 4  /
      DATA UCH(1,11),UCH(2,11)                    / 0, 2 /
C     ---------mode----------------------------
      DATA NCH(12), CHBW(12), NBIT(12)            / 2, 4, 1 /
      DATA S2M(12), GRP(12)                       / '8s2-1', 8  /
      DATA UCH(1,12),UCH(2,12)                    / 0, 2 /
C     ---------mode----------------------------
      DATA NCH(13), CHBW(13), NBIT(13)            / 4, 16, 1 /
      DATA S2M(13), GRP(13)                       / '32s4-1',1  /
      DATA UCH(1,13),UCH(2,13),UCH(3,13),UCH(4,13)/ 0, 2, 4, 6 /
C     ---------mode----------------------------
      DATA NCH(14), CHBW(14), NBIT(14)            / 2, 16, 2 /
      DATA S2M(14), GRP(14)                       / '32a4-2', 1  /
      DATA UCH(1,14),UCH(2,14),UCH(3,14),UCH(4,14)/ 0, 1, 4, 5 /
C     ---------mode----------------------------
      DATA NCH(15), CHBW(15), NBIT(15)            / 2, 16, 2 /
      DATA S2M(15), GRP(15)                       / '32b4-2', 1  /
      DATA UCH(1,15),UCH(2,15),UCH(3,15),UCH(4,15)/ 2, 3, 6, 7 /
C     ---------mode----------------------------
      DATA NCH(16), CHBW(16), NBIT(16)            / 2, 8, 1 /
      DATA S2M(16), GRP(16)                       / '16i4-1', 2  /
      DATA UCH(1,16),UCH(2,16)                    / 0, 2 /
C     ---------mode----------------------------
      DATA NCH(17), CHBW(17), NBIT(17)            / 2, 4, 1 /
      DATA S2M(17), GRP(17)                       / '8i4-1', 4  /
      DATA UCH(1,17),UCH(2,17)                    / 0, 2 /
C     ---------mode----------------------------
      DATA NCH(18), CHBW(18), NBIT(18)            / 4, 8, 2 /
      DATA S2M(18), GRP(18)                       / '16x8-2', 1  /
      DATA UCH(1,18),UCH(2,18),UCH(3,18),UCH(4,18)/ 0, 1, 2, 3 /
      DATA UCH(5,18),UCH(6,18),UCH(7,18),UCH(8,18)/ 4, 5, 6, 7 /
C     ---------mode----------------------------
      DATA NCH(19), CHBW(19), NBIT(19)            / 4, 4, 2 /
      DATA S2M(19), GRP(19)                       / '8x8-2', 2  /
      DATA UCH(1,19),UCH(2,19),UCH(3,19),UCH(4,19)/ 0, 1, 2, 3 /
      DATA UCH(5,19),UCH(6,19),UCH(7,19),UCH(8,19)/ 4, 5, 6, 7 /
C     ---------mode----------------------------
      DATA NCH(20), CHBW(20), NBIT(20)            / 4, 2, 2 /
      DATA S2M(20), GRP(20)                       / '4x8-2', 4  /
      DATA UCH(1,20),UCH(2,20),UCH(3,20),UCH(4,20)/ 0, 1, 2, 3 /
      DATA UCH(5,20),UCH(6,20),UCH(7,20),UCH(8,20)/ 4, 5, 6, 7 /
C     ---------mode----------------------------
      DATA NCH(21), CHBW(21), NBIT(21)            / 8, 8, 1 /
      DATA S2M(21), GRP(21)                       / '16i8-1', 1  /
      DATA UCH(1,21),UCH(2,21),UCH(3,21),UCH(4,21)/ 0, 2, 4, 6 /
      DATA UCH(5,21),UCH(6,21),UCH(7,21),UCH(8,21)/ 8, 10, 12, 14 /
C     ---------mode----------------------------
      DATA NCH(22), CHBW(22), NBIT(22)            / 8, 4, 1 /
      DATA S2M(22), GRP(22)                       / '8i8-1', 2  /
      DATA UCH(1,22),UCH(2,22),UCH(3,22),UCH(4,22)/ 0, 2, 4, 6 /
      DATA UCH(5,22),UCH(6,22),UCH(7,22),UCH(8,22)/ 8, 10, 12, 14 /
C     ---------mode----------------------------
      DATA NCH(23), CHBW(23), NBIT(23)            / 8, 2, 1 /
      DATA S2M(23), GRP(23)                       / '4i8-1', 4  /
      DATA UCH(1,23),UCH(2,23),UCH(3,23),UCH(4,23)/ 0, 2, 4, 6 /
      DATA UCH(5,23),UCH(6,23),UCH(7,23),UCH(8,23)/ 8, 10, 12, 14 /
C     ---------mode----------------------------
      DATA NCH(24), CHBW(24), NBIT(24)            / 4, 8, 2 /
      DATA S2M(24), GRP(24)                       / '16p8-2', 1  /
      DATA UCH(1,24),UCH(2,24),UCH(3,24),UCH(4,24)/ 0, 1, 2, 3 /
      DATA UCH(5,24),UCH(6,24),UCH(7,24),UCH(8,24)/ 8, 9, 10, 11 /
C     ---------mode----------------------------
      DATA NCH(25), CHBW(25), NBIT(25)            / 4, 4, 2 /
      DATA S2M(25), GRP(25)                       / '8p8-2', 2  /
      DATA UCH(1,25),UCH(2,25),UCH(3,25),UCH(4,25)/ 0, 1, 2, 3 /
      DATA UCH(5,25),UCH(6,25),UCH(7,25),UCH(8,25)/ 8, 9, 10, 11 /
C     ---------mode----------------------------
      DATA NCH(26), CHBW(26), NBIT(26)            / 4, 2, 2 /
      DATA S2M(26), GRP(26)                       / '4p8-2', 4  /
      DATA UCH(1,26),UCH(2,26),UCH(3,26),UCH(4,26)/ 0, 1, 2, 3 /
      DATA UCH(5,26),UCH(6,26),UCH(7,26),UCH(8,26)/ 8, 9, 10, 11 /
C     ---------mode----------------------------
      DATA NCH(27), CHBW(27), NBIT(27)            / 4, 8, 1 /
      DATA S2M(27), GRP(27)                       / '16s4-1', 2  /
      DATA UCH(1,27),UCH(2,27),UCH(3,27),UCH(4,27)/ 0, 2, 4, 6 /
C     ---------mode----------------------------
      DATA NCH(28), CHBW(28), NBIT(28)            / 4, 4, 1 /
      DATA S2M(28), GRP(28)                       / '8s4-1', 4  /
      DATA UCH(1,28),UCH(2,28),UCH(3,28),UCH(4,28)/ 0, 2, 4, 6 /
C     ---------mode----------------------------
      DATA NCH(29), CHBW(29), NBIT(29)            / 4, 2, 1 /
      DATA S2M(29), GRP(29)                       / '4s4-1', 8  /
      DATA UCH(1,29),UCH(2,29),UCH(3,29),UCH(4,29)/ 0, 2, 4, 6 /
C     ---------mode----------------------------
      DATA NCH(30), CHBW(30), NBIT(30)            / 8, 4, 2 /
      DATA S2M(30), GRP(30)                       / '8x16-2', 1  /
      DATA UCH(1,30),UCH(2,30),UCH(3,30),UCH(4,30)/ 0, 1, 2, 3 /
      DATA UCH(5,30),UCH(6,30),UCH(7,30),UCH(8,30)/ 4, 5, 6, 7 /
      DATA UCH(9,30),UCH(10,30),UCH(11,30),UCH(12,30) / 8, 9, 10, 11 /
      DATA UCH(13,30),UCH(14,30),UCH(15,30),UCH(16,30)/ 12, 13, 14, 15 /
C     ---------mode----------------------------
      DATA NCH(31), CHBW(31), NBIT(31)            / 8, 2, 2 /
      DATA S2M(31), GRP(31)                       / '4x16-2', 2  /
      DATA UCH(1,31),UCH(2,31),UCH(3,31),UCH(4,31)/ 0, 1, 2, 3 /
      DATA UCH(5,31),UCH(6,31),UCH(7,31),UCH(8,31)    / 4, 5, 6, 7 /
      DATA UCH(9,31),UCH(10,31),UCH(11,31),UCH(12,31) / 8, 9, 10, 11 /
      DATA UCH(13,31),UCH(14,31),UCH(15,31),UCH(16,31)/ 12, 13, 14, 15 /
C ----------------------------------------------------------------------
C
C     Typed that in, now do something with it....
C
      OKRATE = .FALSE.
      DO IMODE = 1, MS2
         IF( NDATCH .EQ. NCH(IMODE) .AND.
     1       BSAMPL .EQ. NBIT(IMODE) .AND.
     2       MBITSC .EQ. 2.0*CHBW(IMODE) ) THEN
C
C           worth while to check the cables
C
            ALLOK = .TRUE.            
            OKRATE = .TRUE.
            DO ICH = 1, NCH(IMODE)*NBIT(IMODE)
C
C              check if all active channels of this mode are required
C
               ACTIVE = VLBCH(UCH(ICH,IMODE) + 1)
               READ( ACTIVE , '( I1, A1, A1 )' ) MODBBC, MODSB, MODBIT
C
               FOUND = .FALSE.
               DO JCH = 1, NDATCH
                  IF( BBC(JCH) .EQ. MODBBC ) FOUND = .TRUE. 
               END DO
               IF( .NOT. FOUND ) ALLOK = .FALSE.
C
C              if OK see whether SB is need
C
               CALL UPCASE( MODSB )
               FOUND = .FALSE.
               DO JCH = 1, NDATCH
                  IF( BBCSD(JCH) .EQ. MODSB ) FOUND = .TRUE.
               END DO
               IF( .NOT. FOUND ) ALLOK = .FALSE.
C
C              and test if bit are required
C
               FOUND = .FALSE.
               DO JCH = 1, NDATCH
C
C                 sign bit always required, magnitude only for 2-bit
C        
                  IF ( MODBIT .EQ. 's' .OR. 
     1                ( MODBIT .EQ. 'm' .AND. BSAMPL .EQ. 2 ) )
     2                FOUND = .TRUE.
               END DO
               IF( .NOT. FOUND ) ALLOK = .FALSE.    
            END DO
C
C              if all OK, this is a match, remember IMODE is OK
C
            IF( ALLOK ) GOTO 100
         END IF
      END DO
C
C     Unable to match
C
      VALID = .FALSE.
      IF ( OKRATE ) THEN
         WRITE( MSGTXT, '( A )' )
     1       'VXS2VL: Unable to match input channels config.'
         CALL WLOG( 1, MSGTXT )
      ELSE
         WRITE( MSGTXT, '( A, I2, A, I2, A, F5.2, A )' ) 
     1       'VXS2VL: S2 cannot record', NDATCH, 'chan ',BSAMPL,
     2       'bits/w ', MBITSC, 'bits/s'
         CALL WLOG( 1, MSGTXT )
      END IF
      GOTO 999
C
C     reach this if found a match
C
  100 VALID = .TRUE.
      IF( NGROUP .EQ. 0 ) THEN
         NGROUP = GRP(IMODE)
      ELSE
         IF( NGROUP .NE. GRP(IMODE)) CALL ERRLOG(
     1       ' VXS2VL: Inconsistent Tapemode for this S2 Mode' )
      END IF
      S2MODE = S2M(IMODE)
      NS2USD = NCH(IMODE) * NBIT(IMODE)
      DO ICH = 1, NS2USD
         IS2USD(ICH) = VLBCH( UCH(ICH,IMODE) + 1)
      END DO
C
  999 RETURN
      END













