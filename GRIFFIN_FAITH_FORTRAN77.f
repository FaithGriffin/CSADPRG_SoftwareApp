C Name:     Griffin, Faith Juliamae Orendain
C Language: Fortran (Fortran77)
C Paradigm: Legacy



      PROGRAM APP
      IMPLICIT NONE
      CHARACTER*64 STR1, STR2
      INTEGER DIST, N, LENGTH
      EXTERNAL DIST, LENGTH
C Asks for input String from user; it is assumed that input 
C has no leading nor trailing spaces and inputs are single words
      PRINT *, 'Input string 1 = '
      READ *, STR1
      PRINT *, 'Input string 2 = '
      READ *, STR2
      PRINT *, ''
      PRINT *, 'Operations:'
C Given two input strings, store the yielded edit distance to a 
C temporary variable and display it after displaying the operations
C involved in converting STR1 into STR2.
      N = DIST(STR1, STR2, LENGTH(STR1), LENGTH(STR2))
      PRINT *, ' '
      PRINT *, 'Edit Distance: ', N
      END


C This function computes the minimum number of single-character
C edits to convert STR1 into STR2.
      INTEGER FUNCTION DIST(STR1, STR2, LEN1, LEN2)
      CHARACTER*(*) STR1, STR2
      INTEGER LEN1, LEN2, TABLE(LEN1 + 1, LEN2 + 1), I, J, K, L
C number of insertions it would take to turn
C an empty string to the second string
      DO I = 1, LEN1 + 1, 1
            TABLE(1, I) = I-1
      ENDDO
C number of deletions it would take to turn the first 
C string and the extra empty string to an empty string
      DO J = 1, LEN2 + 1, 1
            TABLE(J, 1) = J-1
      ENDDO
C Fill out the rest of the lookup table (TABLE) by storing 
C the smallest edit distance among the yielded edit 
C distances from the three operations (deletion, insertion, 
C and substitution) plus one; since the cost of each 
C operation is one. If the characters from the two strings 
C are similar, there would be no need for the aforementioned 
C process, rather, TABLE[K][L] would store the edit distance 
C stored at TABLE[K-1][L-1].
      DO K = 2, LEN1 + 1, 1
            DO L = 2, LEN2 + 1, 1
                  IF(STR1(K-1:K-1) .EQ. STR2(L-1:L-1)) THEN
                        TABLE(K,L) = TABLE(K-1, L-1)
                  ELSE
            TABLE(K,L) = MIN(TABLE(K-1,L),TABLE(K,L-1),TABLE(K-1,L-1))+1
                  ENDIF
            ENDDO
      ENDDO
C Print the operations (Replace, Insert, Delete) involved in 
C converting STR1 into STR2.
      CALL PRNTOP(TABLE, STR1, STR2, LEN1, LEN2)
      DIST = TABLE(LEN1 + 1, LEN2 + 1)
      END


C Returns length of STRING ignoring trailing blanks 
C since Fortran77 doesn't have LEN_TRIM().
      INTEGER FUNCTION LENGTH(STRING) 
      CHARACTER*(*) STRING
      INTEGER I 
      DO 1, I = LEN(STRING), 1, -1 
            IF(STRING(I:I) .NE. ' ') GO TO 2 
1     CONTINUE
2     LENGTH = I 
      END 


C When called, this subroutine prints the operations involved in 
C converting STR1 into STR2 and the form of STR1 after every edit, 
C as guided by the lookup table (TABLE).
      SUBROUTINE PRNTOP(TABLE, STR1, STR2, LEN1, LEN2)
      CHARACTER*(*) STR1, STR2
      CHARACTER*64 INSRT, DEL
      EXTERNAL INSRT, DEL
      INTEGER TABLE(LEN1 + 1, LEN2 + 1), LEN1, LEN2, I, J
      
      I = LEN1 + 1
      J = LEN2 + 1
      DO WHILE(I .NE. 1 .OR. J .NE. 1)
            IF(STR1(I-1:I-1) .EQ. STR2(J-1:J-1)) THEN
                  I = I - 1
                  J = J - 1
            ELSE IF(TABLE(I,J) .EQ. TABLE(I-1,J-1)+1) THEN
           PRINT *,'=>Replace ',STR1(i-1:i-1),' with ',STR2(J-1:J-1)
                  STR1(I-1:I-1) = STR2(J-1:J-1)
                  CALL PRNTST(STR1, STR2)
                  I = I - 1
                  J = J - 1
            ELSE IF(TABLE(I,J) .EQ. TABLE(I,J-1)+1) THEN
                  PRINT *, '=>Insert ', STR2(J-1:J-1)
                  STR1 = INSRT(I-1, STR1, STR2(J-1:J-1))
                  CALL PRNTST(STR1, STR2)
                  J = J - 1
            ELSE IF(TABLE(I,J) .EQ. TABLE(I-1,J)+1) THEN
                  PRINT *, '=>Delete ', STR1(I-1:I-1)
                  STR1 = DEL(I-1, STR1)
                  CALL PRNTST(STR1, STR2)
                  I = I - 1
            ENDIF
      ENDDO
      END


C When called, this subroutine prints the current form of 
C STR1 after every single-character edit done in order for it 
C to be converted to STR2. It also prints STR2 for guidance.
      SUBROUTINE PRNTST(STR1, STR2)
      CHARACTER*(*) STR1, STR2
      PRINT *, STR1
      PRINT *, STR2
      PRINT *, "===================="
      END


C This function returns the updated form of STR1 
C after CHAR has been inserted into it.
      CHARACTER*64 FUNCTION INSRT(I, STR1, CHAR)
      INTEGER I, K
      CHARACTER*(*) STR1
      CHARACTER CHAR
      K = LENGTH(STR1) + 1
      DO WHILE(K .GT. I)
            STR1(K:K) = STR1(K-1:K-1)
            K = K - 1
      ENDDO
      STR1(I+1:I+1) = CHAR
      INSRT = STR1
      END


C This function returns the updated form of STR1
C after deletion of a character has taken place.
      CHARACTER*64 FUNCTION DEL(I, STR1)
      INTEGER I, J, K, L
      CHARACTER*(*) STR1
      K = LENGTH(STR1)
      l = LENGTH(STR1)
      J = I
      DO WHILE(K .GT. I)
            STR1(J:J) = STR1(J+1:J+1)
            K = K - 1
            J = J + 1
      ENDDO
      STR1(L:L) = ''
      DEL = STR1
      END