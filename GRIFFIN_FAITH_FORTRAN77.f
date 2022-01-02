C Name:     Griffin, Faith Juliamae Orendain
C Language: Fortran77
C Paradigm: Legacy

      PROGRAM SoftwareApplication
      IMPLICIT NONE
      CHARACTER*64 string1, string2
      INTEGER*4 editDistance, n, LENGTH
      EXTERNAL editDistance, LENGTH
      PRINT *, 'Input string 1 = '
      READ *, string1
      PRINT *, 'Input string 2 = '
      READ *, string2
      PRINT *, ''
      PRINT *, 'Operations:'
      n=editDistance(string1, string2, LENGTH(string1), LENGTH(string2))
      PRINT *, 'Edit Distance: ', n
      END

C
C
      INTEGER FUNCTION editDistance(string1, string2, nLen1, nLen2)
      CHARACTER*(*) string1, string2
      INTEGER nLen1, nLen2, table(nLen1,nLen2), nCost
      DO 10, i = 1, nLen1, 1
            table(i, 1) = i
10    CONTINUE
      DO 15, j = 1, nLen2, 1
            table(1, j) = j
15    CONTINUE
      
      DO 25, k = 2, nLen1, 1
            DO 20, l = 2, nLen2, 1
                  IF(string1(k:k) .EQ. string2(l:l)) THEN 
                        nCost = 0
                  ELSE 
                        nCost = 1
                  END IF
                  GO TO 30
20    CONTINUE
25    CONTINUE
30    table(k,l)=MIN(table(k,l)+1,table(k,l-1)+1,table(k-1,l-1)+nCost)
      CALL printOperations(table, string1, string2, nLen1, nLen)
      editDistance = table(nLen1,nLen2)
      END

C Returns length of string ignoring trailing blanks 
C since Fortran77 doesn't have LEN_TRIM()
C Reference: 
      INTEGER FUNCTION LENGTH(STRING) 
      CHARACTER*(*) STRING 
      DO 20, I = LEN(STRING), 1, -1 
            IF(STRING(I:I) .NE. ' ') GO TO 25 
20    CONTINUE 
25    LENGTH = I 
      END 

C 
C 
C Reference:
      SUBROUTINE printOperations(table, string1, string2, nLen1, nLen2)
      CHARACTER*(*) string1, string2
      INTEGER table(nLen1,nLen2), nLen1, nLen2, i, j
      
      i = nLen1
      j = nLen2
      DO 35, WHILE(i .NE. 1 .OR. j .NE. 1)
            IF(string1(i-1:i-1) .EQ. string2(j-1:j-1)) THEN
                  i = i - 1
                  j = j - 1
            ELSE IF(table(i,j) .EQ. table(i-1,j-1)+1) THEN
                  GO TO 40
                  i = i - 1
                  j = j - 1
            ELSE IF(table(i,j) .EQ. table(i,j-1)+1) THEN
                  PRINT *, 'Insert ', string2(j-1:j-1)
                  j = j - 1
            ELSE IF(table(i,j) .EQ. table(i-1,j)+1) THEN
                  PRINT *, 'Delete ', string1(i-1:i-1)
                  i = i - 1
            END IF
35    CONTINUE
40    PRINT *,'Replace ',string2(j-1:j-1),' with ',string1(i-1:i-1)
      END