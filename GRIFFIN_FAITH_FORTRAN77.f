C Name:     Griffin, Faith Juliamae Orendain
C Language: Fortran77
C Paradigm: Legacy

      PROGRAM SoftwareApplication
      IMPLICIT NONE
      CHARACTER*64 string1, string2
      INTEGER editDistance, n, LENGTH
      EXTERNAL editDistance, LENGTH
      PRINT *, 'Input string 1 = '
      READ *, string1
      PRINT *, 'Input string 2 = '
      READ *, string2
      PRINT *, ''
      PRINT *, 'Operations:'
      n=editDistance(string1, string2, LENGTH(string1), LENGTH(string2))
      PRINT *, ' '
      PRINT *, 'Edit Distance: ', n
      END

C
C
      INTEGER FUNCTION editDistance(string1, string2, nLen1, nLen2)
      CHARACTER*(*) string1, string2
      INTEGER nLen1, nLen2, table(nLen1 + 1,nLen2 + 1)
      DO i = 1, nLen1 + 1, 1
            table(1, i) = i-1
      END DO
      DO j = 1, nLen2 + 1, 1
            table(j, 1) = j-1
      END DO
      
      DO k = 2, nLen1 + 1, 1
            DO l = 2, nLen2 + 1, 1
                  IF(string1(k-1:k-1) .EQ. string2(l-1:l-1)) THEN
                        table(k,l) = table(k-1,l-1)
                  ELSE
            table(k,l) = MIN(table(k-1,l),table(k,l-1),table(k-1,l-1))+1
                  END IF
            END DO
      END DO

      CALL printOperations(table, string1, string2, nLen1, nLen2)
      editDistance = table(nLen1 + 1,nLen2 + 1)
      END

C Returns length of string ignoring trailing blanks 
C since Fortran77 doesn't have LEN_TRIM()
C Reference: 
      INTEGER FUNCTION LENGTH(STRING) 
      CHARACTER*(*) STRING 
      DO I = LEN(STRING), 1, -1 
            IF(STRING(I:I) .NE. ' ') GO TO 25 
      END DO
25    LENGTH = I 
      END 

C 
C 
C Reference:
      SUBROUTINE printOperations(table, string1, string2, nLen1, nLen2)
      CHARACTER*(*) string1, string2
      CHARACTER*64 insert, delete
      EXTERNAL insert, delete
      INTEGER table(nLen1 + 1,nLen2 + 1), nLen1, nLen2, i, j
      
      i = nLen1 + 1
      j = nLen2 + 1
      DO WHILE(i .NE. 1 .OR. j .NE. 1)
            IF(string1(i-1:i-1) .EQ. string2(j-1:j-1)) THEN
                  i = i - 1
                  j = j - 1
            ELSE IF(table(i,j) .EQ. table(i-1,j-1)+1) THEN
           PRINT *,'Replace ',string1(i-1:i-1),' with ',string2(j-1:j-1)
                  string1(i-1:i-1) = string2(j-1:j-1)
                  CALL printStrings(string1, string2)
                  i = i - 1
                  j = j - 1
            ELSE IF(table(i,j) .EQ. table(i,j-1)+1) THEN
                  PRINT *, 'Insert ', string2(j-1:j-1)
                  string1 = insert(i-1,string1,string2(j-1:j-1))
                  CALL printStrings(string1, string2)
                  j = j - 1
            ELSE IF(table(i,j) .EQ. table(i-1,j)+1) THEN
                  PRINT *, 'Delete ', string1(i-1:i-1)
                  string1 = delete(i-1,string1)
                  CALL printStrings(string1, string2)
                  i = i - 1
            END IF
      END DO
      END

C
C
      SUBROUTINE printStrings(string1, string2)
      CHARACTER*(*) string1, string2
      PRINT *, string1
      PRINT *, string2
      PRINT *, "===================="
      END

C
C
      CHARACTER*64 FUNCTION insert(i, string1, char)
      INTEGER i, k
      CHARACTER*(*) string1
      CHARACTER char
      k = LENGTH(string1) + 1
      DO WHILE(k .GT. i)
            string1(k:k) = string1(k-1:k-1)
            k = k - 1
      END DO
      string1(i+1:i+1) = char
      insert = string1
      END

C
C
      CHARACTER*64 FUNCTION delete(i, string1)
      INTEGER i, j, k, l
      CHARACTER*(*) string1
      k = LENGTH(string1)
      l = LENGTH(string1)
      j = i
      DO WHILE(k .GT. i)
            string1(j:j) = string1(j+1:j+1)
            k = k - 1
            j = j + 1
      END DO
      string1(l:l) = ''
      delete = string1
      END