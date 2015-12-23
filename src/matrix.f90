! *****************************************************************************
! * Assignment:   Final Program                                                  *
! * Submitted By: Nicholas Pelham                                             *
! *                                                                           *
! * Purpose: Compute the result of various mathematical operation on          *
! *          matrices.                                                        *
! *                                                                           *
! * Functions  : menu, determinant, reducedRow_matrix, findCofactorMatrix,    *
! *              matrix_transpose                                             *
! * Subroutines: matrixAddition, matrixSubtraction, scalarMultiplication,     *
! *              matrixMultiplication, findCofactorMatrix, readMatrix,        *
! *              printMatrix, printMatrix_real, matrixDeterminant,            *
! *              matrixInverse, printMatrixInverse, cramersRuleSolutions      *
! *****************************************************************************

! TODO:
!  1. Improve determinant by using row reduction to multiply 
!     across the main diagonal.

      MODULE matrix_module
        IMPLICIT NONE
    
        TYPE MATRIX
          INTEGER :: rowSize, colSize
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: element
        END TYPE MATRIX
        
        TYPE MATRIX_REAL
          INTEGER :: rowSize, colSize
          REAL, DIMENSION(:,:), ALLOCATABLE :: element
        END TYPE MATRIX_REAL
    
      END MODULE matrix_module
  
      PROGRAM program05
        USE matrix_module
        IMPLICIT NONE
    
        INTERFACE
    
        FUNCTION menu()
          IMPLICIT NONE
          INTEGER :: menu
        END FUNCTION menu
        
        RECURSIVE FUNCTION determinant(Mtx) RESULT(det)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN) :: Mtx
          INTEGER :: det
        END FUNCTION determinant
        
        SUBROUTINE findCofactorMatrix(Mtx, cofactorMtx)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN)  :: Mtx
          TYPE(MATRIX), INTENT(OUT) :: cofactorMtx
        END SUBROUTINE findCofactorMatrix

        SUBROUTINE readMatrix(M)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(OUT)  :: M
        END SUBROUTINE readmatrix

        SUBROUTINE printMatrix(header, M)
          USE matrix_module
          IMPLICIT NONE
          CHARACTER, INTENT(IN)    :: header*(*)
          TYPE(MATRIX), INTENT(IN) :: M
        END SUBROUTINE printmatrix
        
        SUBROUTINE printMatrix_real(header, M)
          USE matrix_module
          IMPLICIT NONE
          CHARACTER, INTENT(IN)         :: header*(*)
          TYPE(MATRIX_REAL), INTENT(IN) :: M
        END SUBROUTINE printMatrix_real
     
        SUBROUTINE matrixAddition(MtxA, MtxB, MtxSum)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN)  :: MtxA, MtxB
          TYPE(MATRIX), INTENT(OUT) :: MtxSum
        END SUBROUTINE matrixAddition
     
        SUBROUTINE matrixSubtraction(MtxA, MtxB, MtxDifference)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN)  :: MtxA, MtxB
          TYPE(MATRIX), INTENT(OUT) :: MtxDifference
        END SUBROUTINE matrixSubtraction
        
        SUBROUTINE scalarMultiplication(scalar, Mtx, MtxProduct)
          USE matrix_module
          IMPLICIT NONE
          INTEGER, INTENT(IN)       :: scalar
          TYPE(MATRIX), INTENT(IN)  :: Mtx
          TYPE(MATRIX), INTENT(OUT) :: MtxProduct
        END SUBROUTINE scalarMultiplication
     
        SUBROUTINE matrixMultiplication(MtxA, MtxB, MtxProduct)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN)  :: MtxA, MtxB
          TYPE(MATRIX), INTENT(OUT) :: MtxProduct
        END SUBROUTINE matrixMultiplication
        
        SUBROUTINE matrixDeterminant(Mtx)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN) :: Mtx
        END SUBROUTINE matrixDeterminant
        
        SUBROUTINE matrixInverse(Mtx)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN)  :: Mtx
        END SUBROUTINE matrixInverse
      
        SUBROUTINE print_inverseMatrix(determinant, cofactor_matrix)
          USE matrix_module
          IMPLICIT NONE
          INTEGER, INTENT(IN)      :: determinant
          TYPE(MATRIX), INTENT(IN) :: cofactor_matrix
        END SUBROUTINE print_inverseMatrix
        
        FUNCTION reducedRow_matrix(mtx)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN) :: mtx
          TYPE(MATRIX_REAL)        :: reducedRow_matrix
        END FUNCTION reducedRow_matrix
        
        SUBROUTINE cramersRule_solutions(mtx, vect, solutions)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN)  :: mtx, vect
          TYPE(MATRIX_REAL), INTENT(OUT) :: solutions
        END SUBROUTINE cramersRule_solutions
    
        END INTERFACE

        TYPE(MATRIX) :: A, B, C
        TYPE(MATRIX_REAL) :: Z
        INTEGER      :: userSelect, scalar  ! move scalar to scalar multiplication
    
        DO 
          userSelect = menu() 

          IF (userSelect == 9) THEN
            EXIT  ! quit is selected on the menu, exit the loop
          ELSE IF (userSelect == 1) THEN 
            CALL readMatrix(A)
            CALL readMatrix(B)
            CALL matrixAddition(A, B, C)
            CALL printMatrix("Sum", C)
          ELSE IF (userSelect == 2) THEN 
            CALL readMatrix(A)
            CALL readMatrix(B)
            CALL matrixSubtraction(A, B, C)
            CALL printMatrix("Difference", C)
          ELSE IF (userSelect == 3) THEN
            CALL readMatrix(A)
            WRITE(*,*) "Enter a scalar to multiply the matrix by: "
            READ(*,*) scalar
            CALL scalarMultiplication(scalar, A, C)
            CALL printMatrix("Product", C)
          ELSE IF (userSelect == 4) THEN
            CALL readMatrix(A)
            CALL readMatrix(B)
            CALL matrixMultiplication(A, B, C)
            CALL printMatrix("Product", C)
          ELSE IF (userSelect == 5) THEN
            CALL readMatrix(A)
            CALL matrixDeterminant(A)
          ELSE IF (userSelect == 6) THEN
            WRITE(*,*) "Enter the non-augmented matrix for the system: "
            CALL readMatrix(A)
            WRITE(*,*) "Enter the solution vector for the system: "
            CALL readMatrix(B)
            CALL cramersRule_solutions(A,B,Z)
            CALL printMatrix_real("Solutions", Z)
          ELSE IF (userSelect == 7) THEN
            WRITE(*,*) "Enter the augmented matrix for the system:"
            CALL readMatrix(A)
            Z = reducedRow_matrix(A)
            CALL printMatrix_real("Solutions", Z)
          ELSE IF (userSelect == 8) THEN
            CALL readMatrix(A)
            CALL matrixInverse(A)
        END IF
        
        END DO

      END PROGRAM program05

     ! ***************************************************************
     ! *                       function menu                         *
     ! * Purpose: Display the program options to the user and        *
     ! *          collect their choice for the program to proceed.   *
     ! *                                                             *
     ! * Accepts: None                                               *
     ! * Returns: Integer representation of the users choice.        *
     ! * Input  : Any integer. (Note that the main program handles   *
     ! *          invlid integer options in much the same way as it  *
     ! *          handles valid options.                             *
     ! * Output : Menu options for the user to choose from.          *
     ! ***************************************************************
     
      FUNCTION menu()
        IMPLICIT NONE
        INTEGER :: menu
    
        WRITE(*,101, advance='no') "Menu options:", &
                                     "1. Matrix Addition", &
                                     "2. Matrix Subtraction", &
                                     "3. Scalar Multiplication", &
                                     "4. Matrix Multiplication", &
                                     "5. Matrix Determinant", &
                                 "6. Solve System Using Cramer's Rule", &
                      "7. Solve System Using Gauss-Jordan Elimination", &
                                     "8. Inverse Matrix", &
                                     "9. Exit", &
                                   "Selection: "
        READ(*,*) menu
    
        ! ---------- Formatting ----------
        101 FORMAT(/, a, /, 9(Tr2, a, /), /, a)

      END FUNCTION menu


     ! ***************************************************************
     ! *                  subroutine readMatrix                      *
     ! * Purpose: Create a matrix from user input.                   *
     ! *                                                             *
     ! * Accepts: M - Matrix which will be overwritten with new      *
     ! *              data                                           *
     ! * Returns: M - New matrix created from user input.            *
     ! * Input  : Data to populate the fields of matrix M.           *
     ! *          rowSize      - Amount of rows in matrix M          *
     ! *          colSize      - Amount of columns in matrix M       *
     ! *          element(:,:) - Integer data in matrix M            *
     ! * Output : Promts for user input                              *
     ! ***************************************************************
     
      SUBROUTINE readMatrix(M)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(OUT) :: M
    
        INTEGER :: i, j
   
        WRITE(*,101,advance='no') "Please enter the size of the ", &
                                                  "matrix (row,column): "
        READ(*,*) M%rowSize, M%colSize
        IF (ALLOCATED(M%element)) DEALLOCATE(M%element)
        ALLOCATE(M%element(M%rowSize, M%colSize))
        DO i=1,M%rowSize
          WRITE(*,11,ADVANCE="no")"Row", i, ":  "
          READ(*,*) (M%element(i,j), j=1,M%colSize)
        END DO

        ! ----- Format area -------
        11 FORMAT (t2,a,i2,a)
        101  FORMAT (t2,a,a)

      END SUBROUTINE readmatrix

     ! ***************************************************************
     ! *                  subroutine printMatrix                     *
     ! * Purpose: Output the data contained in Matrix M              *
     ! *                                                             *
     ! * Accepts: header - Title to describe matrix M                *
     ! *          M      - Matrix to be displayed                    *
     ! * Returns: None                                               *
     ! * Input  : None                                               *
     ! * Output : Data contained the matrix M as element(:,:)        *
     ! ***************************************************************
     
      SUBROUTINE printMatrix(header, M)
        USE matrix_module
        IMPLICIT NONE
        CHARACTER, INTENT(IN)    :: header*(*)
        TYPE(MATRIX), INTENT(IN) :: M

        INTEGER :: i, j
        
        IF (.NOT.ALLOCATED(M%element)) RETURN

        WRITE(*,5) header
        DO i=1,M%rowSize
          WRITE(*,20,advance = "no")"[ "
          DO j=1,M%colSize
            WRITE(*,10,ADVANCE="no") M%element(i,j)
          END DO
          WRITE(*,*)"]"
        END DO

        ! ----- Format area -------
        5   FORMAT(//t7,a/)
        10  FORMAT (i3,tr2)
        20  FORMAT (t3,a)

      END SUBROUTINE printmatrix

     ! ***************************************************************
     ! *                subroutine printMatrix_real                  *
     ! * Purpose: Output the data contained in Matrix M              *
     ! *                                                             *
     ! * Accepts: header - Title to describe matrix M                *
     ! *          M      - Matrix to be displayed                    *
     ! * Returns: None                                               *
     ! * Input  : None                                               *
     ! * Output : Data contained the matrix M as element(:,:)        *
     ! ***************************************************************
     
      SUBROUTINE printMatrix_real(header, M)
        USE matrix_module
        IMPLICIT NONE
        CHARACTER, INTENT(IN)         :: header*(*)
        TYPE(MATRIX_REAL), INTENT(IN) :: M

        INTEGER :: i, j

        WRITE(*,5) header
        DO i=1,M%rowSize
          WRITE(*,20,advance = "no")"[ "
          DO j=1,M%colSize
            WRITE(*,10,ADVANCE="no") M%element(i,j)
          END DO
          WRITE(*,*)"]"
        END DO

        ! ----- Format area -------
        5   FORMAT(//t7,a/)
        10  FORMAT (f6.3,tr2)
        20  FORMAT (t3,a)

      END SUBROUTINE printmatrix_real

     ! ***************************************************************
     ! *                  subroutine printMatrices                   *
     ! * Purpose: Output two Matrices side by side                   *
     ! *                                                             *
     ! * Accepts: Matrices M1 and M2                                 *
     ! * Returns: None                                               *
     ! * Input  : None                                               *
     ! * Output : Data contained in M1 and M2 element(:,:) formatted *
     ! *          to be readable while side by side.                 *
     ! ***************************************************************
     
      SUBROUTINE printMatrices(M1, M2)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN) :: M1, M2

        INTEGER :: i, j, max_rows
        
        ! max_rows assignment
        IF (M1%rowSize > M2%rowSize) THEN
          max_rows = M1%rowSize
        ELSE
          max_rows = M2%rowSize
        END IF

        WRITE(*,101, advance = 'no') "   Matrix A"
        DO i = 1, M1%colSize*4 - 6
          WRITE(*,101, advance = 'no') " "
        END DO
        WRITE(*,101) "Matrix B"
        DO i=1, max_rows
          IF (i <= M1%rowSize) THEN
            WRITE(*,102,advance = "no") "[ "
          ELSE
            WRITE(*,102,advance = "no") "  "
          END IF
          IF (i <= M1%rowSize) THEN
            DO j=1, M1%colSize
              WRITE(*, 103, advance = 'no') M1%element(i,j)
            END DO
          ELSE
            DO j=1, M1%colSize
              WRITE(*, 101, advance = 'no') "   "
            END DO
          END IF
          IF (i <= M1%rowSize) THEN
            WRITE(*,101,advance = "no") "]"
          ELSE
            WRITE(*,101,advance = "no") " "
          END IF
          IF (i <= M2%rowSize) THEN
            WRITE(*,104, advance = 'no') " [ "
            DO j=1, M2%colSize
              WRITE(*, 103, ADVANCE= 'no') M2%element(i,j)
            END DO
            WRITE(*,104, advance = 'no')"]"
          END IF
        END DO
        WRITE(*,*)

        ! ----- Format area -------
        101 FORMAT(a)
        102 FORMAT(/t3,a)
        103 FORMAT(i2,tr1)
        104 FORMAT(a)

      END SUBROUTINE printmatrices

     ! ***************************************************************
     ! *                  subroutine matrixAddition                  *
     ! * Purpose: Add the elements of two matrices                   *
     ! *                                                             *
     ! * Accepts: Matrices MtxA, MtxB, and MtxSum                    *
     ! * Returns: MtxSum - the sum of MtxA and MtxB                  *
     ! * Input  : None                                               *
     ! * Output : Error if the matrices don't match each other in    *
     ! *          size                                               *
     ! ***************************************************************
     
      SUBROUTINE matrixAddition(MtxA, MtxB, MtxSum)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN)  :: MtxA, MtxB
        TYPE(MATRIX), INTENT(OUT) :: MtxSum
        
        INTEGER :: i, j
        
        IF (.NOT.ALLOCATED(MtxA%element) .OR. &
                                  .NOT.ALLOCATED(MtxB%element)) THEN
          WRITE(*,*) "ERROR: CANNOT ADD THE GIVEN MATRICES"
          RETURN
        ELSE IF (MtxA%rowSize /= MtxB%rowSize) THEN
          WRITE(*,*) "ERROR: CANNOT ADD THE GIVEN MATRICES"
          RETURN
        ELSE IF (MtxA%colSize /= MtxB%colSize) THEN
          WRITE(*,*) "ERROR: CANNOT ADD THE GIVEN MATRICES"
          RETURN
        END IF
        
        MtxSum%rowSize = MtxA%rowSize
        MtxSum%colSize = MtxA%colSize
        IF (ALLOCATED(MtxSum%element)) DEALLOCATE(MtxSum%element)
        ALLOCATE(MtxSum%element(MtxSum%rowSize, MtxSum%colSize))
        
        DO i = 1, MtxA%rowSize
          DO j = 1, MtxA%colSize
            MtxSum%element(i,j) = MtxA%element(i,j) + &
                                                   MtxB%element(i,j)
          END DO
        END DO
      END SUBROUTINE matrixAddition

     ! ***************************************************************
     ! *                  subroutine matrixSubtraction               *
     ! * Purpose: Subtract the elements of two matrices              *
     ! *                                                             *
     ! * Accepts: Matrices MtxA, MtxB, and MtxDifference             *
     ! * Returns: MtxDifference - the difference between MtxA and    *
     ! *                          MtxB                               *
     ! * Input  : None                                               *
     ! * Output : Error if the matrices don't match each other in    *
     ! *          size                                               *
     ! ***************************************************************
     
      SUBROUTINE matrixSubtraction(MtxA, MtxB, MtxDifference)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN)  :: MtxA, MtxB
        TYPE(MATRIX), INTENT(OUT) :: MtxDifference
        
        INTEGER :: i, j
        
        IF (.NOT.ALLOCATED(MtxA%element) .OR. &
                                  .NOT.ALLOCATED(MtxB%element)) THEN
          WRITE(*,*) "ERROR: CANNOT SUBTRACT THE GIVEN MATRICES"
          RETURN
        ELSE IF (MtxA%rowSize /= MtxB%rowSize) THEN
          WRITE(*,*) "ERROR: CANNOT SUBTRACT THE GIVEN MATRICES"
          RETURN
        ELSE IF (MtxA%colSize /= MtxB%colSize) THEN
          WRITE(*,*) "ERROR: CANNOT SUBTRACT THE GIVEN MATRICES"
          RETURN
        END IF
        
        MtxDifference%rowSize = MtxA%rowSize
        MtxDifference%colSize = MtxA%colSize
        IF (ALLOCATED(MtxDifference%element)) &
                                        DEALLOCATE(MtxDifference%element)
        ALLOCATE(MtxDifference%element(MtxDifference%rowSize, &
                                                  MtxDifference%colSize))
        
        DO i = 1, MtxA%rowSize
          DO j = 1, MtxA%colSize
            MtxDifference%element(i,j) = MtxA%element(i,j) - &
                                                   MtxB%element(i,j)
          END DO
        END DO
      END SUBROUTINE matrixSubtraction

     ! ***************************************************************
     ! *               subroutine scalarMultiplication               *
     ! * Purpose: Multiply a matrix by a scalar                      *
     ! *                                                             *
     ! * Accepts: An integer scalar, and two matrices of integers    *
     ! *          Mtx and MtxProduct.                                *
     ! * Returns: The product of sclar multiplication between Mtx    *
     ! *          and scalar.                                        *
     ! * Input  : None                                               *
     ! * Output : None                                               *
     ! ***************************************************************
     
      SUBROUTINE scalarMultiplication(scalar, Mtx, MtxProduct)
        USE matrix_module
        IMPLICIT NONE
        INTEGER, INTENT(IN)       :: scalar
        TYPE(MATRIX), INTENT(IN)  :: Mtx
        TYPE(MATRIX), INTENT(OUT) :: MtxProduct
        
        INTEGER :: i, j
        
        MtxProduct%rowSize = Mtx%rowSize
        MtxProduct%colSize = Mtx%colSize
        IF (ALLOCATED(MtxProduct%element)) DEALLOCATE(MtxProduct%element)
        ALLOCATE(MtxProduct%element(MtxProduct%rowSize, &
                                                MtxProduct%colSize))
        
        DO i = 1, Mtx%rowSize
          DO j = 1, Mtx%colSize
            MtxProduct%element(i,j) = scalar * Mtx%element(i,j)
          END DO
        END DO
        
      END SUBROUTINE scalarMultiplication

     ! ***************************************************************
     ! *               subroutine matrixMultiplication               *
     ! * Purpose: Compute the product of two matrices                *
     ! *                                                             *
     ! * Accepts: Three matrices, MtxA, MtxB, and MtxProduct         *
     ! * Returns: The product of MtxA and MtxB                       *
     ! * Input  : None                                               *
     ! * Output : Error if the cross product is not defined for the  *
     ! *          given matrices.                                    *
     ! ***************************************************************
     
      SUBROUTINE matrixMultiplication(MtxA, MtxB, MtxProduct)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN)  :: MtxA, MtxB
        TYPE(MATRIX), INTENT(OUT) :: MtxProduct
                
        INTEGER :: i, j, k, sum_of_products
        
        IF (MtxA%colSize /= MtxB%rowSize) THEN
          WRITE(*,*) "ERROR: CROSS PRODUCT NOT DEFINED FOR GIVEN MATRICES"
          RETURN
        END IF
        
        MtxProduct%rowSize = MtxA%rowSize
        MtxProduct%colSize = MtxB%colSize
        IF (ALLOCATED(MtxProduct%element)) DEALLOCATE(MtxProduct%element)
        ALLOCATE(MtxProduct%element(MtxProduct%rowSize, &
                                                     MtxProduct%colSize))
        
        DO i = 1, MtxA%rowSize
          DO j = 1, MtxA%colSize
            sum_of_products = 0
            DO k = 1, MtxA%rowSize
              sum_of_products = sum_of_products + &
                                    MtxA%element(i,k) * MtxB%element(k,j)
            END DO
            MtxProduct%element(i,j) = sum_of_products
          END DO
        END DO
        
      END SUBROUTINE matrixMultiplication
      
     ! ***************************************************************
     ! *                subroutine matrixDeterminant                 *
     ! * Purpose: Print the determinant of a supplied matrix.        *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx                                      *
     ! * Returns: None                                               *
     ! * Input  : None                                               *
     ! * Output : Error if the determinant can not be found, the     *
     ! *          determinant of the supplied matrix otherwise.      *
     ! ***************************************************************
     
      SUBROUTINE matrixDeterminant(Mtx)
        USE matrix_module
        IMPLICIT NONE
        
        INTERFACE
          RECURSIVE FUNCTION determinant(Mtx) RESULT(det)
            USE matrix_module
            IMPLICIT NONE
            TYPE(MATRIX), INTENT(IN) :: Mtx
            INTEGER :: det
          END FUNCTION determinant
        END INTERFACE
        
        TYPE(MATRIX), INTENT(IN) :: Mtx
        
        IF (Mtx%rowSize /= Mtx%colSize) THEN
          WRITE(*,*) "ERROR: DETERMINANT CAN NOT BE FOUND FOR ", &
                                                   "NON-SQUARE MATRICES."
          RETURN
        END IF
        
        WRITE(*,*) "Determinant of supplied matrix is: ", determinant(Mtx)
      END SUBROUTINE matrixDeterminant

     ! ***************************************************************
     ! *                   function determinant                      *
     ! * Purpose: Calculate the determinant of a matrix              *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx                                      *
     ! * Returns: The determinant of Mtx, or 0 if the determinant is *
     ! *          not defined                                        *
     ! * Input  : None                                               *
     ! * Output : Error if the determinant is not defined            *
     ! ***************************************************************
      
      RECURSIVE FUNCTION determinant(Mtx) RESULT(det)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN) :: Mtx
        INTEGER :: det
        
        TYPE(MATRIX) :: SubMtx
        INTEGER      :: i, j, k
        
        det = 0
        
        IF (Mtx%rowSize == 2) THEN
          det = Mtx%element(1,1) * Mtx%element(2,2) - &
                                      Mtx%element(1,2) * Mtx%element(2,1)
          RETURN
        END IF
        
        subMtx%rowSize = Mtx%rowSize - 1
        subMtx%colSize = Mtx%colSize - 1
        IF (ALLOCATED(subMtx%element)) DEALLOCATE(subMtx%element)
        ALLOCATE(subMtx%element(subMtx%rowSize, subMtx%colSize))
        DO i = 1, Mtx%colSize
          DO j = 2, Mtx%rowSize
            DO k = 1, Mtx%colSize
              IF (i > k) THEN
                subMtx%element(j-1,k) = Mtx%element(j,k)
              ELSE IF (i < k) THEN
                subMtx%element(j-1,k-1) = Mtx%element(j,k)
              END IF
            END DO
          END DO
          IF ((i/2)*2 /= i) THEN
            det = det + Mtx%element(1,i)*determinant(subMtx)
          ELSE
            det = det - Mtx%element(1,i)*determinant(subMtx)
          END IF
        END DO
      END FUNCTION determinant

     ! ***************************************************************
     ! *               subroutine findCofactorMatrix                 *
     ! * Purpose: Create the cofactor matrix for a given matrix      *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx                                      *
     ! * Returns: The cofactor matrix of the supplied matrix         *
     ! * Input  : None                                               *
     ! * Output : Error if the determinant is not defined            *
     ! ***************************************************************
	  
      SUBROUTINE findCofactorMatrix(Mtx, cofactorMtx)
        USE matrix_module
        IMPLICIT NONE
        
        INTERFACE
        RECURSIVE FUNCTION determinant(Mtx) RESULT(det)
          USE matrix_module
          IMPLICIT NONE
          TYPE(MATRIX), INTENT(IN) :: Mtx
          INTEGER :: det
        END FUNCTION determinant
        END INTERFACE
        
        TYPE(MATRIX), INTENT(IN)  :: Mtx
        TYPE(MATRIX), INTENT(OUT) :: cofactorMtx
        TYPE(MATRIX)              :: subMtx
        INTEGER                   :: i, j, k, l
        
        IF (Mtx%rowSize /= Mtx%colSize) THEN
          ! Not a square matrix
          RETURN
        END IF
        
        cofactorMtx%rowSize = Mtx%rowSize
        cofactorMtx%colSize = Mtx%colSize
        ALLOCATE(cofactorMtx%element(cofactorMtx%rowSize, &
                                                 cofactorMtx%colSize))
        subMtx%rowSize = Mtx%rowSize - 1
        subMtx%colSize = Mtx%colSize - 1
        DO i = 1, cofactorMtx%rowSize
          DO j = 1, cofactorMtx%colSize
            ! build a submatrix
            IF (ALLOCATED(subMtx%element)) DEALLOCATE(subMtx%element)
            ALLOCATE(subMtx%element(subMtx%rowSize, subMtx%colSize))
            DO k = 1, cofactorMtx%rowSize - 1
              DO l = 1, cofactorMtx%colsize - 1
                IF (i <= k .AND. j <= l) THEN
                  subMtx%element(k,l) = Mtx%element(k+1,l+1)
                ELSE IF (i <= k) THEN
                  subMtx%element(k,l) = Mtx%element(k+1,l)
                ELSE IF (j <= l) THEN
                  subMtx%element(k,l) = Mtx%element(k,l+1)
                ELSE
                  subMtx%element(k,l) = Mtx%element(k,l)
                END IF
              END DO
            END DO
            cofactorMtx%element(i,j) = (-1)**(i+j) * determinant(subMtx)
          END DO
        END DO
      RETURN
      END SUBROUTINE findCofactorMatrix
      
     ! ***************************************************************
     ! *                 function matrix_transpose                   *
     ! * Purpose: Create the transpose of a given matrix             *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx                                      *
     ! * Returns: The transpose of the supplied matrix               *
     ! * Input  : None                                               *
     ! * Output : none                                               *
     ! ***************************************************************
     
      FUNCTION matrix_transpose(Mtx)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN) :: Mtx
        TYPE(MATRIX)             :: matrix_transpose
        
        INTEGER :: i, j
        
        matrix_transpose%rowSize = Mtx%colSize 
        matrix_transpose%colSize = Mtx%rowSize
        ALLOCATE(matrix_transpose%element(matrix_transpose%rowSize, &
                                               matrix_transpose%colSize))
        DO i = 1, Mtx%rowSize
          DO j = 1, Mtx%colSize
            matrix_transpose%element(i,j) = Mtx%element(j,i)
          END DO
        END DO
      END FUNCTION matrix_transpose
      
     ! ***************************************************************
     ! *                  subroutine matrixInverse                   *
     ! * Purpose: Create and print the inverse of the supplied       *
     ! *          matrix.                                            *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx                                      *
     ! * Returns: None                                               *
     ! * Input  : None                                               *
     ! * Output : Errors for cases where the inverse does not exist. *
     ! ***************************************************************
     
      SUBROUTINE matrixInverse(Mtx)
        USE matrix_module
        IMPLICIT NONE
        
        INTERFACE
        
          SUBROUTINE scalarMultiplication(scalar, Mtx, MtxProduct)
            USE matrix_module
            IMPLICIT NONE
            INTEGER, INTENT(IN)       :: scalar
            TYPE(MATRIX), INTENT(IN)  :: Mtx
            TYPE(MATRIX), INTENT(OUT) :: MtxProduct
          END SUBROUTINE scalarMultiplication
        
          RECURSIVE FUNCTION determinant(Mtx) RESULT(det)
            USE matrix_module
            IMPLICIT NONE
            TYPE(MATRIX), INTENT(IN) :: Mtx
            INTEGER :: det
          END FUNCTION determinant
		
          FUNCTION matrix_transpose(Mtx)
            USE matrix_module
            IMPLICIT NONE
            TYPE(MATRIX), INTENT(IN) :: Mtx
            TYPE(MATRIX)             :: matrix_transpose
          END FUNCTION matrix_transpose
        
          SUBROUTINE findCofactorMatrix(Mtx, cofactorMtx)
            USE matrix_module
            IMPLICIT NONE
            TYPE(MATRIX), INTENT(IN)  :: Mtx
            TYPE(MATRIX), INTENT(OUT) :: cofactorMtx
          END SUBROUTINE findCofactorMatrix
          
        END INTERFACE
        
        TYPE(MATRIX), INTENT(IN)  :: Mtx
        
        TYPE(MATRIX) :: cofactor_matrix
        INTEGER      :: i_determinant
        
        IF (Mtx%rowSize /= Mtx%colSize) THEN
          WRITE(*,*) "INVERSE DOES NOT EXIST FOR NON SQUARE MATRICES"
          RETURN
        END IF
        
        CALL findCofactorMatrix(Mtx, cofactor_matrix)
        cofactor_matrix = matrix_transpose(cofactor_matrix)
        i_determinant = determinant(Mtx)
        
        IF (i_determinant == 0) THEN
          WRITE(*,*) "Matrices with a determinant of 0 do not have ", &
                                                            "an inverse."
          RETURN
        END IF
        CALL print_inverseMatrix(i_determinant, cofactor_matrix)
      END SUBROUTINE matrixInverse
      
     ! ***************************************************************
     ! *              subroutine print_inverseMatrix                 *
     ! * Purpose: Prints the an inverse matrix as represented by     *
     ! *          it's determinant, and the transpose of it's        *
     ! *          cofactor matrix.                                   *
     ! *                                                             *
     ! * Accepts: determinant as an integer, and a matrix which is   *
     ! *          asumed to be the transpose of a cofactor matrix.   *
     ! * Returns: None                                               *
     ! * Input  : None                                               *
     ! * Output : The fraction 1 over determinant and the transpose  *
     ! *          of the cofactor matrix.                            *
     ! ***************************************************************
      
      SUBROUTINE print_inverseMatrix(determinant, cofactor_matrix)
        USE matrix_module
        IMPLICIT NONE
        
        INTEGER, INTENT(IN)      :: determinant
        TYPE(MATRIX), INTENT(IN) :: cofactor_matrix
        
        INTEGER :: i, j, k
        k = 0
        DO i = 1, cofactor_matrix%rowSize
          DO j = 1, cofactor_matrix%colSize
            IF ((i < cofactor_matrix%rowSize/2 .OR. &
                   i > cofactor_matrix%rowSize/2 + 3) .AND. j == 1) THEN
              WRITE(*,101, advance = 'no') "[ "
            ELSE IF (j == 1) THEN! (i >= cofactor_matrix%rowSize/2)
              IF (k == 0) THEN
                WRITE(*,102, advance = 'no') '1', "[ "
                k = k+1
              ELSE IF (k == 1) THEN
                WRITE(*,103, advance = 'no') "-----", "[ "
                k = k+1
              ELSE IF (k == 2) THEN
                IF (determinant < 100 .AND. determinant > -10) THEN
                  WRITE(*, 104, advance = 'no') determinant, "[ "
                ELSE IF (determinant < 10000 .AND. &
                                               determinant > -1000) THEN
                  WRITE(*, 105, advance = 'no') determinant, "[ "
                END IF
              END IF
            END IF
            WRITE(*, 106, advance = 'no') cofactor_matrix%element(i,j)
            IF (j == cofactor_matrix%colSize) &
              WRITE(*,107) " ]"
          END DO
        END DO
        
        ! ----- Format area -------
        101 FORMAT(T7, A)
        102 FORMAT(T3, A1, TR3, A)
        103 FORMAT(T1, A, TR1, A)
        104 FORMAT(T3, I2, TR2, A)
        105 FORMAT(T2, I3, TR2, A)
        106 FORMAT(" ", I3, " ")
        107 FORMAT(A)
        
      END SUBROUTINE print_inverseMatrix
      
     ! ***************************************************************
     ! *                 function reducedRow_matrix                  *
     ! * Purpose: Reduce a given matrix to reduced row echelon form. *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx                                      *
     ! * Returns: The RREF form of the given matrix.                 *
     ! * Input  : None                                               *
     ! * Output : none                                               *
     ! ***************************************************************
     
      FUNCTION reducedRow_matrix(mtx)
        USE matrix_module
        IMPLICIT NONE
        TYPE(MATRIX), INTENT(IN) :: mtx
        TYPE(MATRIX_REAL)        :: reducedRow_matrix
        
        INTEGER :: i, j, k, l
        REAL :: divisor, multiplier, temp
        
        ! copy mtx data into a real valued matrix
        reducedRow_matrix%rowSize = mtx%rowSize
        reducedRow_matrix%colSize = mtx%colSize
        ALLOCATE(reducedRow_matrix%element(reducedRow_matrix%rowSize, &
                                              reducedRow_matrix%colSize))
        DO i = 1, reducedRow_matrix%rowSize
          DO j = 1, reducedRow_matrix%colSize
            reducedRow_matrix%element(i,j) = mtx%element(i,j)
          END DO
        END DO
        
        ! Row swapping
        DO i = 1, reducedRow_matrix%colSize
          DO j = i, reducedRow_matrix%rowSize - 1
            IF (reducedRow_matrix%element(j,i) == 0) THEN
              DO k = j, reducedRow_matrix%rowSize
                IF (reducedRow_matrix%element(k,i) /= 0) THEN
                  DO l = 1, reducedRow_matrix%colSize
                    temp = reducedRow_matrix%element(j,l)
                    reducedRow_matrix%element(j,l) = &
                                           reducedRow_matrix%element(k,l)
                    reducedRow_matrix%element(k,l) = temp
                  END DO
                END IF
              END DO
            END IF
          END DO
        END DO
          
        ! Row reduction
        DO i = 1, reducedRow_matrix%rowSize
          divisor = reducedRow_matrix%element(i,i)
          DO j = 1, reducedRow_matrix%colSize
            IF (divisor == 0) EXIT
            reducedRow_matrix%element(i,j) = &
                                 reducedRow_matrix%element(i,j) / divisor
          END DO
          DO j = 1, reducedRow_matrix%rowSize
            IF (j /= i .AND. reducedRow_matrix%element(i,i) /= 0) THEN
              multiplier = reducedRow_matrix%element(j,i)
              DO k = 1, reducedRow_matrix%colSize
                reducedRow_matrix%element(j,k) = &
                                      reducedRow_matrix%element(j,k) - &
                              multiplier * reducedRow_matrix%element(i,k)
              END DO
            END IF
          END DO
        END DO
        
      END FUNCTION reducedRow_matrix
      
     ! ***************************************************************
     ! *             subroutine cramersRule_solutions                *
     ! * Purpose: Solve a system of linear equations using Cramer's  *
     ! *          Rule.                                              *
     ! *                                                             *
     ! * Accepts: A matrix, Mtx, to solutions vector (as a matrix    *
     ! *          type), vect, and a matrix type for the solutions   *
     ! *          to be returned in, solutions.                      *
     ! * Returns: The solutions to the system of equations.          *
     ! * Input  : None                                               *
     ! * Output : Errors if the given matrices are not correctly     *
     ! *          sized.                                             *
     ! ***************************************************************
      
      SUBROUTINE cramersRule_solutions(mtx, vect, solutions)
        USE matrix_module
        IMPLICIT NONE
        
        INTERFACE
          RECURSIVE FUNCTION determinant(Mtx) RESULT(det)
            USE matrix_module
            IMPLICIT NONE
            TYPE(MATRIX), INTENT(IN) :: Mtx
            INTEGER :: det
          END FUNCTION determinant
        END INTERFACE
        
        TYPE(MATRIX), INTENT(IN)  :: mtx, vect
        TYPE(MATRIX_REAL), INTENT(OUT) :: solutions
        
        INTEGER :: i, j
        TYPE(MATRIX) :: tmp_mtx
        REAL :: det
        
        IF (mtx%rowSize /= vect%rowSize .OR. mtx%rowSize /= &
                                                        mtx%colSize) THEN
          WRITE(*,*) "Matrices not corrently sized"
          RETURN
        END IF
        
        solutions%rowSize = mtx%rowSize
        solutions%colSize = 1
        ALLOCATE(solutions%element(solutions%rowSize,solutions%colSize))
                
        DO i = 1, mtx%colSize
          tmp_mtx = mtx
          DO j = 1, mtx%rowSize
            tmp_mtx%element(j,i) = vect%element(j,1)
          END DO
          solutions%element(i,1) = determinant(tmp_mtx)
        END DO
        
        det = determinant(mtx)
        DO i = 1, solutions%rowSize
          solutions%element(i,1) = solutions%element(i,1) / det
        END DO
        
      END SUBROUTINE cramersRule_solutions
