       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile ASSIGN TO 'employee.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EmployeeFile.
       01 EmployeeRecord.
           05 EmployeeID       PIC X(10).
           05 EmployeeName     PIC X(30).
           05 EmployeeDept     PIC X(20).
           05 EmployeeSalary   PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-Found             PIC X VALUE 'N'.
       01 WS-EmployeeID        PIC X(10).
       01 WS-EmployeeName      PIC X(30).
       01 WS-EmployeeDept      PIC X(20).
       01 WS-EmployeeSalary    PIC 9(7)V99.
       01 WS-SearchID          PIC X(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILES
           PERFORM WRITE-RECORDS
           PERFORM READ-RECORDS
           PERFORM UPDATE-RECORD
           PERFORM DELETE-RECORD
           PERFORM CLOSE-FILES
           STOP RUN.

       OPEN-FILES.
           OPEN INPUT EmployeeFile
           IF FILE-STATUS = '00'
               DISPLAY 'File opened successfully.'
           ELSE
               DISPLAY 'Error opening file.'
               STOP RUN
           END-IF.

       WRITE-RECORDS.
           MOVE '001' TO EmployeeID
           MOVE 'John Doe' TO EmployeeName
           MOVE 'Sales' TO EmployeeDept
           MOVE 50000.00 TO EmployeeSalary
           PERFORM WRITE-EMPLOYEE-RECORD

           MOVE '002' TO EmployeeID
           MOVE 'Jane Smith' TO EmployeeName
           MOVE 'HR' TO EmployeeDept
           MOVE 60000.00 TO EmployeeSalary
           PERFORM WRITE-EMPLOYEE-RECORD.

       WRITE-EMPLOYEE-RECORD.
           WRITE EmployeeRecord
           IF FILE-STATUS = '00'
               DISPLAY 'Record written successfully.'
           ELSE
               DISPLAY 'Error writing record.'
               STOP RUN
           END-IF.

       READ-RECORDS.
           PERFORM UNTIL WS-EOF = 'Y'
               READ EmployeeFile INTO EmployeeRecord
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY EmployeeID SPACE EmployeeName SPACE EmployeeDept SPACE EmployeeSalary
               END-READ
           END-PERFORM.

       UPDATE-RECORD.
           MOVE '001' TO WS-SearchID
           PERFORM FIND-RECORD
           IF WS-Found = 'Y'
               MOVE 'Marketing' TO EmployeeDept
               REWRITE EmployeeRecord
               IF FILE-STATUS = '00'
                   DISPLAY 'Record updated successfully.'
               ELSE
                   DISPLAY 'Error updating record.'
               END-IF
           ELSE
               DISPLAY 'Record not found.'
           END-IF.

       DELETE-RECORD.
           MOVE '002' TO WS-SearchID
           PERFORM FIND-RECORD
           IF WS-Found = 'Y'
               DELETE EmployeeRecord
               IF FILE-STATUS = '00'
                   DISPLAY 'Record deleted successfully.'
               ELSE
                   DISPLAY 'Error deleting record.'
               END-IF
           ELSE
               DISPLAY 'Record not found.'
           END-IF.

       FIND-RECORD.
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-Found
           PERFORM UNTIL WS-EOF = 'Y' OR WS-Found = 'Y'
               READ EmployeeFile INTO EmployeeRecord
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF EmployeeID = WS-SearchID
                       MOVE 'Y' TO WS-Found
                   ELSE
                       CONTINUE
                   END-IF
           END-PERFORM.

       CLOSE-FILES.
           CLOSE EmployeeFile
           IF FILE-STATUS = '00'
               DISPLAY 'File closed successfully.'
           ELSE
               DISPLAY 'Error closing file.'
           END-IF.