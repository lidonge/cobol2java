    IDENTIFICATION DIVISION.
    PROGRAM-ID. TESTEVAL.
    DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AGE PIC 9(9).
       01 INCOME PIC 9(9).
       01 GRADE PIC 9(9).
    PROCEDURE DIVISION.
       EVALUATE AGE ALSO INCOME
           WHEN 18 THRU 25 ALSO 30000 THRU 50000
               DISPLAY "Young Adult with moderate income"
           WHEN 26 THRU 35 ALSO 50000 THRU 80000
               DISPLAY "Adult with good income"
           WHEN OTHER
               DISPLAY "Not in criteria"
       END-EVALUATE.

       EVALUATE (GRADE*1)
           WHEN 90 THRU 100
               DISPLAY "Excellent"
           WHEN 80 THRU 89
               DISPLAY "Good"
           WHEN 70 THRU 79
               DISPLAY "Satisfactory"
           WHEN OTHER
               DISPLAY "Fail"
       END-EVALUATE.
       EVALUATE (GRADE/10)
           WHEN 9
               DISPLAY "Excellent"
           WHEN 8
               DISPLAY "Good"
           WHEN 7
               DISPLAY "Satisfactory"
           WHEN OTHER
               DISPLAY "Fail"
       END-EVALUATE.
       EVALUATE TRUE
           WHEN GRADE >= 90
               DISPLAY "A"
           WHEN GRADE >= 80
               DISPLAY "B"
           WHEN GRADE >= 70
               DISPLAY "C"
           WHEN OTHER
               DISPLAY "F"
       END-EVALUATE.
    STOP RUN.