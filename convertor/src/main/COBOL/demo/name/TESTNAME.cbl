    IDENTIFICATION DIVISION.
    PROGRAM-ID. TESTNAME.
    DATA DIVISION.
        WORKING-STORAGE SECTION.
*   LOCAL
        01 VAR-SIMPLE PIC 9(9).
        01 CLS1.
            05 VAR-LV-A PIC 9(9).
        01 CLS-AMB1.
            05 VAR-AMB-AB PIC 9(9).
                10 VAR-AMB-A PIC 9(9).
        01 CLS-AMB2.
            05 VAR-AMB-AB PIC 9(9).
                10 VAR-AMB-A PIC 9(9).
        01 CLS-AMB3.
            05 VAR-AMB-AB.
                10 VAR-AMB-A PIC 9(9).
            05 VAR-AMB-ABC.
                10 VAR-AMB-A PIC 9(9).
*COPY
        01 NAME1.
            COPY NAME1.
        01 NAME2.
            COPY NAME2.
        01 NAME22.
            COPY NAME2.
        01 NAME33.
            05 VAR-NUM PIC 9(9).
            05 VAR-CP.
                COPY NAME3.
            05 NAME3.
                COPY NAME3.
    PROCEDURE DIVISION.
    001-LOCAL.
        DISPLAY VAR-SIMPLE.
        DISPLAY CLS1 VAR-LV-A.
    002-LOCAL-AMBIGUOUS.
        DISPLAY VAR-AMB-A OF CLS-AMB1.
        DISPLAY VAR-AMB-A OF VAR-AMB-AB OF CLS-AMB3.
    003-COPY.
        DISPLAY CB-SIMPLE.
        DISPLAY CB-SIMP2-A.
    004-COPY-OF-ONE.
        DISPLAY CB-SIMP2-B OF NAME1.
        DISPLAY CB-SIMP2-B OF CP-SIMP2.
        DISPLAY CB-SIMPC3-AB OF VAR-CP.
    005-COPY-OF-TWO.
        DISPLAY CB-SIMP3-AB OF CB-SIMPB3-A OF NAME22.
        DISPLAY CB-SIMP3-AB OF MY-NAME-1 OF NAME22.
    STOP RUN.