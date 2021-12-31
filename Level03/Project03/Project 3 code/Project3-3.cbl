      ******************************************************************
      * Author: Franklin Yu, Hongyan Liu, Mengying Li, Doris Gao
      *            Rong Fu, Simon Ao
      * Date: 2021/08/14
      * Purpose: This program is designed to generate a new student
      *            report by
      *            1) read and search program table with matching names
      *            2) calculate student average scores using subprogram.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3-3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-IN
                ASSIGN TO "INDEXSTUFILE3.TXT"
                  ORGANISATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS STUDENT-NUMBER
                  FILE STATUS IS STATUS-FIELD.

           SELECT PROGRAM-IN
                ASSIGN TO "PROGRAM.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-OUT
                ASSIGN TO "REPORT.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-IN.
       01  STUDENT-INFOR.
           05 STUDENT-NUMBER   PIC 9(6).
           05 TUITION-OWED     PIC 9(4)V99.
           05 STUDENT-NAME     PIC X(40).
           05 PROGRAM-CODE     PIC X(5).
           05 COURSE-CODE-1    PIC X(7).
           05 COURSE-AVG-1     PIC 9(3).
           05 COURSE-CODE-2    PIC X(7).
           05 COURSE-AVG-2     PIC 9(3).
           05 COURSE-CODE-3    PIC X(7).
           05 COURSE-AVG-3     PIC 9(3).
           05 COURSE-CODE-4    PIC X(7).
           05 COURSE-AVG-4     PIC 9(3).
           05 COURSE-CODE-5    PIC X(7).
           05 COURSE-AVG-5     PIC 9(3).
       FD  PROGRAM-IN.
           COPY "PROGRAM-INFOR.TXT".
      * 01  PROGRAM-INFOR.
      *     05 PROGRAM-CODE     PIC X(5).
      *     05 PROGRAM-NAME     PIC X(20).

       FD  REPORT-OUT.
       01  PRINTLINE           PIC X(500).

       WORKING-STORAGE SECTION.
       01  REPORT-WS.
           05 STUDENT-NAME-WS  PIC X(40).
           05 FILLER           PIC X(2) VALUE SPACES.
           05 STUDENT-AVG-WS   PIC 9(3).
           05 FILLER           PIC X(4) VALUE SPACES.
           05 PROGRAM-NAME-WS  PIC X(20).
           05 FILLER           PIC X(4) VALUE SPACES.
           05 TUITION-OWED-WS  PIC Z,ZZ9.99.
       01  REPORT-HEADING.
           05 FILLER           PIC X(40) VALUES
                               "NAME".
           05 FILLER           PIC X(9) VALUES
                               "AVERAGE".
           05 FILLER           PIC X(20) VALUES
                               "PROGRAM".
           05 FILLER           PIC X(12) VALUES
                               "TUITION OWED".
       01  CONTROL-FIELDS.
           05  EOF-PROGRAM         PIC X(3).
           05  EOF-STUDENT         PIC X(3).
           05  FOUND-TBL           PIC X(3).
           05  SUB                 PIC 99.
       01  AUDIT-TRAIL.
           05  COUNTER-READ     PIC 99.
           05  COUNTER-WRITE    PIC 99.
       01  PROGRAM-TBL.
           05 PROGRAM-VALUES-TBL OCCURS 20 TIMES .
               10 PROGRAM-CODE-TBL     PIC X(5).
               10 PROGRAM-NAME-TBL     PIC X(20).
       01  STATUS-FIELD                PIC X(2).
       01  SUB-PROGRAM          PIC X(75)
           VALUE "Project3-subprog.CBL".


       PROCEDURE DIVISION.
       100-PRODUCE-REPORT.
           PERFORM 201-INITIATE-STUDENT-FILE.
           PERFORM 202-WRITE-A-REPORT
               UNTIL EOF-STUDENT = "YES".
           PERFORM 203-TERMINATE-PROGRAM.
           STOP RUN.

       201-INITIATE-STUDENT-FILE.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-LOAD-PROGRAM-INFOR
               VARYING SUB FROM 1 BY 1
               UNTIL EOF-PROGRAM = "YES" OR SUB > 20.
           PERFORM 303-PRINT-HEADING.
           PERFORM 304-READ-STUDENT-INFOR.

       202-WRITE-A-REPORT.
           PERFORM 305-CALCULATE-AVERAGE.
           MOVE "NO" TO FOUND-TBL.
           PERFORM 306-SEARCH-PROGRAM-NAME
               VARYING SUB FROM 1 BY 1
               UNTIL FOUND-TBL = "YES" OR SUB > 20.
           PERFORM 307-MOVE-WRITE-DATA.
           PERFORM 304-READ-STUDENT-INFOR.

       203-TERMINATE-PROGRAM.
           PERFORM 308-DISPLAY-COUNTER.
           PERFORM 309-CLOSE-FILE.

       301-OPEN-FILES.
           OPEN INPUT STUDENT-IN, PROGRAM-IN.
           OPEN OUTPUT REPORT-OUT.

       302-LOAD-PROGRAM-INFOR.
           READ PROGRAM-IN
               AT END MOVE "YES" TO EOF-PROGRAM
               NOT AT END MOVE PROGRAM-INFOR TO PROGRAM-VALUES-TBL(SUB).

       303-PRINT-HEADING.
           WRITE PRINTLINE FROM REPORT-HEADING.

       304-READ-STUDENT-INFOR.
           READ STUDENT-IN
               AT END MOVE "YES" TO EOF-STUDENT
               NOT AT END ADD 1 TO  COUNTER-READ.

       305-CALCULATE-AVERAGE.
      *     COMPUTE STUDENT-AVG-WS ROUNDED
      *       = (COURSE-AVG-1 + COURSE-AVG-2 + COURSE-AVG-3 +
      *       COURSE-AVG-4+ COURSE-AVG-5) / 5.
           PERFORM 401-CALL-SUB-PROGRAM.

       306-SEARCH-PROGRAM-NAME.
           IF PROGRAM-CODE IN STUDENT-IN = PROGRAM-CODE-TBL(SUB)
           MOVE PROGRAM-NAME-TBL(SUB) TO PROGRAM-NAME-WS
           MOVE "YES" TO FOUND-TBL.

       307-MOVE-WRITE-DATA.
           MOVE STUDENT-NAME TO STUDENT-NAME-WS.
           MOVE TUITION-OWED TO TUITION-OWED-WS.
           WRITE PRINTLINE FROM REPORT-WS.
           ADD 1 TO COUNTER-WRITE.

       308-DISPLAY-COUNTER.
           DISPLAY "READ COUNTER: " COUNTER-READ
           "; "
           SPACE "WRITE COUNTER: " COUNTER-WRITE.

       309-CLOSE-FILE.
           CLOSE STUDENT-IN, PROGRAM-IN, REPORT-OUT.

       401-CALL-SUB-PROGRAM.
           CALL "Project3-subprog"
           USING STUDENT-AVG-WS,COURSE-AVG-1, COURSE-AVG-2,COURSE-AVG-3,
           COURSE-AVG-4,COURSE-AVG-5.

       END PROGRAM PROJECT3-3.
