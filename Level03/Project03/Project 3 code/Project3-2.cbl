      ******************************************************************
      * Author: Franklin Yu, Hongyan Liu, Mengying Li, Doris Gao
      *            Rong Fu, Simon Ao
      * Date: 2021/08/14
      * Purpose: This program is designed to update INDEXSTUFILE3 using
      *            screen section pop up.
      *            This program only allows one update at a time.
      *            User to enter student ID and tuition payment.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3-2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INDEXED-STUDENT-FILE
                ASSIGN TO "INDEXSTUFILE3.TXT"
                 ORGANISATION IS INDEXED
                 ACCESS MODE IS RANDOM
                 RECORD KEY IS STUDENT-NUMBER-IN
                 FILE STATUS IS STATUS-FIELD.
       DATA DIVISION.
       FILE SECTION.
       FD  INDEXED-STUDENT-FILE.
       01  STUDENT-RECORD-IN.
           05 STUDENT-NUMBER-IN  PIC 9(6).
           05 TUITION-OWED-IN    PIC 9(4)V99.
           05 STUDENT-NAME-IN    PIC X(40).
           05 PROGRAM-CODE-IN    PIC X(5).
           05 COURSE-CODE-1-IN   PIC X(7).
           05 AVERAGE-1-IN       PIC 9(3).
           05 COURSE-CODE-2-IN   PIC X(7).
           05 AVERAGE-2-IN       PIC 9(3).
           05 COURSE-CODE-3-IN   PIC X(7).
           05 AVERAGE-3-IN       PIC 9(3).
           05 COURSE-CODE-4-IN   PIC X(7).
           05 AVERAGE-4-IN       PIC 9(3).
           05 COURSE-CODE-5-IN   PIC X(7).
           05 AVERAGE-5-OUT      PIC 9(3).

       WORKING-STORAGE SECTION.
       01  STUDENT-RECORD-OUT.
           05 STUDENT-NUMBER-WS  PIC 9(6).
           05 TUITION-OWED-WS    PIC 9(4)V99.
           05 STUDENT-NAME-WS    PIC X(40).
           05 PROGRAM-CODE-WS    PIC X(5).
           05 COURSE-CODE-1-WS   PIC X(7).
           05 AVERAGE-1-WS       PIC 9(3).
           05 COURSE-CODE-2-WS   PIC X(7).
           05 AVERAGE-2-WS       PIC 9(3).
           05 COURSE-CODE-3-WS   PIC X(7).
           05 AVERAGE-3-WS       PIC 9(3).
           05 COURSE-CODE-4-WS   PIC X(7).
           05 AVERAGE-4-WS       PIC 9(3).
           05 COURSE-CODE-5-WS   PIC X(7).
           05 AVERAGE-5-WS       PIC 9(3).

       01  TUITION-PAYMENT-WS          PIC 9(5)V99.
       01  NEW-TUITION-OWNED-WS        PIC 9(5)V99.
       01  STUDENT-NUMBER-INPUT-WS     PIC 9(6).
       01  STATUS-FIELD                PIC X(2).

       SCREEN SECTION.
       01 STUDENT-DATA-DISPLAY-SCREEN.
           05 PROMP-STU-NUM VALUE "STUDENT NUMBER" LINE 5 COLUMN 5.
           05 STUDENT-NUMBER                       LINE 5 COLUMN 25
                 PIC 9(6) TO STUDENT-NUMBER-IN.
           05 PROMP-TUITION VALUE "TUITION PAYMENT"LINE 7 COLUMN 5.
           05 TUITION-PAYMENT                      LINE 7 COLUMN 25
                 PIC $9999.99 TO TUITION-PAYMENT-WS.

       PROCEDURE DIVISION.
       100-CREATE-UPDATED-STU-RECORD.
           PERFORM 200-OPEN-FILES.
           PERFORM 201-UPDATE-ONE-TUITION.
           PERFORM 202-READ-FILES.
           PERFORM 203-UPDATE-ONE-STUDENT-RECORD.
           PERFORM 204-CLOSE-FILES.
           STOP RUN.

       200-OPEN-FILES.
           OPEN I-O INDEXED-STUDENT-FILE.

       201-UPDATE-ONE-TUITION.
           DISPLAY PROMP-STU-NUM
           ACCEPT STUDENT-NUMBER
           DISPLAY PROMP-TUITION
           ACCEPT TUITION-PAYMENT.

       202-READ-FILES.
           READ INDEXED-STUDENT-FILE
               INVALID KEY PERFORM 401-ERROR-RTN
           END-READ.

       203-UPDATE-ONE-STUDENT-RECORD.
           MOVE STUDENT-RECORD-IN TO STUDENT-RECORD-OUT.
           PERFORM 301-CALCULATE-TUITION-OWNED.
           PERFORM 302-REWRITE-STUDENT-RECORDS.

       204-CLOSE-FILES.
           CLOSE INDEXED-STUDENT-FILE.

       301-CALCULATE-TUITION-OWNED.
           SUBTRACT TUITION-PAYMENT-WS FROM TUITION-OWED-WS
               GIVING TUITION-OWED-WS.

       302-REWRITE-STUDENT-RECORDS.
           REWRITE STUDENT-RECORD-IN FROM STUDENT-RECORD-OUT
               INVALID KEY PERFORM 401-ERROR-RTN
               NOT INVALID KEY PERFORM 402-ERROR-FREE-RTN
           END-REWRITE.

       401-ERROR-RTN.
           DISPLAY "Invalid key" BLANK SCREEN LINE 21
                   COLUMN 16.

       402-ERROR-FREE-RTN.
           DISPLAY "Update successful" BLANK SCREEN LINE 21
                   COLUMN 16.

       END PROGRAM PROJECT3-2.
