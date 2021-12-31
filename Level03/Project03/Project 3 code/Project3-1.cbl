      ******************************************************************
      * Author: Franklin Yu, Hongyan Liu, Mengying Li, Doris Gao
      *            Rong Fu, Simon Ao
      * Date: 2021/08/14
      * Purpose: This program is designed to convert STUFILE3 to index
      *            file.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3-1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-IN
                     ASSIGN TO "STUFILE3.TXT"
                       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INDEXED-STUDENT-FILE
                     ASSIGN TO "INDEXSTUFILE3.TXT"
                       ORGANIZATION IS INDEXED
                       RECORD KEY IS STUDENT-NUMBER-OUT
                       ACCESS MODE IS SEQUENTIAL
                       FILE STATUS IS STATUS-FIELD.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE-IN.
       01  STUDENT-RECORD-IN.
           05 STUDENT-NUMBER  PIC 9(6).
           05 TUITION-OWED    PIC 9(4)V99.
           05 STUDENT-NAME    PIC X(40).
           05 PROGRAM-CODE    PIC X(5).
           05 COURSE-CODE-1   PIC X(7).
           05 AVERAGE-1       PIC 9(3).
           05 COURSE-CODE-2   PIC X(7).
           05 AVERAGE-2       PIC 9(3).
           05 COURSE-CODE-3   PIC X(7).
           05 AVERAGE-3       PIC 9(3).
           05 COURSE-CODE-4   PIC X(7).
           05 AVERAGE-4       PIC 9(3).
           05 COURSE-CODE-5   PIC X(7).
           05 AVERAGE-5       PIC 9(3).

       FD  INDEXED-STUDENT-FILE.
       01  STUDENT-RECORD-OUT.
           05 STUDENT-NUMBER-OUT  PIC 9(6).
           05 TUITION-OWED-OUT    PIC 9(4)V99.
           05 STUDENT-NAME-OUT    PIC X(40).
           05 PROGRAM-CODE-OUT    PIC X(5).
           05 COURSE-CODE-1-OUT   PIC X(7).
           05 AVERAGE-1-OUT       PIC 9(3).
           05 COURSE-CODE-2-OUT   PIC X(7).
           05 AVERAGE-2-OUT       PIC 9(3).
           05 COURSE-CODE-3-OUT   PIC X(7).
           05 AVERAGE-3-OUT       PIC 9(3).
           05 COURSE-CODE-4-OUT   PIC X(7).
           05 AVERAGE-4-OUT       PIC 9(3).
           05 COURSE-CODE-5-OUT   PIC X(7).
           05 AVERAGE-5-OUT       PIC 9(3).

       WORKING-STORAGE SECTION.
       01  CONTROL-FIELD.
           05 EOF-FLAG      PIC X(3) VALUE "NO".
           05 VALID-FLAG    PIC X(3).
       01  STATUS-FIELD     PIC X(2).

       PROCEDURE DIVISION.
       100-CREATE-INDEXED-STUDENT-FILE.
           PERFORM   201-INITIALIZE-CREATE-FILE.
           PERFORM   202-CREATE-IND-STUDENT-RECORDS
                       UNTIL  EOF-FLAG = "YES".
           PERFORM   203-TERMINATE-PROGRAM.
           STOP RUN.
       201-INITIALIZE-CREATE-FILE.
           PERFORM  301-OPEN-STUDENT-FILES.
           PERFORM  302-READ-STUDENT-FILE-IN.

       202-CREATE-IND-STUDENT-RECORDS.
           PERFORM  303-WRITE-IND-STUDENT-RECORD.
           PERFORM  302-READ-STUDENT-FILE-IN.

       203-TERMINATE-PROGRAM.
           PERFORM 304-CLOSE-FILES.

       301-OPEN-STUDENT-FILES.
           OPEN INPUT  STUDENT-FILE-IN
           OPEN OUTPUT INDEXED-STUDENT-FILE.

       303-WRITE-IND-STUDENT-RECORD.
           WRITE STUDENT-RECORD-OUT FROM STUDENT-RECORD-IN
                INVALID KEY PERFORM 401-ERROR-RTN
                NOT INVALID KEY MOVE "YES" TO VALID-FLAG
                                DISPLAY STUDENT-RECORD-OUT.

       302-READ-STUDENT-FILE-IN.
           READ  STUDENT-FILE-IN
           AT END  MOVE "YES" TO EOF-FLAG.

       304-CLOSE-FILES.
           CLOSE STUDENT-FILE-IN INDEXED-STUDENT-FILE.

       401-ERROR-RTN.
             DISPLAY "Invalid key".

       END PROGRAM PROJECT3-1.
