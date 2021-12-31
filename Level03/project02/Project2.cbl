      ******************************************************************
      * Date:2021-07-19
      * Lab Section:CST8283 303
      * Author:Rong Fu(040956578), Simon Ao(040983402)

      *Purpose:The program needs to read student records and course
      *records from external files and generate a student report
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Project2.

      *-----------------------------------------------------------------
      *Definition file input and output
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT   STUFILE-FILE-IN
               ASSIGN TO "D:\STUFILE.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT   PROGRAM-FILE-IN
               ASSIGN TO "D:\PROGRAM.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT   STUFILEOUT-FILE-OUT
               ASSIGN TO "D:\STUFILEOUT.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      *-----------------------------------------------------------------
      *Input records for STUFILE.TXT file
      *-----------------------------------------------------------------
       FD  STUFILE-FILE-IN.
       01  INPUT-RECORDS.
           05 STUDENT-NUMBER        PIC 9(6).
           05 TUITION-OWED          PIC 9(4)V99.
           05 STUDENT-NAME          PIC X(40).
           05 PROGRAM-OF-STUDY      PIC X(5).
           05 COURSE-CODE-1         PIC X(7).
           05 COURSE-AVERAGE-1      PIC 9(3).
           05 COURSE-CODE-2         PIC X(7).
           05 COURSE-AVERAGE-2      PIC 9(3).
           05 COURSE-CODE-3         PIC X(7).
           05 COURSE-AVERAGE-3      PIC 9(3).
           05 COURSE-CODE-4         PIC X(7).
           05 COURSE-AVERAGE-4      PIC 9(3).
           05 COURSE-CODE-5         PIC X(7).
           05 COURSE-AVERAGE-5      PIC 9(3).

      *-----------------------------------------------------------------
      *Input records for PROGRAM.TXT file
      *-----------------------------------------------------------------
       FD  PROGRAM-FILE-IN.
       01  PROGRAM-RECORD-IN.
           05 PROGRAM-CODE-IN          PIC X(5).
           05 PROGRAM-NAME-IN          PIC X(20).

      *-----------------------------------------------------------------
      *Output records for STUFILEOUT.TXT file
      *-----------------------------------------------------------------
       FD  STUFILEOUT-FILE-OUT.
       01  STUFILEOUT-RECORD         PIC X(83).

       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      *Output STUFILEOUT information
      *-----------------------------------------------------------------
       01  STUDENT-REPORT-INFO.
           05 STUDENT-NAME-INFO          PIC X(40).
           05 FILLER                      PIC X(2) VALUE SPACES.
           05 STUDENT-AVERAGE-INFO       PIC ZZ9.
           05 FILLER                      PIC X(4) VALUE SPACES.
           05 PROGRAM-NAME-INFO          PIC X(20).
           05 FILLER                      PIC X(4) VALUE SPACES.
           05 TUITION-OWED-INFO          PIC Z,ZZ9.99.
      *-----------------------------------------------------------------
      *Describes information for program
      *-----------------------------------------------------------------
       01  PROGRAM-REPORT-INFO.
           05 PROGRAM-INFO OCCURS 20 TIMES.
              10 PROGRAM-CODE-INFO          PIC X(5).
              10 PROGRAM-NAME-INFO-ORI      PIC X(20).
      *-----------------------------------------------------------------
      *Describes student report header
      *-----------------------------------------------------------------
       01  RECORD-INFO-HEADER.
           05 FILLER                PIC X(40) VALUE "NAME".
           05 FILLER                PIC X(1) VALUE SPACES.
           05 FILLER                PIC X(7) VALUE "AVERAGE".
           05 FILLER                PIC X(2) VALUE SPACES.
           05 FILLER                PIC X(20) VALUE "PROGRAM".
           05 FILLER                PIC X(1) VALUE SPACES.
           05 FILLER                PIC X(20) VALUE "TUITION OWED".
      *-----------------------------------------------------------------
      *Describes the response of user
      *-----------------------------------------------------------------
       01  CONTROL-INFO-FIELDS.
           05 EOF-FLAG-RE            PIC X(3).
           05 EOF-FLAG-TE            PIC X(3).
           05 READ-FLAG            PIC X(3) VALUE "NO".
           05 TOTAL-GRADE             PIC 9(3).
           05 FOUND-PROGRAM-CODE   PIC X(5).
       01  SUBSCRIPT.
           05 SUB-1             PIC 9(2) VALUE 0.
           05 SUB-2             PIC 9(2) VALUE 0.
       01  TRAIL-FILE.
           05 SEARCH-FRI-1        PIC 9(2).
           05 SEARCH-FRI-2        PIC 9(2).
           05 WRITE-FRI         PIC 9(2).

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      * Main procedure
      * Run the code
      *-----------------------------------------------------------------
       100-PRODUCE-REPORTS.
           PERFORM 201-INITIATE-REPORTS.
           PERFORM 202-PRODUCE-REPORTS
             UNTIL EOF-FLAG-TE EQUALS "YES".
           PERFORM 203-TERMINATE-REPORTS.
           STOP RUN.

      *-----------------------------------------------------------------
      * Executed paragraph of the start-up procedure
      *-----------------------------------------------------------------
       201-INITIATE-REPORTS.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-READ-PROGRAM-RECORD
             VARYING SUB-1   FROM  1  BY  1
             UNTIL  EOF-FLAG-RE  IS  EQUAL  TO  "YES".
           PERFORM 303-READ-STUDENT-RECORD.
           PERFORM 304-OUTPUT-HEADER-FILE.

      *-----------------------------------------------------------------
      * Output student report
      *-----------------------------------------------------------------
       202-PRODUCE-REPORTS.
           PERFORM 305-SEARCH-PROGRAM-FILE
             VARYING SUB-1   FROM  1  BY  1
             UNTIL  SUB-1 > SEARCH-FRI-1 OR READ-FLAG IS EQUAL TO "YES".
           PERFORM 306-NOT-EXIST-RTN.
           PERFORM 307-OUTPUT-REPORT-RTN.
           PERFORM 303-READ-STUDENT-RECORD.

      *-----------------------------------------------------------------
      * End a program
      *-----------------------------------------------------------------
       203-TERMINATE-REPORTS.
           CLOSE STUFILE-FILE-IN.
           CLOSE PROGRAM-FILE-IN.
           CLOSE STUFILEOUT-FILE-OUT.
           DISPLAY SEARCH-FRI-2 " STUDENT RECORDS WERE READ. ".
           DISPLAY WRITE-FRI " STUDENT RECORDS WERE OUTPUT. ".
      *-----------------------------------------------------------------
      * Open the file
      *-----------------------------------------------------------------
       301-OPEN-FILES.
           OPEN INPUT STUFILE-FILE-IN.
           OPEN INPUT PROGRAM-FILE-IN.
           OPEN OUTPUT STUFILEOUT-FILE-OUT.
      *-----------------------------------------------------------------
      * Read the program file
      *-----------------------------------------------------------------
       302-READ-PROGRAM-RECORD.
           READ PROGRAM-FILE-IN
              AT END
              MOVE  "YES"  TO  EOF-FLAG-RE
              NOT AT END
                 ADD 1 TO SEARCH-FRI-1.
                 IF SUB-1 <= SEARCH-FRI-1
                 MOVE PROGRAM-RECORD-IN TO PROGRAM-INFO (SUB-1)
                 END-IF.

      *-----------------------------------------------------------------
      * Read the student file
      *-----------------------------------------------------------------
       303-READ-STUDENT-RECORD.
           READ STUFILE-FILE-IN
              AT END
              MOVE  "YES"  TO  EOF-FLAG-TE
               NOT AT END
                ADD 1 TO SEARCH-FRI-2
                MOVE STUDENT-NAME TO STUDENT-NAME-INFO
                ADD COURSE-AVERAGE-1, COURSE-AVERAGE-2, COURSE-AVERAGE-3
                COURSE-AVERAGE-4, COURSE-AVERAGE-5 GIVING TOTAL-GRADE
                DIVIDE TOTAL-GRADE BY 5
                       GIVING STUDENT-AVERAGE-INFO ROUNDED
                MOVE TUITION-OWED TO TUITION-OWED-INFO
                MOVE PROGRAM-OF-STUDY TO FOUND-PROGRAM-CODE.
      *-----------------------------------------------------------------
      * Output the student report header
      *-----------------------------------------------------------------
       304-OUTPUT-HEADER-FILE.
             DISPLAY RECORD-INFO-HEADER.
              WRITE STUFILEOUT-RECORD FROM RECORD-INFO-HEADER.

      *-----------------------------------------------------------------
      * Search the program name
      *-----------------------------------------------------------------
       305-SEARCH-PROGRAM-FILE.
             IF FOUND-PROGRAM-CODE = PROGRAM-CODE-INFO(SUB-1)
                MOVE  PROGRAM-NAME-INFO-ORI(SUB-1)
                      TO PROGRAM-NAME-INFO
                MOVE "YES" TO READ-FLAG
             END-IF.

      *-----------------------------------------------------------------
      *The executed paragraph that runs when the  code is not found
      *-----------------------------------------------------------------
       306-NOT-EXIST-RTN.
            IF READ-FLAG IS EQUAL TO "NO"
                 MOVE "PROGRAM NOT FOUND" TO PROGRAM-NAME-INFO
            END-IF.
      *-----------------------------------------------------------------
      *Output student report
      *-----------------------------------------------------------------
       307-OUTPUT-REPORT-RTN.
             DISPLAY STUDENT-REPORT-INFO.
             WRITE STUFILEOUT-RECORD FROM STUDENT-REPORT-INFO.
             ADD 1 TO WRITE-FRI.
             INITIALIZE READ-FLAG.

       END PROGRAM Project2.
