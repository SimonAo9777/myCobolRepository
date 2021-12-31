      *AUTHOR:Simon A0
      *STUDENT NUMBER:040983402
      *Date:2021-06-08
      *LAB SECTONS: CST8283 303
       
       
       PROGRAM-ID. PROGRAM1 AS "PROGRAM1".

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  STUDENT-FILE-OUT
               ASSIGN TO  "D:\STUDENT-FILE.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.                     
       CONFIGURATION SECTION.
       DATA DIVISION.

       FILE SECTION.

       FD  STUDENT-FILE-OUT.
       01  STUDENT-RECORD-OUT    PIC X(52).
           
       WORKING-STORAGE SECTION.
       01  STUDENT-RECORD-IN.
           05  STUDENT-NUMBER-IN           PIC 9(6).
           05  STUDENT-TUITION-OWED-IN     PIC 9(6).
           05  STUDENT-NAME-IN             PIC X(40).

    

       01  PROMPTS-MESSAGES-RESPONSES.
           05  RECORD-PROMPT         PIC X(37)
               VALUE  "Please enter  Y or N".
           05  INPUT-RESPONSE        PIC X(1).
           
           05  STUDENT-NUMBER-PROMPT    PIC X(22)
               VALUE   "Enter Student Number:".
           
           05  TUITION-OWED-PROMPT    PIC X(24)
               VALUE  "Enter Tuition Owed:".
           
           05  STUDENT-NAME-IN-PROMPT     PIC X(26)
               VALUE  "Enter Student Name:".
       01 COUNTER.
           05 RECORD-COUNTER         PIC 9(1)    VALUE ZERO.  


       PROCEDURE DIVISION.

       100-CREATE-STUDENT-FILE.
           PERFORM  201-INITIATE-CREATE-FILE.
           PERFORM  202-CREATE-RECORDS
                    UNTIL INPUT-RESPONSE = "N".
           PERFORM  203-TERMINATE-CREATE-FILE.

           STOP RUN.

       201-INITIATE-CREATE-FILE.
           PERFORM  301-OPEN-FILES.
           PERFORM  302-PROMPT-FOR-DATA.

       202-CREATE-RECORDS.
           PERFORM 303-ENTER-STUDENT-DATA.
           PERFORM 304-WRITE-STUDENT-RECORD.
           PERFORM 302-PROMPT-FOR-DATA.

       203-TERMINATE-CREATE-FILE.
           PERFORM  305-CLOSE-FILES.
           PERFORM  306-END-MESSAGE.
      
       301-OPEN-FILES.
           OPEN OUTPUT STUDENT-FILE-OUT.

       302-PROMPT-FOR-DATA.
     
           DISPLAY RECORD-PROMPT
               LINE 16 COLUMN 10.
           ACCEPT INPUT-RESPONSE
               LINE 17 COLUMN 10.

       303-ENTER-STUDENT-DATA.
     
           INITIALIZE   STUDENT-RECORD-IN.
           
           DISPLAY "  " WITH BLANK SCREEN.
           
           ADD 1 TO RECORD-COUNTER.
           DISPLAY "RECORD " LINE 2 COLUMN 3.
           
           DISPLAY RECORD-COUNTER LINE 2 COLUMN 10.
           
          
           DISPLAY  STUDENT-NUMBER-PROMPT        LINE 3 COLUMN 6.
           ACCEPT   STUDENT-NUMBER-IN            LINE 3 COLUMN 28. 

           DISPLAY  TUITION-OWED-PROMPT          LINE 4  COLUMN 6.
           ACCEPT   STUDENT-TUITION-OWED-IN      LINE 4  COLUMN 28.
  
           DISPLAY  STUDENT-NAME-IN-PROMPT       LINE 5  COLUMN 6.
           ACCEPT   STUDENT-NAME-IN              LINE 5  COLUMN 28.

       304-WRITE-STUDENT-RECORD.
      
           MOVE  STUDENT-RECORD-IN  TO  STUDENT-RECORD-OUT.
           WRITE STUDENT-RECORD-OUT.
          
       305-CLOSE-FILES.
           CLOSE  STUDENT-FILE-OUT.
           
       306-END-MESSAGE.
           DISPLAY "PROJECT PROGRAM FINISHED"
               LINE 16 COLUMN 10.
           
           
      
       END PROGRAM PROGRAM1.