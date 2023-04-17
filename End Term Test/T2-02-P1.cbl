       identification division.
       program-id. T2-02-P1. 
       author. yournamehere.
       date-written. date.
      *Program Description:
      *
       environment division.
       input-output section.
       file-control.
           select in-file 
           	   assign "../../../data/T2-02-P1.dat"
               organization is line sequential.
      *
           select print-file 
               assign "../../../data/T2-02-P1.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd in-file
           data record is in-rec
           record contains 20 characters.
      *
       01 in-rec.
           05 in-inv-code              pic xxx.
           05 in-product-name          pic x(10).
           05 in-type                  pic x.
           05 in-class                 pic x.
           05 in-unit-price            pic 999v99.
      *
       fd print-file
           record contains 132 characters
           data record is print-line.
      *
       01 print-line                   pic x(132).
      *
       working-storage section.
      *
       01 ws-heading1.
           05 filler                   pic x(30)
               value "     Name            Errors   ".
      *               ----+----1----+----2----+----3
       01 ws-eof-flag                  pic x
           value 'n'.
      *
       01 ws-detail-line.
           05 filler                   pic x(5).
           05 ws-dl-nam                pic x(10).
           05 filler                   pic x(5).
           05 ws-dl-error1             pic x(20).
           05 filler                   pic x(5).
           05 ws-dl-error2             pic x(20).
           05 filler                   pic x(5).
           05 ws-dl-error3             pic x(20).
           05 filler                   pic x(5).
           05 filler                   pic x(37).           
      *
       procedure division.
      *
       000-main.
      * 
          open input  in-file,
               output print-file.
      *
          read in-file 
          	  at end move 'y'          to ws-eof-flag.
      *
          write print-line from ws-heading1 
          	  after advancing 2 lines.
      *
          perform 100-process-logic 
          	  until ws-eof-flag = 'y'.
      *
          close in-file, 
                print-file.
      *
          stop run.
      * 
       100-process-logic.
      *
           write print-line from ws-detail-line 
           	   after advancing 2 lines.
      *
           read in-file 
           	   at end move 'y' to ws-eof-flag.
      *
	   end program T2-02-P1.
