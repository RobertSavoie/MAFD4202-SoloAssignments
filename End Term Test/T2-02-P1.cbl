       identification division.
       program-id. T2-02-P1. 
       author. Rob Savoie.
       date-written. April 17/2023.
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
           05 in-inv-code              pic 999.
           05 in-product-name          pic x(10).
           05 in-type                  pic x.
               88 is-correct-type      value "A", "B", "C".
           05 in-class                 pic x.
               88 is-correct-class     value "2", "4".
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
           05 filler                   pic x(102).
      *
       01 ws-eof-flag                  pic x value "n".
       01 ws-eof-y                     pic x value "y".
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
      *constants
       77 inv-code-err                 pic x(16) value
                                       "INV CODE INVALID".
       77 type-err                     pic x(12) value "TYPE INVALID".
       77 class-err                    pic x(13) value "CLASS INVALID".
      *
      *counters
       01 ws-counters.
           05 ws-err-cnt               pic 9 value 0.
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
           if in-inv-code is not numeric
               add 1 to ws-err-cnt
               move inv-code-err to ws-dl-error1
           end-if.
           if not is-correct-type
               if ws-err-cnt = 0
                   add 1 to ws-err-cnt
                   move type-err to ws-dl-error1
               else if ws-err-cnt = 1
                   add 1 to ws-err-cnt
                   move type-err to ws-dl-error2
               end-if
               end-if
           end-if.
           if not is-correct-class
               if ws-err-cnt = 0
                   add 1 to ws-err-cnt
                   move class-err to ws-dl-error1
               else if ws-err-cnt = 1
                   add 1 to ws-err-cnt
                   move class-err to ws-dl-error2
               else if ws-err-cnt = 2
                       add 1 to ws-err-cnt
                       move class-err to ws-dl-error3
               end-if
               end-if
               end-if
           end-if.
           if ws-err-cnt > 0
               move in-product-name to ws-dl-nam
               write print-line from ws-detail-line 
           	       after advancing 2 lines
               move 0 to ws-err-cnt
               move spaces to ws-detail-line
           end-if.
      *
           read in-file 
           	   at end move 'y' to ws-eof-flag.
      *
       200-error-check.
      *

      *
	   end program T2-02-P1.
