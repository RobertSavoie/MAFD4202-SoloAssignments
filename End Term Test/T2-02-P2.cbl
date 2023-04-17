       identification division.
       program-id. T2-02-P2.  
       author. Rob Savoie.
       date-written. April 17/2023.
      *Program Description:
      *
       environment division.
       input-output section.
       file-control.
      *
           select sales-file  
               assign to "../../../data/T2-02-P2.dat"
               organization is line sequential.
      *
           select print-file 
               assign to "../../../data/T2-02-P2.out"
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd sales-file 
           data record is sales-rec
           record contains 26 characters.
      *
       01 sales-rec.
           05 in-name                  pic x(20).
           05 in-comm                  pic 9(5).
           05 in-rating                pic x.
              88 in-rating-A-88
                  value 'A'.
              88 in-rating-B-88
                  value 'B'.
              88 in-rating-C-88
                  value 'C'.
              88 in-rating-D-88
                  value 'D'.
      *
       fd print-file
           data record is print-line
           record contains 132 characters.
      *
       01 print-line                   pic x(132).
      *
       working-storage section.
      *
       01 ws-eof-flag                  pic x     value "N".
       01 ws-math.
           05 ws-total-incr-amount-adj pic 9.
           05 ws-total-incr-amount-calc pic 9.
      *
       01 ws-detail-output.
           05 filler                   pic x(5).
           05 ws-prt-name              pic x(20).
           05 filler                   pic x(5).
           05 ws-prt-old-comm          pic zzz,zz9.
           05 filler                   pic x(5).
           05 ws-prt-calc-incr         pic zzz,zz9.
           05 filler                   pic x(5).
           05 ws-prt-actual-incr       pic zzz,zz9.
           05 filler                   pic x(6).
           05 ws-prt-new-comm          pic zzz,zz9.
           05 filler                   pic x(4).
           05 ws-prt-comment           pic x(30).
           05 filler                   pic x(24)
               value spaces.
      *
       01 ws-heading1.
           05 filler                   pic x(8)  value '    name'.
           05 filler                   pic x(23) value spaces.
           05 filler                   pic x(3)  value 'old'.
           05 filler                   pic x(7)  value spaces.
           05 filler                   pic x(10) value 'calculated'.
           05 filler                   pic x(4)  value spaces.
           05 filler                   pic x(6)  value 'actual'.
           05 filler                   pic x(8)  value spaces.
           05 filler                   pic x(3)  value 'new'.
           05 filler                   pic x(60)
               value spaces. 
      *
       01 ws-heading2.
           05 filler                   pic x(30) value spaces.
           05 filler                   pic x(6)  value ' comm '.
           05 filler                   pic x(7)  value spaces.
           05 filler                   pic x(6)  value ' incr '.
           05 filler                   pic x(6)  value spaces.
           05 filler                   pic x(6)  value ' incr '.
           05 filler                   pic x(7)  value spaces.
           05 filler                   pic x(6)  value ' comm '.
           05 filler                   pic x(58)
               value spaces.
      *
       01 ws-total-line.
           05 filler                   pic x(39)
               value " total calculated increase amount    = ".
      *               ----+----1----+----2----+----3----+----
           05 ws-tl-total-incr-calc    pic $$,$$$,$$9.
           05 filler                   pic x(83)
               value spaces.
      *
       01 ws-total-adj-line.
           05 filler                   pic x(30)
               value " total actual increase amount ".
      *               ----+----1----+----2----+----3----+----4---
           05 filler                   pic x(21)
               value "                   = ".
           05 ws-tl-total-incr-adj     pic $$,$$$,$$9.
           05 filler                   pic x(81)
               value spaces.
      *       
       procedure division.
       000-main.
      *
           open input  sales-file,
                output print-file.
      *
           write print-line from ws-heading1
               after advancing 1 line.
           write print-line from ws-heading2
               after advancing 1 line.
      *
           read sales-file
               at end move "Y"          to ws-eof-flag.
      *
           perform 100-process-logic
               until ws-eof-flag = "Y".
      *
           move ws-total-incr-amount-adj
             to ws-tl-total-incr-adj.
           move ws-total-incr-amount-calc
             to ws-tl-total-incr-calc.
      *
           write print-line from ws-total-line
               after advancing 2 lines.
           write print-line from ws-total-adj-line
               after advancing 2 lines.
      *
           close sales-file, print-file.
      *
           stop run.
      *
       100-process-logic.
      *  main logic paragraph - add main loop logic here
      *

      *
           write print-line from ws-detail-output 
               after advancing 2 lines.
      *
           read sales-file 
               at end move "Y"         to ws-eof-flag.
      *
       end program T2-02-P2.