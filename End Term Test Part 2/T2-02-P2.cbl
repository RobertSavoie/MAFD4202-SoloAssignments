       identification division.
       program-id. T2-02-P2.  
       author. Rob Savoie.
       date-written. April 17/2023.
      *Program Description: Creates generated output from input file
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
           05 in-name                   pic x(20).
           05 in-comm                   pic 9(5).
           05 in-rating                 pic x.
              88 in-rating-A
                  value 'A'.
              88 in-rating-B
                  value 'B'.
              88 in-rating-C
                  value 'C'.
              88 in-rating-D
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
       01 ws-eof-flag                  pic x       value "N".
      *
       01 ws-math.
           05 total-incr-amount-adj    pic 9(6).
           05 total-incr-amount-calc   pic 9(6).
           05 calc-increase-total      pic 9(6).
           05 calc-increase-temp       pic 9(6).
           05 calc-increase-actual     pic 9(6).
           05 calc-above-max           pic 9(6).
      *constants
      *
       77 cnst-max-comm                pic 9(5)    value 80000.
       77 cnst-pcnt-b-inc-small        pic 9v999   value 0.155.
       77 cnst-pcnt-c-inc-small        pic 9v999   value 0.315.
       77 cnst-pcnt-flat-inc-small     pic 9v9     value 0.1.
       77 cnst-pcnt-b-inc-big          pic 9v999   value 1.155.
       77 cnst-pcnt-c-inc-big          pic 9v999   value 1.315.
       77 cnst-pcnt-flat-inc-big       pic 9v9     value 1.1.
       77 cnst-comm-adj                pic x(19) value
                                       "COMMISSION ADJUSTED".

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
           05 filler                   pic x(24)   value spaces.
      *
       01 ws-heading1.
           05 filler                   pic x(8)    value '    name'.
           05 filler                   pic x(23)   value spaces.
           05 filler                   pic x(3)    value 'old'.
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(10)   value 'calculated'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(6)    value 'actual'.
           05 filler                   pic x(8)    value spaces.
           05 filler                   pic x(3)    value 'new'.
           05 filler                   pic x(60)   value spaces.
      *
       01 ws-heading2.
           05 filler                   pic x(30)   value spaces.
           05 filler                   pic x(6)    value ' comm '.
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(6)    value ' incr '.
           05 filler                   pic x(6)    value spaces.
           05 filler                   pic x(6)    value ' incr '.
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(6)    value ' comm '.
           05 filler                   pic x(58)   value spaces.
      *
       01 ws-total-line.
           05 filler                   pic x(39)
               value " total calculated increase amount    = ".
      *               ----+----1----+----2----+----3----+----
           05 ws-tl-total-incr-calc    pic $$,$$$,$$9.
           05 filler                   pic x(83)   value spaces.
      *
       01 ws-total-adj-line.
           05 filler                   pic x(39)
               value " total actual increase amount        = ".
      *               ----+----1----+----2----+----3----+----
           05 ws-tl-total-incr-adj     pic $$,$$$,$$9.
           05 filler                   pic x(83)   value spaces.
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
           move total-incr-amount-adj
             to ws-tl-total-incr-adj.
           move total-incr-amount-calc
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
           perform 200-logic.
      *
           write print-line from ws-detail-output 
               after advancing 2 lines.
      *
           move spaces to ws-detail-output.
      *
           read sales-file 
               at end move "Y"         to ws-eof-flag.
      *
       200-logic.
      *runs all logic for the program
      *
           move in-name to ws-prt-name.
           move in-comm to ws-prt-old-comm.
           multiply in-comm
                 by cnst-pcnt-flat-inc-small
             giving calc-increase-temp.
      *
           add calc-increase-temp
            to total-incr-amount-calc
      *
           multiply in-comm
                 by cnst-pcnt-flat-inc-big
             giving calc-increase-total.
           perform 300-a-d-logic.
           perform 400-b-logic.
           perform 500-c-logic.
      *
       300-a-d-logic.
      *does logic for calculting a and b type entries
      *
           if in-rating-A or in-rating-D
               multiply in-comm
                     by cnst-pcnt-flat-inc-big
                 giving ws-prt-new-comm
      *
               move calc-increase-temp to ws-prt-calc-incr
               move calc-increase-temp to ws-prt-actual-incr
                add calc-increase-temp
                 to total-incr-amount-adj
           end-if.
      *
       400-b-logic.
      *does logic for calculting b-type entries
      *
           if in-rating-B
               multiply calc-increase-total
                     by cnst-pcnt-b-inc-small
                 giving calc-increase-actual rounded
      *
                    add calc-increase-actual   to total-incr-amount-calc
      *        
                    add calc-increase-temp       to calc-increase-actual
                   move calc-increase-actual     to ws-prt-calc-incr
      *      
                    add in-comm
                     to calc-increase-actual
                 giving calc-increase-total
      *
               if in-comm = cnst-max-comm
                   move 0                        to ws-prt-actual-incr
                   move cnst-comm-adj            to ws-prt-comment
                   move calc-increase-actual     to ws-prt-calc-incr
                   move cnst-max-comm            to ws-prt-new-comm
               else
      *
                   if calc-increase-total >= cnst-max-comm
                       move cnst-max-comm            to ws-prt-new-comm
                       subtract cnst-max-comm
                           from calc-increase-total
                         giving calc-above-max
      *
                       subtract calc-above-max
                           from calc-increase-actual
      *
                       move cnst-comm-adj        to ws-prt-comment
                   end-if
                       move calc-increase-actual to ws-prt-actual-incr
                       move calc-increase-total  to ws-prt-new-comm
      *
                       add calc-increase-actual
                        to total-incr-amount-adj
               end-if
           end-if.
      *
       500-c-logic.
      *does logic for calculting c-type entries
      *
           if in-rating-C
               multiply calc-increase-total
                     by cnst-pcnt-c-inc-small
                 giving calc-increase-actual rounded
      *
                    add calc-increase-actual   to total-incr-amount-calc
      *        
                    add calc-increase-temp       to calc-increase-actual
                   move calc-increase-actual     to ws-prt-calc-incr
      *      
                    add in-comm
                     to calc-increase-actual
                 giving calc-increase-total
      *
               if in-comm >= cnst-max-comm
                   move 0                        to ws-prt-actual-incr
                   move cnst-comm-adj            to ws-prt-comment
                   move calc-increase-actual     to ws-prt-calc-incr
                   move cnst-max-comm            to ws-prt-new-comm
      *
               else
      *
                   if calc-increase-total >= cnst-max-comm
                       move cnst-max-comm            to ws-prt-new-comm
                       subtract cnst-max-comm
                           from calc-increase-total
                         giving calc-increase-actual
      *
                       subtract calc-above-max
                           from calc-increase-actual
      *
                       move cnst-comm-adj        to ws-prt-comment
                   end-if
                       move calc-increase-actual to ws-prt-actual-incr
                       move calc-increase-total  to ws-prt-new-comm
      *
                       add calc-increase-actual
                        to total-incr-amount-adj
               end-if
           end-if.
      *
       end program T2-02-P2.