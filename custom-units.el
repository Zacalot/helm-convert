(eval-after-load "calc-units"
  '(progn
     (setq math-additional-units
           '(
             (bit "b" "bit")
             (byte "8 bit" "Byte")
             (kb "1024 byte" "Kilobyte")
             (mb "1024 kb" "Megabyte")
             (gb "1024 mb" "Gigabyte")
             (tb "1024 gb" "Terabyte")
             (pb "1024 tb" "Petabyte")
             )
           math-units-table nil)))
