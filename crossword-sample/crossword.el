(require 'matrix)

(defun make-crossword (size)
  "Make a crossword grid with SIZE rows and columns."
  (if (zerop (% size 2))
      (error "make-crossword: size must be odd"))
  (if (< size 3)
      (error "make-crossword: size must be 3 or greater"))
  (make-matrix size size nil))


(defun crossword-size (crossword)
  "Number of rows/columns (it's square) in CROSSWORD."
  (matrix-rows crossword))

(defun crossword-ref (crossword row column)
  "Get the element of CROSSWORD at ROW and COLUMN."
  (matrix-ref crossword row column))

(defun crossword--set (crossword row column)
  "Internal function for setting (ROW, COLUMN) in CROSSWORD."
  (matrix-set crossword row column elt))

(defun crossword-cousin-position (crossword row column)
  "Return the cousin position for (ROW, COLUMN) in CROSSWORD."
  (let ((size (crossword-size crossword)))
    (cons (- size row 1) (- size column 1))))

(defun crossword-cousin-ref (crossword row column)
  "Get the cousin element to (ROW, COLUMN) in CROSSWORD."
  (let ((cousin-position (crossword-cousin-position crossword
						    row
						    column)))
    (crossword-ref crossword
		   (car cousin-position)
		   (cdr cousin-position))))

(defun crossword--cousin-set (crossword row column elt)
  "Internal function for setting the cousin of a cell."
  (let ((cousin-position (crossword-cousin-position crossword
						    row
						    column)))
    (crossword--set crossword (car cousin-position)
		              (cdr cousin-position)
			      elt)))

(defun crossword-store-letter (crossword row column letter)
  "Set (ROW, COLUMN) in CROSSWORD to LETTER."
  (crossword--set crossword row column letter)
  (if (numberp (crossword-cousin-ref crossword row))
      nil
    (crossword--cousin-set crossword row column 'letter)))

(defun crossword-store-block (crossword row column)
  "Make (ROW, COLUMN) in CROSSWORD a block."
  (crossword--set crossword row column 'block)
  (crossword--cousin-set crossword row column 'block))
