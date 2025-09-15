;|
Quick Block
Creates a block instantly out of the objects that you select
Found at http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Quick-block/td-p/3454228
|;

(defun c:QB	 (/ ss basePt number Blockname)
;;; Tharwat 11. May. 2012 ;;
 (if (and (setq ss (ssget "_:L"))
		  (setq basePt (getpoint "\n Specify insertion point :")))
  (progn (setq number	 1
			   Blockname (strcat "MyBlock" (itoa number)))
		 (while	(tblsearch "BLOCK" Blockname)
		  (setq Blockname (strcat "MyBlock" (itoa (setq number (1+ number))))))
		 (command "_.-Block" Blockname basePt ss "")
		 (command "_.-insert" Blockname basePt "" "" "")) 
  (princ))
 (princ))
