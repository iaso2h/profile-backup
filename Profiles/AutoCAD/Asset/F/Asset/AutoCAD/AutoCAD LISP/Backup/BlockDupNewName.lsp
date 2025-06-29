;|
BlockDupNewName.lsp [command name: BDN]
To make a new Block that is a Duplicate of a selected Block [or WMF], with
  a New block Name, replacing the selected Block insertion, and which can
  then be edited if desired for variance(s) in content compared to the selected
  one, without affecting other insertions of the original Block name.
Offers default new Block name = old name with "-1" added, "-2" or beyond
  if already used.  If name already ends with hyphenated-number suffix,
  offers next not-yet-used number in place of current number ending.
  Accepts any typed-in new name instead; asks again if User input is already
  a Block name in the drawing, or if emptied to nothing or only space(s).
If elements in Block definition are on Layers that are off/frozen/locked,
  will NOT include them in new Block definition.
[When making new Block definition from WMF, all elements within seem
  to become 2D Plines, incl. pieces of Text characters, segmented circles, etc.
  Remove references to WMF in prompts if you don't want them mentioned,
  but will still convert them if selected.]
Kent Cooper, last edited 25 January 2019
|;

(defun C:BDN ; = Block Duplicate with New block name
  (/ *error* doc cmde blksel blkent blkdata blkobj blkname inc newname)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); end if
    (setvar 'cmdecho cmde)
    (vla-endundomark doc)
    (princ)
  ); end defun - *error*

  (vla-startundomark (setq doc (vla-get-activedocument (vlax-get-acad-object))))
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  (prompt "\nTo convert Block/WMF into Duplicate Block with New name,")
  (while
    (not
      (and
        (setq blksel (ssget "_+.:E:S:L" '((0 . "INSERT")))); Block/Xref/WMF/Minsert, unlocked Layer
        (setq
          blkent (ssname blksel 0)
          blkdata (entget blkent); need in entity-data format for (entmake)
        ); setq
        (= (cdr (assoc 100 (reverse blkdata))) "AcDbBlockReference"); Block/Xref/WMF, not Minsert
        (= (logand 4 (cdr (assoc 70 (tblsearch "block" (cdr (assoc 2 blkdata)))))) 0); not Xref
      ); and
    ); not
    (prompt "\nNothing selected, or not a Block/WMF, or on a locked Layer.")
  ); while
  (setq
    blkobj (vlax-ename->vla-object blkent); for easier normalization below, & bounding box
    blkname (vla-get-Name blkobj)
    inc 1
  ); setq
  (if (= (last (vl-string->list (vl-string-right-trim "0123456789" blkname))) 45)
    ; name ends with hyphen and digit character(s) only [likely already result of earlier
    ;   BDN command, accepting offered hypenated-number suffix -- count from there]
    (setq
      hyphen (vl-string-position 45 blkname 0 T); where hyphen is
      inc (1+ (atoi (substr blkname (+ hyphen 2)))); next hyphenated number to increment
      blkname (substr blkname 1 hyphen); block name stripped of hyphen/number(s)
    ); setq
  ); if
  (while (tblsearch "block" (strcat blkname "-" (itoa inc))); already Block named that way
    (setq inc (1+ inc)); count up one, check that name
  ); while
  (prompt "\nEdit or replace Block name in window (not via Full editor).")
  (princ); [somehow needed for prompt to show at Command: line before (lisped) comes up]
  (setq newname (lisped (strcat blkname "-" (itoa inc))))
  (if (= newname "") (setq newname (strcat blkname "-" (itoa inc)))); User hit Enter
  (while (or (tblsearch "block" newname) (= newname "") (= (vl-string-trim " " newname) ""))
    ; typed already-in-use name, or only space(s), or emptied string
    (alert
      (strcat
        "Block name in use or empty -- edit/type different name,"
        "\nEnter/OK to accept offered name, or Esc/Cancel to exit."
      ); strcat
    ); alert
    (setq newname (lisped (strcat blkname "-" (itoa inc))))
  ); while
  (vla-put-XScaleFactor blkobj 1); normalize insertion -- basis for new Block definition
  (vla-put-YScaleFactor blkobj 1)
  (vla-put-ZScaleFactor blkobj 1)
  (vla-put-Rotation blkobj 0)
  (vla-getboundingbox blkobj 'minpt 'maxpt)
  (command "_.zoom" (vlax-safearray->list minpt) (vlax-safearray->list maxpt))
    ; to ensure all resulting objects are found for new Block definition
  (command
    "_.explode" blkent
    "_.block" newname (cdr (assoc 10 blkdata)) "_previous" "" ; removes entities
    "_.zoom" "_previous"
  ); command
  (entmake ; put new one in, replacing source Block
    (append
      (vl-remove-if
        '(lambda (x) (member (car x) '(-1 330 5 2))); no entity names, handle, Block name
        blkdata ; keeps layer, ins. pt., scales, rotation, extr dir, any overrides
      ); remove
      (list (cons 2 newname))
    ); append
  ); entmake

  (setvar 'cmdecho cmde)
  (vla-endundomark doc)
  (princ)
); defun

(vl-load-com)
(prompt "\nType BDN to convert Block/WMF into Duplicate Block with New name.")