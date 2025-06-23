
;; RelativeXrefs by Joe Burke

;; Bug reports may be sent to me directly at lowercase@hawaii.rr.com.
;; Program updates will be posted at www.theswamp.org under 
;; "Show your stuff" in a topic named "Relative xref paths".

;; PROGRAM NOTES:

;; The standard disclaimer applies. Use at your own risk...

;; In general it's important to understand how relative paths work
;; before using this program.

;; Please be aware the program is potentially dangerous in the sense
;; relative repathing from one drive to another may cause pointing to 
;; unexpected files in cases where duplicate files exist on different
;; drives and similar conditions.

;; The shortcut is REX.

;; Tested with 2002, 2004, 2006 and 2008. Should work with 2000,
;; but not tested.

;; Thanks to Steve Doman and Jason Piercey for their help.

;; The program works with unloaded xrefs, unlike other similar routines 
;; I've seen.

;; It does not deal with raster image references. I may add support 
;; for those later.

;; Reloading an xref can cause associative dimensions/leaders to do
;; strange things. Something to watch for. If it happens, undo
;; and change such dimensions to non-associative.

;; The program may take a long time depending on the number of xrefs, 
;; how many are reloaded and the size of reloaded files. 

;; Possible scenarios where RelativeXrefs might be useful.

;; First:
;; A project uses relative paths but all paths are not relative.
;; Convert full paths to relative assuming an xref can be found somewhere
;; along its orignal path. If not the original path remains.
;; Note, nested xrefs are not repathed because the path is determined within 
;; the parent file. Run the program on the parent file and save it. Then 
;; reload the parent in the active file to update nested paths.
;; There is at least one case where the path of nested xrefs will change 
;; in the active file. If the parent xref was "not found" and the program 
;; finds it, the parent is repathed. That may update nested paths.

;; Second:
;; A project begins as a group of files in one folder. Eventually the number
;; of files involved requires sub-folder organization and you want to use
;; relative paths. Repath as needed using full paths. Then convert full paths
;; to relative.

;; Third:
;; All project files are kept in a single folder so xrefs are found 
;; without a path. Some xrefs may have a full path which isn't used.
;; The program removes such paths leaving just the file name as the path.

;; VERSION HISTORY

;; Version 1.0 - first beta version 12/31/2007.

;; Version 1.1 - revised the XrefsData function 1/3/2008.

;; Version 1.2 - revised 1/5/2008.
;;   Changed the name of the XrefLoaded function to XrefIsLoaded.
;;   Changed its method of testing loaded or not.
;;   Changed the XrefsData function to return the xref block name
;;   at the end of each list so the reload and report calls are
;;   cosmetically correct in terms of the actual xref block name.

;; Version 1.3 - revised 1/8/2008.
;;   Removed the front end question about reload xrefs.
;;   Uses the xref command "path" rather than "reload".
;;   Removed checking for xref is loaded, not need now.
;;   Revised the XrefsData function per above.
;;   This version should be faster. 
;;   Also cleaner in terms the report at the end.
;;   Added report regarding number of nested xrefs found.
;;   Added a check near the end regarding an xref which should
;;   have been repathed, but possibly was not.

;; Version 1.4 - revised 1/12/2008.
;;   Added the Spinbar function with message, Searching for xrefs...

;; Version 1.5 - revised 2/22/2008.
;;   Removed the FileName sub-function. Added the SpinWait function so 
;;   the spinbar will work with some files where it otherwise does not.
;;   Added error checking in the XrefSearch sub-function.
;;   Fixed a bug which occured with 2004 and earlier versions. In those 
;;   versions an xref block definition does not have a Path property.


(defun c:RelativeXrefs ( / *error* doc blocks cnt datalst strlst str 
                           xpath xname reportlst nestcnt PathList
                           XrefsData XrefSearch Spinbar SpinWait)

  (defun *error* (msg)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*"))
      (T (princ (strcat "\nError: " msg)))
    )
    (setvar "cmdecho" 1)
    (vla-EndUndoMark doc)
    (princ)
  ) ;end error

 ;;;; START SUB-FUNCTIONS ;;;;

  ;; Argument: path string
  ;; Returns the path portion as a list of strings in reverse order.
  ;; (setq s "..\\..\\Common ABC\\XRefs ABC\\Plan Unit 3BR KLSC.dwg")
  ;; ("\\XRefs ABC" "\\Common ABC" "\\.." "\\..")
  ;; The file name is not included.
  (defun PathList (str / idx pat pos lst)
    (setq idx 0 pat "\\")
    (while (setq pos (vl-string-search pat str idx))
      (setq lst (cons (strcat pat (substr str (1+ idx) (- pos idx))) lst)
            idx (1+ pos)
      )
    )
    lst
  ) ;end

  ;; Returns a list of lists: (fullname fn blockname expath) 
  (defun XrefsData ( / blkname expath fullname fn xlst NestedXref)

    ;; Argument: block definition vla-object.
    ;; Returns a count number if the xref is nested, otherwise nil.
    ;; Based on code by Stephan Koster in a program named XrefTree.
    ;; Function renamed from nested_p.
    ;; The nestcnt variable is local to the primary routine.
    ;; There is a known flaw in the function which Jason pointed out.
    ;; If an xref is both nested and referenced as a parent, the
    ;; function does not flag it as nested. The fallout from that situation,
    ;; if it occurs, is handled near the end of the primary routine.
    (defun NestedXref (blkdef / elst) 
      (setq elst (entget (vlax-vla-object->ename blkdef)))
      (if
        (or
          (not (vl-position '(102 . "{BLKREFS") elst))
          (and
            (vl-position '(102 . "{BLKREFS") elst)
            (not (cdr (assoc 331 elst)))
          )
        )
        (setq nestcnt (1+ nestcnt))
        ;; Else return nil to the parent function.
      )
    ) ;end

    (vlax-for x blocks
      (if 
        (and
          (= -1 (vlax-get x 'IsXref))
          (setq blkname (vlax-get x 'Name))
          ;; Filter out nested xrefs.
          (not (NestedXref x))
          (setq expath (cdr (assoc 1 (tblsearch "block" blkname))))
          (setq fn (strcat (vl-filename-base expath) ".dwg"))
        )
        (progn 
          (SpinWait 0.25)
          (cond 
            ;; Xref found at full or relative path.
            ((setq fullname (findfile expath)))
            ;; Xref found in the same folder as the active file
            ;; and it the xref has not been renamed.
            ((setq fullname (findfile fn)))
            ;; Xref not found so far. Substitute the path for full name.
            (T (setq fullname expath))
          )
          (setq xlst (cons (list fullname fn blkname expath) xlst))
        )
      )
    )
    xlst
  ) ;end

  ;; Arguments: strlst - a list of strings returned by PathList.
  ;;            path - a file name returned by FileName.
  ;;            dot - a string either ".\\" to search down the folder structure
  ;;            or "..\\" to search up the folder structure.
  ;; Returns: a relative path if the file is found.
  (defun XrefSearch (strlst path dot / xpath)
    (if (and strlst path dot)
      (progn
        (setq path (strcat "\\" path))
        (while 
          (and
            strlst
            (not (findfile (setq xpath (strcat dot 
              (substr (setq path (strcat (car strlst) path)) 2))))
            )
          )
          (princ
            (strcat "\rSearching for xrefs... please ignore any messages  " 
              (setq *sbar (Spinbar *sbar)) "  \t\t\t")
          )
          (setq strlst (cdr strlst))
          (setq dot (strcat dot dot))
        )
        (if (and xpath (findfile xpath))
          xpath
        )
      )
    )
  ) ;end

  ;; Busy indicator. Author unknown.
  (defun Spinbar (sbar)
    (cond ((= sbar "\\") "|")
          ((= sbar "|") "/")
          ((= sbar "/") "-")
          (t "\\")
    )
  ) ;end

  (defun SpinWait (seconds / stop)
    (setq stop (+ (getvar "DATE") (/ seconds 86400.0)))
    (while (> stop (getvar "DATE"))
      (princ
        (strcat "\rSearching for xrefs... please ignore any messages  " 
          (setq *sbar (Spinbar *sbar)) "  \t\t\t")
      )
    )
  )

  ;;;; END SUB-FUNCTIONS ;;;;

  ;;;; START PRIMARY FUNCTION ;;;;

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  (SpinWait 0.1)

  (setvar "cmdecho" 0)

  (setq blocks (vla-get-blocks doc)
        cnt 0
        nestcnt 0
        datalst (XrefsData)
  )
  
  ;; datalst is a list of lists
  ;; xref (fullname fn blkname expath)
  (foreach x datalst
    (setq strlst (PathList (car x))
          ;; Xref file name.
          str (cadr x)
          ;; The xref block name.
          xname (caddr x)
    )

    (cond
      ;; Xref found in the same folder as the active file.
      ((findfile str) (setq xpath str))
      ;; Search for xref down the folder structure.
      ((setq xpath (XrefSearch strlst str ".\\")))
      ;; Search for xref up the folder structure.
      ((setq xpath (XrefSearch strlst str "..\\")))
    )

    (if 
      (and 
        xpath
        ;; Check the path found is not the same as the original path.
        (not (eq xpath (cadddr x)))
      )
      (progn
        (command "._xref" "path" xname xpath)
        (setq reportlst (cons (list xname xpath) reportlst)
              cnt (1+ cnt)
        )
      )
    )
    (SpinWait 0.25)
  ) ;foreach

  ;; Double check each xref which should have been repathed actually was.
  ;; If not try again. In some cases where an xref is both referenced as a
  ;; parent and also nested, this will allow repathing the xref.
  ;; Without it an xref may be reported as repathed when it was not.
  (foreach x reportlst
    (if (not (eq (cadr x) (cdr (assoc 1 (tblsearch "block" xname)))))
      (command "._xref" "path" (car x) (cadr x))
    )
  )

  ;; Report:
  ;; Doing this separate from the repath operation avoids a message,
  ;; "press enter to continue" from getting tangled up with the report.
  ;; I don't know what that message refers to, but it doesn't cause a problem.
  (foreach x reportlst   
    (princ (strcat "\nXref: " (car x) " repathed: \n"))
    (princ (strcat "  " (cadr x)))
  )

  (print)
  (if (not (zerop nestcnt))
    (princ (strcat "\nNumber of nested xrefs found: " (itoa nestcnt)))
  )
  (princ (strcat "\nNumber of xrefs repathed: " (itoa cnt)))

  (*error* nil)
) ;end

  ;;;; END PRIMARY FUNCTION ;;;;

;------------------------------------
;shortcut
(defun c:REX () (c:RelativeXrefs))
;------------------------------------