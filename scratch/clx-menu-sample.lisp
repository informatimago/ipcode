(USE-PACKAGE "XLIB")

(DEFSTRUCT (MENU)
  "A simple menu of text strings."
  (TITLE "Choose an item:")
  ITEM-ALIST                            ;((item-window item-string))
  WINDOW
  GCONTEXT
  WIDTH
  TITLE-WIDTH
  ITEM-WIDTH
  ITEM-HEIGHT
  HIGHLIGHTED-ITEM
  (GEOMETRY-CHANGED-P T))  


(DEFCONSTANT *MENU-ITEM-MARGIN* 5)


(DEFUN CREATE-MENU (PARENT-WINDOW TEXT-COLOR BACKGROUND-COLOR TEXT-FONT)
  (MAKE-MENU
   ;; Create menu graphics context
   :GCONTEXT (CREATE-GCONTEXT :DRAWABLE   PARENT-WINDOW
                              :FOREGROUND TEXT-COLOR
                              :BACKGROUND BACKGROUND-COLOR
                              :FONT       TEXT-FONT)

   ;; Create menu window
   :WINDOW    (CREATE-WINDOW
               :PARENT            PARENT-WINDOW
               :CLASS             :INPUT-OUTPUT
               :X                 0     ;temporary value
               :Y                 0     ;temporary value
               :WIDTH             16    ;temporary value
               :HEIGHT            16    ;temporary value
               :BORDER-WIDTH 2  
               :BORDER            TEXT-COLOR
               :BACKGROUND        BACKGROUND-COLOR
               :SAVE-UNDER        :ON
               :OVERRIDE-REDIRECT :ON ;override window mgr when positioning
               :EVENT-MASK        (MAKE-EVENT-MASK :LEAVE-WINDOW :EXPOSURE))))
        

(DEFUN MENU-SET-ITEM-LIST (MENU &REST ITEM-STRINGS)
  ;; Assume the new items will change the menu's width and height
  (SETF (MENU-GEOMETRY-CHANGED-P MENU) T)

  ;; Destroy any existing item windows
  (DOLIST (ITEM (MENU-ITEM-ALIST MENU))
    (DESTROY-WINDOW (FIRST ITEM)))
  
  ;; Add (item-window item-string) elements to item-alist
  (SETF (MENU-ITEM-ALIST MENU)
        (LET (ALIST)
          (DOLIST (ITEM ITEM-STRINGS (NREVERSE ALIST))
            (PUSH (LIST (CREATE-WINDOW
                         :PARENT       (MENU-WINDOW MENU)
                         :X            0 ;temporary value
                         :Y            0 ;temporary value
                         :WIDTH        16 ;temporary value
                         :HEIGHT       16 ;temporary value
                         :BACKGROUND   (GCONTEXT-BACKGROUND (MENU-GCONTEXT MENU))
                         :EVENT-MASK   (MAKE-EVENT-MASK :ENTER-WINDOW
                                                        :LEAVE-WINDOW
                                                        :BUTTON-PRESS
                                                        :BUTTON-RELEASE))
                        ITEM)
                  ALIST)))))
        


(DEFUN MENU-RECOMPUTE-GEOMETRY (MENU)
  (WHEN (MENU-GEOMETRY-CHANGED-P MENU)
    (LET* ((MENU-FONT   (GCONTEXT-FONT (MENU-GCONTEXT MENU)))
           (TITLE-WIDTH (TEXT-EXTENTS MENU-FONT (MENU-TITLE MENU)))
           (ITEM-HEIGHT (+ (FONT-ASCENT MENU-FONT)
                           (FONT-DESCENT MENU-FONT)
                           *MENU-ITEM-MARGIN*))
           (ITEM-WIDTH     0)
           (ITEMS          (MENU-ITEM-ALIST MENU))
           MENU-WIDTH)

      ;; Find max item string width
      (SETF ITEM-WIDTH
            (+ *MENU-ITEM-MARGIN*
               (DOLIST (NEXT-ITEM ITEMS ITEM-WIDTH)
                 (SETF ITEM-WIDTH (MAX ITEM-WIDTH
                                       (TEXT-EXTENTS MENU-FONT
                                                     (SECOND NEXT-ITEM)))))))

      ;; Compute final menu width, taking margins into account
      (SETF MENU-WIDTH (MAX TITLE-WIDTH (+ ITEM-WIDTH *MENU-ITEM-MARGIN*)))
      (LET ((WINDOW     (MENU-WINDOW MENU)))

        ;; Update width and height of menu window
        (WITH-STATE (WINDOW)
          (SETF (DRAWABLE-WIDTH      WINDOW) MENU-WIDTH
                (DRAWABLE-HEIGHT WINDOW) (* (1+ (LENGTH ITEMS)) ITEM-HEIGHT)))

        ;; Update width, height, position of item         windows
        (LET ((ITEM-LEFT         (ROUND (- MENU-WIDTH ITEM-WIDTH) 2))
              (NEXT-ITEM-TOP (- ITEM-HEIGHT (ROUND *MENU-ITEM-MARGIN* 2))))
          (DOLIST (NEXT-ITEM ITEMS)
            (LET ((WINDOW (FIRST NEXT-ITEM)))
              (WITH-STATE (WINDOW)
                          (SETF (DRAWABLE-HEIGHT WINDOW) ITEM-HEIGHT
                                (DRAWABLE-WIDTH      WINDOW) ITEM-WIDTH
                                (DRAWABLE-X          WINDOW) ITEM-LEFT
                                (DRAWABLE-Y          WINDOW) NEXT-ITEM-TOP)))
            (INCF NEXT-ITEM-TOP ITEM-HEIGHT))))

      ;; Map all item windows
      (MAP-SUBWINDOWS (MENU-WINDOW MENU))

      ;; Save item geometry
      (SETF (MENU-ITEM-WIDTH MENU)         ITEM-WIDTH
            (MENU-ITEM-HEIGHT MENU)        ITEM-HEIGHT
            (MENU-WIDTH MENU)              MENU-WIDTH
            (MENU-TITLE-WIDTH MENU)        TITLE-WIDTH
            (MENU-GEOMETRY-CHANGED-P MENU) NIL))))
        


(DEFUN MENU-REFRESH (MENU)
  (LET* ((GCONTEXT   (MENU-GCONTEXT MENU))
         (BASELINE-Y (FONT-ASCENT (GCONTEXT-FONT GCONTEXT))))
    ;; Show title centered in "reverse-video"
    (LET ((FG (GCONTEXT-BACKGROUND GCONTEXT))
          (BG (GCONTEXT-FOREGROUND GCONTEXT)))
      (WITH-GCONTEXT (GCONTEXT :FOREGROUND FG :BACKGROUND BG)
                     (DRAW-IMAGE-GLYPHS
                      (MENU-WINDOW MENU)
                      GCONTEXT
                      (ROUND (- (MENU-WIDTH MENU)
                                (MENU-TITLE-WIDTH MENU)) 2) ;start x
                      BASELINE-Y        ;start y
                      (MENU-TITLE MENU)))

      ;; Show each menu item (position is relative to item window)
      (LET ((BOX-MARGIN (ROUND *MENU-ITEM-MARGIN* 2))
            (IFG FG)
            (IBG BG))
        (DOLIST (ITEM (MENU-ITEM-ALIST MENU))
          (IF (EQ (CAR ITEM) (MENU-HIGHLIGHTED-ITEM MENU))
            (SETQ IFG BG IBG FG)
            (SETQ IFG FG IBG BG))
          (WITH-GCONTEXT (GCONTEXT :FOREGROUND IFG :BACKGROUND IBG)
                         (DRAW-IMAGE-GLYPHS
                          (FIRST ITEM) GCONTEXT
                          BOX-MARGIN    ;start x
                          (+ BASELINE-Y BOX-MARGIN) ;start y
                          (SECOND ITEM))))))))
        

(DEFUN MENU-PRESENT (MENU X Y)
  (SETF (DRAWABLE-X (MENU-WINDOW MENU)) X)
  (SETF (DRAWABLE-Y (MENU-WINDOW MENU)) Y)
  (MENU-RECOMPUTE-GEOMETRY MENU)
  (MAP-WINDOW (MENU-WINDOW MENU)))


;; BUG: each item could be highlighted independantly.
(DEFUN MENU-HIGHLIGHT-ITEM (MENU ITEM)
  (SETF (MENU-HIGHLIGHTED-ITEM MENU) ITEM))

(DEFUN MENU-UNHIGHLIGHT-ITEM (MENU ITEM)
  (SETF (MENU-HIGHLIGHTED-ITEM MENU) NIL))


(DEFUN MENU-CHOOSE (MENU X Y)
  ;; Display the menu so that first item is at x,y.
  (MENU-PRESENT MENU X Y)

  (LET ((ITEMS (MENU-ITEM-ALIST MENU))
        (MW    (MENU-WINDOW MENU))
        SELECTED-ITEM)

    ;; Event processing loop
    (DO () (SELECTED-ITEM)
      (EVENT-CASE ((DRAWABLE-DISPLAY MW) :FORCE-OUTPUT-P T)
        (:EXPOSURE
         (COUNT)
         ;; Discard all but final :exposure then display the menu
         (WHEN (ZEROP COUNT) (MENU-REFRESH MENU))
         T)

        (:BUTTON-RELEASE
         (EVENT-WINDOW)
         ;;Select an item
         (SETF SELECTED-ITEM (SECOND (ASSOC EVENT-WINDOW ITEMS)))
         T)

        (:ENTER-NOTIFY
         (WINDOW)
         ;;Highlight an item
         (MENU-HIGHLIGHT-ITEM MENU (FIND WINDOW ITEMS :KEY #'FIRST))
         T)

        (:LEAVE-NOTIFY
         (WINDOW KIND)
         (IF (EQL MW WINDOW)
           ;; Quit if pointer moved out of main menu window
           (SETF SELECTED-ITEM (WHEN (EQ KIND :ANCESTOR) :NONE))
           ;; Otherwise, unhighlight the item window left
           (MENU-UNHIGHLIGHT-ITEM MENU
                                  (FIND WINDOW ITEMS :KEY #'FIRST)))
         T)

        (OTHERWISE
         ()
         ;;Ignore and discard any other event
         T)))

    ;; Erase the menu
    (UNMAP-WINDOW MW)

    ;; Return selected item string, if any
    (UNLESS (EQ SELECTED-ITEM :NONE) SELECTED-ITEM)))
        


(DEFUN JUST-SAY-LISP (HOST &OPTIONAL (FONT-NAME "fixed"))
  (LET* ((DISPLAY   (OPEN-DISPLAY HOST))
         (SCREEN    (FIRST (DISPLAY-ROOTS DISPLAY)))
         (FG-COLOR  (SCREEN-BLACK-PIXEL SCREEN))
         (BG-COLOR  (SCREEN-WHITE-PIXEL SCREEN))
         (NICE-FONT (OPEN-FONT DISPLAY FONT-NAME))

         ;; Create a menu as a child of the root window.
         (A-MENU       (CREATE-MENU (SCREEN-ROOT SCREEN)
                                    FG-COLOR BG-COLOR NICE-FONT)))

    (SETF (MENU-TITLE A-MENU) "Please pick your favorite language:")
    (MENU-SET-ITEM-LIST A-MENU "Fortran" "APL" "Forth" "Lisp")

    ;; Bedevil the user until he picks a nice programming language
    (UNWIND-PROTECT
        ;; Determine the current root window position of the pointer
        (MULTIPLE-VALUE-BIND (X Y) (QUERY-POINTER (SCREEN-ROOT SCREEN))
          (MENU-CHOOSE A-MENU X Y))
      (CLOSE-DISPLAY DISPLAY))))
        



;; Local Variables:
;; eval: (batch-cl-indent (1 with-state EVENT-CASE))
;; End:
