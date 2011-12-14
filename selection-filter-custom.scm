(require "i18n.scm")

(define selection-filter-im-name-label (N_ "selection-filter"))
(define selection-filter-im-short-desc (N_ "launch external filter on selection"))

(define-custom-group 'selection-filter
                     selection-filter-im-name-label
                     selection-filter-im-short-desc)

(define-custom 'selection-filter-help-key '("<IgnoreShift>?")
  '(selection-filter)
  '(key)
  (N_ "[selection-filter] help")
  (N_ "long description will be here"))

(define-custom 'selection-filter-undo-key '("u")
  '(selection-filter)
  '(key)
  (N_ "[selection-filter] undo last filter")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-a "nkf -w -f"
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key a")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-b "wc -lwmc"
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key b")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-c "sed -e 's/^/> /'"
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key c")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-d "awk 'BEGIN{print \"<blockquote>\"}{print}END{print \"</blockquote>\"}'"
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key d")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-e ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key e")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-f ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key f")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-g ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key g")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-h ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key h")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-i ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key i")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-j ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key j")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-k ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key k")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-l ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key l")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-m ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key m")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-n ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key n")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-o ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key o")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-p ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key p")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-q ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key q")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-r ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key r")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-s ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key s")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-t ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key t")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-u ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key u")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-v ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key v")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-w ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key w")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-x ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key x")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-y ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key y")
  (N_ "long description will be here"))

(define-custom 'selection-filter-command-z ""
  '(selection-filter)
  '(string ".*")
  (N_ "filter command for key z")
  (N_ "long description will be here"))
