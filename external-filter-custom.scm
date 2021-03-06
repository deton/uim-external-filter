(require "i18n.scm")

(define external-filter-im-name-label (N_ "external-filter"))
(define external-filter-im-short-desc (N_ "launch external filter on selection"))

(define-custom-group 'external-filter
                     external-filter-im-name-label
                     external-filter-im-short-desc)

(define-custom 'external-filter-string-length-max-on-candwin 80
  '(external-filter)
  '(integer 1 99)
  (N_ "Maximum length of candidate string on candidate window")
  (N_ "long description will be here."))

(define-custom 'external-filter-nr-candidate-max 10
  '(external-filter)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'external-filter-enable-register #t
  '(external-filter)
  '(boolean)
  (N_ "enable registering filter command by upper case key")
  (N_ "long description will be here."))

(define-custom 'external-filter-switch-default-im-after-commit #f
  '(external-filter)
  '(boolean)
  (N_ "switch to default IM after commit of filter output")
  (N_ "long description will be here."))

(define-custom 'external-filter-start-command-input-key '("<IgnoreShift>:")
  '(external-filter)
  '(key)
  (N_ "[external-filter] start command input")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-input-paste-key '("<IgnoreCase><Control>v")
  '(external-filter)
  '(key)
  (N_ "[external-filter] paste from clipboard while command input")
  (N_ "long description will be here"))

(define-custom 'external-filter-redo-last-command-key '("<IgnoreShift>.")
  '(external-filter)
  '(key)
  (N_ "[external-filter] redo last input command")
  (N_ "long description will be here"))

(define-custom 'external-filter-paste-last-command-key '("<IgnoreShift>>")
  '(external-filter)
  '(key)
  (N_ "[external-filter] paste last input command")
  (N_ "long description will be here"))

(define-custom 'external-filter-help-key '("<IgnoreShift>?")
  '(external-filter)
  '(key)
  (N_ "[external-filter] help")
  (N_ "long description will be here"))

(define-custom 'external-filter-switch-default-im-key '("<IgnoreShift>~")
  '(external-filter)
  '(key)
  (N_ "[external-filter] switch to default IM")
  (N_ "long description will be here"))

(define-custom 'external-filter-undo-key '("u")
  '(external-filter)
  '(key)
  (N_ "[external-filter] undo last filter")
  (N_ "long description will be here"))

(define-custom 'external-filter-split-toggle-key '("<IgnoreShift>;")
  '(external-filter)
  '(key)
  (N_ "[external-filter] toggle split candidate")
  (N_ "long description will be here"))

(define-custom 'external-filter-go-up-key '("up" "<IgnoreCase><Control>p")
  '(external-filter)
  '(key)
  (N_ "[external-filter] go up")
  (N_ "long description will be here"))

(define-custom 'external-filter-go-down-key '("down" "<IgnoreCase><Control>n")
  '(external-filter)
  '(key)
  (N_ "[external-filter] go down")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-a "nkf -w -f"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key a")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-b ";wc -lwmc"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key b")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-c "nkf -w -f | sed -e 's/^/> /'"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key c")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-d "echo '<blockquote>'; cat -; echo '</blockquote>'"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key d")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-e ";;look $(read a; echo $a)"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key e")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-f ";;ls ~"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key f")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-g "python -mjson.tool"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key g")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-h "date"
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key h")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-i ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key i")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-j ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key j")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-k ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key k")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-l ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key l")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-m ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key m")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-n ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key n")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-o ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key o")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-p ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key p")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-q ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key q")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-r ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key r")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-s ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key s")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-t ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key t")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-u ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key u")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-v ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key v")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-w ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key w")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-x ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key x")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-y ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key y")
  (N_ "long description will be here"))

(define-custom 'external-filter-command-z ""
  '(external-filter)
  '(string ".*")
  (N_ "filter command for key z")
  (N_ "long description will be here"))
