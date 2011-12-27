;;; launch external filter on selection or clipboad
;;;
;;; Copyright (c) 2011 KIHARA Hideto https://github.com/deton/uim-external-filter
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(require-extension (srfi 1 2 8))
(require "i18n.scm")
(require "ustr.scm")
(require "process.scm")
(require-custom "external-filter-custom.scm")

(define external-filter-encoding "UTF-8")

(define external-filter-key-command-alist '())

(define (external-filter-command-symbol key)
  (string->symbol
    (string-append "external-filter-command-" (charcode->string key))))

(define (external-filter-parse-command-string cmd)
  (let ((len (string-length cmd)))
    (cond
      ((and (> len 2)
            (string=? (substring cmd 0 2) ";;"))
        (list (substring cmd 2 (string-length cmd)) 'candwin-split))
      ((and (> len 1)
            (string=? (substring cmd 0 1) ";"))
        (list (substring cmd 1 (string-length cmd)) 'candwin))
      (else
        (list cmd 'commit)))))

(define (external-filter-key-command-alist-update)
  (set! external-filter-key-command-alist
    (append
      (filter-map
        (lambda (x)
          (let ((cmd (symbol-value (external-filter-command-symbol x))))
            (and (not (string=? cmd ""))
                 (cons x (external-filter-parse-command-string cmd)))))
        (iota 26 (char->integer #\a))))))

(define external-filter-context-rec-spec
  (append
    context-rec-spec
    (list
      (list 'key-press-handler #f)
      (list 'get-candidate-handler #f)
      (list 'set-candidate-index-handler #f)
      (list 'ustr '())
      (list 'ustr-prev '())
      (list 'selection-str #f) ; selection string overwritten by preedit
      (list 'nr-cands 0)
      (list 'cand-index 0)
      (list 'undo-len 0)
      (list 'undo-str #f))))
(define-record 'external-filter-context external-filter-context-rec-spec)
(define external-filter-context-new-internal external-filter-context-new)

(define external-filter-context-new
  (lambda args
    (let ((pc (apply external-filter-context-new-internal args)))
      (external-filter-context-set-ustr! pc (ustr-new '()))
      (external-filter-context-set-ustr-prev! pc (ustr-new '()))
      pc)))

(define external-filter-init-handler
  (lambda (id im arg)
    (let ((pc (external-filter-context-new id im)))
      (external-filter-key-command-alist-update)
      pc)))

(define (external-filter-release-handler pc)
  (external-filter-deactivate-candwin pc)
  (let ((cleanup-zombie
          (lambda ()
            (process-waitpid -1
              (assq-cdr '$WNOHANG process-waitpid-options-alist)))))
    (let loop ((ret (cleanup-zombie)))
      (if (> (car ret) 0)
        (loop (cleanup-zombie))))))

(define (external-filter-key-press-handler pc key key-state)
  (define (handle-for-candwin pc key key-state)
    (define (change-cand-index pc offset)
      (let* ((nr (external-filter-context-nr-cands pc))
             (cur-idx (external-filter-context-cand-index pc))
             (idx (+ cur-idx offset))
             (compensated-idx
              (cond
                ((< idx 0) 0)
                ((>= idx nr) (- nr 1))
                (else idx))))
        (external-filter-context-set-cand-index! pc compensated-idx)
        (im-select-candidate pc compensated-idx)))
    (cond
      ((not (external-filter-context-key-press-handler pc))
        #f) ; candwin deactivated
      ((generic-cancel-key? key key-state)
        (let ((sel (external-filter-context-selection-str pc)))
          (if sel
            (begin
              (im-commit pc sel) ; restore selection string for Firefox
              (external-filter-context-set-selection-str! pc #f))))
        (external-filter-deactivate-candwin pc)
        #t)
      ((= (external-filter-context-nr-cands pc) 1)
        #f) ; may cancel selection by cursor move and commit candidate later
      ((generic-next-candidate-key? key key-state)
        (change-cand-index pc 1)
        #t)
      ((generic-prev-candidate-key? key key-state)
        (change-cand-index pc -1)
        #t)
      ((generic-next-page-key? key key-state)
        (change-cand-index pc external-filter-nr-candidate-max)
        #t)
      ((generic-prev-page-key? key key-state)
        (change-cand-index pc (- external-filter-nr-candidate-max))
        #t)
      (else
        #f)))
  (if (ichar-control? key)
    (external-filter-commit-raw pc)
    (let* ((handler (external-filter-context-key-press-handler pc))
           (handled? (and handler (handler pc key key-state))))
      (if (not handled?)
        (if (not (handle-for-candwin pc key key-state))
          (cond
            ((external-filter-start-command-input-key? key key-state)
              (external-filter-start-command-input pc))
            ((external-filter-redo-last-command-key? key key-state)
              (external-filter-redo-last-command pc))
            ((external-filter-paste-last-command-key? key key-state)
              (external-filter-paste-last-command pc))
            ((external-filter-help-key? key key-state)
              (external-filter-help pc))
            ((external-filter-undo-key? key key-state)
              (external-filter-undo pc)
              (external-filter-context-set-undo-str! pc #f))
            ((external-filter-switch-default-im-key? key key-state)
              (im-switch-im pc default-im-name))
            ((or (symbol? key)
                 (and (modifier-key-mask key-state)
                      (not (shift-key-mask key-state))))
              (external-filter-commit-raw pc))
            ((ichar-lower-case? key)
              (let ((key-cmd (assv key external-filter-key-command-alist)))
                (if key-cmd
                  (external-filter-launch pc (cdr key-cmd)))))
            ((ichar-upper-case? key)
              (external-filter-register pc (ichar-downcase key)))
            (else
              (external-filter-commit-raw pc))))))))

(define (external-filter-key-release-handler pc key state)
  (im-commit-raw pc))

(define (external-filter-get-candidate-handler pc idx accel-enum-hint)
  (let ((handler (external-filter-context-get-candidate-handler pc)))
    (if handler
      (handler pc idx accel-enum-hint)
      (list "" "" ""))))

(define (external-filter-set-candidate-index-handler pc idx)
  (let* ((prev (external-filter-context-cand-index pc))
         (prev-page (quotient prev external-filter-nr-candidate-max))
         (new-page (quotient idx external-filter-nr-candidate-max)))
    (external-filter-context-set-cand-index! pc idx)
    (if (= new-page prev-page)
      (let ((handler (external-filter-context-set-candidate-index-handler pc)))
        (if handler
          (handler pc idx)))
      ;; else page changed by next/prev page button click
      )))

(define (external-filter-focus-in-handler pc) #f)

(define (external-filter-focus-out-handler pc)
  (external-filter-context-set-key-press-handler! pc #f)
  (external-filter-deactivate-candwin pc)
  (context-update-preedit pc '())
  (external-filter-context-set-selection-str! pc #f))

(define external-filter-place-handler external-filter-focus-in-handler)
(define external-filter-displace-handler external-filter-focus-out-handler)

(define (external-filter-activate-candwin pc nr-cands)
  (external-filter-context-set-nr-cands! pc nr-cands)
  (im-activate-candidate-selector pc nr-cands external-filter-nr-candidate-max)
  (external-filter-context-set-cand-index! pc 0)
  (im-select-candidate pc 0)) ; to select candidate by click

(define (external-filter-deactivate-candwin pc)
  (external-filter-context-set-key-press-handler! pc #f)
  (im-deactivate-candidate-selector pc))

(register-im
 'external-filter
 "*"
 external-filter-encoding
 external-filter-im-name-label
 external-filter-im-short-desc
 #f
 external-filter-init-handler
 external-filter-release-handler
 context-mode-handler
 external-filter-key-press-handler
 external-filter-key-release-handler
 #f
 external-filter-get-candidate-handler
 external-filter-set-candidate-index-handler
 context-prop-activate-handler
 #f
 external-filter-focus-in-handler
 external-filter-focus-out-handler
 external-filter-place-handler
 external-filter-displace-handler
 )

(define (external-filter-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (external-filter-launch pc cmd-op)
  ;; XXX: process-io without parent reading ret from child to avoid error:
  ;;   Error: in >: integer required but got: #f
  ;; (ex: when cmd is "ls", result of ls is read by parent that is not number)
  (define (my-process-io file . args)
    (let-optionals* args ((argv (list file)))
      (and-let* ((pin (create-pipe))
                 (pout (create-pipe))
                 (pin-in  (car pin))
                 (pin-out (cdr pin))
                 (pout-in  (car pout))
                 (pout-out (cdr pout)))
        (let ((pid (process-fork))
              (ret 0))
          (cond ((< pid 0)
                 (begin
                   (uim-notify-fatal (N_ "cannot fork"))
                   (file-close pin-in)
                   (file-close pin-out)
                   (file-close pout-in)
                   (file-close pout-out)
                   #f))
                ((= 0 pid) ;; child
                 (setsid)
                 (file-close pin-out)
                 (if (< (duplicate-fileno pin-in 0) 0)
                   (begin
                     (uim-notify-fatal (N_ "cannot duplicate stdin"))
                     (set! ret (bitwise-ior ret process-dup2-failed))))
                 (file-close pin-in)

                 (file-close pout-in)
                 (if (< (duplicate-fileno pout-out 1) 0)
                   (begin
                     (uim-notify-fatal (N_ "cannot duplicate stdout"))
                     (set! ret (bitwise-ior ret process-dup2-failed))))
                 (file-close pout-out)

                 (if (= (process-execute file argv) -1)
                   (uim-notify-fatal (format (_ "cannot execute ~a") file)))
                 ;(set! ret (bitwise-ior ret process-exec-failed))
                 ;(file-write-string 1 (number->string ret))
                 (_exit 1)
                 )
                (else ;; parent
                 (file-close pin-in)
                 (file-close pout-out)
                 ;(if (and-let*
                 ;      (((file-ready? (list pout-in) 100))
                 ;       (lst (file-read pout-in 1))
                 ;       ((not (eof-object? lst)))
                 ;       ((> (string->number (list->string lst)) 0))));Error
                 ;  (begin
                 ;    (file-close pout-in)
                 ;    (file-close pin-out)
                 ;    #f)
                 (list pout-in pin-out pid)))))))
  ;; file-read-line without newline check.
  (define (file-read-all port)
    (let loop ((c (file-read-char port))
               (rest '()))
      (cond ((or (eof-object? c)
                 (not c))
             (list->string (reverse rest)))
            (else
             (loop (file-read-char port) (cons c rest))))))
  (define (launch cmd str)
    (and-let*
      ((fds (my-process-io "/bin/sh" (list "/bin/sh" "-c" cmd)))
       (iport (open-file-port (list-ref fds 0)))
       (oport (open-file-port (list-ref fds 1)))
       (pid (list-ref fds 2)))
      (file-display str oport)
      (close-file-port oport)
      (let ((res (file-read-all iport)))
        (close-file-port iport)
        ;; zombie cleanup. XXX: WNOHANG for avoiding block may leave zombie.
        (process-waitpid pid (assq-cdr '$WNOHANG process-waitpid-options-alist))
        (and (string? res)
             (not (string=? res ""))
             res))))
  (define (launch-and-show pc cmd op str undo-str)
    (let ((res (launch cmd str)))
      (if res
        (case op
          ((commit)
            (external-filter-commit pc res undo-str))
          ((candwin)
            (external-filter-show-candwin pc res #f))
          ((candwin-split)
            (external-filter-show-candwin pc res #t)))
        (let ((sel (external-filter-context-selection-str pc)))
          (if sel
            (begin
              (im-commit pc sel) ; restore selection string for Firefox
              (external-filter-context-set-selection-str! pc #f)))
          (external-filter-deactivate-candwin pc)))))
  (let ((cmd (car cmd-op))
        (op (cadr cmd-op))
        (str (or (external-filter-context-selection-str pc)
                 (external-filter-acquire-text pc 'selection))))
    (if (string? str)
      (launch-and-show pc cmd op str str)
      (let ((clip (external-filter-acquire-text pc 'clipboard)))
        (launch-and-show pc cmd op (if (string? clip) clip "") "")))))

(define (external-filter-commit pc commit-str undo-str)
  (define (count-char str)
    (string-length
      (with-char-codec external-filter-encoding
        (lambda ()
          (%%string-reconstruct! (string-copy str))))))
  (external-filter-deactivate-candwin pc)
  (external-filter-context-set-undo-str! pc
    (if (string? undo-str) undo-str ""))
  (external-filter-context-set-undo-len! pc (count-char commit-str))
  (external-filter-context-set-selection-str! pc #f)
  (im-commit pc commit-str)
  (if external-filter-switch-default-im-after-commit
    (im-switch-im pc default-im-name)))

(define (external-filter-commit-raw pc)
  (external-filter-context-set-undo-str! pc #f)
  (im-commit-raw pc))

;; string-to-list in deprecated-util.scm with my encoding and without reverse
(define external-filter-string-to-list
  (lambda (s)
    (with-char-codec external-filter-encoding
      (lambda ()
        (map! (lambda (c)
                (let ((str (list->string (list c))))
                  (with-char-codec "ISO-8859-1"
                    (lambda ()
                      (%%string-reconstruct! str)))))
              ;; if filter output is not UTF-8,
              ;; Error: scm_charcodec_read_char: invalid char sequence
              (guard (err (else '()))
                (string->list s)))))))

(define (external-filter-limit-cand-length str)
  (let* ((strlist (external-filter-string-to-list str))
         (lim (if (> (length strlist)
                     external-filter-string-length-max-on-candwin)
                (append
                  (take strlist external-filter-string-length-max-on-candwin)
                  '("..."))
                strlist)))
    (apply string-append lim)))

(define (external-filter-show-candwin pc candstr split?)
  (external-filter-deactivate-candwin pc)
  (let ((cands
          (if split?
            (filter
              (lambda (x)
                (not (string=? x "")))
              (string-split candstr "\n"))
            (list candstr))))
    (define (commit pc idx)
      ;; acquire text again because selection may be changed
      ;; after showing candidate window.
      (let ((undo-str (or (external-filter-context-selection-str pc)
                          (external-filter-acquire-text pc 'selection))))
        (external-filter-commit pc
          (list-ref cands idx)
          (if (string? undo-str) undo-str ""))))
    (external-filter-context-set-set-candidate-index-handler! pc commit)
    (external-filter-context-set-get-candidate-handler! pc
      (lambda (pc idx accel-enum-hint)
        (let ((idx-in-page (remainder idx external-filter-nr-candidate-max))
              (str (external-filter-limit-cand-length (list-ref cands idx))))
          (list str (number->string idx-in-page) ""))))
    (external-filter-context-set-key-press-handler! pc
      (lambda (pc key key-state)
        (cond
          ((generic-commit-key? key key-state)
            (commit pc (external-filter-context-cand-index pc))
            #t)
          ((external-filter-split-toggle-key? key key-state)
            (external-filter-show-candwin pc candstr (not split?))
            #t)
          ((ichar-numeric? key)
            (let* ((idx-in-page (numeric-ichar->integer key))
                   (page (quotient (external-filter-context-cand-index pc)
                                   external-filter-nr-candidate-max))
                   (idx (+ (* page external-filter-nr-candidate-max)
                           idx-in-page)))
              (if (< idx (external-filter-context-nr-cands pc))
                (commit pc idx)))
            #t)
          (else
            #f))))
    (external-filter-activate-candwin pc (length cands))))

(define (external-filter-help pc)
  (define (commit pc idx)
    (let ((key-cmd (list-ref external-filter-key-command-alist idx)))
      (external-filter-launch pc (cdr key-cmd))))
  (external-filter-deactivate-candwin pc)
  (external-filter-context-set-set-candidate-index-handler! pc commit)
  (external-filter-context-set-get-candidate-handler! pc
    (lambda (pc idx accel-enum-hint)
      (define (get-cmdstr-with-candwin-flag key-cmd)
        (string-append
          (case (list-ref key-cmd 2)
            ((commit) "")
            ((candwin) ";")
            ((candwin-split) ";;"))
          (list-ref key-cmd 1)))
      (let ((key-cmd (list-ref external-filter-key-command-alist idx)))
        (list
          (external-filter-limit-cand-length
            (get-cmdstr-with-candwin-flag key-cmd))
          (charcode->string (car key-cmd))
          ""))))
  (external-filter-context-set-key-press-handler! pc
    (lambda (pc key key-state)
      (cond
        ((generic-commit-key? key key-state)
          (commit pc (external-filter-context-cand-index pc))
          #t)
        (else
          #f))))
  (external-filter-activate-candwin pc
    (length external-filter-key-command-alist)))

(define (external-filter-undo pc)
  (let ((str (external-filter-context-undo-str pc))
        (len (external-filter-context-undo-len pc)))
    (if str
      (begin
        (if (> len 0)
          (im-delete-text pc 'primary 'cursor len 0))
        (external-filter-context-set-selection-str! pc #f)
        (if (not (string=? str ""))
          (im-commit pc str))))))

;;; register and save filter command from selection
(define (external-filter-register pc key)
  (let ((sym (external-filter-command-symbol key))
        (str (or (external-filter-context-selection-str pc)
                 (external-filter-acquire-text pc 'selection))))
    (if (string? str)
      (let* ((len (string-length str))
             (str-trim (if (string=? (substring str (- len 1) len) "\n")
                          (substring str 0 (- len 1))
                          str)))
        (set-symbol-value! sym str-trim)
        (external-filter-key-command-alist-update)
        (external-filter-save-custom-value pc sym str-trim))
      (external-filter-commit pc (symbol-value sym) ""))))

(define (external-filter-save-custom-value pc custom-symbol custom-value)
  (define (write-file filename lines)
    (call-with-open-file-port
      (file-open filename
        (file-open-flags-number '($O_WRONLY $O_CREAT))
        (file-open-mode-number '($S_IRUSR $S_IWUSR $S_IRGRP $S_IROTH)))
      (lambda (port)
        (let loop ((lines lines))
          (if (pair? lines)
            (begin
              (file-write-sexp (car lines) port)
              (file-newline port)
              (loop (cdr lines))))))))
  (define (read-file filename)
    (call-with-open-file-port
      (file-open filename (file-open-flags-number '($O_RDONLY)) 0)
      (lambda (port)
        (let loop ((line (file-read-line port))
                   (lines '()))
          (if (or (not line) (eof-object? line))
              (reverse lines)
              (loop (file-read-line port)
                (cons (read-from-string line) lines)))))))
  (let* ((filename (custom-file-path 'external-filter))
         ;; "~/.uim.d/customs/custom-external-filter.scm"
         (lines (or (read-file filename) '()))
         (updated-lines
          (let ((line (find (lambda (x) (eq? (cadr x) custom-symbol)) lines)))
            (if line
              (begin
                (set-cdr! (cdr line) (list custom-value))
                lines)
              (cons
                (list 'define custom-symbol custom-value)
                lines)))))
    (write-file filename updated-lines)))

(define (external-filter-start-command-input pc)
  (external-filter-deactivate-candwin pc)
  (let ((ustr (external-filter-context-ustr pc))
        (ustr-prev (external-filter-context-ustr-prev pc))
        (sel (or (external-filter-context-selection-str pc)
                 (external-filter-acquire-text pc 'selection))))
    (define (update-preedit)
      (if (eq? (external-filter-context-key-press-handler pc) key-press-handler)
        (context-update-preedit pc
          (list
            (cons preedit-underline ":!")
            (cons preedit-underline
              (apply string-append (ustr-former-seq ustr)))
            (cons preedit-cursor "")
            (cons preedit-underline
              (apply string-append (ustr-latter-seq ustr)))))
        (context-update-preedit pc '())))
    (define (cancel-command-input)
      (external-filter-context-set-key-press-handler! pc #f)
      (ustr-clear! ustr)
      (if (string? sel)
        (begin
          (update-preedit)
          (external-filter-context-set-selection-str! pc #f)
          (im-commit pc sel)))) ; for Firefox
    (define (key-press-handler pc key key-state)
      (cond
        ((generic-commit-key? key key-state)
          (external-filter-context-set-key-press-handler! pc #f)
          (let ((cmd (apply string-append (ustr-whole-seq ustr))))
            (ustr-copy! ustr-prev ustr)
            (ustr-clear! ustr)
            (update-preedit)
            ;; pass selection acquired before command input because selection
            ;; is overwritten by preedit on Firefox or uim-qt4.
            (external-filter-context-set-selection-str! pc sel)
            (external-filter-launch pc
              (external-filter-parse-command-string cmd))))
        ((generic-cancel-key? key key-state)
          (cancel-command-input))
        ((or (external-filter-go-up-key? key key-state)
             ;; exclude " " included in generic-next-candidate-key
             (external-filter-go-down-key? key key-state))
          ;; XXX: currently swap only.
          ;;      support input command history? (switching IM clears history)
          (let ((ustr-tmp (ustr-dup ustr)))
            (ustr-copy! ustr ustr-prev)
            (ustr-copy! ustr-prev ustr-tmp)))
        ((external-filter-command-input-paste-key? key key-state)
          (let ((clip (external-filter-acquire-text pc 'clipboard)))
            (if (string? clip)
              (ustr-insert-seq! ustr (external-filter-string-to-list clip)))))
        ((generic-backspace-key? key key-state)
          (if (ustr-empty? ustr)
            (cancel-command-input)
            (ustr-cursor-delete-backside! ustr)))
        ((generic-delete-key? key key-state)
          (ustr-cursor-delete-frontside! ustr))
        ((generic-kill-key? key key-state)
          (ustr-clear-latter! ustr))
        ((generic-kill-backward-key? key key-state)
          (ustr-clear-former! ustr))
        ((generic-go-left-key? key key-state)
          (ustr-cursor-move-backward! ustr))
        ((generic-go-right-key? key key-state)
          (ustr-cursor-move-forward! ustr))
        ((generic-beginning-of-preedit-key? key key-state)
          (ustr-cursor-move-beginning! ustr))
        ((generic-end-of-preedit-key? key key-state)
          (ustr-cursor-move-end! ustr))
        ((or (symbol? key)
             (and (modifier-key-mask key-state)
                  (not (shift-key-mask key-state))))) ; ignore
        (else
          (ustr-insert-elem! ustr (charcode->string key))))
      (update-preedit)
      #t)
    (external-filter-context-set-key-press-handler! pc key-press-handler)
    (ustr-clear! ustr)
    (update-preedit)))

(define (external-filter-redo-last-command pc)
  (external-filter-launch pc
    (external-filter-parse-command-string
      (apply string-append
        (ustr-whole-seq
          (external-filter-context-ustr-prev pc))))))

(define (external-filter-paste-last-command pc)
  (let ((sel (or (external-filter-context-selection-str pc)
                 (external-filter-acquire-text pc 'selection))))
    (external-filter-commit pc
      (apply string-append
        (ustr-whole-seq
          (external-filter-context-ustr-prev pc)))
      (if (string? sel) sel ""))))
