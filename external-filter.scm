;;; launch external filter on selection
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

(require-extension (srfi 1 2))
(require "i18n.scm")
(require "process.scm")
(require-custom "external-filter-custom.scm")

(define external-filter-encoding "UTF-8")

(define external-filter-key-command-alist ())

(define (external-filter-command-symbol key)
  (string->symbol
    (string-append "external-filter-command-" (charcode->string key))))

(define (external-filter-key-command-alist-update)
  (set! external-filter-key-command-alist
    (append
      (filter-map
        (lambda (x)
          (let ((cmd (symbol-value (external-filter-command-symbol x))))
            (and (not (string=? cmd ""))
                 (list x cmd))))
        (iota 26 (char->integer #\a))))))

(define external-filter-context-rec-spec
  (append
    context-rec-spec
    (list
      (list 'undo-len 0)
      (list 'undo-str #f))))
(define-record 'external-filter-context external-filter-context-rec-spec)
(define external-filter-context-new-internal external-filter-context-new)

(define external-filter-context-new
  (lambda args
    (let ((pc (apply external-filter-context-new-internal args)))
      pc)))

(define external-filter-init-handler
  (lambda (id im arg)
    (let ((pc (external-filter-context-new id im)))
      (external-filter-key-command-alist-update)
      pc)))

(define (external-filter-release-handler pc)
  (im-deactivate-candidate-selector pc))

(define (external-filter-key-press-handler pc key key-state)
  (im-deactivate-candidate-selector pc)
  (if (ichar-control? key)
    (begin
      (external-filter-context-set-undo-str! pc #f)
      (im-commit-raw pc))
    (cond
      ((external-filter-help-key? key key-state)
        (external-filter-help pc)
        (external-filter-context-set-undo-str! pc #f))
      ((external-filter-undo-key? key key-state)
        (external-filter-undo pc)
        (external-filter-context-set-undo-str! pc #f))
      ((or (symbol? key)
           (and (modifier-key-mask key-state)
                (not (shift-key-mask key-state))))
        (external-filter-context-set-undo-str! pc #f)
        (im-commit-raw pc))
      ((ichar-lower-case? key)
        (external-filter-context-set-undo-str! pc #f)
        (let ((key-cmd (assv key external-filter-key-command-alist)))
          (if key-cmd
            (external-filter-launch pc (cadr key-cmd)))))
      ((ichar-upper-case? key)
        (external-filter-context-set-undo-str! pc #f)
        (external-filter-register pc (ichar-downcase key)))
      (else
        (external-filter-context-set-undo-str! pc #f)
        (im-commit-raw pc)))))

(define (external-filter-key-release-handler pc key state)
  (im-commit-raw pc))

(define (external-filter-get-candidate-handler pc idx accel-enum-hint)
  (let ((key-cmd (list-ref external-filter-key-command-alist idx)))
    (list (cadr key-cmd) (charcode->string (car key-cmd)) "")))

(define (external-filter-set-candidate-index-handler pc idx)
  (im-deactivate-candidate-selector pc)
  (let ((key-cmd (list-ref external-filter-key-command-alist idx)))
    (external-filter-launch pc (cadr key-cmd))))

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
 #f
 #f
 #f
 #f
 )

(define (external-filter-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (external-filter-launch pc cmd)
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
                 (cons pout-in pin-out)))))))
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
       (iport (open-file-port (car fds)))
       (oport (open-file-port (cdr fds))))
      (file-display str oport)
      (close-file-port oport)
      (let ((res (file-read-all iport)))
        (close-file-port iport)
        (and (string? res)
             (not (string=? res ""))
             res))))
  (let ((str (external-filter-acquire-text pc 'selection)))
    (if (string? str)
      (external-filter-commit pc (launch cmd str) str)
      (let ((clip (external-filter-acquire-text pc 'clipboard)))
        (if (string? clip)
          (external-filter-commit pc (launch cmd clip) ""))))))

;;; temporarily register filter command from selection
(define (external-filter-register pc key)
  (let ((sym (external-filter-command-symbol key))
        (str (external-filter-acquire-text pc 'selection)))
    (if (string? str)
      (begin
        (set-symbol-value! sym str)
        (external-filter-key-command-alist-update))
      (external-filter-commit pc (symbol-value sym) ""))))

(define (external-filter-commit pc commit-str undo-str)
  (define (count-char str)
    (string-length
      (with-char-codec external-filter-encoding
        (lambda ()
          (%%string-reconstruct! (string-copy str))))))
  (if (string? commit-str)
    (begin
      (external-filter-context-set-undo-str! pc
        (if (string? undo-str) undo-str ""))
      (external-filter-context-set-undo-len! pc (count-char commit-str))
      (im-commit pc commit-str))))

(define (external-filter-undo pc)
  (let ((str (external-filter-context-undo-str pc))
        (len (external-filter-context-undo-len pc)))
    (if str
      (begin
        (if (> len 0)
          (im-delete-text pc 'primary 'cursor len 0))
        (if (not (string=? str ""))
          (im-commit pc str))))))

(define (external-filter-help pc)
  (let ((nr (length external-filter-key-command-alist)))
    (im-activate-candidate-selector pc nr nr) ; TODO: display-limit
    (im-select-candidate pc 0))) ; to select candidate by click
