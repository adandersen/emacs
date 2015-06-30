(require 'package)
  (push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives)
	(package-initialize) ;; necessary for packages installed from melpa to be found in the load-path

(add-to-list 'load-path "~/.emacs.d/evil")

(require 'evil-leader)
    (setq evil-leader/in-all-states 1) ; activate for all states
    (global-evil-leader-mode) ;; must be activated before evil or wont be in *Scratch* buffers or *Messages*
    (evil-leader/set-leader "<SPC>")
(require 'evil)
    (evil-mode 1)
(require 'helm-misc)
(require 'helm-config)
    (helm-mode 1)
(require 'helm-locate)
(setq helm-quick-update t)
(setq evil-operator-state-cursor '("blue" hollow))

(global-set-key (kbd "M-x") 'helm-M-x)
(desktop-save-mode 1)
(setq compilation-auto-jump-to-first-error t)

(define-key evil-normal-state-map "  " 'helm-mini)
(define-key evil-normal-state-map "\M-x" 'helm-M-x)
(define-key evil-normal-state-map "\C-xc" 'compile)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("blue" bar))
(setq evil-replace-state-cursor '("blue" bar))
(setq evil-operator-state-cursor '("blue" hollow))


;;; allows quitting helm and other mini-buffers when escape is pressed
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(require 'compile)
(add-hook 'c++-mode-hook
           (lambda ()
	     (unless (file-exists-p "Makefile")
	       (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -o %s %s %s %s"
                              (or (getenv "c++") "c++")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-std=c++11 -stdlib=libc++ -Weverything -g")
			      file))))))

;; Run C programs directly from within emacs
(defun execute-c++-program ()
  (interactive)
  (defvar foo)
  (let (filename (file-name-sans-extension buffer-file-name))
    (setq compile-c-program (concat "c++ " (buffer-file-name) " -o " filename ".o -std=c++11 -stdlib=libc++ -Weverything -g && ./" filename ".o"))
  (shell-command compile-c-program)))

;;; rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)
