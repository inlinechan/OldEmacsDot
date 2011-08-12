;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hangul configuration
(set-language-environment "korean")

;; mark could be noticable
(setq-default transient-mark-mode t)

;; no backup ( start with ~(tilt) )
(setq-default make-backup-files nil)

;; column limit 80
(setq fill-column 80)

;; text-mode is default
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; c-mode,c++-mode coding style bsd
(add-hook 'c-mode-hook '
          (lambda () 
            (c-set-style "bsd")
            (setq default-tab-width 4)
            (setq c-basic-offset 4)         ;; indent use only 2 blank
            ;; (setq indent-tabs-mode nil)     ;; no tab
            ))

(add-hook 'c++-mode-hook '
          (lambda () 
            (c-set-style "bsd")
            (setq default-tab-width 4)
            (setq c-basic-offset 4)         ;; indent use only 2 blank
            ;; (setq indent-tabs-mode nil)     ;; no tab
            ))

;; python also hate tab ( using 4 space instead )
(add-hook 'python-mode-hook '
		  (lambda ()
            (setq default-tab-width 4)
            ;; (setq indent-tabs-mode nil)     ;; no tab
            ))

;; no splash
(setq inhibit-startup-message t)

;; hide toolbar & menubar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; tab width
(setq default-tab-width 4)
(setq c-basic-offset 4)         ;; indent use only 2 blank
(setq indent-tabs-mode nil)     ;; no tab

;; parenthesis matching
;; http://www.emacswiki.org/cgi-bin/wiki/parenthesismatching
(defun goto-match-paren (arg)
  "go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; purpose: when you visit a file, point goes to the last place where
;; it was when you previously visited the same file.
;;
;; http://www.emacswiki.org/cgi-bin/wiki/saveplace
(require 'saveplace)
(setq-default save-place t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make it more quick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c c") 'compile)    
(global-set-key (kbd "C-c r y") 'comment-region)
(global-set-key (kbd "C-c r u") 'uncomment-region)
(global-set-key (kbd "M-g") 'goto-line)

;; fast move next, previous buffer
(global-set-key (kbd "C-c C-n") 'next-buffer)
(global-set-key (kbd "C-c C-p") 'previous-buffer)

;; (global-set-key "\c-xt" 'goto-line)        ;; goto-line
(global-set-key (kbd "C-c m") 'manual-entry)    ;; manpage
;; (global-set-key "\c-cs" 'shell-command)    ;; shell-cmd

(global-set-key (kbd "M-]") 'goto-match-paren)  ;; goto matching parenthesis

;; execute the shell buffer in utf-8 encoding.
;; (defun unicode-shell ()
;;   "execute the shell buffer in utf-8 encoding.
;; note that you'll need to set the environment variable lang and others
;; appropriately."
;;   (interactive)
;;   (let ((coding-system-for-read 'utf-8)
;;         (coding-system-for-write 'utf-8)
;;         (coding-system-require-warning t))
;;     (call-interactively 'shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)

;; css-mode
(autoload 'css-mode "css-mode-simple")
(setq auto-mode-alist       
	  (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; javascirpt-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
;; (autoload 'javascript-mode "javascript-mode")
;; (setq auto-mode-alist       
;;      (cons '("\\.js\\'" . javascript-mode) auto-mode-alist))

;; js2 mode
;; http://code.google.com/p/js2-mode/wiki/installationinstructions
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; makefile
(setq auto-mode-alist       
	  (cons '("\\.min\\'" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist       
	  (cons '("\\.mak\\'" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist       
	  (cons '("\\.make\\'" . makefile-mode) auto-mode-alist))

;; perl mode
(add-to-list 'auto-mode-alist '("\\.\\([pp][llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cedet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.emacswiki.org/emacs/collectionofemacsdevelopmentenvironmenttools
(setq byte-compile-warnings nil)

;; load cedet.
;; see cedet/common/cedet.info for configuration details.
(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")

;; enable ede (project management) features
(global-ede-mode 1)

;; enable ede for a pre-existing c++ project
;; (ede-cpp-root-project "name" :file "~/myproject/makefile")


;; enabling semantic (code-parsing, smart completion) features
;; select one of the following:

;; * this enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * this enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * this enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)

;; * this enables the use of exuberent ctags if you have it installed.
;;   if you use c++ templates or boost, you should not enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   or, use one of these two types of support.
;;   add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; enable srecode (template management) minor-mode.
(global-srecode-minor-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ecb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/ecb-snap")
(require 'ecb)
(require 'ecb-autoloads)

;; (custom-set-variables
;;   ;; custom-set-variables was added by custom.
;;   ;; if you edit it by hand, you could mess it up, so be careful.
;;   ;; your init file should contain only one such instance.
;;   ;; if there is more than one, they won't work right.
;;  '(ecb-options-version "2.40")
;;  '(ecb-source-path (quote (("/home/hyungchan/android/external/webkit" "webkit")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; ecb window hotkey
(global-set-key (kbd "M-0") 'ecb-goto-window-edit-last)
(global-set-key (kbd "M-1") 'ecb-goto-window-directories)
(global-set-key (kbd "M-2") 'ecb-goto-window-sources)
(global-set-key (kbd "M-3") 'ecb-goto-window-methods)
(global-set-key (kbd "M-4") 'ecb-goto-window-history)

(global-set-key (kbd "C-c C-e") 'ecb-activate)
(global-set-key (kbd "C-c C-d") 'ecb-deactivate)

;; switch h <-> cpp
(global-set-key (kbd "M-p") 'eassist-switch-h-cpp)

;; global regexp search
(global-set-key (kbd "C-c , h") 'semantic-symref-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; android webkit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ede-cpp-root-project "androidwebkit"
					  :name "android webkit"
					  :file "~/android/allfiles"
					  :include-path '("~/android/external/webkit"
									  "~/android/external/skia"
									  "~/android/frameworks/base/core/java/android")
					  ;; :include-path '("/"
					  ;; 				  "/common"
					  ;; 				  "/interfaces"
					  ;; 				  "/libs"
					  ;; 				  )
					  ;; :system-include-path '("~/exp/include")
					  ;; :spp-table '(("isunix" . "")
					  ;; 			   ("boost_test_dyn_link" . ""))
					  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)
(global-set-key (kbd "C-c C-f") 'gtags-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
