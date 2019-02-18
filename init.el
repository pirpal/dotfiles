;; Packages.el DONT DELETE ------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  ;; Melpa archive choice ----------------------------------
  ;; Comment/uncomment these two lines to enable/disable
  ;; MELPA and MELPA Stable as desired
  (add-to-list 'package-archives
  	       (cons "melpa"
  		     (concat proto "://melpa.org/packages/")))
  (add-to-list
    'package-archives
     (cons "melpa-stable"
       (concat proto "://stable.melpa.org/packages/")))

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
 (cons "gnu"
		       (concat proto "://elpa.gnu.org/packages/")))))

;; require statements --------------------------------------
(require 'multiple-cursors)
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(yas-reload-all)

;; Libs and custom files imports ---------------------------
;; fix dead circumflex issue
(load-library "iso-transl")
;; (load "~/.emacs.d/my-custom-faces.el")
;;(load "~/.emacs.d/my-writer-mode.el")

;; Global settings -----------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(prefer-coding-system 'utf-8)
(setq debug-on-error t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Aliases -------------------------------------------------
(defalias 'color 'list-colors-display)
(defalias 'bind 'describe-bindings)
(defalias 'face 'list-faces-display)
(defalias 'sff 'set-face-foreground)
(defalias 'sfb 'set-face-background)
(defalias 'sffo 'set-face-font)

;;(defalias 'myw 'custom-writer-mode)

;; Appearance ----------------------------------------------
(global-hl-line-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)
(setq frame-title-format '(buffer-file-name "%b (%f)" "%b")) ;; "%b (%f)" "%b"
(add-to-list 'default-frame-alist
	     '(font . "Monaco 10"))
(setq font-lock-maximumsize nil)

;; Ace-jump pimp -------------------------------------------
;; (set-face-foreground 'ace-jump-face-foreground "LimeGreen")

;; Startup configuration -----------------------------------
;; (defun my-startup-setup()
;;   (toggle-frame-maximized)
;;   (find-file "~/todo.org")
;;   (find-file "~/.emacs.d/init.el"))
;; (my-startup-setup)

;; Register files ------------------------------------------
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?t '(file . "~/todo.org"))

;;; Cursor --------------------------------------------------
(setq-default cursor-type 'box)
(blink-cursor-mode 0)
(setq x-cursor-fore-pixel t)

;;; Mode line ----------------------------------------------
(line-number-mode t)     ;; show line num in mod-line
(column-number-mode t)   ;; show col num in mod-line
(size-indication-mode t) ;; mod-line current-line nb / lines nb
(visual-line-mode)

;;; Backup -------------------------------------------------
(setq make-backup-files nil)
;; (auto-image-file-mode 1)
;;; Ido ----------------------------------------------------
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

 ;;; Hooks -------------------------------------------------
(defun my-favorites-modes()
  (setq autopair-mode t)
  (setq auto-complete-mode t)
  (setq superword-mode t)
  (setq rainbow-mode t))

(add-hook 'markdown-mode-hook 'auto-fill-mode '(setq-default fill-column 70))
(add-hook 'markdown-mode-hook 'my-favorites-modes)
(setq markdown-fontify-code-blocks-natively t)

(add-hook 'org-mode-hook 'auto-fill-mode '(setq-default fill-column 70))
(add-hook 'org-mode-hook 'my-favorites-modes)

(add-hook 'emacs-lisp-mode-hook 'my-favorites-modes)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(add-hook 'python-mode-hook 'my-favorites-modes)
(add-hook 'python-mode-hook 'hs-minor-mode)

(add-hook 'haskell-mode-hook 'my-favorites-modes)

;; Bindings ----------------------------------------------------------
(global-set-key (kbd "C-0") 'dired-other-window)
(global-set-key (kbd "C-S-b") 'list-buffers)
(global-set-key (kbd "C-S-n") 'next-buffer)
(global-set-key (kbd "C-S-e") 'eval-buffer)
(global-set-key (kbd "C-s-r") 'replace-rectangle)
(global-set-key (kbd "C-<enter>") 'ace-window)
(global-set-key (kbd "C-+") 'ace-jump-mode)
(global-set-key (kbd "<f12>") 'window-configuration-to-register)

;; Folding with hs-minor-mode
;; see https://www.emacswiki.org/emacs/HideShow
(defun toggle-selective-display (column)
  (interactive "p")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "p")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))

(global-set-key (kbd "C-<tab>") 'toggle-hiding)

;; shortcuts for my broken (: /) keyboard key
(global-set-key (kbd "C-?") (lambda () (interactive) (insert ":")))
(global-set-key (kbd "C-.") (lambda () (interactive) (insert "/")))

(global-set-key (kbd "C-=") 'save-buffer)

;; Custom faces
;; No custom theme needed : use this only configuration in addition
;; with Ubuntu Equilux-compact dark theme.

;; Mode line
(set-face-background 'mode-line "#1f2942")
(set-face-background 'mode-line-inactive "gray12")
(set-face-foreground 'mode-line "GhostWhite")
(set-face-background 'mode-line-buffer-id "#1f2942")
(set-face-foreground 'mode-line-buffer-id "PowderBlue")
(set-face-foreground 'mode-line-highlight "GhostWhite")

;; General faces
;; (set-face-foreground 'font-lock-builtin-face "white")
(set-face-foreground 'font-lock-doc-face "SpringGreen")
(set-face-foreground 'font-lock-comment-face "gray50")
(set-face-foreground 'font-lock-comment-delimiter-face "firebrick3")
(set-face-foreground 'font-lock-string-face "brown1")
;;(set-face-foreground 'font-lock-builtin-face "burlywood")
(set-face-foreground 'font-lock-keyword-face "orange")
(set-face-foreground 'font-lock-function-name-face
		     "#37c440")
(set-face-foreground 'font-lock-type-face "DodgerBlue1")

;; Org mode
(set-face-background 'org-code "gray6")
(set-face-foreground 'org-code "GhostWhite")
(set-face-foreground 'org-block-begin-line "gray42")
(set-face-foreground 'org-block-end-line "gray42")
(set-face-background 'org-block "#1d1902")
(set-face-foreground 'org-table "GhostWhite")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3e83abe75cebf5621e34ce1cbe6e12e4d80766bed0755033febed5794d0c69bf" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" default)))
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (easy-kill ace-window zenity-color-picker yasnippet vimish-fold sublime-themes rainbow-mode org multiple-cursors mmm-mode markdown-preview-eww markdown-mode impatient-mode gruvbox-theme flymd evil-terminal-cursor-changer emmet-mode egg nedit-color-stamp borland-blue-theme blackboard-theme binclock autumn-light-theme autopair auto-complete anaconda-mode ample-theme ahungry-theme abyss-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#1f2942" :foreground "#f5f5f5" :box nil :slant normal :weight extra-bold :height 1.3 :width normal)))))
