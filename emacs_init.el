;; EMACS INIT FILE BACKUP

;; Packages ---------------------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(require 'multiple-cursors)
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(yas-reload-all)

;; Global settings ---------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(prefer-coding-system 'utf-8)
(setq debug-on-error t)

;;; Appearance -------------------------------------------------------
(add-to-list 'default-frame-alist
	     '(font . "Monaco 10"))
(global-hl-line-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)
(setq frame-title-format '(buffer-file-name "%b (%f)" "%b"))
(setq font-lock-maximumsize nil)

;; Startup configuration ---------------------------------------------
(defun my-startup-setup()
  (toggle-frame-maximized)
  (find-file "~/.emacs.d/_bindings.md"))
(my-startup-setup)

;; Register files ----------------------------------------------------
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?b '(file . "~/.emacs.d/_bindings.md"))
(set-register ?t '(file . "~/todo.org"))

;; Theme and colors -------------------------------------------------
(load-theme 'gruvbox)

;; mode-line
(set-face-background 'mode-line "DarkOrange1")
(set-face-foreground 'mode-line "gray12")
(set-face-foreground 'mode-line-buffer-id "black")

;; comments
(set-face-foreground 'font-lock-doc-face "DarkOrange2")
(set-face-foreground 'font-lock-comment-face "DarkOrange2")
(set-face-foreground 'font-lock-comment-delimiter-face "DarkOrange2")

;; hl-line color
(set-face-background 'hl-line "gray5")

;; markdown
;;(set-face-foreground 'markdown-link-face "DeepSkyBlue1")
;;(set-face-background 'markdown-inline-code-face "gray16")
;;(set-face-foreground 'markdown-inline-code-face "DeepSkyBlue1")
;;(set-face-foreground 'markdown-list-face "GhostWhite")
;;(set-face-background 'markdown-list-face "gray16")
;;(set-face-foreground 'markdown-table-face "GhostWhite")
;;(set-face-background 'markdown-table-face "gray16")

;; Emacs buffers (help, packages, etc)
(set-face-foreground 'button "SpringGreen2")
;;(set-face-foreground 'package-name "SpringGreen2")
;; org
;;(set-face-foreground 'org-code "red4")
;;(set-face-background 'org-code "GhostWhite")
;;(set-face-font 'org-code "Monaco 11")
;;(set-face-foreground 'org-block "GhostWhite")
;;(set-face-background 'org-block-begin-line "tan")
;;(set-face-background 'org-block-end-line "tan")
;;(set-face-font 'org-level-1 "Monaco 11")
;;(set-face-font 'org-level-2 "Monaco 11")
;;(set-face-font 'org-level-3 "Monaco 11")
;;(set-face-font 'org-level-4 "Monaco 11")
;;(set-face-font 'org-level-5 "Monaco 11")
;;(set-face-font 'org-level-6 "Monaco 11")
;;(set-face-foreground 'org-level-1 "GhostWhite")
;;(set-face-foreground 'org-level-2 "GhostWhite")
;;(set-face-foreground 'org-level-3 "GhostWhite")
;;(set-face-foreground 'org-level-4 "GhostWhite")
;;(set-face-foreground 'org-level-5 "GhostWhite")
;;(set-face-foreground 'org-level-6 "GhostWhite")
;;(set-face-background 'org-level-1 "gray14")
;;(set-face-background 'org-level-2 "gray14")
;;(set-face-background 'org-level-3 "gray14")
;;(set-face-background 'org-level-4 "gray14")
;;(set-face-background 'org-level-5 "gray14")
;;(set-face-background 'org-level-6 "gray14")
;;(set-face-background 'org-table "gray18")
;;(set-face-foreground 'org-table "GhostWhite")
;;(set-face-foreground 'highlight nil)

;;; Cursor -----------------------------------------------------------
(setq-default cursor-type 'block)
(blink-cursor-mode 0)
(setq x-cursor-fore-pixel t)

;;; Mode line --------------------------------------------------------
(line-number-mode t)     ;; show line num in mod-line
(column-number-mode t)   ;; show col num in mod-line
(size-indication-mode t) ;; mod-line current-line nb / lines nb
(visual-line-mode)

;;; Backup ------------------------------------------------------------
(setq make-backup-files nil)

;;; Ido ---------------------------------------------------------------
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;; Hooks -------------------------------------------------------------
(defun my-favorites-modes()
  "Modes loaded for Elisp, Org, Python, Haskell, Markdown, HTML, CSS, etc."
  (setq autopair-mode t)
  (setq auto-complete-mode t)
  (setq superword-mode t)
  (setq rainbow-mode t))

(add-hook 'markdown-mode-hook 'auto-fill-mode '(setq-default fill-column 70))
(add-hook 'markdown-mode-hook 'my-favorites-modes)
(add-hook 'org-mode-hook 'my-favorites-modes)
(add-hook 'elisp-mode-hook 'my-favorites-modes)
'add-hook 'elisp-mode-hook 'outline-minor-mode
(add-hook 'haskell-mode-hook 'my-favorites-modes)
(add-hook 'python-mode-hook 'my-favorites-modes)
(add-hook 'python-mode-hook 'yas-minor-mode)

;;; Bindings ---------------------------------------------------------
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

;; easy-kill package ; see $HOME/shortcuts.org
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "C-S-b") 'list-buffers)
(global-set-key (kbd "C-S-n") 'next-buffer)
;;(global-set-key (kbd "C-S-m") 'mc/edit-lines)
(global-set-key (kbd "C-S-d") 'dired-other-window)

;; folding
(global-set-key (kbd "C-x 7") #'vimish-fold)
(global-set-key (kbd "C-x 8") #'vimish-fold-unfold)
(global-set-key (kbd "C-x 9") #'vimish-fold-unfold-all)

(global-set-key (kbd "C-0") 'dired-other-window)
(global-set-key (kbd "C-S-e") 'eval-buffer)
(global-set-key (kbd "C-s-r") 'replace-rectangle)
(global-set-key (kbd "C-<enter>") 'ace-window)
(global-set-key (kbd "C-+") 'ace-jump-mode)

(global-set-key (kbd "<f5>") 'describe-bindings)
(global-set-key (kbd "<f6>") 'list-colors-display)
(global-set-key (kbd "<f7>") 'list-faces-display)
(global-set-key (kbd "<f8>") 'list-packages)
6
;;--------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-safe-themes
   (quote
    ("3e83abe75cebf5621e34ce1cbe6e12e4d80766bed0755033febed5794d0c69bf" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" default)))
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (easy-kill ace-window zenity-color-picker yasnippet vimish-fold sublime-themes rainbow-mode org multiple-cursors mmm-mode markdown-preview-eww markdown-mode impatient-mode gruvbox-theme flymd evil-terminal-cursor-changer emmet-mode egg edit-color-stamp borland-blue-theme blackboard-theme binclock autumn-light-theme autopair auto-complete anaconda-mode ample-theme ahungry-theme abyss-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
