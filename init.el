;; ~/.emacs.d/init.el file von Nasser Attar
;; Erstellt: 		14. April 2017
;; Aktualisiert: 	1. Dezember 2018
;;
;; *** Wichtige Tastenkombination ***
;; C-x z: Letztes Kommando wiederholen. Wenn "z" n-mal gedrückt wird,
;; 	  dann wird das vorherige Kommando auch n-mal neu ausgeführt
;; C-h b: Alle Tastenkombinationen anzeigen, die im derzeitigen Major- und
;;        Minormode aktiv sind.
;; C-/: Eingabe rückgangig machen.
;; C-x C-q: Read-only Mode aktivieren.
;; C-h m: Dokumentation zum aktuellen Hauptmodus
;; C-x 4 C-f <Dateiname>: Datei in einem anderem Fenster öffnen
;; C-q TAB: Insert a tab
;; M-m: Bewege Cursor zum ersten Nicht-Leerzeichen der aktuellen Zeile
;; C-x C-o: Alle leeren Zeilen unter dem Cursor löschen, bis auf eine
;;          einzige
;; C-x h: Gesamten Text im Buffer  markieren
;; M-n,M-p: In der inkrementellen Suche, nach vorherigen oder späteren
;;          Suchausdrücken suchen
;; C-t: Zwei Buchstaben vertauschen ( Auf beiden Seiten des Punktes )
;; C-x C-e: Lisp Interpreter aufrufen, damit er einen Lisp-Ausdruck
;; 		 evaluiert.
;; C-x r s <register>: Speichert aktuelle Region in <register>
;; C-x r i <register>: Fügt Inhalt von <register> in aktuellen Buffer ein
;; M-: : Führt Lisp-Ausdruch in Minibuffer aus, benutzt aber als Wert des
;;       Cursors, den im aktuellen Buffer
;; C-x C-v: Öffnen einer neuen Datei und gleichzeitig schließen des
;;	    aktuellen Buffers
;; C-x SPC: Aktiviere Rechteck-Marker-Mode
;; C-u [Argument] <Command>: Übergibt <Command> das <Argument>
;; C-h i: Das Info-System wird aufgerufen
;; C-u C-x C-e: Einen Lisp-Ausdruck ausführen und Rückgabewert in *scratch* ausgeben
;; C-x n n: Buffer auf Region eingrenzen (Narrowing)
;; C-x n w: Eingrenzung aufheben
;;
;; *** Wichtige Funktionen ***
;; revert-buffer: Buffer aktualisieren.
;; check-parens: Finde alle Klammern, die nicht geschlossen sind
;; load-file: Alle Lisp-Kommandos einer Datei ausführen.
;;		  Nützlich um .emacs neu einzulesen
;; fill-paragraph: Formatiert Text in Region auf eingestellte
;; 		       Zeilelänge um.
;; delete-desktop: Lösche den Desktop ohne desktop-dirname auf nil zu setzen
;; buffer-file-name: Gibt den Pfad der zum aktuellen Buffer gehörigen Datei aus
;; sr-speedbar-toggle: Öffnen der frame-losen Speedbar

;; *** Laden eigener Funktionsdefinitionen ***
(load "~/.emacs.d/ana_func.el")

;; *** Maximieren des Emacs-Frames ***
;; The bottom commands maximize the window to full size
(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

;; *** Die Fensterleiste wird so eingestellt, dass der Name des aktuellen Buffers
;; *** angezeigt wird ***
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; *** Zeilennummern aktivieren ***
(global-linum-mode t)

;; *** Sicherung von Buffern ***
;; Setze das Verzeichnis für die Backups
(setq
   backup-by-copying t
   backup-directory-alist
   '(("." . "~/.backups"))
)

;; Setze das zeitliche Intervall zwischen automatischen Sicherungen
(setq auto-save-timeout 180)

;; *** Ausschalten der Werkzeug- und Menüleiste ***
(tool-bar-mode -1)
(menu-bar-mode -1)

;; *** Verhindere dass der Begrüßungsschirm angezeigt wird ***
(setq inhibit-startup-screen t)

;; *** Automatisches Schließen von Klammern ***
(electric-pair-mode 1)

;; *** Anzeigen von einander zugehörigen Klammern ***
(show-paren-mode 1)
(setq show-paren-delay 0)

;; *** Benutze Leerzeichen für Einrückungen ***
(setq-default indent-tabs-mode nil)

;; *** Einstellen der Tab-Weite ***
(setq-default tab-width 4)

;; *** Speichern des Desktops (Quelle: https://www.emacswiki.org/emacs/Desktop)
;; Use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

(defun delete-desktop ()
  "Delete desktop file without setting desktop-dirname to nil"
  (interactive)
  (setq desktop-dirname-tmp desktop-dirname)
  (desktop-remove)
  (setq desktop-dirname desktop-dirname-tmp))

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook 'delete-desktop)

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

(add-hook 'kill-emacs-hook 'session-save)

(setq desktop-load-locked-desktop t)

;; *** Installieren der notwendigen Packages, falls dies noch nicht getan wurde ***
(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    exec-path-from-shell
    magit
    ace-window
    sr-speedbar))

(defvar pyPackages
  '(elpy
    flycheck
    ein
    pylint
    py-autopep8
    realgud))

(install-necessary-packages (concat-multiple-lists (list
						    myPackages
						    pyPackages)))

;; *** Konfiguriere die Speedbar ***
(setq speedbar-show-unknown-files t)
(require 'sr-speedbar)
(sr-speedbar-refresh-turn-off)

;; *** Füge einen Pfad für Custom-Themes hinzu ***
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-safe-themes t)

;; *** Lade die Einstellungen für eine bestimmte Programmiersprache ***
(defun load-python-ide-settings ()
  "The settings are altered such that Emacs can be better used as Python IDE"
  (interactive)
  (load "~/.emacs.d/python_ide.el"))

(defun load-cpp-ide-settings ()
  "The settings are altered such that Emacs can be better used as Python IDE"
  (interactive)
  (load "~/.emacs.d/cpp_ide.el"))

(require 'ido)
(defun my-pick-one ()
  "Prompt user to pick a choice from a list."
  (interactive)
  (let ((choices '("nil" "c/c++" "python" "all")))
    (setq chosen (message "%s" (ido-completing-read "Choose language: " choices )))
    (cond ((equal chosen "c/c++") (load-cpp-ide-settings))
          ((equal chosen "python") (load-python-ide-settings)))))

(add-hook 'after-init-hook 'my-pick-one)

;; *** Abfrage ob der Desktop wieder hergestellt werden soll ***
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))

;; *** Laden des "tron"-Themes, das aus den offiziellen Packetquellen stammt ***
(add-hook 'after-init-hook (lambda () (load-theme 'tron)))
