;; ~/.emacs.d/init.el file von Nasser Attar
;; Erstellt: 		14. April 2017
;; Aktualisiert: 	31. Januar 2019
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
;; profiler-start: Starten des Profilers
;; profiler-report: Einen Statusbericht des Profilers anzeigen
;; profiler-stop: Profiler stoppen

;; *** Ergänze die Suchpfade ***
(add-to-list 'load-path "~/.emacs.d/dired+")
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/custom/basic")
(add-to-list 'load-path "~/.emacs.d/custom/cpp_ide")
(add-to-list 'load-path "~/.emacs.d/custom/python_ide")
(add-to-list 'load-path "~/.emacs.d/custom/tex")
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom/themes")

;; *** Laden eigener Funktionsdefinitionen ***
(require 'ana-func)

;; *** Laden der Basis-Konfiguration ***
(require 'setup-basic)

;; *** Lade die Einstellungen für eine bestimmte Programmiersprache ***
(setq ide-mode (load-ide-mode))

;; *** Die Fensterleiste wird so eingestellt, dass der Name des aktuellen Buffers
;; *** angezeigt wird ***
(setq frame-title-format
      (list ide-mode "   -   Emacs " emacs-version))

;; *** Laden des "tron"-Themes, das aus den offiziellen Packetquellen stammt ***
(setq custom-safe-themes t)
(add-hook 'after-init-hook (lambda () (load-theme 'tron)))
