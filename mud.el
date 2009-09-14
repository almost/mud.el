;; Tom's MUD client!
;; Uses requires from circe (some of the code in this file is based on circe also)
;; May not work for you, this is just me experimenting at the moment
;; I've only tested it on discworld.atuin.net

(require 'lui)
(require 'ansi-color)
(require 'queue-f)

;; config

(defvar mud-prompt-string (concat (propertize "MUD>"
                                              'face 'mud-prompt-face)
                                  " "))
(defvar mud-prompt-face 'mud-prompt-face
  "The face for the Mud prompt.")


(defvar mud-filter-out-regexp-list 
  (list "^\\(> \\)*\n?"         ;; The prompt, not much use if not using telnet
        "[\373-\377\C-a\C-x]*"  ;; Telnet codes
        )
  "List of regexps to remove from the incoming stream.")
                                         

(defface mud-prompt-face
  '((t (:weight bold :foreground "Black" :background "LightSeaGreen")))
  "The face for the Mud prompt."
  :group 'mud)

(defface mud-echo-face
  '((t (:weight bold :foreground "Yellow")))
  "The face for the echoed commands."
  :group 'mud)


;; vars
(defvar mud-host nil
  "")
(make-variable-buffer-local 'mud-host)

(defvar mud-port nil
  "")
(make-variable-buffer-local 'mud-port)

(defvar mud-server-process nil
  "")
(make-variable-buffer-local 'mud-server-process)

(defvar mud-filter-data ""
  "")
(make-variable-buffer-local 'mud-filter-data)

(defvar mud-line-filter-functions '()
  "An abnormal hook that processes lines comming from the mud.
  If a function returns a string then it will replace the line to
  be passed to the next function. Returning the keyword :ignore
  will cause the line to not be passed to the remaining functions
  and to not be displayed. ")

(defun mud (host port)
  (interactive "sHost: \nsPort: ")
  (when (equal port "")
    (setq port "23"))
  (let* ((buffer-name (format "%s:%s" host port))
         (server-buffer (generate-new-buffer buffer-name)))
    (with-current-buffer server-buffer
      (mud-mode)
      (setq mud-host host
            mud-port port)
      (mud-reconnect))
    (switch-to-buffer server-buffer)))

(defun mud-mode ()
  (lui-mode)
  (setq major-mode 'mud-mode
        mode-name "MUD"
        lui-input-function 'mud-input)
  (setq mud-previous-inserted-line-beg (make-marker))
  (lui-set-prompt mud-prompt-string)
  (goto-char (point-max))
  (set (make-local-variable 'completion-ignore-case)
       t)
  (set (make-local-variable 'lui-fill-column) 100)
  (set (make-local-variable 'lui-fill-type) nil)
  (run-hooks 'mud-mode-hook))

(defun mud-reconnect ()
  (interactive)
  (setq mud-response-accumulator '())
  (setq mud-server-process (open-network-stream mud-host
                                                (current-buffer)
                                                mud-host
                                                mud-port))
  (set-process-filter mud-server-process
                      #'mud-server-filter-function)
  (set-process-sentinel mud-server-process
                        #'mud-server-sentinel))

(defun mud-input (string)
  (with-current-buffer (process-buffer mud-server-process)
    (lui-insert (propertize string 'face 'mud-echo-face))
    (process-send-string mud-server-process (concat string "\n"))))

(defun mud-process-line (line &optional partial)
  "Process a new line from the mud server. Buffer will already be set correctly."
  ;; TODO: ignore non-command thingies, multiple prompts on a line
  (let ((fns mud-line-filter-functions)) ;
    (while (and (not (equal line :ignore)) fns)
      (setq line (or (apply (car fns) (list line)) line))
      (setq fns (cdr fns))))
  (if (not (eq line :ignore))
      (mud-insert line mud-previous-line-partial))
  (setq mud-previous-line-partial partial))


(defun mud-server-filter-function (process string)
  ;; Get rid of telnet codes
  ;;(setq string (replace-regexp-in-string "[\373-\377\C-a\C-x]*" "" string))
  (with-current-buffer (process-buffer process)
    (setq string (ansi-color-apply string))
    (setq mud-filter-data (concat mud-filter-data string))
    (dolist (re mud-filter-out-regexp-list)
      (setq mud-filter-data(replace-regexp-in-string re "" mud-filter-data)))
    (while (and mud-filter-data
                (string-match "[\n\r]+" mud-filter-data))
      (let ((line (substring mud-filter-data
                             0 (match-beginning 0))))
        (setq mud-filter-data
              (if (= (match-end 0)
                     (length mud-filter-data))
                  nil
                (substring mud-filter-data
                           (match-end 0))))
        (mud-process-line line)))
    ;; partial line heuristics, sometimes a prompt is given with no following newline
    (when (and mud-filter-data
               (string-match ".*[>?:] *\\(\\[a-zA-Z0-9/-\\]\\)? *$" mud-filter-data))
      (mud-process-line mud-filter-data)
      (setq mud-filter-data nil))))
    
(defun mud-server-sentinel (process state)
  (with-current-buffer (process-buffer process)
    (lui-insert (concat "DISCONNECTED: " state))))

; Maybe more specialized stuff for Discworld mud
;; (defun mud-capture-hp (line)
;;   (if (string-match 
;;        "^Hp: \\([0-9]+\\) (\\([0-9]+\\)) Gp: \\([0-9]+\\) (\\([0-9]+\\)) Xp: \\([0-9]+\\)$"
;;        line)
;;       ;(message (concat "HP is: " (match-string 1 line))))
;;     line)
;; (add-hook 'mud-line-filter-functions 'mud-capture-hp) 

(provide 'mud)