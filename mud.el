;; Tom's MUD client!
;; Uses requires from circe (some of the code in this file is based on circe also)
;; May not work for you, this is just me experimenting at the moment

(require 'lui)
(require 'ansi-color)
(require 'queue-f)

;; config

(defvar mud-prompt-string (concat (propertize "MUD>"
                                              'face 'mud-prompt-face)
                                  " "))
(defvar mud-prompt-face 'mud-prompt-face
  "The face for the Mud prompt.")


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

(defvar mud-command-queue nil
  "The commands sent to the server for which a response has not been detected")
(make-variable-buffer-local 'mud-command-queue)

(defvar mud-response-accumulator '())
(make-variable-buffer-local 'mud-response-accumulator)


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
    (set-buffer server-buffer)))


(defun mud-mode ()
  (lui-mode)
  (setq major-mode 'mud-mode
        mode-name "MUD"
        lui-input-function 'mud-input)
  (lui-set-prompt mud-prompt-string)
  (goto-char (point-max))
  (set (make-local-variable 'completion-ignore-case)
       t)
  (setq lui-fill-column 100)
  (setq lui-fill-type nil)
  (run-hooks 'mud-mode-hook))

(defun mud-reconnect ()
  (interactive)
  (setq mud-command-queue (queue-create)) ; empty the command queue
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
    (queue-enqueue mud-command-queue string)
    (lui-insert (propertize string 'face 'mud-echo-face))
    (process-send-string mud-server-process (concat string "\n"))))


(defun mud-process-line (line)
  "Process a new line from the mud server. Buffer will already be set correctly."
  ;; TODO: ignore non-command thingies, multiple prompts on a line
  (if (string-match "^\\(> \\)" line)
      (progn
        (princ (queue-dequeue mud-command-queue))
        (setq mud-response-accumulator nil))
    (push line mud-response-accumulator)
    (lui-insert line)))

(defun mud-server-filter-function (process string)
  (with-current-buffer (process-buffer process)
    (setq string (ansi-color-apply string))
    (setq mud-filter-data (concat mud-filter-data string))
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
    (lui-insert "DISCONNECTED")))

(provide 'mud)