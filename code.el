;;
;; hexl-mode
;;

(setq prev-lineno 1)


(defun get-lineno-from-hexl-address-at-point ()
  (let* ((hexl-address (format "%x" (hexl-current-address))))
    (gethash hexl-address my-map)))


(defun highlight-main ()
  (when (string= (buffer-name) "os")
    (let ((source-lineno (get-lineno-from-hexl-address-at-point)))
      (when source-lineno
        (progn
          (when (not (= source-lineno prev-lineno))
            (highlight-line-in-buffer "os.asm" prev-lineno t))
          (highlight-line-in-buffer "os.asm" source-lineno)
          (setq prev-lineno source-lineno))))))


(remove-hook 'post-command-hook 'get-lineno-from-hexl-address-at-point)
(add-hook 'post-command-hook 'highlight-main)


(setq my-var 5)
(setq my-var 6)


(add-hook 'post-command-hook '(lambda () (message "Hi!")))
(remove-hook 'post-command-hook '(lambda () (message "Hi!")))


(defun get-hexl-current-address-hex ()
  (let ((hexl-address (format "%x" (hexl-current-address))))
    (message hexl-address)))


hexl-follow-ascii-hook


(add-hook 'post-command-hook 'get-hexl-current-address-hex)
(remove-hook 'post-command-hook 'get-hexl-current-address-hex)


(defun highlight-line-in-buffer (buffer line-number &optional remove)
  "Highlight or remove highlighting from the given line number in the specified buffer."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-number))
      (if remove
          (let ((overlay (car-safe (overlays-at (point)))))
            (when overlay
              (delete-overlay overlay)))
        (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
          (overlay-put overlay 'face '(:background "yellow")))))))


(highlight-line-in-buffer "os.asm" 6)
(highlight-line-in-buffer "os.asm" 6 t)


(add-hook 'post-command-hook 'get-lineno-from-hexl-address-at-point)
(remove-hook 'post-command-hook 'get-lineno-from-hexl-address-at-point)


;;
;; hexl -> source code map
;;
(defun get-list-file-contents ()
  (setq list-file-contents
        (with-temp-buffer
          (insert-file-contents "list.lst")
          (buffer-string))))


(defun string-contains-only-zeros-p (str)
  "Check if a string consists entirely of zeros."
  (catch 'non-zero
    (mapc (lambda (char)
            (unless (char-equal char ?0)
              (throw 'non-zero nil)))
          str)
    t))


(defun is-hex-number (possible-number)
  (or (string-contains-only-zeros-p possible-number)
      (not (= (string-to-number possible-number 16) 0))))


(defun is-code-line (line)
  (let ((tokens (split-string line)))
    (and (> (length tokens) 2)
         (let ((lineno (nth 0 tokens))
               (possible-address (nth 1 tokens))
               (possible-bytes (nth 2 tokens)))
           (and (is-hex-number possible-address)
                (is-hex-number possible-bytes))))))


(defun get-address-to-lineno-map (lines)
  (let ((my-map (make-hash-table :test 'equal)))
    (dolist (line lines)
      (when (is-code-line line)
        (let* ((tokens (split-string line))
               (lineno (string-to-number (nth 0 tokens)))
               (address (nth 1 tokens))
               (hex-address (format "%x" (string-to-number address 16)))
               (bytes (nth 2 tokens))
               (command (mapconcat 'identity (nthcdr 3 tokens) " ")))
          (puthash hex-address lineno my-map))))
    my-map))


(setq list-file-contents (get-list-file-contents))
(setq lines (split-string list-file-contents "\n"))
(setq my-map (get-address-to-lineno-map lines))

