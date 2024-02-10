;; hexasm


;;
;; Code highlighting functions
;;

(setq hexasm-prev-lineno 1)
(defun get-lineno-from-hexl-address-at-point ()
  (let* ((address (hexl-current-address)))
    (while (not (gethash address my-map))
      (setq address (1- address)))
    (gethash address my-map)))


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


(defun hexasm-un-highlight-line (lineno)
  (highlight-line-in-buffer "os.asm" lineno t))


(defun hexasm-highlight-line (lineno)
  (highlight-line-in-buffer "os.asm" lineno))


(defun highlight-main ()
  (when (string= (buffer-name) "os")
    (let ((source-lineno (get-lineno-from-hexl-address-at-point)))
      (when source-lineno
        (when (not (= source-lineno hexasm-prev-lineno))
          (progn
            (hexasm-un-highlight-line hexasm-prev-lineno)
            (hexasm-highlight-line source-lineno)
            (setq hexasm-prev-lineno source-lineno)))))))


;;
;; Parse listing file functions
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
               (address-string (nth 1 tokens))
               (address (string-to-number address-string 16))
               (bytes (nth 2 tokens))
               (command (mapconcat 'identity (nthcdr 3 tokens) " ")))
          (puthash address lineno my-map))))
    my-map))


(setq list-file-contents (get-list-file-contents))
(setq lines (split-string list-file-contents "\n"))
(setq my-map (get-address-to-lineno-map lines))


(remove-hook 'post-command-hook 'highlight-main)
(add-hook 'post-command-hook 'highlight-main)
