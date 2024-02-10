;;
;; hexl-mode
;;
(defun print-lineno-from-hexl-address-at-point (hexl-address-to-source-lineno-map)
  (let ((hexl-address (format "%x" (hexl-current-address))))
    (gethash "0" hexl-address-to-source-lineno-map)))


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
               (lineno (nth 0 tokens))
               (address (nth 1 tokens))
               (hex-address (format "%X" (string-to-number address 16)))
               (bytes (nth 2 tokens))
               (command (mapconcat 'identity (nthcdr 3 tokens) " ")))
          (puthash hex-address lineno my-map))))
    my-map))


(setq list-file-contents (get-list-file-contents))
(setq lines (split-string list-file-contents "\n"))
(setq my-map (get-address-to-lineno-map lines))

