;; hexasm


;;
;; asm highlighting functions
;;

(setq hexasm-prev-lineno 1)
(defun get-lineno-from-hexl-address-at-point ()
  (let* ((address (hexl-current-address)))
    (while (not (gethash address hexasm-hex-to-asm-map))
      (setq address (1- address)))
    (gethash address hexasm-hex-to-asm-map)))


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

(cd "~/Code/os/print_alphabet")

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
  (let ((hexasm-hex-to-asm-map (make-hash-table :test 'equal)))
    (dolist (line lines)
      (when (is-code-line line)
        (let* ((tokens (split-string line))
               (lineno (string-to-number (nth 0 tokens)))
               (address-string (nth 1 tokens))
               (address (string-to-number address-string 16))
               (bytes (nth 2 tokens))
               (command (mapconcat 'identity (nthcdr 3 tokens) " ")))
          (puthash address lineno hexasm-hex-to-asm-map))))
    hexasm-hex-to-asm-map))


;;
;; asm -> hexl functions
;;


(defun swap-keys-values (original-map)
  "Create a new map where keys and values are swapped."
  (let ((new-map (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (puthash value key new-map))
             original-map)
    new-map))


(setq hexasm-prev-lineno 1)
(defun get-address-from-lineno-at-point ()
  (let* ((lineno (line-number-at-pos)))
    (while (not (gethash lineno hexasm-asm-to-hex-map))
      (setq lineno (1- lineno)))
    (gethash lineno hexasm-asm-to-hex-map)))


(defun move-point-to-address (address)
  (progn
    (windmove-right)
    (hexl-goto-address address)
    (windmove-left)))


(defun highlight-main-asm ()
  (when (string= (buffer-name) "os.asm")
    (let ((address (get-address-from-lineno-at-point)))
      (when address
        (move-point-to-address address)))))


(setq list-file-contents (get-list-file-contents))
(setq lines (split-string list-file-contents "\n"))
(setq hexasm-hex-to-asm-map (get-address-to-lineno-map lines))
(setq hexasm-asm-to-hex-map (swap-keys-values hexasm-hex-to-asm-map))


(remove-hook 'post-command-hook 'highlight-main)
(add-hook 'post-command-hook 'highlight-main)

(remove-hook 'post-command-hook 'highlight-main-asm)
(add-hook 'post-command-hook 'highlight-main-asm)


(setq hexasm-previous-buffer (buffer-name))


(defun hexasm-clear-highlighting-on-buffer-change-to-asm ()
  (progn
    (when (and (string= (buffer-name) "os.asm")
               (string= hexasm-previous-buffer "os"))
      (hexasm-un-highlight-line hexasm-prev-lineno))
    (setq hexasm-previous-buffer (buffer-name))))



;; (hexasm-un-highlight-line hexasm-prev-lineno)

(remove-hook 'post-command-hook 'hexasm-clear-highlighting-on-buffer-change-to-asm)
(add-hook 'post-command-hook 'hexasm-clear-highlighting-on-buffer-change-to-asm)

(buffer-name)
