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


(defun hexasm-un-highlight-line (line-number)
  "Highlight or remove highlighting from the given line number in the specified buffer."
  (with-current-buffer "os.asm"
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-number))
      (let ((existing-overlay (car-safe (overlays-at (point)))))
        (when existing-overlay
          (delete-overlay existing-overlay))))))


(defun hexasm-highlight-line (line-number)
  (with-current-buffer "os.asm"
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-number))
      (let ((existing-overlay (car-safe (overlays-at (point)))))
        (when (not existing-overlay)
          (let ((new-overlay (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put new-overlay 'face '(:background "yellow"))))))))


(defun highlight-main ()
  (when (string= (buffer-name) "os")
    (let ((source-lineno (get-lineno-from-hexl-address-at-point)))
      (when source-lineno
        (progn
          (when (not (= source-lineno hexasm-prev-lineno))
            (progn
              (hexasm-un-highlight-line hexasm-prev-lineno)
              (setq hexasm-prev-lineno source-lineno)))
          (hexasm-highlight-line source-lineno)
          (windmove-left)
          (goto-line source-lineno)
          (windmove-right))))))


;;
;; Parse listing file functions
;;

(cd "~/Code/os/print_hello")

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
    (and (> (length tokens) 3)
         (let ((lineno (nth 0 tokens))
               (possible-address (nth 1 tokens))
               (possible-bytes (nth 2 tokens)))
           (is-hex-number possible-address)))))


(defun get-hex-to-asm-map (lines)
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
    (while (and (>= lineno 0)
                (not (gethash lineno hexasm-asm-to-hex-map)))
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


(setq hexasm-previous-buffer (buffer-name))
(defun hexasm-clear-highlighting-on-buffer-change-to-asm ()
  (progn
    (when (and (string= (buffer-name) "os.asm")
               (string= hexasm-previous-buffer "os"))
      (hexasm-un-highlight-line hexasm-prev-lineno))
    (setq hexasm-previous-buffer (buffer-name))))


(setq hexasm-previous-buffer-2 (buffer-name))
(defun hexasm-highlight-on-buffer-change-to-hexl ()
  (progn
    (when (and (string= (buffer-name) "os")
               (string= hexasm-previous-buffer-2 "os.asm"))
      (hexasm-highlight-line hexasm-prev-lineno))
    (setq hexasm-previous-buffer-2 (buffer-name))))



(defun get-listing-file-lines ()
  (let ((list-file-contents (get-list-file-contents)))
    (split-string list-file-contents "\n")))


(defun hexasm-init ()
  (setq lines (get-listing-file-lines))
  (setq hexasm-hex-to-asm-map (get-hex-to-asm-map lines))
  (setq hexasm-asm-to-hex-map (swap-keys-values hexasm-hex-to-asm-map))

  (remove-hook 'post-command-hook 'highlight-main)
  (add-hook 'post-command-hook 'highlight-main)

  (remove-hook 'post-command-hook 'highlight-main-asm)
  (add-hook 'post-command-hook 'highlight-main-asm)

  (remove-hook 'post-command-hook 'hexasm-clear-highlighting-on-buffer-change-to-asm)
  (add-hook 'post-command-hook 'hexasm-clear-highlighting-on-buffer-change-to-asm)

  (remove-hook 'post-command-hook 'hexasm-highlight-on-buffer-change-to-hexl)
  (add-hook 'post-command-hook 'hexasm-highlight-on-buffer-change-to-hexl t) ; high priority
  )

