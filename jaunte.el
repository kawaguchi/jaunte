;; jaunte.el --- Emacs Hit a Hint

;; 参考
;; yafastnav.el
;; vimperator

;; 設定
;; (require 'jaunte)
;; (global-set-key (kbd "C-c C-j") 'jaunte)

(defvar jaunte-keys (mapcar #'identity "jklasdfghyuiopqwertnmzxcvb"))

(defface jaunte-hint-face
  '((t
     (:foreground "white"
      :background "blue"
      :italic nil
      :bold nil)))
  nil)

(defvar jaunte--hints nil)

(defun jaunte-forward-word ()
  "Move to beginning of a forward word, and return point."
  (interactive)
  (if (looking-at "\\w")
      (forward-word))
  (if (re-search-forward "\\w" nil 'eob)
      (backward-char))
  (point))

(defun jaunte-make-hint (key overlay window point)
  (let ((hint (make-hash-table :test 'equal)))
    (puthash 'key key hint)
    (puthash 'overlay overlay hint)
    (puthash 'window window hint)
    (puthash 'point point hint)
    hint))

(defun jaunte-show-hints ()
  (let ((index 0))
    (mapcar
     (lambda (window)
       (save-excursion
         (save-window-excursion
           (select-window window)
           (move-to-window-line 0)
           (let ((point (if (looking-at "\\w")
                            (point)
                            (jaunte-forward-word)))
                 (window-end (window-end window))
                 (key (jaunte-make-key index)))
             (while (< point window-end)
               (add-to-list 'jaunte--hints
                            (jaunte-make-hint (jaunte-make-key index)
                                              (jaunte-make-overlay point key)
                                              window
                                              point))
               (jaunte-forward-word)
               (setq index (1+ index)
                     point (point)
                     key (jaunte-make-key index)))))))
     (window-list))))

(defun jaunte-hint-match (key hint &optional perfect-match)
  (let ((hint-key (gethash 'key hint)))
    (if perfect-match
        (string= key hint-key)
      (equal 0 (string-match key hint-key)))))

(defun jaunte-search (key &optional perfect-match)
  (let (result)
    (mapcar
     (lambda (hint)
       (if (jaunte-hint-match key hint perfect-match)
           (add-to-list 'result hint)
         (delete-overlay (gethash 'overlay hint))))
     jaunte--hints)
    (setq jaunte--hints result)))

(defun jaunte-remove-hints ()
  (jaunte-delete-overlays)
  (setq jaunte--hints nil))

(defun jaunte-delete-overlays ()
  (mapcar
   (lambda (hint)
     (delete-overlay (gethash 'overlay hint)))
   jaunte--hints))

(defun jaunte-make-overlay (point key)
  (save-excursion
    (select-window window)
    (goto-char point)

    (let* ((width (length key))
           (rest width)
           (begin point)
           end
           overlay)
      (while (and (> rest 0) (not (eolp)))
        (setq rest (- rest (char-width (char-after))))
        (forward-char))
      (setq end (point))

      (setq overlay (make-overlay begin end))
      (overlay-put overlay' before-string (propertize key 'face 'jaunte-hint-face))
      (overlay-put overlay 'window (selected-window))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'width width)
      (overlay-put overlay 'priority 100)
      overlay)))

(defun jaunte-make-key (index)
  (let* ((key-length (length jaunte-keys))
         (excess (/ index key-length))
         prefix
         (n (% index key-length)))
    (setq prefix (if (zerop excess)
                          "" (jaunte-make-key (1- excess))))
    (concat prefix (char-to-string (nth n jaunte-keys)))))

(defun jaunte-to (hint)
  (select-window (gethash 'window hint))
  (goto-char (gethash 'point hint)))

(defun jaunte ()
  (interactive)
  (jaunte-show-hints)
  (unwind-protect
      (let (k key)
        (while (not (null jaunte--hints))
          (setq k (read-event (concat "Jaunte to " key)))
          (if (and (not (null key))
                   (or (equal k 13) ;; C-m
                       (equal k 10) ;; C-j
                       (equal k 'return))) ;; RET
              (jaunte-search key t)
            (setq key (concat key (char-to-string k)))
            (jaunte-search key))
          (if (= 1 (length jaunte--hints))
              (progn (jaunte-to (car jaunte--hints)) (jaunte-remove-hints)))))
    (jaunte-remove-hints)))

(provide 'jaunte)
;;; jaunte.el ends here