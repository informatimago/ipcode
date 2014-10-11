(defun encode-string (string &key (start 0) (end (length string)))
  (map '(vector (unsigned-byte 8)) (function char-code)
    (if (and (zerop start) (= end (length string)))
        string
        (subseq string start end))))
(defun decode-string (octets &key (start 0) (end (length octets)))
  (map 'string (function code-char)
    (if (and (zerop start) (= end (length octets)))
        octets
        (subseq octets start end))))
