(ql:quickload "drakma")
(ql:quickload "cl-json")
(ql:quickload "cl-interpol")

(defun return-key (a-file)
	(let* ((f a-file) (in (open f)) (k))
		(setf k (read-line in))
		(close in)
		k))
