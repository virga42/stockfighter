
(load "~/bottega/stockfighter/key.lisp")
(load "~/bottega/unit_test_framework/unit_test_framework.lisp")

(defvar first-time-loadp t)
(defvar *debug* nil)
(defvar base-url "https://api.stockfighter.io/ob/api")
(defvar persistent-key (return-key "~/bottega/stockfighter/api-key.txt"))

(if first-time-loadp
	(progn
		(cl-interpol:ENABLE-INTERPOL-SYNTAX)
		(setf first-time-loadp nil)))

(defun api-get (request)
	(if *debug* (print request))
	(cl-json:decode-json-from-source (drakma:http-request request
		:additional-headers (list (cons "X-Starfighter-Authorization"  persistent-key))
		:method :get
		:want-stream t)))

(defun api-post (url content)
	(cl-json:decode-json-from-source 
		(drakma:http-request url
			:additional-headers (list (cons "X-Starfighter-Authorization"  persistent-key))
			:method :post
			:content content
			:want-stream t)))

(defun api-delete (url content)
	(if *debug* (print url))
		(cl-json:decode-json-from-source 
			(drakma:http-request url
				:additional-headers (list (cons "X-Starfighter-Authorization"  persistent-key))
				:method :delete
				:content content
				:want-stream t)))

(defun get-heartbeat ()
	(let ((url #?"${base-url}/heartbeat"))
	(api-get url)))

(defun first-level ()
	(let ((url "https://api.stockfighter.io/gm/levels/first_steps"))
	(api-post url nil)))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defmacro prepare-response (transaction &rest my-items)
  `(progn
  	,@(loop for i in my-items collect `(setf ,i (cdr (assoc (make-keyword ',i) ,transaction))))))

(defparameter account "")
(defparameter tickers '("FOOBAR"))
(defparameter venues '("TESTEX"))
(defparameter *resp* '())

(defun process-response ()
	(prepare-response *resp* account tickers venues))

(defun check-venue-up ()
	(let* ((venue (car venues))
			  (url #?"${base-url}/venues/${venue}/heartbeat"))
	(api-get url)))

(defun get-stocks-on-venue ()
	(let* ((venue (car venues))
			  (url #?"${base-url}/venues/${venue}/stocks"))
		(api-get url)))

(defun get-order-book-for-stock (&optional (stock "FOOBAR"))
	(let* ((venue (car venues))
				 (url #?"${base-url}/venues/${venue}/stocks/${stock}"))
		(api-get url)))

(defun get-cheapest-price (order-book)
	(let ((bids-list (cdr (assoc :BIDS order-book)))
				 (cheapest-bid '())
				 (cheapest-price 0))
		(loop for bid in bids-list do (when (or (= cheapest-price 0) 
																				(< (cdr (assoc :PRICE bid)) cheapest-price)) 
																					(progn 
																						(setf cheapest-price (cdr (assoc :PRICE bid)))
																						(setf cheapest-bid (list (cdr (assoc :PRICE bid))
																																		 (cdr (assoc :QTY bid)))))))
		cheapest-bid))

; (defun get-response-stats (response)
	

(defun place-order (stock-sym price qty dir order-type)
	(let* ((venue (car venues))
				 (order (cl-json:encode-json-to-string (list 
																			`("account" . ,account)
																			`("venue" . ,venue)
																			`("stock" . ,stock-sym)
																			`("qty" . ,qty)
																			`("price" . ,price)
																			`("direction" . ,dir)
																			`("orderType" . ,order-type))))
			(url #?"${base-url}/venues/${venue}/stocks/${stock-sym}/orders"))
		(api-post url order)))

(defun verify-get-heartbeat ()
	(eq t (cdr (assoc :OK (get-heartbeat)))))

(defun verify-venue-up ()
	(eq t (cdr (assoc :OK (check-venue-up)))))

(defun verify-stocks-on-venue ()
	(eq t (cdr (assoc :OK (get-stocks-on-venue)))))

(defun verify-order-book ()
	(eq t (cdr (assoc :OK (get-order-book-for-stock)))))
	
(defun perform-tests ()
	(check
		(verify-get-heartbeat)
		(verify-venue-up)
		(verify-stocks-on-venue)
		(verify-order-book)))

(perform-tests)

(setf *resp* (first-level))
(process-response)

(defparameter response '())

(defun place-an-order ()
	(let* ((stock (car tickers)) 
				 (order-book (get-order-book-for-stock stock))
				 (best-bid (get-cheapest-price order-book)) 
				 (price (+ 100 (car best-bid))) 
				 (qty 100)
				 (response '())) 
		(setf response (place-order stock price qty "buy" "market"))
		response))

(setf response (place-an-order))