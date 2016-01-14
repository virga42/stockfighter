
(load "~/bottega/stockfighter/key.lisp")
(load "~/bottega/unit_test_framework/unit_test_framework.lisp")

(defparameter first-time-loadp t)
(defparameter *debug* nil)
(defparameter base-url "https://api.stockfighter.io/ob/api")
(defparameter persistent-key (return-key "~/bottega/stockfighter/api-key.txt"))

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

(defun second-level ()
	(let ((url "https://www.stockfighter.io/ui/play/blotter#chock_a_block"))
		(api-post url nil)))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun get-value-by-key (attribute response)
	(cdr (assoc attribute response)))

(defmacro with-response (response attributes &body body)
  `(let ,(loop for attribute in attributes collect `(,attribute (get-value-by-key (make-keyword ',attribute) ,response)))
    ,@body))

(defun get-order-book (session) 
	(with-response session 
								 (venues tickers account) 
								 (GET-ORDER-BOOK-FOR-STOCK (car venues) 
								 													 (car tickers))))

(defun get-bids-from-order-book (order-book)
	(with-response order-book
								 (bids)
								 bids))

(defun check-venue-up ()
	(let* ((venue (car venues))
			  (url #?"${base-url}/venues/${venue}/heartbeat"))
	(api-get url)))

(defun get-stocks-on-venue ()
	(let* ((venue (car venues))
			  (url #?"${base-url}/venues/${venue}/stocks"))
		(api-get url)))

(defun get-order-book-for-stock (venue &optional (stock "FOOBAR"))
	(let ((url #?"${base-url}/venues/${venue}/stocks/${stock}"))
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

; (perform-tests)

; (setf session (first-level))

; (setf order (with-response session (account venues tickers) (cl-json:encode-json-to-string (list 
; 								(list "account" account)
; 								(list "venue" (car venues))
; 								(list "stock" (car tickers))
; 								(list "qty" 100)
; 								(list "price" 2000)
; 								(list "direction" "buy")
; 								(list "orderType" "fill-or-kill")))))

; '(XYZ (((name "Toilet Paper Industries") (price 2000) (timestamp 2016-01-12))))

(defun make-stock (stock-symbol &key (name "Anonymous Stock") (initial-price '(0 nil)))
	(list stock-symbol name (list initial-price)))

(defun make-stock-price (price &key timestamp)
		(list price timestamp))

(defun stock-symbol (stock)
	(first stock))

(defun stock-name (stock)
	(second stock))

(defun stock-prices (stock)
	(third stock))

(defun append-price (stock price)
	(list (stock-symbol stock) (stock-name stock) (append (list price) (stock-prices stock))))

(defun verify-stock-creation ()
	(let ((test-stock (make-stock 'YYY :name "Test Stock" :initial-price (make-stock-price 999 :timestamp 1))))
		(print test-stock)
		(setf test-stock (append-price test-stock (make-stock-price 777 :timestamp 2)))
		(print test-stock)))



; (make-stock 'zyx :name "Toilet Paper Industries" :snapshot (make-snapshot '(price 2000) '(timestamp 2016-01-12)))



