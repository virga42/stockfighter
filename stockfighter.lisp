
(load "~/bottega/stockfighter/key.lisp")

(defvar *base-url* "https://api.stockfighter.io/ob/api")
(defvar *api-key* (return-key "~/bottega/stockfighter/api-key.txt"))

(setf *venue* "HOPBEX")
(setf *stock* "FKUI")
(setf *account* "SAH13759300") 

(defvar *req* '(("account" . *account*) ("venue" . *venue*) ("symbol" . *stock*) ("price" . 2500) ("qty" . 100) ("direction" . "buy") ("orderType" . "limit")))

; (defun make-trade (request)
;   (let* ((extra-headers (list (cons "X-Starfighter-Authorization" *api-key*)))
;         (url (concatenate 'string *base-url* "/venues/" *venue* "/stocks/" *stock* "/orders"))
;         (stream (drakma:http-request url
;           :additional-headers extra-headers
;           :accept "application/json"
;           :method :post
;           :content-type "application/json"
;           :external-format-out :utf-8
;           :external-format-in :utf-8
;           :content (json:encode-json-to-string request)
;           :want-stream t)))
;   (st-json:read-json stream)))

; (defun get-quote ()
  ; (let* ((extra-headers (list (cons "X-Starfighter-Authorization" *api-key*)))
        ; (url (concatenate 'string *base-url* "/venues/" *venue* "/stocks/" *stock* "/quote"))
        ; (stream (drakma:http-request url
                                    ; :additional-headers extra-headers
                                    ; :want-stream t)))
  ; (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
  ; (yason:parse stream :object-as :alist)))
; 
; (defun get-bid-from-quote (a-quote)
  ; (cdr (assoc "bid" a-quote :test #'string=)))

;(setf my-quote (get-quote))

;(get-bid-from-quote my-quote)

;(get-quote)