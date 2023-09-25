(define (read-integer prompt)
  (display prompt)
  (let ((input (read)))
    (if (integer? input)
       input
       (begin
         (display "Invalid input! Please enter a valid number.\n")
         (read-integer prompt)))))

(define (read-real prompt)
  (display prompt)
  (let ((input (read)))
    (if (real? input)
       input
       (begin
         (display "Invalid input! Please enter a valid number.\n")
         (read-real prompt)))))

(define (read-string prompt)
  (display prompt)
  (let ((input (read)))
    (if (string? input)
        input
        (begin
          (display "Invalid input! Please enter a valid word.\n")
          (read-string prompt)))))
          
(define (create-asset-info)
  (define symbol-input (read-string "Enter the stock symbol wrapped in quotation marks: "))

  (define sector-input (read-string "Enter the stock sector wrapped in quotation marks: "))

  (define number-of-stocks-input (read-integer "Enter the total number of stocks purchased in portfolio for this asset: "))

  (define purchased-price-input (read-real "Enter the price at which you purchased the stock: "))

  (define current-price-input (read-real "Enter the stock's current price: "))

  (define dividend-yield-input (read-real "Enter the dividend yield per stock: "))

  (define total-return (* (- current-price-input purchased-price-input) number-of-stocks-input))

  (define percent-return (* (/ total-return purchased-price-input) 100))

  (define estimated-annual-income (* (/ dividend-yield-input 100) number-of-stocks-input))

  (list symbol-input sector-input number-of-stocks-input purchased-price-input current-price-input dividend-yield-input total-return percent-return estimated-annual-income))

(define (create-portfolio portfolio)
  (display "Do you want to add an asset? (yes/no): ")
  (define answer (read))
  (if (equal? answer 'yes)
      (let ((new-asset (create-asset-info)))  ; Here we are calling create-asset-info to create a new asset
        (create-portfolio (cons new-asset portfolio)))  ; Here we add the new asset to the portfolio and recursively call create-portfolio
      portfolio))

(define (value-of-single-asset current-price number-of-stocks)
  (* current-price number-of-stocks))

(define (value-portfolio portfolio)
  (if (null? portfolio) ; base case: if the portfolio is empty
      0 ; return 0
      (+ (value-of-single-asset
          (caddr (car portfolio))
          (caddddr (car portfolio)))
         (value-portfolio (cdr portfolio))))) ; to the total value of the rest of the portfolio

(define (portfolio-weight value-of-single-asset value-portfolio)
  (/ value-of-single-asset value-portfolio))

(define (compute-and-add-weight portfolio)
  (define total-value (value-portfolio portfolio))
  (map (lambda (asset)
         (let ((asset-value (value-of-single-asset (caddr asset) (caddddr asset))))
           (let ((weight (/ asset-value total-value)))
             (append asset (list weight)))))
       portfolio))

; Updating the portfolio with the weights of each asset
(define my-portfolio (create-portfolio '()))
(define updated-portfolio (compute-and-add-weight my-portfolio))

;;displaying the portfolio
(define (display-asset asset)
  (display "Symbol: ") (display (car asset)) (newline)
  (display "Sector: ") (display (cadr asset)) (newline)
  (display "Number of Stocks: ") (display (caddr asset)) (newline)
  (display "Purchased Price: ") (display (cadddr asset)) (newline)
  (display "Current Price: ") (display (cadddddr asset)) (newline)
  (display "Dividend Yield: ") (display (caddddddr asset)) (newline)
  (display "Total Return: ") (display (cadddddddr asset)) (newline)
  (display "Percent Return: ") (display (caddddddddr asset)) (newline)
  (display "Estimated Annual Income: ") (display (cadddddddddr asset)) (newline)
  (display "Weight in Portfolio: ") (display (caddddddddddr asset)) (newline)
  (display "---------------------------------------") (newline))

(define (display-portfolio portfolio)
  (if (null? portfolio)
      (display "Portfolio is empty.")
      (begin
        (display-asset (car portfolio))
        (display-portfolio (cdr portfolio)))))

(display-portfolio my-portfolio)

