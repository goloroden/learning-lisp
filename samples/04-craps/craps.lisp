(defun throw-die ()
  (1+ (random 6)))

(defun throw-dice ()
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  (equal throw '(1 1)))

(defun boxcars-p (throw)
  (equal throw '(6 6)))

(defun throw-value (throw)
  (apply #'+ throw))

(defun instant-win-p (throw)
  (let ((sum (throw-value throw)))
    (or (equal sum 7)
        (equal sum 11))))

(defun instant-loss-p (throw)
  (let ((sum (throw-value throw)))
    (or (equal sum 2)
        (equal sum 3)
        (equal sum 12))))

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (throw-value throw))))

(defun craps ()
  (let ((throw (throw-dice)))
    (cond ((instant-win-p throw) (list 'throw (car throw) 'and (cadr throw) '-- (say-throw throw) '-- 'you 'win))
          ((instant-loss-p throw) (list 'throw (car throw) 'and (cadr throw) '-- (say-throw throw) '-- 'you 'lose))
          (t (list 'throw (car throw) 'and (cadr throw) '-- 'your 'point 'is (say-throw throw))))))

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (sum (throw-value throw)))
    (cond ((equal sum point) (list 'throw (car throw) 'and (cadr throw) '-- sum '-- 'you 'win))
          ((equal sum 7) (list 'throw (car throw) 'and (cadr throw) '-- sum '-- 'you 'lose))
          (t (list 'throw (car throw) 'and (cadr throw) '-- sum '-- 'throw 'again)))))
