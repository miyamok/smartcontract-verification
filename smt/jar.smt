;(set-option :produce-models true)
(set-option :produce-proofs true)
(set-logic HORN)

(define-sort A () (_ BitVec 256)) ;address
(define-sort BUINT () (_ BitVec 256)) ; bounded 256bit unsigned integer
(define-sort M () (Array A BUINT)) ;mapping from address to Int
(declare-fun P_alpha (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun P_omega (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun Q_alpha (M A BUINT BUINT M A BUINT BUINT Int) Bool)
;;(declare-fun Q_0 (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun Q_1 (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun Q_2 (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun Q_3 (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun Q_omega (M A BUINT BUINT M A BUINT BUINT Int) Bool)
(declare-fun S (M BUINT A BUINT M BUINT Int) Bool)
(declare-fun T (M BUINT A BUINT M BUINT Int) Bool)
(declare-fun Ext (M BUINT M BUINT) Bool)
(declare-fun Jar (M BUINT) Bool)
(declare-fun Init (M BUINT) Bool)

(define-fun buint0 () BUINT
	    #x0000000000000000000000000000000000000000000000000000000000000000)

;; deposit()
(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT))
	 (=> (and (= b l_b) (= s l_s) (= v l_v) (bvule buint0 v) (= l_r 0)
		  (= l_tb (bvadd tb l_v)) (bvule tb l_tb) (bvule l_v l_tb))
	     (P_alpha b s v tb l_b l_s l_v l_tb l_r))))

(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT) (l_b^ M))
	 (=> (and (P_alpha b s v tb l_b l_s l_v l_tb l_r)
		  (= l_b^ (store l_b l_s (bvadd (select l_b l_s) l_v)))
		  (bvule (select l_b l_s) (select l_b^ l_s))
		  (bvule l_v (select l_b^ l_s)))
	     (P_omega b s v tb l_b^ l_s l_v l_tb l_r))))

;; Summary deposit()
(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT) (b^ M) (tb^ BUINT) (r Int))
	 (=> (and (P_omega b s v tb l_b l_s l_v l_tb l_r)
		  (=> (not (= l_r 0)) (and (= b^ b) (= tb^ tb)))
		  (=> (= l_r 0) (and (= b^ l_b) (= tb^ l_tb)))
		  (= r l_r))
	     (S b tb s v b^ tb^ r))))

;; withdraw()
(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT))
	 (=> (and (= b l_b) (= s l_s) (= v l_v) (= l_r 0) (= l_tb tb))
	     (Q_alpha b s v tb l_b l_s l_v l_tb l_r))))

(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT))
	 (=> (and (Q_alpha b s v tb l_b l_s l_v l_tb l_r)
		  (not (= (select l_b l_s) buint0)))
	     (Q_1 b s v tb l_b l_s l_v l_tb l_r))))

(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT) (l_tb^ BUINT))
	 (=> (and (Q_1 b s v tb l_b l_s l_v l_tb l_r)
		  (= l_tb^ (bvsub l_tb (select l_b l_s)))
		  (bvule (select l_b l_s) l_tb))
	     (Q_2 b s v tb l_b l_s l_v l_tb^ l_r))))

(assert
 (forall ((b M) (l_b M) (l_b^ M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT) (l_tb^ BUINT) (b^ M) (tb^ BUINT) (b^^ M) (tb^^ BUINT))
	 (=> (and (Q_2 b s v tb l_b l_s l_v l_tb l_r)
		  (and (Ext b^ tb^ b^^ tb^^)
		       (= b^ l_b) (= b^^ l_b^)
		       (= tb^ l_tb) (= tb^^ l_tb^)))
	     (Q_3 b s v tb l_b^ l_s l_v l_tb^ l_r))))

(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT) (l_b^ M))
	 (=> (and (Q_3 b s v tb l_b l_s l_v l_tb l_r)
		  (= l_b^ (store l_b l_s buint0)))
	     (Q_omega b s v tb l_b^ l_s l_v l_tb l_r))))

;; Summary withdraw()
(assert
 (forall ((b M) (l_b M) (s A) (l_s A) (v BUINT) (l_v BUINT) (l_r Int)
	  (tb BUINT) (l_tb BUINT) (b^ M) (tb^ BUINT) (r Int))
	 (=> (and (Q_omega b s v tb l_b l_s l_v l_tb l_r)
		  (=> (not (= l_r 0)) (and (= b^ b) (= tb^ tb)))
		  (=> (= l_r 0) (and (= b^ l_b) (= tb^ l_tb)))
		  (= r l_r))
	     (T b tb s v b^ tb^ r))))

;; Init
(assert (forall ((b M) (tb BUINT)) (Init b tb)))

(assert (forall ((b M) (tb BUINT)) (=> (Init b tb) (Jar b tb))))

;; Jar
(assert
 (forall ((b M) (s A) (v BUINT) (tb BUINT) (b^ M) (tb^ BUINT) (r Int))
	 (=> (and (Jar b tb)
		  (T b tb s v b^ tb^ r)
		  (= r 0))
	     (Jar b^ tb^))))

(assert
 (forall ((b M) (s A) (v BUINT) (tb BUINT) (b^ M) (tb^ BUINT) (r Int))
	 (=> (and (Jar b tb)
		  (S b tb s v b^ tb^ r)
		  (= r 0))
	     (Jar b^ tb^))))

;; Ext
(assert (forall ((b M) (tb BUINT)) (Ext b tb b tb)))

(assert (forall ((b M) (s A) (v BUINT) (r Int)
		 (tb BUINT) (b^ M) (tb^ BUINT) (b^^ M) (tb^^ BUINT))
		(=> (and (Ext b tb b^ tb^)
			 (S b^ tb^ s v b^^ tb^^ r)
			 (= r 0))
		    (Ext b tb b^^ tb^^))))
			 
(assert (forall ((b M) (s A) (v BUINT) (r Int)
		 (tb BUINT) (b^ M) (tb^ BUINT) (b^^ M) (tb^^ BUINT))
		(=> (and (Ext b tb b^ tb^)
			 (T b^ tb^ s v b^^ tb^^ r)
			 (= r 0))
		    (Ext b tb b^^ tb^^))))

;; Safety property
;;
;; (assert
;;   (forall ((b M) (tb BUINT) (s A) (v BUINT)
;;  	   (b^ M) (tb^ BUINT) (r^ Int))
;;   	  (not (and (Jar b tb)
;; 		    (T b tb s v b^ tb^ r^)
;; 		    (not (= r^ 0))))))

;; (check-sat)
;; z3> sat

;; A determinacy property in a Horn clause
;; (assert
;;  (forall ((b M) (tb BUINT) (s A) (v BUINT)
;;  	  (b^ M) (tb^ BUINT) (r^ Int) (r_^ Int)
;; 	  (b_^ M) (tb_^ BUINT))
;;   	  (not (and (Jar b tb)
;; 		    (T b tb s v b^ tb^ r^)
;; 		    (T b tb s v b_^ tb_^ r_^)
;; 		    (= r^ r_^)
;; 		    (not (and (= b^ b_^) (= tb^ tb_^)))))))
 
(check-sat)
;; z3> unsat

(get-proof)
;; (set-option :produce-models true)
;; (set-option :produce-proofs true)
;; (set-logic HORN)

;; (define-sort A () (_ BitVec 256)) ;address
;; (define-sort M () (Array A Int)) ;mapping from address to Int
;; (declare-fun P_alpha (M A Int Int M A Int Int Int) Bool)
;; (declare-fun P_omega (M A Int Int M A Int Int Int) Bool)
;; (declare-fun Q_alpha (M A Int Int M A Int Int Int) Bool)
;; (declare-fun Q_1 (M A Int Int M A Int Int Int) Bool)
;; (declare-fun Q_2 (M A Int Int M A Int Int Int) Bool)
;; (declare-fun Q_3 (M A Int Int M A Int Int Int) Bool)
;; (declare-fun Q_omega (M A Int Int M A Int Int Int) Bool)
;; (declare-fun S (M Int A Int M Int Int) Bool)
;; (declare-fun T (M Int A Int M Int Int) Bool)
;; (declare-fun Ext (M Int M Int) Bool)
;; (declare-fun Jar (M Int) Bool)
;; (declare-fun Init (M Int) Bool)

;; ;; deposit()
;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int))
;; 	 (=> (and (= b l_b) (= s l_s) (= v l_v) (<= 0 v) (= l_r 0) (= l_tb (+ tb l_v)))
;; 	     (P_alpha b s v tb l_b l_s l_v l_tb l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int) (l_b^ M))
;; 	 (=> (and (P_alpha b s v tb l_b l_s l_v l_tb l_r)
;; 		  (= l_b^ (store l_b l_s (+ (select l_b l_s) l_v))))
;; 	     (P_omega b s v tb l_b^ l_s l_v l_tb l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int) (b^ M) (tb^ Int) (r Int))
;; 	 (=> (and (P_omega b s v tb l_b l_s l_v l_tb l_r)
;; 		  (=> (not (= l_r 0)) (and (= b^ b) (= tb^ l_tb)))
;; 		  (=> (= l_r 0) (and (= b^ l_b) (= tb^ l_tb)))
;; 		  (= r l_r))
;; 	     (S b tb s v b^ tb^ r))))

;; ;; withdraw()
;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int))
;; 	 (=> (and (= b l_b) (= s l_s) (= v l_v) (= l_r 0) (= l_tb tb))
;; 	     (Q_alpha b s v tb l_b l_s l_v l_tb l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int))
;; 	 (=> (and (Q_alpha b s v tb l_b l_s l_v l_tb l_r)
;; 		  (not (= (select l_b l_s) 0)))
;; 	     (Q_1 b s v tb l_b l_s l_v l_tb l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int) (l_tb^ Int))
;; 	 (=> (and (Q_1 b s v tb l_b l_s l_v l_tb l_r)
;; 		  (= l_tb^ (- l_tb (select l_b l_s))))
;; 	     (Q_2 b s v tb l_b l_s l_v l_tb^ l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (l_b^ M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int) (l_tb^ Int) (b^ M) (tb^ Int) (b^^ M) (tb^^ Int))
;; 	 (=> (and (Q_2 b s v tb l_b l_s l_v l_tb l_r)
;; 		  (and (Ext b^ tb^ b^^ tb^^)
;; 		       (= b^ l_b) (= b^^ l_b^)
;; 		       (= tb^ l_tb) (= tb^^ l_tb^)))
;; 	     (Q_3 b s v tb l_b^ l_s l_v l_tb^ l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int) (l_b^ M))
;; 	 (=> (and (Q_3 b s v tb l_b l_s l_v l_tb l_r)
;; 		  (= l_b^ (store l_b l_s 0)))
;; 	     (Q_omega b s v tb l_b^ l_s l_v l_tb l_r))))

;; (assert
;;  (forall ((b M) (l_b M) (s A) (l_s A) (v Int) (l_v Int) (l_r Int)
;; 	  (tb Int) (l_tb Int) (b^ M) (tb^ Int) (r Int))
;; 	 (=> (and (Q_omega b s v tb l_b l_s l_v l_tb l_r)
;; 		  (=> (not (= l_r 0)) (and (= b^ b) (= tb^ l_tb)))
;; 		  (=> (= l_r 0) (and (= b^ l_b) (= tb^ l_tb)))
;; 		  (= r l_r))
;; 	     (T b tb s v b^ tb^ r))))

;; (assert (forall ((b M) (tb Int))
;; 		(=> (and (forall ((i A)) (>= (select b i) 0))
;; 			 (>= tb 0))
;; 		    (Init b tb))))

;; (assert (forall ((b M) (tb Int))
;; 		(=> (Init b tb) (Jar b tb))))

;; (assert
;;  (forall ((b M) (s A) (v Int) (tb Int) (b^ M) (tb^ Int) (r Int))
;; 	 (=> (and (Jar b tb)
;; 		  (T b tb s v b^ tb^ r)
;; 		  (= r 0))
;; 	     (Jar b^ tb^))))

;; (assert
;;  (forall ((b M) (s A) (v Int) (tb Int) (b^ M) (tb^ Int) (r Int))
;; 	 (=> (and (Jar b tb)
;; 		  (S b tb s v b^ tb^ r)
;; 		  (= r 0))
;; 	     (Jar b^ tb^))))

;; (assert (forall ((b M) (tb Int)) (Ext b tb b tb)))

;; (assert (forall ((b M) (s A) (v Int) (r Int)
;; 		 (tb Int) (b^ M) (tb^ Int) (b^^ M) (tb^^ Int))
;; 		(=> (and (Ext b tb b^ tb^)
;; 			 (S b^ tb^ s v b^^ tb^^ r)
;; 			 (= r 0))
;; 		    (Ext b tb b^^ tb^^))))
			 
;; (assert (forall ((b M) (s A) (v Int) (r Int)
;; 		 (tb Int) (b^ M) (tb^ Int) (b^^ M) (tb^^ Int))
;; 		(=> (and (Ext b tb b^ tb^)
;; 			 (T b^ tb^ s v b^^ tb^^ r)
;; 			 (= r 0))
;; 		    (Ext b tb b^^ tb^^))))

;; ;; A determinacy property in a Horn clause
;; (assert
;;  (forall ((b M) (tb Int) (s A) (v Int)
;;  	  (b^ M) (tb^ Int) (r^ Int) (r_^ Int)
;; 	  (b_^ M) (tb_^ Int))
;;   	 (not (and (forall ((i A)) (>= (select b i) 0))
;; 		   (>= tb 0)
;; 		   (Jar b tb)
;; 		   (T b tb s v b^ tb^ 0)
;; 		   (T b tb s v b_^ tb_^ 0)
;; 		   (not (= tb^ tb_^))))))

;; ;; (assert
;; ;;  (forall ((b M) (tb Int) (s A) (v Int)
;; ;;  	  (b^ M) (tb^ Int) (r^ Int) (r_^ Int)
;; ;; 	  (b_^ M) (tb_^ Int))
;; ;;   	 (not (and (Jar b tb)
;; ;; 		   (Init b tb)
;; ;; 		   (T b tb s v b^ tb^ 0)
;; ;; 		   (T b tb s v b_^ tb_^ 0)
;; ;; 		   (not (and (= tb^ tb_^)))))))

;; ;; (>= tb tb^) (>= tb tb_^)

;; ;; (assert
;; ;;  (forall ((b M) (tb Int) (s A) (v Int)
;; ;;  	  (b^ M) (tb^ Int) (r^ Int) (r_^ Int)
;; ;; 	  (b_^ M) (tb_^ Int))
;; ;;   	 (not (and (Jar b tb)
;; ;; 		   (Init b tb)
;; ;; 		   (T b tb s v b^ tb^ 0)
;; ;; 		   (T b tb s v b_^ tb_^ 0)
;; ;; 		   (not (and (= b^ b_^) (= tb^ tb_^)))))))

;; (check-sat)
;; (get-info :reason-unknown)
;; ;(get-proof)
