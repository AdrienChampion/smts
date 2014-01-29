(set-logic AUFNIRA)
(set-info :source |
NASA benchmarks from "Using Automated Theorem Provers to Certify Auto-generated
Aerospace Software", IJCAR 2004.  Translated from TPTP format by Yeting Ge and
Clark Barrett

|)
(set-info :smt-lib-version 2.0)
(set-info :category "industrial")
(set-info :status unknown)
(declare-fun center () (Array Int (Array Int Real)))
(declare-fun def () Real)
(declare-fun pv10 () Int)
(declare-fun pv12 () Int)
(declare-fun pv70 () Real)
(declare-fun pv71 () Int)
(declare-fun q () (Array Int (Array Int Real)))
(declare-fun use () Real)
(declare-fun x () (Array Int Real))
(declare-fun uniform_int_rnd (Int Int) Int)
(declare-fun abs_ (Real) Real)
(declare-fun log (Real) Real)
(declare-fun exp (Real) Real)
(declare-fun cos (Real) Real)
(declare-fun sin (Real) Real)
(declare-fun sqrt (Real) Real)
(declare-fun divide (Real Real) Real)
(declare-fun cond (Int Real Real) Real)
(declare-fun tptp_term_equal (Real Real) Int)
(declare-fun tptp_term_equals (Real Real) Int)
(declare-fun tptp_term_and (Real Real) Int)
(declare-fun sum (Int Int Real) Real)
(declare-fun dim (Int Int) Int)
(declare-fun trans ((Array Int (Array Int Real))) (Array Int (Array Int Real)))
(declare-fun inv ((Array Int (Array Int Real))) (Array Int (Array Int Real)))
(declare-fun tptp_mmul ((Array Int (Array Int Real)) (Array Int (Array Int Real))) (Array Int (Array Int Real)))
(declare-fun tptp_madd ((Array Int (Array Int Real)) (Array Int (Array Int Real))) (Array Int (Array Int Real)))
(declare-fun tptp_msub ((Array Int (Array Int Real)) (Array Int (Array Int Real))) (Array Int (Array Int Real)))
(declare-fun tptp_const_array1 (Int Real) (Array Int Real))
(declare-fun tptp_const_array2 (Int Int Real) (Array Int (Array Int Real)))
(assert (forall ((?X_0 Int) (?C_1 Int)) (=> (>= ?X_0 0) (<= (uniform_int_rnd ?C_1 ?X_0) ?X_0))))
(assert (forall ((?X_2 Int) (?C_3 Int)) (=> (>= ?X_2 0) (>= (uniform_int_rnd ?C_3 ?X_2) 0))))
(assert (forall ((?I_4 Int) (?L_5 Int) (?U_6 Int) (?Val_7 Real)) (=> (and (<= ?L_5 ?I_4) (<= ?I_4 ?U_6)) (= (select (tptp_const_array1 (dim ?L_5 ?U_6) ?Val_7) ?I_4) ?Val_7))))
(assert (forall ((?I_8 Int) (?L1_9 Int) (?U1_10 Int) (?J_11 Int) (?L2_12 Int) (?U2_13 Int) (?Val_14 Real)) (=> (and (and (and (<= ?L1_9 ?I_8) (<= ?I_8 ?U1_10)) (<= ?L2_12 ?J_11)) (<= ?J_11 ?U2_13)) (= (select (select (tptp_const_array2 (dim ?L1_9 ?U1_10) (dim ?L2_12 ?U2_13) ?Val_14) ?I_8) ?J_11) ?Val_14))))
(assert (forall ((?I0_15 Int) (?J0_16 Int) (?A_17 (Array Int (Array Int Real))) (?B_18 (Array Int (Array Int Real))) (?N_19 Int)) (let ((?v_0 (tptp_mmul ?A_17 (tptp_mmul ?B_18 (trans ?A_17))))) (=> (and (and (and (and (>= ?I0_15 0) (<= ?I0_15 ?N_19)) (>= ?J0_16 0)) (<= ?J0_16 ?N_19)) (= (select (select ?B_18 ?I0_15) ?J0_16) (select (select ?B_18 ?J0_16) ?I0_15))) (= (select (select ?v_0 ?I0_15) ?J0_16) (select (select ?v_0 ?J0_16) ?I0_15))))))
(assert (forall ((?I0_20 Int) (?J0_21 Int) (?I_22 Int) (?J_23 Int) (?A_24 (Array Int (Array Int Real))) (?B_25 (Array Int (Array Int Real))) (?N_26 Int) (?M_27 Int)) (let ((?v_0 (tptp_mmul ?A_24 (tptp_mmul ?B_25 (trans ?A_24))))) (=> (and (and (and (and (and (and (and (and (>= ?I0_20 0) (<= ?I0_20 ?N_26)) (>= ?J0_21 0)) (<= ?J0_21 ?N_26)) (>= ?I_22 0)) (<= ?I_22 ?M_27)) (>= ?J_23 0)) (<= ?J_23 ?M_27)) (= (select (select ?B_25 ?I_22) ?J_23) (select (select ?B_25 ?J_23) ?I_22))) (= (select (select ?v_0 ?I0_20) ?J0_21) (select (select ?v_0 ?J0_21) ?I0_20))))))
(assert (forall ((?I_28 Int) (?J_29 Int) (?A_30 (Array Int (Array Int Real))) (?B_31 (Array Int (Array Int Real))) (?N_32 Int)) (let ((?v_0 (tptp_madd ?A_30 ?B_31))) (=> (and (and (and (and (and (>= ?I_28 0) (<= ?I_28 ?N_32)) (>= ?J_29 0)) (<= ?J_29 ?N_32)) (= (select (select ?A_30 ?I_28) ?J_29) (select (select ?A_30 ?J_29) ?I_28))) (= (select (select ?B_31 ?I_28) ?J_29) (select (select ?B_31 ?J_29) ?I_28))) (= (select (select ?v_0 ?I_28) ?J_29) (select (select ?v_0 ?J_29) ?I_28))))))
(assert (forall ((?I_33 Int) (?J_34 Int) (?A_35 (Array Int (Array Int Real))) (?B_36 (Array Int (Array Int Real))) (?N_37 Int)) (let ((?v_0 (tptp_msub ?A_35 ?B_36))) (=> (and (and (and (and (and (>= ?I_33 0) (<= ?I_33 ?N_37)) (>= ?J_34 0)) (<= ?J_34 ?N_37)) (= (select (select ?A_35 ?I_33) ?J_34) (select (select ?A_35 ?J_34) ?I_33))) (= (select (select ?B_36 ?I_33) ?J_34) (select (select ?B_36 ?J_34) ?I_33))) (= (select (select ?v_0 ?I_33) ?J_34) (select (select ?v_0 ?J_34) ?I_33))))))
(assert (forall ((?I_38 Int) (?J_39 Int) (?A_40 (Array Int (Array Int Real))) (?N_41 Int)) (let ((?v_0 (trans ?A_40))) (=> (and (and (and (and (>= ?I_38 0) (<= ?I_38 ?N_41)) (>= ?J_39 0)) (<= ?J_39 ?N_41)) (= (select (select ?A_40 ?I_38) ?J_39) (select (select ?A_40 ?J_39) ?I_38))) (= (select (select ?v_0 ?I_38) ?J_39) (select (select ?v_0 ?J_39) ?I_38))))))
(assert (forall ((?I_42 Int) (?J_43 Int) (?A_44 (Array Int (Array Int Real))) (?N_45 Int)) (let ((?v_0 (inv ?A_44))) (=> (and (and (and (and (>= ?I_42 0) (<= ?I_42 ?N_45)) (>= ?J_43 0)) (<= ?J_43 ?N_45)) (= (select (select ?A_44 ?I_42) ?J_43) (select (select ?A_44 ?J_43) ?I_42))) (= (select (select ?v_0 ?I_42) ?J_43) (select (select ?v_0 ?J_43) ?I_42))))))
(assert (forall ((?I0_46 Int) (?J0_47 Int) (?I_48 Int) (?J_49 Int) (?A_50 (Array Int (Array Int Real))) (?B_51 (Array Int (Array Int Real))) (?C_52 (Array Int (Array Int Real))) (?D_53 (Array Int (Array Int Real))) (?E_54 (Array Int (Array Int Real))) (?F_55 (Array Int (Array Int Real))) (?N_56 Int) (?M_57 Int)) (let ((?v_0 (tptp_madd ?A_50 (tptp_mmul ?B_51 (tptp_mmul (tptp_madd (tptp_mmul ?C_52 (tptp_mmul ?D_53 (trans ?C_52))) (tptp_mmul ?E_54 (tptp_mmul ?F_55 (trans ?E_54)))) (trans ?B_51)))))) (=> (and (and (and (and (and (and (and (and (and (and (>= ?I0_46 0) (<= ?I0_46 ?N_56)) (>= ?J0_47 0)) (<= ?J0_47 ?N_56)) (>= ?I_48 0)) (<= ?I_48 ?M_57)) (>= ?J_49 0)) (<= ?J_49 ?M_57)) (= (select (select ?D_53 ?I_48) ?J_49) (select (select ?D_53 ?J_49) ?I_48))) (= (select (select ?A_50 ?I0_46) ?J0_47) (select (select ?A_50 ?J0_47) ?I0_46))) (= (select (select ?F_55 ?I0_46) ?J0_47) (select (select ?F_55 ?J0_47) ?I0_46))) (= (select (select ?v_0 ?I0_46) ?J0_47) (select (select ?v_0 ?J0_47) ?I0_46))))))
(assert (forall ((?Body_58 Real)) (= (sum 0 (- 1) ?Body_58) 0.0)))
(assert (not (= def use)))
(assert (let ((?v_2 (>= pv10 0)) (?v_3 (<= pv10 (- 135300 1))) (?v_1 (- 5 1)) (?v_0 (- (select (select center pv71) 0) (select x pv10)))) (not (=> (and (and (and (and (and (and (= pv70 (sum 0 ?v_1 (sqrt (* ?v_0 ?v_0)))) ?v_2) (>= pv12 0)) ?v_3) (<= pv12 ?v_1)) (forall ((?A_59 Int) (?B_60 Int)) (let ((?v_5 (select x pv10))) (let ((?v_4 (- (select (select center ?A_59) 0) ?v_5)) (?v_6 (- (select (select center ?B_60) 0) ?v_5))) (=> (and (>= ?A_59 0) (<= ?A_59 (- pv12 1))) (= (select (select q pv10) ?A_59) (divide (sqrt (* ?v_4 ?v_4)) (sum 0 ?v_1 (sqrt (* ?v_6 ?v_6)))))))))) (forall ((?C_61 Int) (?D_62 Int)) (=> (and (>= ?C_61 0) (<= ?C_61 (- pv10 1))) (= (sum 0 ?v_1 (select (select q ?C_61) ?D_62)) 1.0)))) (and (and (and ?v_2 ?v_3) (forall ((?E_63 Int) (?F_64 Int)) (let ((?v_8 (select x pv10))) (let ((?v_7 (- (select (select center pv12) 0) ?v_8)) (?v_9 (- (select (select center ?E_63) 0) ?v_8)) (?v_10 (- (select (select center ?F_64) 0) ?v_8))) (=> (and (>= ?E_63 0) (<= ?E_63 (- (+ 1 pv12) 1))) (= (select (select (store q pv10 (store (select q pv10) pv12 (divide (sqrt (* ?v_7 ?v_7)) pv70))) pv10) ?E_63) (divide (sqrt (* ?v_9 ?v_9)) (sum 0 ?v_1 (sqrt (* ?v_10 ?v_10)))))))))) (forall ((?G_65 Int) (?H_66 Int)) (let ((?v_11 (- (select (select center pv12) 0) (select x pv10)))) (=> (and (>= ?G_65 0) (<= ?G_65 (- pv10 1))) (= (sum 0 ?v_1 (select (select (store q pv10 (store (select q pv10) pv12 (divide (sqrt (* ?v_11 ?v_11)) pv70))) ?G_65) ?H_66)) 1.0)))))))))
(check-sat)
(exit)
