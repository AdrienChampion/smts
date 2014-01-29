(set-logic AUFNIRA)
(set-info :source |
NASA benchmarks from "Using Automated Theorem Provers to Certify Auto-generated
Aerospace Software", IJCAR 2004.  Translated from TPTP format by Yeting Ge and
Clark Barrett

|)
(set-info :smt-lib-version 2.0)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun def () Real)
(declare-fun h_ds1_filter () (Array Int (Array Int Real)))
(declare-fun id_ds1_filter () (Array Int (Array Int Real)))
(declare-fun phi_ds1_filter () (Array Int (Array Int Real)))
(declare-fun pminus_ds1_filter () (Array Int (Array Int Real)))
(declare-fun pv5 () Int)
(declare-fun q_ds1_filter () (Array Int (Array Int Real)))
(declare-fun r_ds1_filter () (Array Int (Array Int Real)))
(declare-fun u () (Array Int (Array Int Real)))
(declare-fun use () Real)
(declare-fun xhatmin_ds1_filter () (Array Int (Array Int Real)))
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
(assert (let ((?v_0 (> pv5 0))) (not (=> (and (and (and (and (and (and (and (not ?v_0) (>= pv5 0)) (<= pv5 (- 999 1))) ?v_0) (forall ((?A_59 Int) (?B_60 Int)) (let ((?v_1 (- 6 1))) (=> (and (and (and (>= ?A_59 0) (>= ?B_60 0)) (<= ?A_59 ?v_1)) (<= ?B_60 ?v_1)) (= (select (select q_ds1_filter ?A_59) ?B_60) (select (select q_ds1_filter ?B_60) ?A_59)))))) (forall ((?C_61 Int) (?D_62 Int)) (let ((?v_2 (- 3 1))) (=> (and (and (and (>= ?C_61 0) (>= ?D_62 0)) (<= ?C_61 ?v_2)) (<= ?D_62 ?v_2)) (= (select (select r_ds1_filter ?C_61) ?D_62) (select (select r_ds1_filter ?D_62) ?C_61)))))) (forall ((?E_63 Int) (?F_64 Int)) (let ((?v_3 (- 6 1))) (=> (and (and (and (>= ?E_63 0) (>= ?F_64 0)) (<= ?E_63 ?v_3)) (<= ?F_64 ?v_3)) (= (select (select pminus_ds1_filter ?E_63) ?F_64) (select (select pminus_ds1_filter ?F_64) ?E_63)))))) (forall ((?G_65 Int)) (=> (and (>= ?G_65 0) (<= ?G_65 (- 6 1))) (forall ((?H_66 Int)) (=> (and (>= ?H_66 0) (<= ?H_66 (- 6 1))) (= (select (select id_ds1_filter ?G_65) ?H_66) (select (select id_ds1_filter ?H_66) ?G_65))))))) (forall ((?I_67 Int) (?J_68 Int)) (let ((?v_4 (- 6 1)) (?v_16 (select (select xhatmin_ds1_filter 5) 0)) (?v_15 (select (select u 2) pv5)) (?v_13 (select (select u 1) pv5)) (?v_12 (select (select xhatmin_ds1_filter 4) 0)) (?v_9 (select (select xhatmin_ds1_filter 3) 0)) (?v_8 (select (select u 0) pv5)) (?v_17 (trans h_ds1_filter))) (let ((?v_18 (tptp_mmul pminus_ds1_filter (tptp_mmul ?v_17 (inv (tptp_madd r_ds1_filter (tptp_mmul h_ds1_filter (tptp_mmul pminus_ds1_filter ?v_17)))))))) (let ((?v_19 (tptp_msub id_ds1_filter (tptp_mmul ?v_18 h_ds1_filter))) (?v_6 (divide 1.0 400.0))) (let ((?v_5 (store phi_ds1_filter 2 (store (select phi_ds1_filter 2) 1 (* ?v_6 (- ?v_9 ?v_8)))))) (let ((?v_7 (store ?v_5 2 (store (select ?v_5 2) 0 (* ?v_6 (- ?v_13 ?v_12)))))) (let ((?v_10 (store ?v_7 1 (store (select ?v_7 1) 2 (* ?v_6 (- ?v_8 ?v_9)))))) (let ((?v_11 (store ?v_10 1 (store (select ?v_10 1) 0 (* ?v_6 (- ?v_16 ?v_15)))))) (let ((?v_14 (store ?v_11 0 (store (select ?v_11 0) 2 (* ?v_6 (- ?v_12 ?v_13)))))) (let ((?v_20 (store ?v_14 0 (store (select ?v_14 0) 1 (* ?v_6 (- ?v_15 ?v_16)))))) (let ((?v_21 (tptp_madd q_ds1_filter (tptp_mmul ?v_20 (tptp_mmul (tptp_madd (tptp_mmul ?v_18 (tptp_mmul r_ds1_filter (trans ?v_18))) (tptp_mmul ?v_19 (tptp_mmul pminus_ds1_filter (trans ?v_19)))) (trans ?v_20)))))) (=> (and (and (and (>= ?I_67 0) (>= ?J_68 0)) (<= ?I_67 ?v_4)) (<= ?J_68 ?v_4)) (= (select (select ?v_21 ?I_67) ?J_68) (select (select ?v_21 ?J_68) ?I_67))))))))))))))))))
(check-sat)
(exit)
