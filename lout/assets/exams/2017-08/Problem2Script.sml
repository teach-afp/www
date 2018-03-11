open HolKernel listTheory boolLib bossLib Parse

val _ = new_theory "Problem2"

val _ = Datatype`
  Tree = Leaf 'a | Node Tree Tree
`;

(* Types of stuff *)
type_of ``Node``;
type_of ``Leaf``;
(* Functor stuff *)

val fmap_def = Define`
  (fmap f (Leaf v)   = Leaf (f v))
∧ (fmap f (Node l r) = Node (fmap f l) (fmap f r))
`;

val id_def = Define`
  id x = x
`;

val ISPECL_TAC =
 fn tl => Q.ISPECL_THEN tl (fn thm => ONCE_REWRITE_TAC [Once thm]);

val f_identity = Q.store_thm("f_identity",
  `fmap id = id`,
  REWRITE_TAC [FUN_EQ_THM]
  \\ GEN_TAC
  \\ Induct_on `x`
     (* Base case *)
     >- (GEN_TAC
        \\ Q.ABBREV_TAC `goal = id (Leaf a)`
        \\ ONCE_REWRITE_TAC [fmap_def]
        \\ ONCE_REWRITE_TAC [id_def]
        \\ ISPECL_TAC [`Leaf a`] (GSYM id_def)
        \\ Q.UNABBREV_TAC `goal`
        \\ REWRITE_TAC [])
     (* Inductive case *)
     >- (Q.ABBREV_TAC `goal = id (Node x x')`
        \\ ONCE_REWRITE_TAC [fmap_def]
        (* Apply IH *)
        \\ ASM_REWRITE_TAC []
        \\ ONCE_REWRITE_TAC [id_def]
        \\ ISPECL_TAC [`Node x x'`] (GSYM id_def)
        \\ Q.UNABBREV_TAC `goal`
        \\ REWRITE_TAC [])
);

val comp_def = Q.store_thm("comp_def",
  `∀f g x. (f o g) x = f (g x)`,
  rw []
);

val f_map_fusion = Q.store_thm("f_map_fusion",
  `fmap (f o g) = fmap f o fmap g`,
  REWRITE_TAC [FUN_EQ_THM]
  \\ Induct_on `x`
     (* Base case *)
     >- (GEN_TAC
        \\ Q.ABBREV_TAC `goal = (fmap f ∘ fmap g) (Leaf a)`
        \\ ONCE_REWRITE_TAC [fmap_def]
        \\ ONCE_REWRITE_TAC [comp_def]
        \\ ONCE_REWRITE_TAC [GSYM fmap_def]
        \\ ONCE_REWRITE_TAC [GSYM fmap_def]
        \\ ISPECL_TAC [`fmap f`, `fmap g`] (GSYM comp_def)
        \\ Q.UNABBREV_TAC `goal`
        \\ REWRITE_TAC [])
    (* Inductive case *)
    >- (Q.ABBREV_TAC `goal = (fmap f ∘ fmap g) (Node x x')`
       \\ ONCE_REWRITE_TAC [fmap_def]
       (* Apply IH *)
       \\ ASM_REWRITE_TAC []
       \\ ONCE_REWRITE_TAC [comp_def]
       \\ ONCE_REWRITE_TAC [GSYM fmap_def]
       \\ ONCE_REWRITE_TAC [GSYM fmap_def]
       \\ ISPECL_TAC [`fmap f`,`fmap g`] (GSYM comp_def)
       \\ Q.UNABBREV_TAC `goal`
       \\ REWRITE_TAC [])
);

val foldT_def = Define`
  (foldT f (Leaf a) = a)
∧ (foldT f (Node l r) = f (foldT f l) (foldT f r))
`;

val height_tree_def = Define`
  height_tree = foldT (λl r. MAX l r + 1) o fmap (K 0)
`;

val sum_tree_def = Define`
  sum_tree = (foldT (+) : num Tree -> num)
`;

val leaves_def = Define`
  leaves = foldT (++) o fmap (λx. [x])
`;

type_of ``foldT``;
type_of ``height_tree``;
type_of ``sum_tree``;
type_of ``leaves``;

APPEND;

val MAP_0 = GSYM MAP |> CONJUNCTS |> hd;

val map_append = Q.store_thm("map_append",
  `MAP f (l ++ r) = MAP f l ++ MAP f r`,
  Induct_on `l`
  >- (Q.ABBREV_TAC `goal = MAP f [] ⧺ MAP f r`
     \\ REWRITE_TAC [APPEND]
     \\ ONCE_REWRITE_TAC [Once (GSYM APPEND)]
     \\ ISPECL_TAC [`f`] (MAP_0)
     \\ Q.UNABBREV_TAC `goal`
     \\ REWRITE_TAC [])
  >- (GEN_TAC
     \\ Q.ABBREV_TAC `goal = MAP f (h::l) ⧺ MAP f r`
     \\ REWRITE_TAC [APPEND]
     \\ REWRITE_TAC [MAP]
     \\ ASM_REWRITE_TAC []
     \\ ONCE_REWRITE_TAC [Once (GSYM APPEND)]
     \\ ONCE_REWRITE_TAC [Once (GSYM MAP)]
     \\ Q.UNABBREV_TAC `goal`
     \\ REWRITE_TAC [])
);

val CONS_def = Q.store_thm("CONS_def",
  `[a] = (λx. [x]) a`,
  rw []
);

val foldT_Leaf = GSYM foldT_def |> CONJUNCTS |> hd;
val foldT_Node = GSYM foldT_def |> CONJUNCTS |> el 2;

val map_leaves = Q.store_thm("map_leaves",
  `MAP f o leaves = leaves o fmap f`,
  REWRITE_TAC [FUN_EQ_THM]
  \\ Induct_on `x`
     (* Base case  *)
     >- (GEN_TAC
        \\ Q.ABBREV_TAC `goal = (leaves ∘ fmap f) (Leaf a)`
        \\ REWRITE_TAC [leaves_def]
        \\ REWRITE_TAC [comp_def]
        \\ REWRITE_TAC [fmap_def]
        \\ BETA_TAC
        \\ REWRITE_TAC [foldT_def]
        \\ REWRITE_TAC [MAP]
        \\ ISPECL_TAC [`$++`] foldT_Leaf
        \\ ONCE_REWRITE_TAC [CONS_def]
        \\ ONCE_REWRITE_TAC [GSYM fmap_def]
        \\ ISPECL_TAC [`foldT $++`] (GSYM comp_def)
        \\ ONCE_REWRITE_TAC [GSYM leaves_def]
        \\ ONCE_REWRITE_TAC [GSYM fmap_def]
        \\ ISPECL_TAC [`leaves`] (GSYM comp_def)
        \\ Q.UNABBREV_TAC `goal`
        \\ REWRITE_TAC [])
     (* Inductive case *)
     >- (Q.ABBREV_TAC `goal = (leaves ∘ fmap f) (Node x x')`
        \\ REWRITE_TAC [comp_def]
        \\ REWRITE_TAC [leaves_def]
        \\ REWRITE_TAC [comp_def]
        \\ REWRITE_TAC [fmap_def]
        \\ REWRITE_TAC [foldT_def]
        \\ REWRITE_TAC [map_append]
        \\ ISPECL_TAC [`foldT $++`] (GSYM comp_def)
        \\ ISPECL_TAC [`foldT $++`] (GSYM comp_def)
        \\ REWRITE_TAC [GSYM leaves_def]
        \\ ISPECL_TAC [`MAP f`] (GSYM comp_def)
        \\ ISPECL_TAC [`MAP f`] (GSYM comp_def)
        \\ ASM_REWRITE_TAC []
        \\ REWRITE_TAC [leaves_def]
        \\ REWRITE_TAC [comp_def]
        \\ ISPECL_TAC [`$++`] (foldT_Node)
        \\ REWRITE_TAC [GSYM fmap_def]
        \\ ISPECL_TAC [`foldT $++`] (GSYM comp_def)
        \\ REWRITE_TAC [GSYM leaves_def]
        \\ ISPECL_TAC [`leaves`] (GSYM comp_def)
        \\ Q.UNABBREV_TAC `goal`
        \\ REWRITE_TAC [])
);

val _ = export_theory ();
