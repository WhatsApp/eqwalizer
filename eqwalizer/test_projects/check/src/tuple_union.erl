-module(tuple_union).

-compile([export_all, nowarn_export_all]).

-type t1() ::
    {msg, ok | err, arg}.
-type t2() ::
    {msg, ok, arg} | {msg, err, arg}.
    
-spec test_01(t1 ()) -> t2().
test_01(X) -> X.

% More complex with several incompatible tuples
-type t3() ::
    {msg, ok, arg} | {msg, err} | {foo, ok, argfoo} | {msg, err, arg}.

-spec test_02(t1 ()) -> t3().
test_02(X) -> X.

% We do not handle doubly-factorisable unions
-type t4() ::
    {msg, ok | err, arg | nil}.
-type t5() ::
    {msg, ok, arg} | {msg, err, arg} | {msg, ok, nil} | {msg, err, nil}.
   
-spec test_03_neg(t4 ()) -> t5 ().
test_03_neg(X) -> X.

% Aliases
-type t6a() ::
  {msg, ok, arg}.
-type t6b() ::
  {msg, err, arg}.
-type t6() ::
  t6a() | t6b().

-spec test_04(t1()) -> t6().
test_04(X) -> X.

% Recursive types
-type tree1() ::
  {leaf, atom()}
  | {b1, tree1()}
  | {b2, tree1()}.

-type branch() :: b1 | b2.

-type tree2() ::
  {leaf, atom()}
  | {branch(), tree2()}.

-type tree3() ::
  {leaf, atom()}
  | {b1 | b2 | b3, tree2()}.

-spec tree1_as_tree2
    (tree1()) -> tree2().
tree1_as_tree2(T) -> T.

-spec tree2_as_tree1
    (tree2()) -> tree1().
tree2_as_tree1(T) -> T.

-spec tree3_as_tree1_neg
    (tree3()) -> tree1().
tree3_as_tree1_neg(T) -> T.
