--- Change in a binary tree all leaves to the maximum value of all leaves
--- (in one pass!)


max :: Int -> Int -> Int
max x y = if x>y then x else y


data Btree a = Leaf a | Node (Btree a) (Btree a)


pass_tree (Leaf n) mx = (Leaf mx, n)
pass_tree (Node t1 t2) mx
 | (mt1,m1) =:= pass_tree t1 mx  &  (mt2,m2) =:= pass_tree t2 mx
  = (Node mt1 mt2, max m1 m2)
 where mt1,mt2,m1,m2 free

maxtree :: Btree Int -> Btree Int

maxtree t | (mt,mx) =:= pass_tree t mx  = mt  where mt,mx free


--- goals:
goal1 = maxtree (Node (Leaf 0)
                      (Node (Leaf 1)
                            (Leaf 2)))

goal2 = maxtree (Node (Node (Leaf 1)
                            (Leaf 0))
                      (Node (Leaf 3)
                            (Leaf 2)))
