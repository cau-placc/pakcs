import Assertion
import RandomTest
import FiniteMap
import List
import Sort
import Maybe

fm f = f . (listToFM (<)) . map (\x ->(x,x))

fms f = map fst . fmToList . fm f

fms' f = map snd . fmToList . fm f

so f = spnub . mergeSort (<) . f 

test1 = eq "addToFM" (fms (\x-> addToFM x 73 73)) (so (73:))

test2 = test "delFromFM" 
         (\nums -> fms (flip delListFromFM (take 500 nums)) nums == so (drop 500) nums)

test3 = test "plusFM" 
          (\nums -> let l=length nums 
                        (xs,ys) = splitAt (div l 2) nums 
                      in (map fst $ fmToList $ plusFM (fm id xs) (fm id ys)) == so id nums)

test4 = test "minusFM" 
          (\nums -> let l=length nums 
                        (xs,ys) = splitAt (div l 2) nums 
                      in (map fst $ fmToList $ minusFM (fm id nums) (fm id ys)) == so id xs)

test5 = test "intersectFM" 
          (\nums -> let l=length nums 
                        (_,ys) = splitAt (div l 2) nums 
                      in (map fst $ fmToList $ intersectFM (fm id nums) (fm id ys)) == so id ys)

test6 = eq "foldFM" (fm (foldFM (\x _ z->x+z) 0)) (foldl (+) 0)

test7 = eq "mapFM" (fms' (mapFM (\_ z->z+1))) (so (map (+1)))

test8 = eq "filterFM" (fms (filterFM (\x _->x>0))) (so (filter (>0)))

test9 = eq "sizeFM" (fm sizeFM) length

test10 = test "eqFM" 
          (\nums -> let l=length nums 
                        (xs,ys) = splitAt (div l 2) nums 
                     in eqFM (fm id nums) (fm id (ys++xs)))

test11 = eq "elemFM, lookupFM" (fm (\x-> elemFM 73 (addToFM x 73 73))) (const True)

test12 = test "keysFM, eltsFM" 
         (\nums -> let finm=fm id nums in unzip (fmToList finm)==(keysFM finm, eltsFM finm))

test13 = eq "fmSortBy" (fmSortBy (<)) (so id)

test14 = eq "minFM, maxFM" (fm (\finm -> (fst $ fromJust $ minFM finm,
                                          fst $ fromJust $ maxFM finm))) 
                           ((\l->(head l,last l)) .so id)

test15 = eq "updFM" (fm (\x-> lookupFM (updFM (addToFM x 73 73) 73 (+7)) 73)) (const $ Just 80)


spnub [] = []
spnub [x] = [x]
spnub (x:y:xs) = if x==y then spnub (y:xs) else x:spnub (y:xs)