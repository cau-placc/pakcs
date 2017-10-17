import RandomTest
import FiniteMap
import List
import Sort
import Maybe

fm f = f . (listToFM (<)) . map (\x ->(x,x))

fms f = map fst . fmToList . fm f

fms' f = map snd . fmToList . fm f

so f = spnub . mergeSortBy (<) . f 

testAddToFM = eq (fms (\x-> addToFM x 73 73)) (so (73:))

testDelFromFM = test
  (\nums -> fms (flip delListFromFM (take 500 nums)) nums == so (drop 500) nums)

testPlusFM =
 test (\nums -> let l=length nums 
                    (xs,ys) = splitAt (div l 2) nums 
         in (map fst $ fmToList $ plusFM (fm id xs) (fm id ys)) == so id nums)

testMinusFM =
 test (\nums -> let l = length nums 
                    (xs,ys) = splitAt (div l 2) nums 
         in (map fst $ fmToList $ minusFM (fm id nums) (fm id ys)) == so id xs)

testIntersectFM = test
   (\nums -> let l=length nums 
                 (_,ys) = splitAt (div l 2) nums 
     in (map fst $ fmToList $ intersectFM (fm id nums) (fm id ys)) == so id ys)

testFoldFM = eq (fm (foldFM (\x _ z->x+z) 0)) (foldl (+) 0)

testMapFM = eq (fms' (mapFM (\_ z->z+1))) (so (map (+1)))

testFilterFM = eq (fms (filterFM (\x _->x>0))) (so (filter (>0)))

testSizeFM = eq (fm sizeFM) length

testEqFM = test
          (\nums -> let l=length nums 
                        (xs,ys) = splitAt (div l 2) nums 
                     in eqFM (fm id nums) (fm id (ys++xs)))

testElemFM_LookupFM = eq (fm (\x-> elemFM 73 (addToFM x 73 73))) (const True)

testKeysFM_eltsFM = test
         (\nums -> let finm=fm id nums in unzip (fmToList finm)==(keysFM finm, eltsFM finm))

testFmSortBy = eq (fmSortBy (<)) (so id)

testMinFM_MaxFM = eq (fm (\finm -> (fst $ fromJust $ minFM finm,
                                          fst $ fromJust $ maxFM finm))) 
                           ((\l->(head l,last l)) .so id)

testUpdFM = eq (fm (\x-> lookupFM (updFM (addToFM x 73 73) 73 (+7)) 73)) (const $ Just 80)


spnub [] = []
spnub [x] = [x]
spnub (x:y:xs) = if x==y then spnub (y:xs) else x:spnub (y:xs)