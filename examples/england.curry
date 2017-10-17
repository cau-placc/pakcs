import Findall(allSolutions)

-- Geographical database (from John Lloyd's Escher report):

data County = Avon | Bedfordshire | Berkshire | Buckinghamshire |
              Cambridgeshire | Cornwall | Devon | Dorset | Essex |
              Gloucestershire | Hampshire | Herefordshire |
              Hertfordshire |Kent |London |Northamptonshire | Oxfordshire |
              Somerset | Surrey | Sussex | Warwickshire | Wiltshire |
              Worcestershire
 deriving Eq

data City = Bath | Bournemouth | Bristol | Cheltenham | Cirencester |
            Dorchester | Exeter | Gloucester | Penzance | Plymouth |
            Salisbury | Shaftesbury | Sherbourne | Taunton | Torquay |
            Truro | Winchester
 deriving Eq

neighbours :: County -> County -> Bool
neighbours Devon           Cornwall          = True
neighbours Devon           Dorset            = True
neighbours Devon           Somerset          = True
neighbours Avon            Somerset          = True
neighbours Avon            Wiltshire         = True
neighbours Avon            Gloucestershire   = True
neighbours Dorset          Wiltshire         = True
neighbours Somerset        Wiltshire         = True
neighbours Gloucestershire Wiltshire         = True
neighbours Dorset          Somerset          = True
neighbours Dorset          Hampshire         = True
neighbours Hampshire       Wiltshire         = True
neighbours Hampshire       Berkshire         = True
neighbours Hampshire       Sussex            = True
neighbours Hampshire       Surrey            = True
neighbours Sussex          Surrey            = True
neighbours Sussex          Kent              = True
neighbours London          Surrey            = True
neighbours London          Kent              = True
neighbours London          Essex             = True
neighbours London          Hertfordshire     = True
neighbours London          Buckinghamshire   = True
neighbours Surrey          Buckinghamshire   = True
neighbours Surrey          Kent              = True
neighbours Surrey          Berkshire         = True
neighbours Oxfordshire     Berkshire         = True
neighbours Oxfordshire     Wiltshire         = True
neighbours Oxfordshire     Gloucestershire   = True
neighbours Oxfordshire     Warwickshire      = True
neighbours Oxfordshire     Northamptonshire  = True
neighbours Oxfordshire     Buckinghamshire   = True
neighbours Berkshire       Wiltshire         = True
neighbours Berkshire       Buckinghamshire   = True
neighbours Gloucestershire Worcestershire    = True
neighbours Worcestershire  Herefordshire     = True
neighbours Worcestershire  Warwickshire      = True
neighbours Bedfordshire    Buckinghamshire   = True
neighbours Bedfordshire    Northamptonshire  = True
neighbours Bedfordshire    Cambridgeshire    = True
neighbours Bedfordshire    Hertfordshire     = True
neighbours Hertfordshire   Essex             = True
neighbours Hertfordshire   Cambridgeshire    = True
neighbours Hertfordshire   Buckinghamshire   = True
neighbours Buckinghamshire Northamptonshire  = True



distance1 :: City -> City -> Int
distance1 Plymouth    Exeter       = 42
distance1 Exeter      Bournemouth  = 82
distance1 Bristol     Taunton      = 43
distance1 Bristol     Gloucester   = 35
distance1 Torquay     Exeter       = 23
distance1 Plymouth    Torquay      = 24 
distance1 Bristol     Bath         = 13
distance1 Exeter      Taunton      = 34
distance1 Penzance    Plymouth     = 78
distance1 Taunton     Bournemouth  = 70 
distance1 Bournemouth Salisbury    = 28
distance1 Taunton     Salisbury    = 64
distance1 Salisbury   Bath         = 40
distance1 Bath        Gloucester   = 39
distance1 Bournemouth Bath         = 65
distance1 Truro       Penzance     = 26
distance1 Plymouth    Truro        = 52
distance1 Shaftesbury Salisbury    = 20
distance1 Sherbourne  Shaftesbury  = 16
distance1 Dorchester  Bournemouth  = 28
distance1 Salisbury   Winchester   = 24 
distance1 Exeter      Sherbourne   = 53
distance1 Sherbourne  Taunton      = 29
distance1 Bath        Cirencester  = 32
distance1 Cirencester Cheltenham   = 16
distance1 Cheltenham  Gloucester   = 9
distance1 Dorchester  Sherbourne   = 19
distance1 Bath        Shaftesbury  = 33
distance1 Winchester  Bournemouth  = 41
distance1 Exeter      Dorchester   = 53


distance :: City -> City -> Int
distance city1 city2 = distance1 city1 city2 ? distance1 city2 city1


isin :: City -> County -> Bool
isin Bristol     Avon             = True
isin Taunton     Somerset         = True
isin Salisbury   Wiltshire        = True
isin Bath        Avon             = True
isin Bournemouth Dorset           = True
isin Gloucester  Gloucestershire  = True
isin Torquay     Devon            = True
isin Penzance    Cornwall         = True
isin Plymouth    Devon            = True
isin Exeter      Devon            = True
isin Winchester  Hampshire        = True
isin Dorchester  Dorset           = True
isin Cirencester Gloucestershire  = True
isin Truro       Cornwall         = True
isin Cheltenham  Gloucestershire  = True
isin Shaftesbury Dorset           = True
isin Sherbourne  Dorset           = True


-- Some queries and their expected results:
q1 x = solve $ distance Bristol x < 40
-- {x=Gloucester}  | {x=Bath} 

q2 x y = solve $ distance1 x y < 20
-- {y=Bath,x=Bristol}  | {y=Shaftesbury,x=Sherbourne}  | {y=Sherbourne,x=Dorchester}  | {y=Cheltenham,x=Cirencester}  | {y=Gloucester,x=Cheltenham} 

q3 x = (neighbours Oxfordshire x ? neighbours x Oxfordshire)
-- {x=Berkshire}  | {x=Wiltshire}  | {x=Gloucestershire}  | {x=Warwickshire}  | {x=Northamptonshire}  | {x=Buckinghamshire} 

q4 x = solve $ let y free in isin x y && y/=Wiltshire
-- {x=Bristol}  | {x=Taunton}  | {x=Bath}  | {x=Bournemouth}  | {x=Gloucester}  | {x=Torquay}  | {x=Penzance}  | {x=Plymouth}  | {x=Exeter}  | {x=Winchester}  | {x=Dorchester}  | {x=Cirencester}  | {x=Truro}  | {x=Cheltenham}  | {x=Shaftesbury}  | {x=Sherbourne} 

q4l = allSolutions (\x -> let y free in isin x y && y /= Wiltshire)
-- [Bristol,Taunton,Bath,Bournemouth,Gloucester,Torquay,Penzance,Plymouth,Exeter,Winchester,Dorchester,Cirencester,Truro,Cheltenham,Shaftesbury,Sherbourne]

q5 x = solve $ (neighbours Oxfordshire y ? neighbours y Oxfordshire)
               && isin x y  where y free
-- {x=Salisbury}  | {x=Gloucester}  | {x=Cirencester}  | {x=Cheltenham} 

q5l = allSolutions
       (\x -> let y free in
             (neighbours Oxfordshire y ? neighbours y Oxfordshire) && isin x y)
-- [Salisbury,Gloucester,Cirencester,Cheltenham]

q6 x = solve $ let y free in y `elem` [Devon,Cornwall,Somerset,Avon] && isin x y
-- {x=Torquay}  | {x=Plymouth}  | {x=Exeter}  | {x=Penzance}  | {x=Truro}  | {x=Taunton}  | {x=Bristol}  | {x=Bath} 

q7 x = solve $ let y free in distance Bristol y < 50 && isin y x
-- {x=Somerset}  | {x=Gloucestershire}  | {x=Avon} 

-- the further queries require encapsulated search:

-- implementation of Escher's forall-construct:
forall :: (a->Bool) -> (a->Bool) -> Bool
forall domain cond = all cond (allSolutions domain)
 
q8 = forall (\x -> (neighbours Avon x ? neighbours x Avon))
            (\x -> isin _ x)
-- True | True | True

q9 = let x free in
     isin Bristol x &&
     forall (\z -> distance Bristol z < 40)
            (\z -> isin z x)
-- No solution.

q10 = length
       (allSolutions (\x->neighbours Oxfordshire x ? neighbours x Oxfordshire))
-- 6
