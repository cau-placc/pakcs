-- Geographical database (from John Lloyd's Escher report):

data County = Avon | Bedfordshire | Berkshire | Buckinghamshire |
              Cambridgeshire | Cornwall | Devon | Dorset | Essex |
              Gloucestershire | Hampshire | Herefordshire |
              Hertfordshire |Kent |London |Northamptonshire | Oxfordshire |
              Somerset | Surrey | Sussex | Warwickshire | Wiltshire |
              Worcestershire

data City = Bath | Bournemouth | Bristol | Cheltenham | Cirencester |
            Dorchester | Exeter | Gloucester | Penzance | Plymouth |
            Salisbury | Shaftesbury | Sherbourne | Taunton | Torquay |
            Truro | Winchester


neighbours :: County -> County -> Success
       
neighbours Devon           Cornwall          = success
neighbours Devon           Dorset            = success
neighbours Devon           Somerset          = success
neighbours Avon            Somerset          = success
neighbours Avon            Wiltshire         = success
neighbours Avon            Gloucestershire   = success
neighbours Dorset          Wiltshire         = success
neighbours Somerset        Wiltshire         = success
neighbours Gloucestershire Wiltshire         = success
neighbours Dorset          Somerset          = success
neighbours Dorset          Hampshire         = success
neighbours Hampshire       Wiltshire         = success
neighbours Hampshire       Berkshire         = success
neighbours Hampshire       Sussex            = success
neighbours Hampshire       Surrey            = success
neighbours Sussex          Surrey            = success
neighbours Sussex          Kent              = success
neighbours London          Surrey            = success
neighbours London          Kent              = success
neighbours London          Essex             = success
neighbours London          Hertfordshire     = success
neighbours London          Buckinghamshire   = success
neighbours Surrey          Buckinghamshire   = success
neighbours Surrey          Kent              = success
neighbours Surrey          Berkshire         = success
neighbours Oxfordshire     Berkshire         = success
neighbours Oxfordshire     Wiltshire         = success
neighbours Oxfordshire     Gloucestershire   = success
neighbours Oxfordshire     Warwickshire      = success
neighbours Oxfordshire     Northamptonshire  = success
neighbours Oxfordshire     Buckinghamshire   = success
neighbours Berkshire       Wiltshire         = success
neighbours Berkshire       Buckinghamshire   = success
neighbours Gloucestershire Worcestershire    = success
neighbours Worcestershire  Herefordshire     = success
neighbours Worcestershire  Warwickshire      = success
neighbours Bedfordshire    Buckinghamshire   = success
neighbours Bedfordshire    Northamptonshire  = success
neighbours Bedfordshire    Cambridgeshire    = success
neighbours Bedfordshire    Hertfordshire     = success
neighbours Hertfordshire   Essex             = success
neighbours Hertfordshire   Cambridgeshire    = success
neighbours Hertfordshire   Buckinghamshire   = success
neighbours Buckinghamshire Northamptonshire  = success



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

distance city1 city2 | distance1 city1 city2 =:= d  = d  where d free
distance city1 city2 | distance1 city2 city1 =:= d  = d  where d free


isin :: City -> County -> Success

isin Bristol     Avon             = success
isin Taunton     Somerset         = success
isin Salisbury   Wiltshire        = success
isin Bath        Avon             = success
isin Bournemouth Dorset           = success
isin Gloucester  Gloucestershire  = success
isin Torquay     Devon            = success
isin Penzance    Cornwall         = success
isin Plymouth    Devon            = success
isin Exeter      Devon            = success
isin Winchester  Hampshire        = success
isin Dorchester  Dorset           = success
isin Cirencester Gloucestershire  = success
isin Truro       Cornwall         = success
isin Cheltenham  Gloucestershire  = success
isin Shaftesbury Dorset           = success
isin Sherbourne  Dorset           = success


-- list membership:
member e l = let xs,ys free in xs ++ (e:ys) =:= l


-- Some queries and their expected results:
q1 x = (distance Bristol x < 40) =:= True
-- {x=Gloucester}  | {x=Bath} 

q2 x y = (distance1 x y < 20) =:= True
-- {y=Bath,X=Bristol}  | {y=Shaftesbury,X=Sherbourne}  | {y=Sherbourne,X=Dorchester}  | {y=Cheltenham,X=Cirencester}  | {y=Gloucester,X=Cheltenham} 

q3 x = (neighbours Oxfordshire x ? neighbours x Oxfordshire)
-- {x=Berkshire}  | {x=Wiltshire}  | {x=Gloucestershire}  | {x=Warwickshire}  | {x=Northamptonshire}  | {x=Buckinghamshire} 

q4 x = (isin x y & (y==Wiltshire)=:=False)  where y free
-- {x=Bristol}  | {x=Taunton}  | {x=Bath}  | {x=Bournemouth}  | {x=Gloucester}  | {x=Torquay}  | {x=Penzance}  | {x=Plymouth}  | {x=Exeter}  | {x=Winchester}  | {x=Dorchester}  | {x=Cirencester}  | {x=Truro}  | {x=Cheltenham}  | {x=Shaftesbury}  | {x=Sherbourne} 

q4l = findall (\x -> let y free in (isin x y & (y==Wiltshire)=:=False))
-- [Bristol,Taunton,Bath,Bournemouth,Gloucester,Torquay,Penzance,Plymouth,Exeter,Winchester,Dorchester,Cirencester,Truro,Cheltenham,Shaftesbury,Sherbourne]

q5 x = ((neighbours Oxfordshire y ? neighbours y Oxfordshire)
        & isin x y)  where y free
-- {x=Salisbury}  | {x=Gloucester}  | {x=Cirencester}  | {x=Cheltenham} 

q5l = findall (\x-> let y free in
                    ((neighbours Oxfordshire y ? neighbours y Oxfordshire)
                     & isin x y))
-- [Salisbury,Gloucester,Cirencester,Cheltenham]

q6 x = member y [Devon,Cornwall,Somerset,Avon] & isin x y  where y free
-- {x=Torquay}  | {x=Plymouth}  | {x=Exeter}  | {x=Penzance}  | {x=Truro}  | {x=Taunton}  | {x=Bristol}  | {x=Bath} 

q7 x = (distance Bristol y < 50)=:=True & isin y x  where y free
-- {x=Somerset}  | {x=Gloucestershire}  | {x=Avon} 

-- the further queries require encapsulated search:

-- implementation of Escher's forall-construct:
forall :: (a->Success) -> (a->Success) -> Success
forall domain cond = foldr (&) success (map cond (findall domain))
 
q8 = forall (\x->(neighbours Avon x ? neighbours x Avon))
            (\x->let y free in isin y x)
-- success | success | success

q9 = let x free in
     isin Bristol x &
     forall (\z->(distance Bristol z < 40)=:=True)
             (\z->(isin z x))
-- No solution.

q10 = length (findall (\x->(neighbours Oxfordshire x ? neighbours x Oxfordshire)))
-- 6
