-- Module SportsDB from Escher report

data Person = Mary | Bill | Joe | Fred

data Sport = Cricket | Football | Tennis

likes :: Person -> Sport -> Success

likes Mary Cricket  = success
likes Mary Tennis   = success
likes Bill Cricket  = success
likes Bill Tennis   = success
likes Joe  Tennis   = success
likes Joe  Football = success


q1 s = likes Fred s   --> no solution

-- Auxiliaries:
-- list membership defined by a conditional rule based on concatenation:

member(e,l) = let l1,l2 free in l1++(e:l2)=:=l


-- implementation of Escher's forall-construct:
forall :: (a->Success) -> (a->Success) -> Success
forall domain cond = foldr (&) success (map cond (findall domain))


q2 x = forall (\y-> member(y,[Cricket,Tennis]))
              (\y-> likes x y)

--> {x=Mary} | {x=Bill}
