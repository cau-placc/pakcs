-- Module "Higher" from the Escher report:

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun


map_ :: (a -> b , [a]) -> [b] 

map_(_,[])     = []
map_(f,x:xs) = f x : map_(f,xs)


filter_ :: (a -> Bool , [a]) -> [a]

filter_(_,[])   = []
filter_(p,x:xs) = if p x then x : filter_(p,xs) else filter_(p,xs)


next :: Day -> Day

next(Mon) = Tue
next(Tue) = Wed
next(Wed) = Thu
next(Thu) = Fri
next(Fri) = Sat
next(Sat) = Sun
next(Sun) = Mon


weekday :: Day -> Bool

weekday(Mon) = True
weekday(Tue) = True
weekday(Wed) = True
weekday(Thu) = True
weekday(Fri) = True
weekday(Sat) = False
weekday(Sun) = False


-- goals:
goal1 = map_(next,[Mon,Tue,Wed])
goal2 = filter_(weekday,[Sun,Mon,Wed])
