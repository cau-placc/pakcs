-- MEMBER: list membership defined by a conditional rule based on append:

append([]  ,ys) = ys
append(x:xs,ys) = x:append(xs,ys)


member(e,l) | append(xs,e:ys)=:=l  = True  where xs,ys free


-- Goals:
goal1 x = member(x,[1,2,3])
goal2 x = member(x,[1,2,3]) && member(x,[2,3,4])
