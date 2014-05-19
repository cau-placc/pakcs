import CLPFD

-- send more money puzzle in Curry with FD constraints:

smm l =
        l =:= [s,e,n,d,m,o,r,y] &
        domain l 0 9 &
        s ># 0 &
        m ># 0 &
        allDifferent l  &
                         1000 *# s +# 100 *# e +# 10 *# n +# d
        +#               1000 *# m +# 100 *# o +# 10 *# r +# e
        =# 10000 *# m +# 1000 *# o +# 100 *# n +# 10 *# e +# y &
        labeling [] l
        where s,e,n,d,m,o,r,y free


-- smm [s,e,n,d,m,o,r,y]  where s,e,n,d,m,o,r,y free

