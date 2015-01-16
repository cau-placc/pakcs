-- Curry with arithmetic constraints:
--
-- import the CLPR library:

import CLP.R


-- Example:
-- Mortgage relationship between:
--     p:  Principal
--     t:  Life of loan in months
--     ir: Fixed (but compounded) monthly interest rate
--     r:  Monthly repayment
--     b:  Outstanding balance at the end

mortgage p t ir r b | t >. 0 & t <=. 1  -- lifetime not more than 1 month?
                    =  b =:= p * (1 + t*ir) - t*r
mortgage p t ir r b | t >. 1              -- lifetime more than 1 month?
                    =  mortgage (p * (1+ir) - r) (t-1) ir r b

-- mortgage 100000 180 0.01 r 0      where r free

-- mortgage 100000 time 0.01 1400 0  where time free

-- mortgage h 180 0.01 r b           where h,r,b free
