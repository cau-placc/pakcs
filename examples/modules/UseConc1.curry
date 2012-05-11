import ModConc hiding ((+))  -- prelude.+ should still be visible

main1 = [1] .+. [1+1]

main2 = [1] ModConc..+. [1+1]

main3 = conc [1] [2]
