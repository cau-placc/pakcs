-- Concurrent object-oriented programming in Curry:
-- a bank account implemented as an object waiting for messages of type
-- Deposit a, Withdraw a, or Balance b:

data Message = Deposit Int | Withdraw Int | Balance Int

account :: Int -> [Message] -> Success
account _ []                 =  success
account n (Deposit  a : ms)  =  account (n+a) ms
account n (Withdraw a : ms)  =  account (n-a) ms
account n (Balance  b : ms)  =  b=:=n & account n ms

makeAccount s = account 0 (ensureSpine s) -- create bank account

-- goals:
goal1 b = let s free in
          makeAccount s & s=:=[Deposit 200, Deposit 50, Balance b]
goal2 b = let s free in
          makeAccount s &
            s=:=[Deposit 200, Withdraw 100, Deposit 50, Balance b]

-- send a message:
sendMsg msg obj | obj =:= msg:obj1  = obj1  where obj1 free  -- send a message

-- client process for bank account:
client s | s1 =:= sendMsg (Balance b) s =
  if b==50 then s1=:=[]   -- stop process
           else if b>50 then client (sendMsg (Withdraw 30) s1)  -- buy
                        else client (sendMsg (Deposit  70) s1)  -- work
  where s1,b free

goal3 s = makeAccount s & client (sendMsg (Deposit 100) s) -- simulation


