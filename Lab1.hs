discount :: Float->Float
discount price
	|price > 100 = price*0.9
	|otherwise = price

amount :: Int->Float->Float
amount n p
	|n < 0 = 0
	|p < 0 = 0
	|n < 5 = discount (p*fromIntegral(n))
	|n < 9 = discount (p*fromIntegral(n-1))
	|n == 9 = discount (p*fromIntegral(n-2))
	|otherwise = discount (p*fromIntegral(n-2-k)) where k=(n-9)`div`3

calculateHours :: Int->Int->Int
calculateHours a b
	|dif < 0 = 24+dif
	|otherwise = dif where dif = (a-b)

cost :: (Int,Int,Int)->(Int,Int,Int)->Float
cost (h1,m1,s1) (h2,m2,s2)
	|secs == 0 = 0.0
	|secs < 180 = 0.58
	|otherwise = (0.58 + (secs-180) * 0.003) where secs=fromIntegral((s2-s1)) + 60.0*fromIntegral((m2-m1)) + 3600.0*fromIntegral(calculateHours (h2) (h1))
{-cost (h1,m1,s1) (h2,m2,s2) = 0.003*(secs) where secs=fromIntegral(s2-s1) + 60.0*fromIntegral((m2-m1)) + 360.0*fromIntegral((h2-h1))-}
{-difsec (h1,m1,s1) (h2,m2,s2) = s2-s1
difmin (h1,m1,s1) (h2,m2,s2) = 60*(m2-m1)
difhour (h1,m1,s1) (h2,m2,s2) = 360*(h2-h1)
secs (difhour,difmin,difsec) = difsec+difmin+difhour
cost secs = 0.58*fromIntegral(secs)-}                          
