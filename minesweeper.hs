type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


maxX (S (px,py) [] s p) max = if max > px then max else px
maxX (S (px,py) ((x,y):xs) s p) max = if x > max then maxX(S (px,py) xs s p) x
					else maxX(S (px,py) (xs) s p) max
					
maxY (S (px,py) [] s p) max = if max > py then max else py
maxY (S (px,py) ((x,y):xs) s p) max = if y > max then maxY(S (px,py) xs s p) y
					else maxY(S (px,py) (xs) s p) max

up:: MyState -> MyState
up Null = Null
up (S (x,y) z c v) = if x-1 < 0 then Null 
					else S (x-1,y) z "up" (S (x,y) z c v)
	
down:: MyState -> MyState					
down Null = Null
down (S (x,y) z c v) = if (x+1) > maxX (S (x,y) z c v) 0 then Null 
					else S (x+1,y) z "down" (S (x,y) z c v)
			
left:: MyState -> MyState			
left Null = Null
left (S (x,y) z c v) = if y-1 < 0 then Null 
					else S (x,y-1) z "left" (S (x,y) z c v)
	
right:: MyState -> MyState		
right Null = Null
right (S (x,y) z c v) = if (y+1) > maxY (S (x,y) z c v) 0 then Null 
					else S (x,y+1) z "right" (S (x,y) z c v)						
	
removeCell _ [] = []
removeCell x (y:ys) | x == y  = removeCell x ys
                    | otherwise = y : removeCell x ys	
	
collect:: MyState -> MyState -> MyState	
collect (S position (x:xs) s p) (S positionBefore y sBefore pBefore) = 
                    if position == x then  S position (removeCell x y) "collect" (S positionBefore y sBefore pBefore)
					else if xs == [] then Null
					else collect (S position xs s p) (S positionBefore y sBefore pBefore)

isNotMember (S position m s p) [] = True
isNotMember (S position m s p) ((S position2 m2 s2 p2):xs)
    | position == position2 = False
    | otherwise = isNotMember (S position m s p) xs	
	
nextMyStates::MyState->[MyState] ->[MyState]	 
nextMyStates (S position m s p) y = (if up (S position m s p) /= Null && isNotMember (up (S position m s p)) y then [up (S position m s p)] else []) ++ 
					(if down (S position m s p) /= Null && isNotMember (down (S position m s p)) y  then [down (S position m s p)] else []) ++ 
					(if left (S position m s p) /= Null && isNotMember (left (S position m s p)) y  then [left (S position m s p)] else []) ++ 
					(if right (S position m s p) /= Null && isNotMember (right (S position m s p)) y  then [right (S position m s p)] else []) ++ 
					(if collect (S position m s p) (S position m s p) /= Null then [collect (S position m s p)(S position m s p)] else [])

isGoal::MyState->Bool
isGoal (S position [] s p) = True
isGoal (S position m s p) = False

search::[MyState]-> [MyState]-> MyState
search ((S position m s p):xs) y = if isGoal (S position m s p) then (S position m s p) 
					else search (xs++ nextMyStates (S position m s p) (xs)) (xs) 

constructSolution:: MyState ->[String]
constructSolution (S position m "" p) = []
constructSolution (S position m s p) =  (constructSolution p)++[s]


solve position mines = constructSolution (search([S position mines "" Null]) ([]))