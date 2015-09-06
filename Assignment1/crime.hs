data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


says :: Boy -> Boy -> Bool
says Matthew x = not (x == Carl) && not (x == Matthew)
says Peter x = x == Matthew || x == Jack
says Jack x = not (says Matthew x) && not(says Peter x)
says Arnold x = (says Matthew x) /= (says Peter x)
says Carl x = not(says Arnold x)
	 

accusers :: Boy -> [Boy]
accusers b = map (\(b,_)->b) ( filter (\(_,a)-> a == True) (zip boys (map (says b) boys)))


possible_guilty:: [Boy] -> [Boy]
possible_guilty (boy:boys) = accusers boy ++ (possible_guilty boys)
possible_guilty [] = []


instances:: Boy -> [Boy] -> Int
instances x [] = 0
instances x (y:ys)
	| x == y = 1+(instances x ys)
	| otherwise = instances x ys


guilty:: [Boy] -> [Boy]
guilty [] = []
guilty (boy:ys) 
	| (instances boy (possible_guilty(boys))) == 3 = boy : guilty ys
	| otherwise = guilty ys

