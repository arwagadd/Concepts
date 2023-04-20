type Cell = (Int,Int) --(row,column) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq) -- s(robotPosition [minesPositions] lastAction parentStateBeforelastAction)

up :: MyState ->  MyState 
up (S (a,b) h j k) = if a-1 <0 then Null else S (((a-1),b)) h "up" (S(a,b) h j k)

down :: MyState ->  Int -> Int -> MyState 
down (S (a,b) h j k) x y  = if a+1 >x then Null else S (((a+1),b)) h "down" (S(a,b) h j k)

left :: MyState -> MyState 
left (S (a,b) h j k) = if b-1 <0 then Null else S ((a,(b-1))) h "left" (S(a,b) h j k)

right :: MyState ->  Int -> Int -> MyState 
right (S (a,b) h j k) x y = if b+1 >y then Null else S ((a,(b+1))) h "right" (S(a,b) h j k)



collect:: MyState -> MyState
collect (S p c j k ) = if collectHelper p c == False then Null else S p (collectHelper2 p c ) "collect" (S p c j k)


collectHelper :: Cell -> [Cell] -> Bool 
collectHelper _ []  = False
collectHelper (a,b) ((x,y):t)  = if a==x && b==y then True else collectHelper (a,b) t

collectHelper2 :: Cell -> [Cell] -> [Cell] 
collectHelper2 _ [] = []
collectHelper2 (a,b) ((x,y):t)  = if a==x && b==y then t else (x,y): collectHelper2 (a,b) t


nextMyStates :: MyState -> Int -> Int->[MyState] 
nextMyStates s x y = [q |q<- (checkUp s   ++ checkDown s x y ++ checkLeft s  ++ checkRight s x y ++ checkCollect s)]

checkUp :: MyState ->  [MyState]
checkUp s  = if up s   == Null then [] else [up s]

checkDown :: MyState ->  Int -> Int -> [MyState]
checkDown s x y  = if down s x y  == Null then [] else [down s x y ]

checkLeft :: MyState ->  [MyState]
checkLeft s  = if left s  == Null then [] else [left s  ]

checkRight :: MyState -> Int -> Int ->  [MyState]
checkRight s x y  = if right s x y  == Null then [] else [right s x y ]

checkCollect  :: MyState ->  [MyState]
checkCollect s   = if collect s  == Null then [] else [collect s  ]



isGoal::MyState->Bool 
isGoal (S p c j k ) = if c == [] then True else False


search::[MyState]->Int -> Int ->MyState
search (h:t) x y = if isGoal h then h else search `seq` search (t ++ nextMyStates h x y) x y




constructSolution :: MyState -> [String]
constructSolution Null = []
constructSolution (S _ _ "" _) = []
constructSolution (S p c j k )= constructSolution k  ++ [j]

solve :: Cell->[Cell]->[String]
solve a l = constructSolution `seq ` constructSolution (search `seq` search ([S a l "" Null ]) (getSize ([a]++ l) 0) (getSize2 ([a]++ l) 0) )

getSize :: [Cell ] ->Int -> Int
getSize [] maxX = maxX
getSize ((x,_):t) maxX = if x> maxX then getSize t x else getSize t maxX

getSize2 :: [Cell]  -> Int -> Int
getSize2 [] maxY = maxY
getSize2 ((_,y):t) maxY = if y> maxY then getSize2 t y else getSize2 t maxY



