type Cell = (Int,Int) --(row,column) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq) -- s(robotPosition [minesPositions] lastAction parentStateBeforelastAction)

up :: MyState -> MyState 
up (S (a,b) h j k) = if a-1 <0 then Null else S (((a-1),b)) h "up" (S(a,b) h j k)

down :: MyState -> MyState 
down (S (a,b) h j k) = if a+1 >3 then Null else S (((a+1),b)) h "down" (S(a,b) h j k)

left :: MyState -> MyState 
left (S (a,b) h j k) = if b-1 <0 then Null else S ((a,(b-1))) h "left" (S(a,b) h j k)

right :: MyState -> MyState 
right (S (a,b) h j k) = if b+1 >3 then Null else S ((a,(b+1))) h "right" (S(a,b) h j k)



collect:: MyState -> MyState
collect (S p c j k ) = if collectHelper p c == False then Null else S p (collectHelper2 p c ) "collect" (S p c j k)


collectHelper :: Cell -> [Cell] -> Bool 
collectHelper _ []  = False
collectHelper (a,b) ((x,y):t)  = if a==x && b==y then True else collectHelper (a,b) t

collectHelper2 :: Cell -> [Cell] -> [Cell] 
collectHelper2 _ [] = []
collectHelper2 (a,b) ((x,y):t)  = if a==x && b==y then t else (x,y): collectHelper2 (a,b) t


nextMyStates :: MyState -> [MyState] 
nextMyStates s = checkUp s ++ checkDown s ++ checkLeft s ++ checkRight s ++ checkCollect s ++ []

checkUp :: MyState -> [MyState]
checkUp s = if up s == Null then [] else [up s]

checkDown :: MyState -> [MyState]
checkDown s = if down s == Null then [] else [down s]

checkLeft :: MyState -> [MyState]
checkLeft s = if left s == Null then [] else [left s]

checkRight :: MyState -> [MyState]
checkRight s = if right s == Null then [] else [right s]

checkCollect  :: MyState -> [MyState]
checkCollect s = if collect s == Null then [] else [collect s]



isGoal::MyState->Bool 
isGoal (S p c j k ) = if c == [] then True else False


search::[MyState]->MyState
search (h:t) = if isGoal h then h else search (t ++ nextMyStates h)


constructSolution :: MyState -> [String]
constructSolution Null = []
constructSolution (S _ _ "" _) = []
constructSolution (S p c j k )= constructSolution k  ++ [j]

solve :: Cell->[Cell]->[String]
solve a l = constructSolution (search [S a l "" Null ])




