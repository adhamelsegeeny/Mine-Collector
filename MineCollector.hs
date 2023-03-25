type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq,Ord)

up  (S (0,y) ((a,b):h) s _)= Null
up (S (x,y) ((a,b):h) s next)= (S (x-1,y) ((a,b):h) "up" (S (x,y) ((a,b):h) s next))

down  (S (x,y) ((a,b):h) s next) max= if(x==max) then Null
							else (S (x+1,y) ((a,b):h) "down" (S (x,y) ((a,b):h) s next))


left  (S (y,0) ((a,b):h) s _)= Null
left (S (x,y) ((a,b):h) s next)=(S (x,y-1) ((a,b):h) "left" (S (x,y) ((a,b):h) s next))

right  (S (x,y) ((a,b):h) s next) max= if(y==max) then Null
								else (S (x,y+1) ((a,b):h) "right" (S (x,y) ((a,b):h) s next))

collect (S (x,y) l s next)= (S (x,y) (checkList l (x,y)) "collect" (S (x,y) l s next))

filter1 (S (c,d) ((a,b):t) s (S (x,y) l string next))= if(man (x,y) (a,b) < man (c,d) (a,b)) then Null
															else (S (c,d) ((a,b):t) s (S (x,y) l string next))

checkList [] (a,b)= []				
checkList (x:xs) (a,b)= if ((a,b)==x) then xs
						else x:checkList xs (a,b)
						
nextMyStates (S (x,y) l s next) max1 max2 =checkUp (S (x,y) l s next) ++ checkDown (S (x,y) l s next) max1 ++ checkLeft (S (x,y) l s next) ++ checkRight (S (x,y) l s next) max2 ++ checkCollect (S (x,y) l s next) 


checkUp (S (x,y) l s next) = if(filterU (x,y) l==False) then [] 
							else if(up (S (x,y) l s next)== Null) then []
                           else [up (S (x,y) l s next)]
						   
checkDown (S (x,y) l s next) max= if(filterD (x,y) l==False) then []
							else if(down (S (x,y) l s next) max== Null) then []
                           else [down (S (x,y) l s next) max]
						   
checkRight (S (x,y) l s next) max= if(filterR (x,y) l==False) then []
							else if(right (S (x,y) l s next) max== Null) then []
                           else [right (S (x,y) l s next) max]
						
checkLeft (S (x,y) l s next)= if(filterL (x,y) l==False) then []
							else if(left (S (x,y) l s next)== Null) then []
                           else [left (S (x,y) l s next)]
						   
checkCollect (S (x,y) l s next)= if(checkList l (x,y) == l) then []
								else [collect (S (x,y) l s next)]
								
isGoal (S (x,y) l s next)= if( l==[] ) then True
							else False

search (a:xs) max1 max2= if(isGoal a==True) then a
               else search (xs ++ (nextMyStates a max1 max2)) max1 max2

constructSolutions Null= []			   
constructSolutions (S (_,_) l action next)= if(action/="")then action:constructSolutions next
											else constructSolutions next

maxR [] (max,_)=max
maxR ((x,_):xs) (max,any2)= if(x>max) then maxR xs (x,any2)	
                            else maxR xs (max,any2)
			
maxC [] (_,max)=max
maxC ((_,x):xs) (any2,max)= if(x>max) then maxC xs (any2,x)	
                            else maxC xs (any2,max)
				
size l (x,y) = if((maxR l (x,y))*(maxC l (x,y))< length l) then chooseSize (length l) (maxR l (x,y)) (maxC l (x,y))
											else [(maxR l (x,y),maxC l (x,y))] ++ []

chooseSize l x y= if((x+1)*(y+1)>l) then [(x+1,y+1)] ++ []
					else chooseSize l (x+1) (y+1)
chooseX [(x,y)]=x
chooseY [(x,y)]=y


man (x,y) (a,b)= abs(x-a)+abs(y-b)



---insertionSort :: Ord a => [a] -> (a,a) -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert (x) (insertionSort (xs))
   
insert x [] = [x]
insert x [(c,d)] = [(c,d),x]
insert (i,j) ((x,y):(c,d):ys)= if (man (x,y) (i,j) <= man (c,d) (x,y)) then  (x,y) : (i,j) : (c,d) : ys
              else (x,y) : (insert (i,j) ((c,d):ys) )

solve (x,y) l= reverse (constructSolutions (search (nextMyStates (S (x,y) (insertionSort (insertLast (nearest (x,y) l (100,100)) (removeItem (nearest (x,y) l (100,100)) l)) ) "" Null) (chooseX (size l (x,y))) (chooseY (size l (x,y)))) (chooseX (size l (x,y))) (chooseY (size l (x,y)))))
					
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
		
insertLast x [] =[x]
insertLast x (y:ys)= y:(ys++[x])	


nearest x [] min= min
nearest x (h:t) min= if(man x h< man x min) then nearest x t h
					else nearest x t min
		

		
filterD (x,y) ((a,b):h)= if(a>x) then True
					else False
					
filterU (x,y) ((a,b):h)= if(a<x) then True
					else False

filterL (x,y) ((a,b):h)= if(y>b) then True
					else False
					
filterR (x,y) ((a,b):h)= if(y<b) then True
					else False

		
