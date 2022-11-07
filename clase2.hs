estanRelacionados x y | (x <(-3)) && (y<(-3)) = True 
                      | (x >7) && (y>7) = True 
                      | (x>(-3)) && (x<7) && (y>(-3)) && (y<7)= True 
                      | otherwise = False

prodInt:: (Float, Float) -> (Float, Float) -> Float
prodInt (vx,vy) (wx, wy)= (vx*wx) + (vy*wy) 

todoMenor:: (Float, Float) -> (Float, Float) -> Bool 
todoMenor (vx, vy) (wx, wy) | (vx < wx) && (vy< wy)= True 
                            | otherwise = False 

distanciaPuntos:: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1,y1) (x2,y2)= sqrt ((x1-x2)^2 + (y1-y2)^2)

sumaTerna:: (Float, Float, Float) -> Float
sumaTerna (x1, x2, x3)= (x1 + x2 + x3)

posicPrimerPar:: (Int, Int, Int) -> Int
posicPrimerPar (x1,x2,x3) | (mod x1 2)==0 = 1
                          | (mod x2 2)==0 = 2
                          | (mod x3 2)==0 = 3
                          | otherwise = 4

crearPar:: (Eq t, Eq u ) => t -> u -> (t,u)
crearPar x y = (x,y)

invertirPar:: (Eq t, Eq u) => (t,u) -> (u,t)
invertirPar (x,y)= (y,x)