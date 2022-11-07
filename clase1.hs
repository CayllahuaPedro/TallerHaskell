absoluto:: Int-> Int 
absoluto x |x>0 = x 
           | otherwise = -x

maximoabsoluto:: Int -> Int -> Int 
maximoabsoluto x y | (absoluto x) > (absoluto y) = x 
                 | (absoluto y)  <= (absoluto y)= y 

maximo2:: Int -> Int -> Int 
maximo2 x y | x > y = x 
           | x <= y = y 

maximo3:: Int -> Int -> Int -> Int 
maximo3 x y z | x > (maximo2 y z) = x 
             | y > (maximo2 x z) = y 
             | z > (maximo2 x y) = z 

algunoEs0:: Int -> Int -> Bool 
algunoEs0 x y | x == 0 = True 
              | y == 0 = True 
              | otherwise = False 

algunoEs0pattern:: Int -> Int -> Bool 
algunoEs0pattern 0 y = True
algunoEs0pattern x 0 = True 
algunoEs0pattern x y = False

ambosSon0:: Int -> Int -> Bool 
ambosSon0 x y |( x == 0) && (y ==0 ) = True
             | otherwise = False

ambosSon0pattern:: Int -> Int -> Bool 
ambosSon0pattern 0 0 = True 
ambosSon0pattern x y = False

esMultiploDe:: Int -> Int -> Bool 
esMultiploDe x y | (mod x y) == 0 = True 
               | otherwise = False

digitoUnidades:: Int -> Int 
digitoUnidades x= (mod x 10)

digitoCentenas:: Int -> Int 
digitoCentenas x| x<999 = (div x 100)
                | x>999 = digitoCentenas(mod x 1000)
                