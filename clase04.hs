--otras sumatorias 
--Ejercicio 1 

f1 n |n>0 = (2**n) + f1 (n-1)
     | n==0 = 1
f2:: Int-> Float-> Float
f2 n q | n>0 = q^n + f2 (n-1) q
       | n==0 = q 

f3:: Int -> Float -> Float 
f3 n q | n>1 = q^(2*n)+ q^(2*n-1)+ (f3 (n-1) q )
       | n==1 = q  

f4:: Int -> Float -> Float 
f4 n q = (f3 n q) - (f2 n q)