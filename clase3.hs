finobacci:: Int -> Int

finobacci n | n==0 =0
            | n==1 = 1
            |otherwise = finobacci(n-2)+finobacci (n-1)

parteEntera:: Float -> Integer
parteEntera x | (1<= x) && (x<2)= 1
              | (0<= x) && (x<1)= 0
              | otherwise = parteEntera(x-1) +1

multiploDe3:: Int -> Bool
multiploDe3 n | n==3 = True 
              | n==1 = False
              |otherwise= multiploDe3(n-3)

sumaImpares:: Int -> Int 
sumaImpares n | n== 1 = 1 
              | n == 0 = 0 
              | otherwise = sumaImpares(n-1)+ 2*n- 1

medioFact:: Int -> Int 
medioFact n | n==2 = 2
            | n==1 = 1
            | otherwise= medioFact(n-2)*n

sumaDigitos:: Int -> Int 
sumaDigitos n| (1<=n)&&(n<=9) = n
             | otherwise = sumaDigitos(div n 10)+ (mod n 10) 

digitosIguales:: Int -> Bool
digitosIguales n | (n>=10) && (n<100) = (div n 10)== (mod n 10)
                 | otherwise= digitosIguales(div n 10) && digitosIguales(mod n 100)