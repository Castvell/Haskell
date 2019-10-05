fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

cuad :: Int->Int
cuad x = x * x

inf :: Int
inf = 1 + inf

tres :: Integer->Integer
tres x = 3

sumar :: [Int]->Int
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

inv :: Ord a=>[a]->[a]
inv [ ] = [ ]
inv (x:xs) = (inv xs)++[x]

igualLista :: Eq a => [a]->[a]->Bool
igualLista l1 l2 = l1 == l2

listord :: Ord a=>[a]->Bool
listord [] = True
listord [_] = True
listord (x:y:xs) = (x<=y) && listord (y:xs)

mostubi :: Ord a=>[a]->Int->a
mostubi l n = l!!n

mayor :: [Int]->Int
mayor (x:xs)
 | x > mayor(xs) = x
 | otherwise = mayor(xs)

contarpares :: [Int]->Int
contarpares [] = 0
contarpares l= length [x | x <- l, mod x 2 == 0]

cuadrados :: [Int]->[Int]
cuadrados [ ] = [ ]
cuadrados l = [x * x | x <- l]

divisible :: Int->Int->Bool
divisible x y = (mod x y) ==0

divisibles :: Int->[Int]
divisibles x = [y | y <-[1..x],divisible x y]

esPrimo :: Int->Bool
esPrimo n = length (divisibles n) <=2

primos :: Int->[Int]
primos n = [x | x <-[1..n], esPrimo x]