import System.Posix.Internals (lstat)




-- Declaração uma lista de números inteiros --

numbers :: [Int] -- Anotação para a variável numbers, que indica que ela é uma lista de inteiros --
numbers = [1,2,3,4] -- Atribuição de valores para a variável numbers --




-- Função que verifica a presença de um número em uma lista --

membro :: Int -> [Int] -> Bool -- Anotação para a função membro, que indica que ela recebe um inteiro e uma lista de inteiros e retorna um booleano --
membro _ [] = False -- Caso base da função membro, que retorna False caso a lista esteja vazia --
membro elt (a:xs) -- Caso recursivo da função membro, que recebe um elemento (elt) e uma lista (a:xs) --
      | elt == a = True -- Caso o elemento procurado seja igual ao primeiro elemento da lista, a função retorna True --
      | otherwise = membro elt xs -- Caso contrário, a função chama a si mesma, passando o elemento procurado e o restante da lista --




-- Função que retorna a interseção entre duas listas de números inteiros --

inter :: [Int] -> [Int] -> [Int] -- Anotação para a função inter, que indica que ela recebe duas listas de inteiros e retorna uma lista de inteiros --
inter [] _ = [] -- Caso base da função inter, que retorna uma lista vazia caso a primeira lista esteja vazia --
inter _ [] = [] -- Caso base da função inter, que retorna uma lista vazia caso a segunda lista esteja vazia --
inter (a:xs) lst -- Caso recursivo da função inter, que recebe uma lista (a:xs) e uma lista (lst) --
      | membro a lst = a : inter xs lst -- Caso o primeiro elemento da lista esteja presente na segunda lista, a função retorna uma lista contendo o elemento e chama a si mesma, passando o restante da primeira lista e a segunda lista --
      | otherwise = inter xs lst -- Caso contrário, a função chama a si mesma, passando o restante da primeira lista e a segunda lista --



-- Função que retorna o primeiro elemento de uma lista de números inteiros --

primeiro :: [Int] -> Int -- Anotação para a função primeiro, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
primeiro (a:xs) = a -- Retorno da função primeiro, que retorna o primeiro elemento da lista definido pela head da lista ( a ) --




-- Função que retorna o último elemento de uma lista de números inteiros --

ultimo :: [Int] -> Int -- Anotação para a função ultimo, que indica que ela recebe uma lista de inteiros e retorna o último elemento da lista --
ultimo [a] = a -- Caso base da função ultimo, que retorna o único elemento da lista caso ela tenha apenas um elemento --
ultimo (a:xs) = ultimo xs -- Caso recursivo da função ultimo, que chama a si mesma, passando o restante da lista --




 -- Função que retorna a soma do primeiro e último elementos de uma lista de números inteiros --

soma_pri_ult :: [Int] -> Int  -- Anotação para a função soma_pri_ult, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
soma_pri_ult [] = 0 -- Caso base da função soma_pri_ult, que retorna 0 caso a lista esteja vazia --
soma_pri_ult lst = (primeiro lst) + (ultimo lst) -- Retorno da função soma_pri_ult, que retorna a soma do primeiro e último elementos da lista --



-- Função que retorna uma lista sem o primeiro elemento especificado por elt ( element ) --

delfirst :: Int -> [Int] -> [Int] -- Anotação para a função delfirst, que indica que ela recebe um inteiro e uma lista de inteiros e retorna uma lista de inteiros --
delfirst _ [] = [] -- Caso base da função delfirst, que retorna uma lista vazia caso a lista esteja vazia --
delfirst elt (x:xs) -- Caso recursivo da função delfirst, que recebe um elemento (elt) e uma lista (x:xs) --
      | elt == x = xs -- Caso o elemento procurado seja igual ao primeiro elemento da lista, a função retorna o restante da lista --
      | otherwise = x : delfirst elt xs -- Caso contrário, a função retorna o primeiro elemento da lista e chama a si mesma, passando o elemento procurado e o restante da lista --




-- Função que retorna o segundo elemento de uma lista de números inteiros --

segundo :: [Int] -> Int -- Anotação para a função segundo, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
segundo [a,b] = b -- Caso base da função segundo, que retorna o segundo elemento da lista caso ela tenha apenas dois elementos --
segundo (a:(b:xs)) = b -- Segundo caso em que a lista possui mais de dois elementos, a função retorna o segundo elemento da lista --




-- Função que retorna o penúltimo elemento de uma lista de números inteiros --

penultimo :: [Int] -> Int -- Anotação para a função penultimo, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
penultimo [a,b] = a -- Caso base da função penultimo, que retorna o primeiro elemento da lista caso ela tenha apenas dois elementos --
penultimo (a:xs) = penultimo xs -- Caso recursivo da função penultimo, que chama a si mesma, passando o restante da lista --




-- Função que retorna a soma do segundo e penúltimo elementos de uma lista de números inteiros --

sum_sec_penult :: [Int] -> Int -- Anotação para a função sum_sec_penult, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
sum_sec_penult [] = 0 -- Caso base da função sum_sec_penult, que retorna 0 caso a lista esteja vazia --
sum_sec_penult lst = (segundo lst) + (penultimo lst) -- Retorno da função sum_sec_penult, que retorna a soma do segundo e penúltimo elementos da lista --




-- Função que retorna uma lista sem o último elemento especificado por elt ( element ) --

delall :: Int -> [Int] -> [Int] -- Anotação para a função delall, que indica que ela recebe um inteiro e uma lista de inteiros e retorna uma lista de inteiros --
delall _ [] = [] -- Caso base da função delall, que retorna uma lista vazia caso a lista esteja vazia --
delall [] _ = [] -- Caso base da função delall, que retorna uma lista vazia caso a lista esteja vazia --
delall elt (x:xs) -- Caso recursivo da função delall, que recebe um elemento (elt) e uma lista (x:xs) --
      | elt == x = delall elt xs -- Caso o elemento procurado seja igual ao primeiro elemento da lista, a função chama a si mesma, passando o elemento procurado e o restante da lista --
      | otherwise = x : delall elt xs -- Caso contrário, a função retorna o primeiro elemento da lista e chama a si mesma, passando o elemento procurado e o restante da lista --




-- Função auxiliar que retorna o menor elemento de uma lista de números inteiros --
low :: Int -> [Int] -> Int -- Anotação para a função low, que indica que ela recebe um inteiro e uma lista de inteiros e retorna um inteiro --
low ac [] = ac -- Caso base da função low, que retorna o menor elemento da lista --
low ac (a:xs) -- Caso recursivo da função low, que recebe um elemento (ac) e uma lista (a:xs) --
      | a < ac = low a xs -- Caso o elemento atual seja menor que o menor elemento atual, a função chama a si mesma, passando o elemento atual e o restante da lista --
      | otherwise = low ac xs -- Caso contrário, a função chama a si mesma, passando o menor elemento atual e o restante da lista --




-- Função que retorna o menor elemento de uma lista de números inteiros --

lower :: [t] -> t -- Anotação para a função lower, que indica que ela recebe uma lista de qualquer tipo ( t ) e retorna um elemento do mesmo tipo ( t ) --
lower [] = "Lista vazia" -- Caso base da função lower, que retorna uma mensagem caso a lista esteja vazia --
lower (a:xs) = low a xs -- Retorno da função lower, que retorna o menor elemento da lista --




-- Função que retorna a soma de todos os elementos de uma lista de números inteiros --

soma :: [Int] -> Int -- Anotação para a função soma, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
soma [] = 0 -- Caso base da função soma, que retorna 0 caso a lista esteja vazia --
soma (a:xs) = a + soma xs  -- Retorno da função soma, que retorna a soma do primeiro elemento da lista com a chamada recursiva da função, passando o restante da lista --




-- Função que retorna a soma de todos os elementos de uma lista de números inteiros, exceto o primeiro --
soma2 :: [Int] -> Int -- Anotação para a função soma2, que indica que ela recebe uma lista de inteiros e retorna um inteiro --
soma2 [] = 0 -- Caso base da função soma2, que retorna 0 caso a lista esteja vazia --
soma2 (a:xs) = s a xs -- Retorno da função soma2, que retorna a chamada da função auxiliar s, passando o primeiro elemento da lista e o restante da lista --



-- Função auxiliar que retorna a soma de todos os elementos de uma lista de números inteiros --
s :: Int -> [Int] -> Int -- Anotação para a função s, que indica que ela recebe um inteiro e uma lista de inteiros e retorna um inteiro, sendo um somador --
s ac [] = ac -- Caso base da função s, que retorna o somador caso a lista esteja vazia --
s ac (a:xs) = s (ac + a) xs -- Caso recursivo da função s, que retorna a soma do somador com o elemento atual e chama a si mesma, passando a soma e o restante da lista --


main :: IO()
main = do
  print (inter [1,2,3,4] [3,4,5,6]) -- [3,4]
  print (membro 3 [1,2,3,4]) -- True
  print (primeiro numbers) -- 1
  print (ultimo numbers) -- 4
  print (soma_pri_ult numbers) -- 5
  print (delfirst 3 numbers) -- [1,2,4]
  print (segundo numbers) -- 2
  print (penultimo numbers) -- 3
  print (sum_sec_penult numbers) -- 6
  print (delall 3 numbers) -- [1,2,4]
  print (lower numbers) -- 1
  print (soma numbers) -- 10
  print (soma2 numbers) -- 9
  