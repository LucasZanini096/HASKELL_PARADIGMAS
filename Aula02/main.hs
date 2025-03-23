


-- Função que retorna a posição de um elemento em uma lista --

onde :: Int -> [Int] -> Int -- Anotação de tipo da função onde que recebe um inteiro e uma lista de inteiros e retorna um inteiro --
onde elt lst = ond elt lst 1 -- A função onde chama a função ond passando o elemento, a lista e o índice 1 --



-- Função auxiliar que retorna a posição de um elemento em uma lista --

ond :: Int -> [Int] -> Int -> Int -- Anotação de tipo da função ond que recebe um inteiro, uma lista de inteiros, um inteiro e retorna um inteiro --
ond elt [] _ = -1 -- Se a lista estiver vazia, a função retorna -1 --
ond elt (x:xs) n -- Se a lista não estiver vazia, a função recebe um elemento, uma lista e um índice --
    | elt == x = n -- Se o elemento for igual ao primeiro elemento da lista, a função retorna o índice ( n ) --
    | otherwise = ond elt xs (n+1) -- Se o elemento não for igual ao primeiro elemento da lista, a função chama a si mesma passando o elemento, o resto da lista e o índice incrementado em 1 --





-- Função que retorna a lista até o elemento desejado --
ate _ [] = [] -- Se a lista estiver vazia, a função retorna uma lista vazia --
ate elt (a:xs) -- Se a lista não estiver vazia, a função recebe um elemento e uma lista --
    | elt == a = [] -- Se o elemento for igual ao primeiro elemento da lista, a função retorna uma lista vazia --
    | otherwise = a : ate elt xs -- Se o elemento não for igual ao primeiro elemento da lista, a função retorna o primeiro elemento da lista e chama a si mesma passando o elemento e o resto da lista --




-- Função que retorna a lista após o elemento desejado --
apos _ [] = [] -- Se a lista estiver vazia, a função retorna uma lista vazia --
apos elt (a:xs) -- Se a lista não estiver vazia, a função recebe um elemento e uma lista --
    | elt == a = xs -- Se o elemento for igual ao primeiro elemento da lista, a função retorna o resto da lista --
    | otherwise = apos elt xs -- Se o elemento não for igual ao primeiro elemento da lista, a função chama a si mesma passando o elemento e o resto da lista --




-- Função que retorna uma lista com os elementos entre dois elementos desejados --
npri n = np 1 n -- A função npri chama a função np passando 1 e o número desejado --

np m n  -- A função np recebe dois números --
    | m == n = [] -- Se os números forem iguais, a função retorna uma lista vazia --
    | otherwise = m : np (m+1) n -- Se os números não forem iguais, a função retorna o primeiro número e chama a si mesma passando o primeiro número incrementado em 1 e o segundo número --





-- Função que realiza o merge de duas listas --

merge :: [Int] -> [Int] -> [Int] -- Anotação de tipo da função merge que recebe duas listas de inteiros e retorna uma lista de inteiros --
merge [] [] = [] -- Se as duas listas estiverem vazias, a função retorna uma lista vazia --
merge [] ys = ys -- Se a primeira lista estiver vazia, a função retorna a segunda lista --
merge xs [] = xs -- Se a segunda lista estiver vazia, a função retorna a primeira lista --
merge (x:xs) (y:ys) -- Se as duas listas não estiverem vazias, a função recebe duas listas --
    | x < y = x : merge xs (y:ys) -- Se o primeiro elemento da primeira lista for menor que o primeiro elemento da segunda lista, a função retorna o primeiro elemento da primeira lista e chama a si mesma passando o resto da primeira lista e a segunda lista --
    | otherwise = y : merge (x:xs) ys -- Se o primeiro elemento da primeira lista não for menor que o primeiro elemento da segunda lista, a função retorna o primeiro elemento da segunda lista e chama a si mesma passando a primeira lista e o resto da segunda lista --





-- Função que realiza o merge sort de uma lista --
msort :: [Int] -> [Int] -- Anotação de tipo da função msort que recebe uma lista de inteiros e retorna uma lista de inteiros --
msort [] = [] -- Se a lista estiver vazia, a função retorna uma lista vazia --
msort [a] = [a] -- Se a lista tiver apenas um elemento, a função retorna a lista --
msort lst = merge (msort l1) (msort l2) -- Se a lista tiver mais de um elemento, a função retorna o merge do merge sort da primeira metade da lista e do merge sort da segunda metade da lista --
    where dupla = split lst -- A função msort chama a função split passando a lista e guarda o resultado em dupla --
          l1 = primeiro dupla -- A primeira metade da lista é o primeiro elemento da dupla --
          l2 = segundo dupla -- A segunda metade da lista é o segundo elemento da dupla --




primeiro (a,_) = a -- A função primeiro recebe uma dupla e retorna o primeiro elemento --
segundo (_,b) = b -- A função segundo recebe uma dupla e retorna o segundo elemento --



-- Função que divide uma lista em duas listas --
split :: [Int] -> ([Int], [Int]) -- Anotação de tipo da função split que recebe uma lista de inteiros e retorna uma dupla de listas de inteiros --
split lst = sp lst [] [] -- A função split chama a função sp passando a lista e duas listas vazias --
sp [] l1 l2 = (l1, l2) -- Se a lista estiver vazia, a função retorna uma dupla com as duas listas --
sp [a] l1 l2 = ((l1 ++ [a]), l2) -- Se a lista tiver apenas um elemento, a função retorna uma dupla com a primeira lista concatenada com o elemento e a segunda lista --
sp (a:(b:xs)) l1 l2 = sp xs (l1 ++ [a]) (l2 ++ [b]) -- Se a lista tiver mais de um elemento, a função chama a si mesma passando o resto da lista, a primeira lista concatenada com o primeiro elemento e a segunda lista concatenada com o segundo elemento --



-- Função que retorna se duas listas possuem o mesmo tamanho --
mesmTam :: [Int] -> [Int] -> Bool -- Anotação de tipo da função mesmTam que recebe duas listas de inteiros e retorna um booleano --
mesmTam [] [] = True -- Se as duas listas estiverem vazias, a função retorna True --
mesmoTam _ [] = False -- Se a primeira lista não estiver vazia e a segunda lista estiver vazia, a função retorna False --
mesmoTam [] _ = False -- Se a primeira lista estiver vazia e a segunda lista não estiver vazia, a função retorna False --
mesmoTam (a:as) (b:bs) = mesmoTam as bs -- Se as duas listas não estiverem vazias, a função chama a si mesma passando o resto das duas listas --




-- Função que deleta elementos duplicados em sequência de uma lista --

deldup :: [Int] -> [Int] -- Anotação de tipo da função deldup que recebe uma lista de inteiros e retorna uma lista de inteiros --
deldup [] = [] -- Se a lista estiver vazia, a função retorna uma lista vazia --
deldup [a] = [a] -- Se a lista tiver apenas um elemento, a função retorna a lista --
deldup (a:(b:xs))  -- Se a lista tiver mais de um elemento, a função recebe os dois primeiros elementos e o resto da lista --
    | a == b = deldup (b:xs) -- Se os dois primeiros elementos forem iguais, a função chama a si mesma passando o segundo elemento e o resto da lista --
    | otherwise = a : deldup (b:xs) -- Se os dois primeiros elementos não forem iguais, a função retorna o primeiro elemento e chama a si mesma passando o segundo elemento e o resto da lista --





 -- Função que retorna o menor número de uma lista de números --
menor :: [Int] -> Int -- Anotação de tipo da função menor que recebe uma lista de inteiros e retorna um inteiro --
menor (a:xs) = men xs a 





-- Função auxiliar da função menor --
men :: [Int] -> Int -> Int 
men [] ac = ac -- Se a lista estiver vazia, a função retorna o acumulador --
men (a:xs) ac -- Se a lista não estiver vazia, a função recebe um elemento e uma lista --
    | a < ac = men xs a  -- Se o elemento for menor que o acumulador, a função chama a si mesma passando o resto da lista e o elemento --
    | otherwise = men xs ac -- Se o elemento não for menor que o acumulador, a função chama a si mesma passando o resto da lista e o acumulador --





-- Função que retorna uma lista de números menores que um número desejado --
menores :: Int -> [Int] -> [Int] -- Anotação de tipo da função menores que recebe um inteiro e uma lista de inteiros e retorna uma lista de inteiros --
menores _ [] = [] -- Se a lista estiver vazia, a função retorna uma lista vazia --
menores elt (a:xs) -- Se a lista não estiver vazia, a função recebe um elemento e uma lista --
    | a < elt = a : menores elt xs -- Se o elemento for menor que o número desejado, a função retorna o elemento e chama a si mesma passando o número desejado e o resto da lista --
    | otherwise = menores elt xs -- Se o elemento não for menor que o número desejado, a função chama a si mesma passando o número desejado e o resto da lista --


-- Função que retorna uma lista de números menores que um número desejado --

menores2 :: Int -> [Int] -> [Int] -- Anotação de tipo da função menores2 que recebe um inteiro e uma lista de inteiros e retorna uma lista de inteiros --
menores2 elt lst = m2 elt lst [] -- A função menores2 chama a função m2 passando o número desejado, a lista e uma lista vazia --

-- Função auxiliar da função menores2 --

m2 :: Int -> [Int] -> [Int] -> [Int] -- Anotação de tipo da função m2 que recebe um inteiro, uma lista de inteiros e uma lista de inteiros e retorna uma lista de inteiros --
m2 _ [] lac = lac -- Se a lista estiver vazia, a função retorna a lista acumuladora --
m2 elt (a:xs) lac -- Se a lista não estiver vazia, a função recebe um elemento, uma lista e uma lista acumuladora --
    | a < elt = m2 elt xs (lac ++ [a]) -- Se o elemento for menor que o número desejado, a função chama a si mesma passando o número desejado, o resto da lista e a lista acumuladora concatenada com o elemento --
    | otherwise = m2 elt xs lac -- Se o elemento não for menor que o número desejado, a função chama a si mesma passando o número desejado, o resto da lista e a lista acumuladora --


main :: IO()
main = do
    print (merge [1,3,5] [2,4,6])
    print (msort [5,3,1,2,4,6])
    print (split [1,2,3,4,5,6,7,8,9,10])