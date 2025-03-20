



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
msort [] = [] -- Se a lista estiver vazia, a função retorna uma lista vazia --
msort [a] = [a] -- Se a lista tiver apenas um elemento, a função retorna a lista --
msort lst = merge (msort l1) (msort l2) -- Se a lista tiver mais de um elemento, a função retorna o merge do merge sort da primeira metade da lista e do merge sort da segunda metade da lista --
    where dupla = split lst -- A função msort chama a função split passando a lista e guarda o resultado em dupla --
          l1 = primeiro dupla -- A primeira metade da lista é o primeiro elemento da dupla --
          l2 = segundo dupla -- A segunda metade da lista é o segundo elemento da dupla --

primeiro (a,_) = a -- A função primeiro recebe uma dupla e retorna o primeiro elemento --
segundo (_,b) = b -- A função segundo recebe uma dupla e retorna o segundo elemento --

split :: [Int] -> ([Int], [Int]) -- Anotação de tipo da função split que recebe uma lista de inteiros e retorna uma dupla de listas de inteiros --
split lst = sp lst [] [] -- A função split chama a função sp passando a lista e duas listas vazias --
sp [] l1 l2 = (l1, l2) -- Se a lista estiver vazia, a função retorna uma dupla com as duas listas --
sp [a] l1 l2 = ((l1 ++ [a]), l2) -- Se a lista tiver apenas um elemento, a função retorna uma dupla com a primeira lista concatenada com o elemento e a segunda lista --
sp (a:(b:xs)) l1 l2 = sp xs (l1 ++ [a]) (l2 ++ [b]) -- Se a lista tiver mais de um elemento, a função chama a si mesma passando o resto da lista, a primeira lista concatenada com o primeiro elemento e a segunda lista concatenada com o segundo elemento --

main :: IO()
main = do
    print (merge [1,3,5] [2,4,6])
    print (msort [5,3,1,2,4,6])
    print (split [1,2,3,4,5,6,7,8,9,10])