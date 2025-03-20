main :: IO()

main = do -- do é uma palavra chave que indica que a função main é uma função que executa uma sequência de ações
  putStrLn "Digite um número"  -- putStrLn é uma função que imprime uma string na tela
  numero <- getLine -- getLine é uma função que lê uma string da entrada padrão
  let numeroInt = read numero :: Int -- read é uma função que converte uma string para um tipo específico
  putStrLn ("O número digitado foi: " ++  show numeroInt) -- show é uma função que converte um tipo específico para uma string
  putStrLn ("O dobro do número digitado é: " ++ show (2 * numeroInt)) -- 2 * numeroInt é uma expressão que multiplica o número digitado por 2