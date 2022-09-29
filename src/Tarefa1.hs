module Tarefa1 where

import System.Random
import Types

-- * Testes tarefa 1

-- | gera um labirinto de largura par e altura impar
t1 :: (Int, Int, Int)
t1 = (20,21,30)

-- | gera um labirinto de largura impar e altura par
t2 :: (Int, Int, Int)
t2 = (41,40,30)

-- | gera um labirinto de largura e altura par
t3 :: (Int, Int, Int)
t3 = (20,20,30)

-- | gera um labirinto de largura e altura impar
t4 :: (Int, Int, Int)
t4 = (41,21,30)


-- | Given a seed returns a list of n integer randomly generated
--
geraAleatorios :: Int   -- ^ número de numeros que queremos gerar
               -> Int   -- ^ número aleatório
               -> [Int] -- ^ lista de números aleatórios
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-99


-- | Converts an integer number into a Peca
-- 3 <=> Food Big
-- 0 <= n < 70 <=> Food Little
-- 70 < n <= 99 <=> Wall
--
convertePeca :: Int   -- ^ número para converter a peça
             -> Piece -- ^ peça resultante
convertePeca peca | peca == 3 = Food Big
                  | peca >= 0 && peca < 70 = Food Little
                  | peca >= 70 && peca <= 99 = Wall


-- | Converts a Corridor to a string
--
printCorridor :: Corridor -- ^ corredor 
              -> String   -- ^ corredor convertido numa string
printCorridor corridor = show corridor


-- | Converts a Maze to a string
--
printMaze :: Maze   -- ^ labirinto
          -> String -- ^ labirinto convertido numa string
printMaze maze = show maze


-- | Converts a list of integers into a Corridor
--
converteCorridor :: [Int]    -- ^ lista de números aleatórios
                 -> Corridor -- ^ corredor resultante dos números aleatórios
converteCorridor [] = []
converteCorridor (h:t) = (convertePeca h) : (converteCorridor t)

-- | Forma o labirinto com a lista de peças
criaCorridor :: Int      -- ^ largura do labirinto
             -> Corridor -- ^ lista de peças
             -> Maze     -- ^ labirinto resultante dos corredores
criaCorridor l [] = []
criaCorridor l ps =
                  let h = take l ps
                      t = drop l ps
                  in h:(criaCorridor l t) 

-- | Gerar o labirinto
generateMaze :: Int  -- ^ largura do labirinto
             -> Int  -- ^ altura do labirinto
             -> Int  -- ^ random seed
             -> Maze -- ^ labirinto obtido
generateMaze l a s = 
      let nr = (l-2) * (a-2)
          randNrs   = geraAleatorios nr s
          pecas     = converteCorridor randNrs
          lab1      = criaCorridor (l-2) pecas 
          sideWalls = geraWallsLaterais lab1
          wall      = geraWallsSupInf l
          walls     = (wall:sideWalls) ++ [wall]
          tunel     = novoLab walls (div a 2)
          tunel2    = if mod a 2 == 0 then novoLabPar tunel (div a 2) else tunel 
          casaFantasmas | mod l 2 /= 0 && mod a 2 /= 0 = geraMazeImpImp tunel2 a 
                        | mod l 2 == 0 && mod a 2 /= 0 = geraMazeParImp tunel2 a
                        | mod l 2 /=0 && mod a 2 ==0 = geraMazeImpPar tunel2 a
                        | otherwise = geraMazeParPar tunel2 a
       in casaFantasmas 


-- | Gerar a casa dos fantamas no labirinto com largura e altura impar
geraMazeImpImp :: Maze -- ^ labirinto sem a casa dos fantasmas
               -> Int  -- ^ altura do labirinto
               -> Maze -- ^ labirinto resultante com a casa dos fantasmas
geraMazeImpImp m a =
        let casaMeio    = casaMeioLabImpar m (div a 2) 
            casaCima    = casaCimaLabImpar casaMeio ((div a 2)-1)
            casaBaixo   = casaBaixoLabImpar casaCima ((div a 2)+1) 
            limparcima  = limparLab casaBaixo ((div a 2)-2)
            limparBaixo = limparLab limparcima ((div a 2)+2)
        in limparBaixo

-- | Gerar a casa dos fantamas no labirinto com largura par e altura impar
geraMazeParImp :: Maze -- ^ labirinto sem a casa dos fantasmas
               -> Int  -- ^ altura do labirinto
               -> Maze -- ^ labirinto resultante com a casa dos fantasmas
geraMazeParImp m a =
        let casaMeio    = casaMeioLabPar m (div a 2) 
            casaCima    = casaCimaLabPar casaMeio ((div a 2)-1) 
            casaBaixo   = casaBaixoLabPar casaCima ((div a 2)+1) 
            limparcima  = limparParLab casaBaixo ((div a 2)-2)
            limparBaixo = limparParLab limparcima ((div a 2)+2)
        in limparBaixo

-- | Gerar a casa dos fantamas no labirinto com largura impar e altura par
geraMazeImpPar :: Maze -- ^ labirinto sem a casa dos fantasmas
               -> Int  -- ^ altura do labirinto
               -> Maze -- ^ labirinto resultante com a casa dos fantasmas
geraMazeImpPar m a =
        let casaMeio    = casaMeioLabImpar m ((div a 2)-1)
            casaCima    = casaCimaLabImpar casaMeio ((div a 2)-2) 
            casaBaixo   = casaBaixoLabImpar casaCima (div a 2) 
            limparcima  = limparLab casaBaixo ((div a 2)+1)
            limparBaixo = limparLab limparcima ((div a 2)-3)
        in limparBaixo

-- | Gerar a casa dos fantamas no labirinto com largura e altura par
geraMazeParPar :: Maze -- ^ labirinto sem a casa dos fantasmas
               -> Int  -- ^ altura do labirinto
               -> Maze -- ^ labirinto resultante com a casa dos fantasmas
geraMazeParPar m a =
        let casaMeio    = casaMeioLabPar m ((div a 2)-1) 
            casaCima    = casaCimaLabPar casaMeio ((div a 2)-2) 
            casaBaixo   = casaBaixoLabPar casaCima (div a 2)
            limparcima  = limparParLab casaBaixo ((div a 2)-3)
            limparBaixo = limparParLab limparcima ((div a 2)+1)
        in limparBaixo


-- | Cria as paredes superiores e inferiores do labirinto
geraWallsSupInf :: Int      -- ^ largura do labirinto
                -> Corridor -- ^ corredor obtido de gerar as paredes inferiores e superiores
geraWallsSupInf tam = (replicate tam Wall )

-- | Cria as paredes laterais do labirinto
geraWallsLaterais :: Maze -- ^ labirinto sem as paredes laterais
                  -> Maze -- ^ labirinto resultante de gerar as paredes laterais
geraWallsLaterais [] = []
geraWallsLaterais (c:cs) = (Wall:c ++ [Wall]) : (geraWallsLaterais cs)


-- | Gerar o tunel no corredor respetivo
geraTunel :: Corridor -- ^ corredor sem o tunel
          -> Corridor -- ^ corredor resultante de colocar o tunel
geraTunel (h:t) = Empty:(init t) ++ [Empty]


-- | Gerar labirinto com o tunel
novoLab :: Maze -- ^ labirinto para colocar o tunel 
        -> Int  -- ^ lugar onde colocar o tunel
        -> Maze -- ^ labirinto resultante
novoLab (h:t) n | n == 0 = (geraTunel h ):t
                | otherwise = h:(novoLab t (n-1))

-- | se a altura do Maze for par acrescenta outro tunel a linha de cima
novoLabPar :: Maze -- ^ labirinto para colocar o tunel 
           -> Int  -- ^ lugar onde colocar o tunel
           -> Maze -- ^ labirinto resultante
novoLabPar (h:t) n | n == 1 = (geraTunel h):t
                   | otherwise = h:(novoLabPar t (n-1))


-- | Gerar a parte do meio da casa nos Corredores nos labirintos cuja largura é par
geraCasaMeioPar :: Corridor -- ^ corredor sem a casa dos fantasmas
                -> Corridor -- ^ corredor com a casa dos fantasmas
geraCasaMeioPar l = geraCasaMeioAux l (length l) 0

-- | funçao auxiliar para gerar a parte do meio da casa nos Corredores nos labirintos cuja largura é par
geraCasaMeioAux (h:t) comp indice | indice < i  = h:geraCasaMeioAux t comp (indice+1)
                                  | indice == i = casa ++ geraCasaMeioAux (drop 8 t) comp (indice+8)
                                  | otherwise   = t
                       where i    = (div (comp-10) 2)
                             casa = [Empty] ++ [Wall] ++ (replicate 6 Empty) ++ [Wall] ++ [Empty]

-- | Gerar a parte de baixo da casa nos Corredores nos labirintos cuja largura é par
geraCasaBaixoPar :: Corridor -- ^ corredor sem a casa dos fantasmas
                 -> Corridor -- ^ corredor com a casa dos fantasmas
geraCasaBaixoPar l = geraCasaBaixoAux l (length l) 0

-- | funçao auxiliar para gerar a parte de baixo da casa nos Corredores nos labirintos cuja largura é par
geraCasaBaixoAux (h:t) comp indice | indice < i  = h:geraCasaBaixoAux t comp (indice+1)
                                   | indice == i = casa ++ geraCasaBaixoAux (drop 8 t) comp (indice+8)
                                   | otherwise   = t
                        where i    = (div (comp-10) 2)
                              casa = [Empty] ++ (replicate 8 Wall) ++ [Empty]

-- | Gerar a parte de cima da casa nos Corredores nos labirintos cuja largura é par
geraCasaCimaPar :: Corridor -- ^ corredor sem a casa dos fantasmas
                -> Corridor -- ^ corredor com a casa dos fantasmas
geraCasaCimaPar l = geraCasaCimaAux l (length l) 0

-- | funçao auxiliar para gerar a parte de cima da casa nos Corredores nos labirintos cuja largura é par
geraCasaCimaAux (h:t) comp indice | indice < i  = h:geraCasaCimaAux t comp (indice+1)
                                  | indice == i = casa ++ geraCasaCimaAux (drop 8 t) comp (indice+8)
                                  | otherwise   = t
                       where i    = (div (comp-10) 2)
                             casa = [Empty] ++ (replicate 3 Wall) ++ [Empty] ++ [Empty] ++ (replicate 3 Wall) ++ [Empty]


-- | Gerar a parte do meio da casa nos Corredores nos labirintos cuja largura é impar
geraCasaMeioImpar :: Corridor -- ^ corredor sem a casa dos fantasmas
                  -> Corridor -- ^ corredor com a casa dos fantasmas
geraCasaMeioImpar l = geraCasaMeioAux2 l (length l) 0

-- | funçao auxiliar para gerar a parte do meio da casa nos Corredores nos labirintos cuja largura é impar
geraCasaMeioAux2 (h:t) comp indice | indice <  i = h:geraCasaMeioAux2 t comp (indice+1)
                                   | indice == i = casa ++ geraCasaMeioAux2 (drop 9 t) comp (indice+9)
                                   | otherwise = t
                        where i    = (div (comp-11) 2)
                              casa = [Empty] ++ [Wall] ++ (replicate 7 Empty) ++ [Wall] ++ [Empty]

-- | Gerar a parte de baixo da casa nos Corredores nos labirintos cuja largura é impar
geraCasaBaixoImpar :: Corridor -- ^ corredor sem a casa dos fantasmas
                   -> Corridor -- ^ corredor com a casa dos fantasmas
geraCasaBaixoImpar l = geraCasaBaixoImpAux l (length l) 0

-- | função auxiliar para gerar a parte de baixo da casa nos Corredores nos labirintos cuja largura é impar
geraCasaBaixoImpAux (h:t) comp indice | indice < i  = h:geraCasaBaixoImpAux t comp (indice+1)
                                      | indice == i = casa ++ geraCasaBaixoImpAux (drop 9 t) comp (indice+9)
                                      | otherwise   = t
                        where i    = (div (comp-11) 2)
                              casa = [Empty] ++ (replicate 9 Wall) ++ [Empty]

-- | Gerar a parte de cima da casa nos Corredores nos labirintos cuja largura é impar
geraCasaCImpar :: Corridor -- ^ corredor sem a casa dos fantasmas
               -> Corridor -- ^ corredor com a casa dos fantasmas
geraCasaCImpar l = geraCasaCimparAux l (length l) 0

-- | funçao auxiliar para gerar a parte de cima da casa nos Corredores nos labirintos cuja largura é impar
geraCasaCimparAux (h:t) comp indice | indice <  i = h:geraCasaCimparAux t comp (indice+1)
                                    | indice == i = casa ++ geraCasaCimparAux (drop 9 t) comp (indice+9)
                                    | otherwise   = t
                                where i=(div (comp-11) 2)
                                      casa = [Empty] ++ (replicate 3 Wall) ++ (replicate 3 Empty) ++ (replicate 3 Wall) ++ [Empty]

-- | limpar a volta da casa dos fantasmas no labirinto cuja largura é par
limpaPar :: Corridor -- ^ corredor 
         -> Corridor -- ^ corredor limpo a volta da casa
limpaPar l = limpaParAux l (length l) 0

-- | funçao auxiliar para limpar a volta da casa dos fantasmas no labirinto cuja largura é par
limpaParAux (h:t) comp indice | indice <  i = h:limpaParAux t comp (indice+1)
                              | indice == i = (replicate 10 Empty) ++ limpaParAux (drop 8 t) comp (indice+8)
                              | otherwise   = t
                      where i = (div (comp-10) 2)

-- | limpar a volta da casa dos fantasmas no labirinto cuja largura é impar
limpar :: Corridor -- ^ corredor
       -> Corridor -- ^ corredor limpo a volta da casa
limpar l = limparAux l (length l) 0

-- | função auxiliar para limpar a volta da casa dos fantasmas no labirinto cuja largura é impar
limparAux (h:t) comp indice | indice <  i  = h:limparAux t comp (indice+1)
                            | indice == i  = (replicate 11 Empty) ++ limparAux (drop 9 t) comp (indice+9)
                            | otherwise    = t
                    where i = (div (comp-11) 2)

-- | Colocar a parte do meio da casa no labirinto cuja largura é par
casaMeioLabPar :: Maze -- ^ labirinto
               -> Int  -- ^ lugar onde colocar a parte do meio da casa
               -> Maze -- ^ labirinto resultante de colocar a casa
casaMeioLabPar (h:t) n | n == 0    = (geraCasaMeioPar h):t
                       | otherwise = h:(casaMeioLabPar t (n-1))

-- | Colocar a parte de cima da casa no labirinto cuja largura é par
casaCimaLabPar :: Maze -- ^ labirinto
               -> Int  -- ^ lugar onde colocar a parte de cima da casa
               -> Maze -- ^ labirinto resultante de colocar a casa
casaCimaLabPar (h:t) n | n == 0    = (geraCasaCimaPar h):t
                       | otherwise = h:(casaCimaLabPar t (n-1))

-- | Colocar a parte de baixo da casa no labirinto cuja largura é par
casaBaixoLabPar :: Maze -- ^ labirinto
                -> Int  -- ^ lugar onde colocar a parte de baixo da casa
                -> Maze -- ^ labirinto resultante de colocar a casa 
casaBaixoLabPar (h:t) n | n == 0    = (geraCasaBaixoPar h):t
                        | otherwise = h:(casaBaixoLabPar t (n-1))

-- | Colocar a parte do meio da casa no labirinto cuja largura é ímpar
casaMeioLabImpar :: Maze -- ^ labirinto
                 -> Int  -- ^ lugar onde colocar a parte do meio da casa
                 -> Maze -- ^ labirinto resultante de colocar a casa 
casaMeioLabImpar (h:t) n | n == 0    = (geraCasaMeioImpar h):t
                         | otherwise = h:(casaMeioLabImpar t (n-1))

-- | Colocar a parte de cima da casa no labirinto de largura impar
casaCimaLabImpar :: Maze -- ^ labirinto
                 -> Int  -- ^ lugar onde colocar a parte de cima da casa
                 -> Maze -- ^ labirinto resultante de colocar a casa 
casaCimaLabImpar (h:t) n | n == 0    = (geraCasaCImpar h):t
                         | otherwise = h:(casaCimaLabImpar t (n-1))

-- | Colocar a parte de baixo da casa no labirinto de largura impar
casaBaixoLabImpar :: Maze -- ^ labirinto
                  -> Int  -- ^ lugar onde colocar a parte de baixo da casa
                  -> Maze -- ^ labirinto resultante de colocar a casa
casaBaixoLabImpar (h:t) n | n == 0    = (geraCasaBaixoImpar h):t
                          | otherwise = h:(casaBaixoLabImpar t (n-1))

-- | Limpar o labirinto à volta da casa dos fantasmas de largura par
limparParLab :: Maze -- ^ labirinto
             -> Int  -- ^ lugar onde limpar
             -> Maze -- ^ labirinto limpo à volta da casa dos fantasmas
limparParLab (h:t) n | n == 0    = (limpaPar h):t
                     | otherwise = h:(limparParLab t (n-1))

-- | Limpar o labirinto à volta da casa dos fantasmas de largura par
limparLab :: Maze -- ^ labirinto
          -> Int  -- ^ lugar onde limpar
          -> Maze -- ^ labirinto limpo à volta da casa dos fantasmas
limparLab (h:t) n | n == 0    = (limpar h):t
                  | otherwise = h:(limparLab t (n-1))

