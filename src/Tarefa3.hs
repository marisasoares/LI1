module Tarefa3 where

import Types
import Tarefa1

-- * Testes

t3t1 :: (Maze)
t3t1 = (generateMaze 30 15 120)
t3t2 :: (Maze)
t3t2 = (generateMaze 20 18 10)
t3t3 :: (Maze)
t3t3 = (generateMaze 21 18 10)
t3t4 :: (Maze)
t3t4 = (generateMaze 21 21 10)

-- | Compactar o labirinto em intruções
compactMaze :: Maze -- ^ labirinto a compactar
            -> Instructions -- ^ lista de intruções do labirinto compactadas 
compactMaze m = geraInstrucoesMazeHor m

-- | Converte o corredor numa lista de pares (Int, Piece)
geraInstrucoesCorredor :: Corridor      -- ^ corredor
                       -> [(Int,Piece)] -- ^ lista de pares (Int, Piece)
geraInstrucoesCorredor [] = []
geraInstrucoesCorredor (Wall:t) = (1,Wall) : geraInstrucoesCorredor t
geraInstrucoesCorredor (Empty:t) = (1,Empty) : geraInstrucoesCorredor t
geraInstrucoesCorredor (Food Little:t) =(1,Food Little): geraInstrucoesCorredor t
geraInstrucoesCorredor (Food Big:t) = (1,Food Big): geraInstrucoesCorredor t

-- | Agrupa os pares iguais seguidos
agrupaHorizontal :: [(Int,Piece)] -- ^ Lista de pares
                 -> [(Int,Piece)] -- ^ lista de pares agrupados
agrupaHorizontal [] = []
agrupaHorizontal [x]= [x]
agrupaHorizontal ((x,xs):(y,ys):t)  | xs == ys  = agrupaHorizontal ((x+y,xs):t) 
                                    | otherwise = (x,xs) : agrupaHorizontal ((y,ys):t) 

-- | Cria uma intrução a partir de uma lista de pares
criaInst :: [(Int,Piece)] -- ^ lista de pares
         -> Instruction   -- ^ intrução obtida
criaInst l = Instruct l

-- | Converte o labirinto numa lista de intruções 
geraInstrucoesMazeHor :: Maze -- ^ labirinto
                      -> Instructions -- ^ lista de intruções
geraInstrucoesMazeHor m = map criaInst (map agrupaHorizontal(map geraInstrucoesCorredor m))