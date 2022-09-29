{-|
= Introdução
        As estratégias implementadas para a realização desta tarefa passaram por
fazer com que os fantasmas agissem de uma certa forma se estes estiverem no modo Alive
e para outra no modo Dead.
 

= Objetivos
        O principal objetivo desta tarefa consistia em fazer jogadas para os fantasmas, 
para cada situação, mais especificamente para quando eles estão no modo Alive ou no modo Dead.
        No caso de estarem Alive teriam que perseguir o Pacman, para isso completamos a função chaseMode, 
caso contrário fugiam do mesmo e então criamos a função scatterMode.
        Resumidamente, o objetivo dos fantasmas é ficar na mesma posição que o Pacman.


= Discussão e conclusão
        Concluindo, a Tarefa 5 realiza aquilo que é pretendido para as diferentes situações, os fantasmas 
perseguem o Pacman de acordo com a sua posição.

-}

module Tarefa5 where 

import Types
import Tarefa2
import FileUtils

testesT5 :: [State]
testesT5 = [loadMaze "maps/1.txt",
            loadMaze "maps/2.txt",
            loadMaze "maps/3.txt"]


-- | Devolve uma lista de jogadas para todos os fantasmas 
ghostPlay :: State  -- ^ Estado do jogo
          -> [Play] -- ^ Lista de jogadas dos fantasmas 
ghostPlay s = ghostPlayAux s 0

-- | Funcao auxiliar para percorrer a lista de jogadores 
--   de modo a todos os fantasmas jogarem 
ghostPlayAux :: State -- ^ Estado do jogo
             -> Int -- ^ Indice da lista de jogadores
             -> [Play] -- ^ Lista de jogadas dos fantasmas 
ghostPlayAux state@(State m js lvl) n | n==length js = []
                                      | pacmanOrGhost j = ghostPlayAux (State m js lvl) (n+1)
                                      | isAlive j = chaseMode (State m js lvl) (getPlayerID j):ghostPlayAux (State m js lvl) (n+1)
                                      | not (isAlive j) = scatterMode (State m js lvl) (getPlayerID j):ghostPlayAux (State m js lvl) (n+1)
                              where j = (!!) js n


-- | Modo de perseguir o Pacman
chaseMode :: State -- ^ Estado do jogo
          -> Int -- ^ Id do fantasma 
          -> Play -- ^ Jogada em direçao ao Pacman
chaseMode (State m js lvl) id = Move id or
                     where or = melhorJogadaChase (getPlayerCoords (getPlayer id js)) (getPacmanCoords js) m

-- | Modo de fugir do Pacman
scatterMode :: State -- ^ Estado do jogo
            -> Int  -- ^ Id do fantasma 
            -> Play -- ^ Jogada na direçao contraria ao Pacman
scatterMode (State m js lvl) id = Move id or
                     where or = melhorJogadaScatter (getPlayerCoords (getPlayer id js)) (getPacmanCoords js) m

-- | Encontra a melhor orientaçao para perseguir o Pacman
melhorJogadaChase :: Coords -- ^ Coordenadas do fantasma
                  -> Coords -- ^ Coordenadas do Pacman
                  -> Maze -- ^ Labirinto
                  -> Orientation -- ^ Melhor orientacao
melhorJogadaChase (x,y) (a,b) m | x<=a && y<=b && not (isWall2 (x,y+1) m)= R 
                                | x<=a && y<=b && not (isWall2 (x+1,y) m)= D
                                | x<=a && y<=b && not (isWall2 (x-1,y) m)= U
                                | x<=a && y<=b = L
                                | x>=a && y>=b && not (isWall2 (x,y-1) m) = L 
                                | x>=a && y>=b && not (isWall2 (x-1,y) m) = U
                                | x>=a && y>=b && not (isWall2 (x+1,y) m) = D
                                | x>=a && y>=b = R
                                | x<=a && y>=b && not (isWall2 (x+1,y) m) = D 
                                | x<=a && y>=b && not (isWall2 (x,y-1) m) = L
                                | x<=a && y>=b && not (isWall2 (x,y+1) m) = R
                                | x<=a && y>=b = U
                                | x>=a && y<=b && not (isWall2 (x-1,y) m) = U 
                                | x>=a && y<=b && not (isWall2 (x,y+1) m) = R
                                | x>=a && y<=b && not (isWall2 (x,y-1) m) = L
                                | x>=a && y<=b = D

-- | Encontra a melhor orientaçao para fugir do Pacman
melhorJogadaScatter :: Coords -- ^ Coordenadas do fantasma
                    -> Coords -- ^ Coordenadas do Pacman
                    -> Maze -- ^ Labirinto
                    -> Orientation -- ^ Melhor orientacao
melhorJogadaScatter (x,y) (a,b) m | x<=a && y<=b && not (isWall2 (x,y-1) m)= L 
                                  | x<=a && y<=b && not (isWall2 (x-1,y) m)= U
                                  | x<=a && y<=b && not (isWall2 (x+1,y) m)= D
                                  | x<=a && y<=b = R
                                  | x>=a && y>=b && not (isWall2 (x+1,y) m) = D 
                                  | x>=a && y>=b && not (isWall2 (x,y+1) m) = R
                                  | x>=a && y>=b && not (isWall2 (x-1,y) m) = U
                                  | x>=a && y>=b = L
                                  | x<=a && y>=b && not (isWall2 (x,y+1) m) = R 
                                  | x<=a && y>=b && not (isWall2 (x-1,y) m) = U
                                  | x<=a && y>=b && not (isWall2 (x+1,y) m) = D
                                  | x<=a && y>=b = L
                                  | x>=a && y<=b && not (isWall2 (x,y-1) m) = L 
                                  | x>=a && y<=b && not (isWall2 (x+1,y) m) = D
                                  | x>=a && y<=b && not (isWall2 (x-1,y) m) = U
                                  | x>=a && y<=b = R

-- | Verifica se o jogador vai jogar para a posição onde se encontra uma parede
isWall2 :: Coords  -- ^ Coordenadas
        -> Maze -- ^ Labirinto
        -> Bool -- ^ Retorna True se nas coordenadas estiver uma parede
isWall2 x lab = pieceMaze x lab == Wall

-- | Devolve as coordenadas do pacman
getPacmanCoords :: [Player] -- ^ Lista de jogadores 
                -> Coords -- ^ Coordenadas do pacman
getPacmanCoords ((Pacman (PacState (id,x,z,or,p,v) time b y)):t) = x
getPacmanCoords (_:t) = getPacmanCoords t

-- | Verifica se o fantasma está morto ou vivo
isAlive :: Player -- ^ Jogador
        -> Bool -- ^ Retorna True se estiver vivo e False se esta morto
isAlive (Ghost (GhoState (id,x,z,or,p,v) Alive)) = True
isAlive (Ghost (GhoState (id,x,z,or,p,v) Dead)) = False




