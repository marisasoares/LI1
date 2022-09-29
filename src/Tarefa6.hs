{-|
= Introdução
        Esta tarefa consistiu em implementar um bot do Pacman, 
que realizasse certas jogadas para cada situação de jogo, ou seja cada State.
        Nomeadamente um jogador que fugisse dos fantasmas, enquanto estes estivessem vivos, 
procurasse as comidas grandes, de modo a ficar no modo 'Mega' e neste último caso
perseguisse os fantasmas, de maneira a que os comesse.
       

= Objetivos
        As nossas estratégias para a realização desta tarefa resumem-se em verificar certas situações:
     - se o pacman está em modo Mega
     - se o pacman está perto de fantasmas
     - se o pacman está perto de comidas grandes
        E escrevemos funções para esses diferentes casos, de modo a que o 
bot do Pacman fizesse diferentes jogadas. 

= Discussão e conclusão
        Concluindo, a Tarefa 6 realiza aquilo que é pretendido e o bot desloca-se
para as diferentes situações, no entanto de uma forma rudimentar.


-}

module Tarefa6 where 

import Types
import Tarefa5 
import Tarefa2
import FileUtils

testesT6 :: [(Int,State)]
testesT6 = [(0,loadMaze "maps/1.txt"),
            (1,loadMaze "maps/2.txt"),
            (2,loadMaze "maps/3.txt")]

-- | Aplica jogadas ao bot dependendo do estado do jogo
bot :: Int        -- ^ id do jogador
    -> State      -- ^ estado que contém o labirinto, lista de jogadores e nível
    -> Maybe Play -- ^ possível jogada
bot id state@(State m js lvl) | isMega js              = Just (Move id chaseFant)
                              | chaseComida   state    = Just (Move id orChase)
                              | not(chaseComida state) = Just (Move id orScat)
                              | otherwise = Nothing
                                      where pacman     = getPacmanCoords js  
                                            comida     = encontraFoodMaze (0,0) m
                                            fantasma   = coordsFantasmas js
                                            coords     = coordMaisPerto (comida ++ fantasma) pacman
                                            coordsFant = coordMaisPerto fantasma pacman
                                            orChase    = melhorJogadaChase pacman coords m
                                            orScat     = melhorJogadaScatter pacman coords m
                                            chaseFant  = melhorJogadaChase pacman coordsFant m

-- | Calcula a distância de Manhattan entre duas Coordenadas.
distMan :: Coords -- ^ coordenadas 
        -> Coords -- ^ coordenadas
        -> Int    -- ^ distância calculada
distMan (y,x) (a,b) = abs (y-a) + abs (x-b)

-- | Percorre o labirinto à procura de 'Food Big' e devolve a lista de coordenadas onde se encontram 
encontraFoodMaze :: Coords   -- ^ coordenadas para percorrer no labirinto
                 -> Maze     -- ^ labirinto
                 -> [Coords] -- ^ lista de coordenadas onde se encontram as comidas
encontraFoodMaze _ [] = []
encontraFoodMaze (i, j) (m:t) = encontraFoodBig (i,j) m ++ encontraFoodMaze (i+1,j) t 

-- | Percorre um corredor à procura de Food Big e devolve a lista de coordenadas onde se encontram 
encontraFoodBig :: Coords   -- ^ coordenadas para percorrer o corredor
                -> Corridor -- ^ corredor
                -> [Coords] -- ^ lista de coordenadas de onde se encontram as comidas
encontraFoodBig _ [] = []
encontraFoodBig (x,y) (h:t) | h==Food Big = (x,y):encontraFoodBig (x,y+1) t
                            | otherwise = encontraFoodBig (x,y+1) t 

-- | Procura numa lista de coordenadas, a mais perto de outras coordenadas
coordMaisPerto :: [Coords] -- ^ lista de coordenadas a comparar 
               -> Coords   -- ^ coordenadas a comparar
               -> Coords   -- ^ coordenadas mais proximas
coordMaisPerto [h] x = h
coordMaisPerto (h:h2:t) x | distMan h x < distMan h2 x = coordMaisPerto (h:t) x
                          | otherwise = coordMaisPerto (h2:t) x 


-- | Devolve a lista de coordenadas dos fantasmas
coordsFantasmas :: [Player] -- ^ lista de jogadores 
                -> [Coords] -- ^ lista de coordenadas dos fantasmas
coordsFantasmas [] = []
coordsFantasmas ((Ghost (GhoState (id,x,z,or,p,v) mode)):t) = x:coordsFantasmas t
coordsFantasmas (_:t) = coordsFantasmas t 

-- | Verifica se a peça mais perto do Pacman é 'Food Big'
chaseComida :: State -- ^ Estado que contem o labirinto, lista de jogadores e nível
            -> Bool  -- ^ True se a peça mais perto for 'Food Big'
chaseComida (State maze js lvl) = pieceMaze (coordMaisPerto (comida ++ fantasma) pacman) maze == Food Big 
                 where pacman   = getPacmanCoords js
                       comida   = encontraFoodMaze (0,0) maze
                       fantasma = coordsFantasmas js                                  