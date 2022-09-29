{-|
= Introdução
        A função principal da Tarefa4 é a passTime, que basicamente atualiza o State,
fazendo as jogadas de todos os jogadores, ou seja esta Tarefa é a responsável pela 
passagem de tempo. 
 

= Objetivos
        O objetivo desta Tarefa é fazer com que o Pacman e os Fantasmas 
joguem ao longo do tempo.
        Para isso criamos funções para o Pacman e para os Fantasmas, uma vez que 
estes últimos mudam de velocidade e por isso jogam de acordo com a mesma: 
se esta for 0.5 apenas jogam de 2 em 2 steps, se for 1 não há restrições.

= Discussão e conclusão
      Conseguimos alcançar os objetivos da Tarefa 4, no entanto contempla 
apenas 2 valores diferentes de velocidade: 1 e 0.5.


-}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5 
import FileUtils

defaultDelayTime = 250 -- 250 ms


-- * Testes

testesT4 :: [(Int,State)]
testesT4 = [(10,loadMaze "maps/1.txt"),
            (6,loadMaze "maps/2.txt"),
            (0,loadMaze "maps/3.txt")]


-- | atualizar o estado a jogar 
passTime :: Int   -- ^ step 
         -> State -- ^ state
         -> State -- ^ state atualizado
passTime step state@(State maze js lvl) = jogar js state step


-- | Atualiza o estado depois das jogadas de cada lista de players
jogar :: [Player] -- ^ lista de jogadores 
      -> State   -- ^ state
      -> Int    -- ^ step
      -> State    -- ^ state atualizado
jogar [] s step = s
jogar ((Pacman (PacState (id,x,z,or,p,v) time b y)):t) state step = jogar t (play (Move id or) state) step
jogar (player@(Ghost (GhoState (id,x,z,or,p,v) m)):t)  state step = jogar t (jogarFant (ghostPlay state) player state step) step
     
                                                           
-- | Função para os fantasmas jogarem
jogarFant :: [Play] -- ^ lista de jogadas dos fantasmas
          -> Player -- ^ fantasma
          -> State  -- ^ state
          -> Int    -- ^ step
          -> State  -- ^ state
jogarFant [] _ state _ = state 
jogarFant ((Move id o):t) player@(Ghost (GhoState (i,x,z,or,p,v) m)) state step | id==i && z==0.5 && (mod step 2)==0 = play (Move id o) state
                                                                                | id==i && z==0.5 && (mod step 2)/=0 = state 
                                                                                | id==i = play (Move id o) state
                                                                                | otherwise = jogarFant t player state step


-- | Devolve o id do pacman
getPacmanID :: [Player] -- ^ lista de jogadores
            -> Int      -- ^ id do pacman
getPacmanID ((Pacman (PacState (id,x,z,or,p,v) time b y)):t) = id
getPacmanID (_:t) = getPacmanID t