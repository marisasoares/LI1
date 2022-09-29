module Tarefa2 where 

import System.Random
import Tarefa1
import Types
import FileUtils

-- * Testes

-- | Testes da Tarefa 2
testesT2 :: [(Play,State)]
testesT2 = [((Move 1 L),(State (generateMaze 30 15 120) js 1)), 
            ((Move 1 L),(State (generateMaze 20 21 490) js2 1)), 
            ((Move 1 U),(State (generateMaze 41 40 130) js 1)), 
            ((Move 1 R),(State (generateMaze 41 40 130) js 1)), 
            ((Move 1 L),(State (generateMaze 20 21 490) [p1] 1)), 
            ((Move 1 R),(State (generateMaze 20 21 490) [p2] 1)), 
            ((Move 1 R),(State (generateMaze 20 21 490) [p3] 1)), 
            ((Move 1 D),(State (generateMaze 20 21 490) [p4] 1)),
            ((Move 1 R),(State (generateMaze 20 21 490) [p5] 1))] 

js = [jogador1,jogador2, jogador3, jogador4]
jogador1=(Pacman (PacState (1,(7,21),1,R,0,1) 0 Open Mega))
jogador2=(Ghost  (GhoState (0,(7,13),1,R,0,1) Dead))
jogador3=(Ghost  (GhoState (2,(7,22),1,L,0,1) Dead))
jogador4=(Ghost  (GhoState (3,(5,14),1,U,0,1) Dead))

js2 = [j1,j2, j3, j4]
j1=(Pacman (PacState (1,(4,5),1,L,0,1) 0 Open Normal))
j2=(Ghost  (GhoState (0,(4,6),1,L,0,1) Alive))
j3=(Ghost  (GhoState (2,(5,13),1,U,0,1) Alive))
j4=(Ghost  (GhoState (3,(5,14),1,D,0,1) Alive))

p1=(Pacman (PacState (1,(10,0),1,L,0,1) 0 Open Normal))
p2=(Pacman (PacState (1,(10,19),1,R,0,1) 0 Open Normal))
p3=(Pacman (PacState (1,(1,2),1,R,0,1) 0 Open Normal))
p4=(Pacman (PacState (1,(1,2),1,D,0,1) 0 Open Normal))
p5=(Pacman (PacState (1,(13,5),1,R,0,1) 0 Open Normal))


-- | Função principal que executa uma jogada de um jogador 
play :: Play -- ^ Jogada que contem o ID e a direção
     -> State -- ^ Estado do jogo incial
     -> State -- ^ Estado depois da jogada
play (Move i o) (State maze js lvl) | not (temVidas js) = (State maze js lvl)
                                    | acabouComida (0,0) maze && lvl==1 = aumentaNivel 2 (loadMaze "maps/2.txt")
                                    | acabouComida (0,0) maze && lvl==2 = aumentaNivel 3 (loadMaze "maps/3.txt")
                                    | acabouComida (0,0) maze = (State maze js lvl)
                                    | pacmanOrGhost (getPlayer i js) = playPacman (Move i o) (State maze js lvl)
                                    | otherwise = playGhost (Move i o) (State maze js lvl)

-- | função play para o pacman
playPacman :: Play -- ^ jogada
           -> State -- ^ estado
           -> State -- ^ estado depois da jogada
playPacman (Move i o) (State maze js lvl) | changeDirection move  js   = State maze (alteraDirection move js') lvl
                                          | isWall move  jogador maze  = (State maze js' lvl)
                                          | fantasmaPosD js coordenadas && isComidaG move jogador maze && isMega js = (State limpamaze (meioFantasma (alteraPontuacao (10*(contaFantasmas js coordenadas)+5) move js') coordenadas maze) lvl)
                                          | fantasmaPosD js coordenadas && isComidaP move jogador maze && isMega js = (State limpamaze (meioFantasma (alteraPontuacao (10*(contaFantasmas js coordenadas)+1) move js') coordenadas maze) lvl)
                                          | fantasmaPosD js coordenadas && isMega js = (State limpamaze (meioFantasma (alteraPontuacao (10*(contaFantasmas js coordenadas)) move js') coordenadas maze) lvl)
                                          | fantasmaPos js coordenadas && not (isMega js) = (State limpamaze (alteraVidas move js') lvl)
                                          | isComidaP move jogador maze = (State limpamaze (alteraPontuacao 1 move js') lvl)
                                          | isComidaG move jogador maze = (State limpamaze (atualizaFantasma (alteraPontuacao 5 move js')) lvl)
                                          | isTunel move jogador maze = (State maze (passarTunel maze move js') lvl)
                                          | otherwise = (State limpamaze (alteraPosicao move js') lvl)
                                         where coordenadas = (somaCoords (getPlayerCoords (getPlayer i js)) o)
                                               jogador     = (getPlayerCoords (getPlayer i js)) 
                                               move        = (Move i o)
                                               coordenadasAntes = getPlayerCoords (getPlayer i js)
                                               limpamaze = limparComida coordenadasAntes maze
                                               js' |isMega js = openOrCloseMouthList (tempoMegaList js)
                                                   |otherwise = fantasmasVivos(openOrCloseMouthList (tempoMegaList js))

-- | play para os ghosts
playGhost :: Play -- ^ jogada
          -> State -- ^ estado
          -> State -- ^ estado depois da jogada
playGhost (Move i o) (State maze js lvl)  | changeDirection move  js   = State maze (alteraDirection move js) lvl
                                          | isWall    move  jogador maze  = State maze js lvl
                                          | isTunel   move  jogador maze  = State maze (passarTunel maze move js) lvl
                                          | otherwise = State maze (alteraPosicao move js) lvl
                                              where  jogador = getPlayerCoords (getPlayer i js)
                                                     move    = Move i o
-- | Aumenta o nível do state
aumentaNivel :: Int -- ^ nivel 
             -> State -- ^ state
             -> State -- ^ state atualizado 
aumentaNivel x (State maze js lvl) = (State maze js x)

-- | atualiza o tempo mega do pacman na lista de players 
tempoMegaList :: [Player] -- ^ lista de jogadores
              -> [Player] -- ^ lista de jogadores atualizada
tempoMegaList [] = []
tempoMegaList ((Pacman x):t) = tempoMega(Pacman x) : tempoMegaList t
tempoMegaList (h:t) = h:tempoMegaList t 

-- | atualiza o tempo mega do pacman
tempoMega :: Player -- ^ pacman
          -> Player -- ^ pacman atualizado
tempoMega (Pacman (PacState x t b y)) | t > 0 = Pacman (PacState x (t-1) b y)
                                      | otherwise = Pacman (PacState x 0 b Normal)

-- | Verifica se é um pacman
pacmanOrGhost :: Player -- ^ jogador
              -> Bool -- ^ True se for pacman 
pacmanOrGhost (Pacman _) = True
pacmanOrGhost (Ghost _)  = False

-- | Muda a direção de um jogador num state
mudaDirJogador :: Play -- ^ jogada 
               -> State -- ^ state
               -> State -- ^ state atualizado
mudaDirJogador p (State m js lvl)  = State m (alteraDirection p js) lvl

-- | Atualiza a boca do pacman na lista de jogadores
openOrCloseMouthList :: [Player] -- ^ lista de jogadores
                     -> [Player] -- ^ lista de jogadores atualizada
openOrCloseMouthList []=[]
openOrCloseMouthList ((Pacman x):t) = openOrCloseMouth (Pacman x) : openOrCloseMouthList t
openOrCloseMouthList (h:t) = h:openOrCloseMouthList t 

-- | Atualiza a boca do pacman
openOrCloseMouth :: Player -- ^ player pacman 
                 -> Player -- ^ pacman atualizado
openOrCloseMouth (Pacman (PacState x t Open y)) = Pacman (PacState x t Closed y)
openOrCloseMouth (Pacman (PacState x t Closed y)) = Pacman (PacState x t Open y)

-- | Atualiza a lista de jogadores para os fantasmas ficarem vivos
fantasmasVivos :: [Player] -- ^ Lista de jogadores  
               -> [Player] -- ^ Lista de jogadores com os fantasmas todos Vivos 
fantasmasVivos [] = []
fantasmasVivos ((Pacman x):t) = (Pacman x):fantasmasVivos t 
fantasmasVivos ((Ghost (GhoState (id,x,z,or,p,v) m)):t) =  Ghost (GhoState (id,x,1,or,p,v) Alive):fantasmasVivos t 

-- | Verifica se o Pacman tem vidas
temVidas :: [Player] -- ^ lista de players
         -> Bool -- ^ devolve True se tiver vidas, False caso contrário
temVidas [] = True
temVidas ((Pacman (PacState (id,_,_,_,_,v) _ _ _)):t) | v<=0 = False
temVidas (_:t) = temVidas t


-- | Função que percorre uma lista
--   de Players para encontrar 
--   o Player com o respetivo ID
getPlayer :: Int -- ^ ID de um Player
          -> [Player] -- ^ Lista de Players 
          -> Player -- ^ Player com o respetivo ID
getPlayer id (j:js) | id==getPlayerID j = j
                    | otherwise = getPlayer id js

-- | Verifica se é preciso mudar de orientação 
changeDirection :: Play -- ^ Jogada que contem o ID e a direção
                -> [Player] -- ^ Lista de jogadores 
                -> Bool -- ^ Retorna True se For preciso mudar de direçao 
changeDirection (Move i o) (j:js) | i == (getPlayerID j) && o /= getPlayerOrientation j = True
                                  | i == (getPlayerID j) = False
                                  | otherwise = changeDirection (Move i o) js

-- | Altera a orientação de um certo jogador na lista de jogadores 
alteraDirection :: Play -- ^ Jogada que contem o ID e a direção
                -> [Player] -- ^ Lista de Jogadores
                -> [Player] -- ^ Lista resultante  de mudar a direçao do jogador 
alteraDirection (Move i o) (j:js) | i == getPlayerID j = (alteraDirecaoPlayer o j) : js
                                  | otherwise = j : alteraDirection (Move i o) js

-- | Altera a orientação de um player 
alteraDirecaoPlayer :: Orientation -- ^ Orientação nova 
                    -> Player -- ^ Jogador
                    -> Player -- ^ Jogador com a nova orientação
alteraDirecaoPlayer o (Pacman (PacState(id,x,z,_,p,v) t b m)) = (Pacman (PacState(id,x,z,o,p,v) t b m))
alteraDirecaoPlayer o (Ghost (GhoState (id,x,z,or,p,v) m)) = (Ghost (GhoState (id,x,z,o,p,v) m))

-- | Verifica se o jogador já está na mesma orientação que a jogada
sameDirection :: Play -- ^ Jogada que contem o ID e a direção
              -> [Player] -- ^ Lista de jogadores
              -> Bool -- ^ Retorna True se o jogador tem a mesma direção 
sameDirection (Move i o) (j:js) | i == (getPlayerID j) && o == getPlayerOrientation j = True
                                | i == (getPlayerID j) = False
                                | otherwise = sameDirection (Move i o) js

-- | Altera a posição de um jogador na lista de jogadores 
alteraPosicao :: Play -- ^ Jogada que contem o ID e a direção
              -> [Player] -- ^ Lista de Jogadores
              -> [Player] -- ^ Lista de jogadores com a posição do jogador com o Id da play mudada
alteraPosicao (Move i o) (j:js) | i == getPlayerID j = (alteraPosicaoPlayer o j) : js
                                | otherwise = j : alteraPosicao (Move i o) js

-- | Altera a posição de um certo jogador 
alteraPosicaoPlayer :: Orientation -- ^ Orientação 
                    -> Player -- ^ Jogador 
                    -> Player -- ^ Jogador com a posição alterada
alteraPosicaoPlayer i (Pacman (PacState(id,x,z,o,p,v) t b m)) = (Pacman (PacState(id,(somaCoords x i),z,o,p,v) t b m))
alteraPosicaoPlayer i (Ghost (GhoState (id,x,z,or,p,v) m)) = (Ghost (GhoState (id,(somaCoords x i),z,or,p,v) m))

-- | Calcula a posição seguinte dado uma orientação 
somaCoords :: Coords -- ^ Coordendas 
           -> Orientation -- ^ Orientação
           -> Coords -- ^ Coordenadas atualizadas dependendo da orientação
somaCoords (x,y) L = (x,y-1)
somaCoords (x,y) R = (x,y+1)
somaCoords (x,y) U = (x-1,y)
somaCoords (x,y) D = (x+1,y)

-- | Verifica se o fantasma que está na posição da jogada do player está morto 
fantasmaMazeDead :: Play -- ^ Jogada que contem o ID e a direção
                 -> Maze -- ^ Labirinto 
                 -> [Player] -- ^ Lista de jogadores
                 -> Bool -- ^ Retorna True se estiver um fantasma morto na mesma posiçao que o jogador do Id da play
fantasmaMazeDead (Move i d) m (j:js) | i == getPlayerID j && fantasmaPosD (j:js) (somaCoords (getPlayerCoords j) d) = True
                                     | i == getPlayerID j = False
                                     | otherwise = fantasmaMazeDead (Move i d) m js

-- | Verifica se o fantasma que está na posição da jogada do player está vivo
fantasmaMazeAlive :: Play -- ^ Jogada que contem o ID e a direção
                  -> Maze -- ^ Labirinto 
                  -> [Player] -- ^ Lista de jogadores
                  -> Bool -- ^ Retorna True se estiver um fantasma vivo na mesma posiçao que o jogador do Id da play
fantasmaMazeAlive (Move i d) m (j:js) | i == getPlayerID j && fantasmaPos (j:js) (somaCoords (getPlayerCoords j) d) = True
                                      | i == getPlayerID j = False
                                      | otherwise = fantasmaMazeAlive (Move i d) m js


-- | Verifica se está um fantasma vivo nas coordenadas dadas
fantasmaPos :: [Player] -- ^ Lista de Jogadores
            -> Coords -- ^ Coordenadas
            -> Bool -- ^ Retorna True se estiver um fantasma vivo na posição das coordenadas
fantasmaPos [] _ = False
fantasmaPos ((Ghost (GhoState g Alive)):js) x | getPlayerCoords (Ghost (GhoState g Alive))==x = True
                                              | otherwise = fantasmaPos js x
fantasmaPos (_:js) x = fantasmaPos js x

-- | Verifica se está um fantasma morto nas coordenadas dadas
fantasmaPosD :: [Player] -- ^ Lista de Jogadores
             -> Coords -- ^ Coordenadas
             -> Bool -- ^ Retorna True se estiver um fantasma morto na posição das coordenadas
fantasmaPosD [] _ = False
fantasmaPosD ((Ghost (GhoState g Dead)):js) x | getPlayerCoords (Ghost (GhoState g Dead))==x = True
                                              | otherwise = fantasmaPosD js x
fantasmaPosD (_:js) x = fantasmaPosD js x

-- | Conta quantos fantasmas se encontram em x coordenadas
contaFantasmas :: [Player] -- ^ Lista de Jogadores
               -> Coords -- ^ Coordenadas
               -> Int -- ^ Retorna o nº de fantasmas que se encontram em x posição
contaFantasmas [] x = 0
contaFantasmas ((Ghost (GhoState g Dead)):js) x | getPlayerCoords (Ghost (GhoState g Dead))==x = 1+contaFantasmas js x
                                                | otherwise = contaFantasmas js x
contaFantasmas (_:js) x = contaFantasmas js x 

-- | Devolve o ID do fantasma morto nas coordenadas dadas
fantasmaPosDTrack :: [Player] -- ^ Lista de Jogadores
                  -> Coords -- ^Coordenadas
                  -> Int -- ^ ID do fantasma que está nas coordenas 
fantasmaPosDTrack [] _ = -1
fantasmaPosDTrack ((Ghost (GhoState g Dead)):js) x | getPlayerCoords (Ghost (GhoState g Dead))==x = getPlayerID (Ghost (GhoState g Dead))
                                                   | otherwise = fantasmaPosDTrack js x
fantasmaPosDTrack (_:js) x = fantasmaPosDTrack js x

-- | Altera a pontuação de um jogador na lista de jogadores 
alteraPontuacao :: Int -- ^ Pontos 
                -> Play -- ^ Jogada que contem o ID e a direção
                -> [Player] -- ^ Lista de jogadores 
                -> [Player] -- ^ Lista de jogadores com o jogador os pontos atualizados
alteraPontuacao pontos (Move i o) (j:js) | i == getPlayerID j = (alteraPontuacaoPlayer pontos (Move i o) j) : js
                                         | otherwise = j : alteraPontuacao pontos (Move i o) js

-- | Altera a pontuação de um jogador 
alteraPontuacaoPlayer :: Int -- ^ Pontos 
                      -> Play -- ^ Jogada que contem o ID e a direção
                      -> Player -- ^ Jogador
                      -> Player -- ^ Jogador com os pontos atualizados 
alteraPontuacaoPlayer pontos (Move i o) (Pacman (PacState(id,x,z,or,p,v) t b m)) | pontos==5 = (Pacman (PacState(id,(somaCoords x o),z,or,p+pontos,v) 20 b Mega))
                                                                                 | otherwise = (Pacman (PacState(id,(somaCoords x o),z,or,p+pontos,v) t b m))
alteraPontuacaoPlayer _ _ x = x

-- | Altera as vidas de um jogador na lista de jogadores 
alteraVidas :: Play -- ^ Jogada que contem o ID e a direção
            -> [Player] -- ^ Lista de jogadores  
            -> [Player]
alteraVidas (Move i o) (j:js) | i == getPlayerID j = (alteraVidasPlayer (Move i o) j) : js
                              | otherwise = j : alteraVidas (Move i o) js

-- | Altera as vidas de um jogador 
alteraVidasPlayer :: Play -- ^ Jogada que contem o ID e a direção
                  -> Player -- ^ Jogador 
                  -> Player -- ^ Jogador com as vidas alteradas 
alteraVidasPlayer (Move i o) (Pacman (PacState (id,x,z,or,p,v) t b m)) | v==1 = (Pacman (PacState(id,(somaCoords x o),z,or,p,0) t b Dying))
                                                                       | otherwise = (Pacman (PacState(id,(somaCoords x o),z,or,p,v-1) t b m))
alteraVidasPlayer _ x = x

-- | Atualiza o mode dos fantasmas todos para mortos 
atualizaFantasma :: [Player] -- ^ Lista de jogadores  
                 -> [Player] -- ^ Lista de jogadores com os fantasmas todos mortos  
atualizaFantasma [] = []
atualizaFantasma ((Pacman x):t) = (Pacman x):atualizaFantasma t 
atualizaFantasma ((Ghost (GhoState (id,x,z,or,p,v) m)):t) =  (Ghost (GhoState (id,x,0.5,or,p,v) Dead)):atualizaFantasma t 

-- | Coloca o fantasma na casa dos fantasmas quando é morto por um pacman
meioFantasma :: [Player] -- ^ Lista de jogadores 
             -> Coords -- ^ Coordenadas  
             -> Maze -- ^ Labirinto 
             -> [Player] -- ^ Lista de jogadores com o fantasma depois de ser comido no meio 
meioFantasma [] x _ = []
meioFantasma (j:js) x maze | getPlayerID j ==fantasmaPosDTrack (j:js) x = (setGhostAlive (setPlayerCoords (getPlayer (fantasmaPosDTrack (j:js) x) (j:js)) (meio maze))):(meioFantasma js x maze)
                           | otherwise = j:meioFantasma js x maze

-- | Coloca o fantasma em modo alive
setGhostAlive :: Player -- ^ Ghost
              -> Player  -- ^ Ghost em mode Alive
setGhostAlive (Ghost (GhoState (id,x,z,or,p,v) m)) = Ghost (GhoState (id,x,z,or,p,v) Alive)

-- | Calcula o meio do labirinto 
meio :: Maze -- ^ Labirinto
    -> Coords -- ^ Coordenadas
meio maze | mod (length maze) 2 == 0 = ((div (length maze) 2)-1,(div (length (head maze)) 2)) 
          |otherwise = ((div (length maze) 2),(div (length (head maze)) 2)) 

-- | Verifica na lista de jogadores se encontra o Pacman se encontra em estado Mega
isMega :: [Player] -- ^ Lista de jogadores 
       -> Bool -- ^ Retorna True se o Pacman estiver Mega
isMega [] = False
isMega ((Pacman (PacState g _ _ Mega)):js) = True
isMega (_:js) = isMega js

-- | Verifica se o fantasma se encontra vivo
isFantasmaAlive :: Piece  -- ^ Peça do labirinto
                -> Bool -- ^ Retorna True se o fantasma estiver vivo
isFantasmaAlive (PacPlayer (Ghost (GhoState _ Alive))) = True
isFantasmaAlive _ = False

-- | Verifica se o fantasma se encontra morto
isFantasmaDead :: Piece -- ^ Peça do labirinto
               -> Bool -- ^ Retorna True se o fantasma estiver morto
isFantasmaDead (PacPlayer (Ghost (GhoState _ Dead))) = True
isFantasmaDead _ = False

-- | Verifica se o jogador vai jogar para a posição onde se encontra uma comida pequena
isComidaP :: Play -- ^ Jogada que contem o ID e a direção
          -> Coords -- ^ Coordenadas
          -> Maze -- ^ Labirinto 
          -> Bool -- ^ Retorna True se nas coordenanas estiver uma comida pequena
isComidaP (Move i o) x lab = (pieceMaze (somaCoords x o) lab) == Food Little

-- | Verifica se o jogador vai jogar para a posição onde se encontra uma comida grande
isComidaG :: Play -- ^ Jogada que contem o ID e a direção
          -> Coords -- ^ Coordenadas
          -> Maze -- ^ Labirinto
          -> Bool -- ^ Retorna True se nas coordenanas estiver uma comida grande
isComidaG (Move i o) x lab = (pieceMaze (somaCoords x o) lab) == Food Big


-- | Verifica se o jogador vai jogar para a posição onde se encontra uma parede
isWall :: Play -- ^ Jogada que contem o ID e a direção
       -> Coords  -- ^ Coordenadas
       -> Maze -- ^ Labirinto
       -> Bool -- ^ Retorna True se nas coordenanas estiver uma comida grande
isWall (Move i o) x lab = (pieceMaze (somaCoords x o) lab) == Wall

-- | Devolve a peça que se encontra nas coordenadas
pieceMaze :: Coords -- ^ Coordenadas
          -> Maze -- ^ Labirinto
          -> Piece -- ^ Peça que esta nas coordenadas
pieceMaze (0,j) (h:t) = indiceLista j h                    
pieceMaze (i, j) (m:ms) = pieceMaze (i-1, j) ms

-- | Devolve a peça que está no indice n do corredor
indiceLista :: Int -- ^ Indice da peça do corredor  
            -> Corridor -- ^ Corredor 
            -> Piece -- ^ Peça que está no indice do corredor 
indiceLista _ [] = Empty
indiceLista 0 (h:t) = h
indiceLista n (x:xs) = indiceLista (n-1) xs 

-- | Verifica se o jogador se encontra num tunel
isTunel :: Play -- ^ Jogada que contem o ID e a direção
        -> Coords -- ^ Coordenadas 
        -> Maze  -- ^ Labirinto
        -> Bool -- ^ Retorna True se for entrar no labirinto
isTunel (Move i R) (x,y) lab | (pieceMaze (x,y+1) lab) == Empty && y==length(head lab)-1 = True
                             | otherwise = False
isTunel (Move i L) (x,y) lab | (pieceMaze (x,y) lab) == Empty && y==0 =True
                             | otherwise = False
isTunel (Move i _) _ _ = False


-- | Altera na lista de jogadores o jogador que passou pelo tunel
passarTunel :: Maze -- ^ Labirinto
            -> Play -- ^ Jogada que contem o ID e a direção
            -> [Player] -- ^ Lista de jogadores 
            -> [Player] -- ^ Lista de jogadores com a posição do jogador atualizada depois de passar o túnel
passarTunel m (Move i o) (j:js) | i == getPlayerID j = (passarTunelPlayer m (Move i o) j) : js
                                | otherwise = j : passarTunel m (Move i o) js

-- | Altera as coordenadas de um jogador de maneira a passar pelo tunel
passarTunelPlayer :: Maze -- ^ Labirinto
                  -> Play -- ^ Jogada que contem o ID e a direção
                  -> Player -- ^ Jogador
                  -> Player -- ^ Jogador na nova posição depois de passar o túnel 
passarTunelPlayer m (Move i R) (Pacman (PacState (id,(x,y),z,or,p,v) t b mode)) = (Pacman (PacState(id,(x,1),z,or,p,v) t b mode))
passarTunelPlayer m (Move i L) (Pacman (PacState (id,(x,y),z,or,p,v) t b mode)) = (Pacman (PacState(id,(x,((length(head m)-1))),z,or,p,v) t b mode))
passarTunelPlayer m (Move i o) (Ghost (GhoState (id,(x,y),z,or,p,v) mode)) | o==R = (Ghost (GhoState (id,(x,1),z,or,p,v) mode))
                                                                           | o==L = (Ghost (GhoState (id,(x,(length(head m)-1)),z,or,p,v) mode))                                       

-- | Limpa a comida que o Pacman comeu
limparComida :: Coords -- ^ coordenadas onde se encontra a comida
             -> Maze   -- ^ labirinto
             -> Maze   -- ^ labirinto limpo 
limparComida (_,_) [] = []
limparComida (0, i) (h:t) = atualizaIndiceLista i Empty h:t
limparComida (i, j) (h:t) = h:limparComida (i-1, j) t

-- | Atualiza indice n do Corredor
atualizaIndiceLista :: Int -- ^ indice que é para alterar 
                    -> Piece -- ^ peça para alterar
                    -> Corridor -- ^ corredor para alterar
                    -> Corridor -- ^ corredor alterado
atualizaIndiceLista 0 a [] = [a]
atualizaIndiceLista 0 a (_:xs) = a : xs
atualizaIndiceLista n a (x:xs) = x : atualizaIndiceLista (n-1) a xs

-- | Retorna True se acabou a comida no labirinto 
acabouComida :: Coords   -- ^ coordenadas para percorrer no labirinto
                -> Maze     -- ^ labirinto
                -> Bool-- ^ lista de coordenadas onde se encontram as comidas
acabouComida _ [] = True
acabouComida (i, j) (m:t) = encontraFood (i, j) m && acabouComida (i + 1, j) t

-- | Percorre um corredor à procura de Food Big e devolve a lista de coordenadas onde se encontram 
encontraFood :: Coords   -- ^ coordenadas para percorrer o corredor
                -> Corridor -- ^ corredor
                -> Bool -- ^ lista de coordenadas de onde se encontram as comidas
encontraFood _ [] = True
encontraFood (x,y) (h:t) | h==Food Big || h==Food Little = False
                         | otherwise = encontraFood (x,y+1) t                                                                       