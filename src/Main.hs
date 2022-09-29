module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa4
import Tarefa5
import Tarefa6
import Tarefa2


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 


loadManager :: Manager
loadManager = Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime 

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyUpArrow    (Manager s p step before delta delay) = Manager (mudaDirJogador (Move p U) s) p step before delta delay       
updateControlledPlayer KeyDownArrow  (Manager s p step before delta delay) = Manager (mudaDirJogador (Move p D) s) p step before delta delay
updateControlledPlayer KeyLeftArrow  (Manager s p step before delta delay) = Manager (mudaDirJogador (Move p L) s) p step before delta delay
updateControlledPlayer KeyRightArrow (Manager s p step before delta delay) = Manager (mudaDirJogador (Move p R) s) p step before delta delay 

-- | Atualizar o manager para o id do jogador a jogar                                          
atualizaManager :: Manager -- ^ manager
                -> Manager -- ^ manager atualizado
atualizaManager (Manager state@(State m js l) pid s b del d) = (Manager state (getPacmanID js) s b del d) 

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager s p step before delta delay) = Manager s p step now (delta+(now-before)) delay

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager s p step before delta delay) = Manager s p step now 0 delay 

nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager s p step before delta delay) = Manager state p (step+1) now 0 delay
                                            where state = passTime step s

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w color_schema man
  if delt > del
    then loop w $ atualizaManager $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if ev' == EventCharacter 'q'
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

