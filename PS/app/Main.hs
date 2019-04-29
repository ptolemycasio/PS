import qualified Data.Array.Repa as R
import Data.List
import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import GameStage

--StarWars 345/2/4

w = 100 :: Int
h = 100 :: Int

type Schale = R.Array R.U R.DIM2 Int
data State  = State {schale :: Schale, stageN :: Int, mypos :: Mypos}

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  initialWindowSize $= Size 600 600
  createWindow "Panic Shooter"
  clearColor $= Color4 1 1 1 1
  state <- newIORef $ State (rTxt $ board $ stage 1) 1 (start $ stage 1)
  displayCallback $= display state
  reshapeCallback $= Just resize
  addTimerCallback 56 (timerProc (timer state))
  keyboardCallback $= Just (keyboard state)
  idleCallback $= Just idle
  mainLoop

display :: IORef State -> DisplayCallback
display state = do
  clear [ColorBuffer]
  s <- get state
  loadIdentity
  mapM_ (colorBox $ schale s) allCell
  color (Color4 ( 65/255) (105/255) (255/255) 0.1 :: Color4 GLdouble)
  mapM_ (renderPrimitive Polygon . mapM_ vertex2d) $ (map box . goal . stage . stageN) s
  color (Color3 (127/255) (255/255) (  0/255) :: Color3 GLdouble)
  mapM_ (renderPrimitive Polygon . mapM_ vertex2d) $ (map box . fly . mypos) s
  flush where
  vertex2d = vertex :: Vertex2 GLdouble -> IO ()
  allCell = map (\(x,y) -> R.Z R.:. x R.:. y) [(x,y) | x <- [0..w-1], y <- [0..h-1]]
  colorBox s x
    | s R.! x == 3 = do
      color (Color3 (255/255) (242/255) (  0/255) :: Color3 GLdouble)
      (renderPrimitive Polygon . mapM_ vertex2d) (box x)
    | s R.! x == 2 = do
      color (Color3 (255/255) (127/255) ( 39/255) :: Color3 GLdouble)
      (renderPrimitive Polygon . mapM_ vertex2d) (box x)
    | s R.! x == 1 = do
      color (Color3 (237/255) ( 28/255) ( 36/255) :: Color3 GLdouble)
      (renderPrimitive Polygon . mapM_ vertex2d) (box x)
    | s R.! x == 0 = do
      color (Color3 (  0/255) (  0/255) (  0/255) :: Color3 GLdouble)
      (renderPrimitive Polygon . mapM_ vertex2d) (box x)

keyboard :: IORef State -> KeyboardCallback
keyboard state c _ = do
  case c of
    'e' -> exitSuccess
    'i' -> state $~! ikey
    'k' -> state $~! kkey
    'j' -> state $~! jkey
    'l' -> state $~! lkey
    'r' -> state $~! rkey
    's' -> state $~! skey
    'q' -> putStrLn "\BEL"
    _   -> return ()
  where
    (dm, pm, fpm, spm) = (direct . mypos, posit . mypos, fst . pm, snd . pm)
    allCell = map (\(x,y) -> R.Z R.:. x R.:. y) [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    wrap (R.Z R.:. x R.:. y) = R.Z R.:. (mod x w) R.:. (mod y h)
    ch n (x,y) s = let l = map (\z@(R.Z R.:. a R.:. b) -> if z == wrap (R.Z R.:. x R.:. y)
                                                          then n
                                                          else s R.! z) allCell
                   in R.fromListUnboxed (R.Z R.:. w R.:. h :: R.DIM2) l
    ikey s = if dm s == Oben
             then State (schale s) (stageN s) (Mypos (fpm s - 1, spm s) Oben)
             else State (schale s) (stageN s) (Mypos (pm s) Oben)
    kkey s = if dm s == Unten
             then State (schale s) (stageN s) (Mypos (fpm s + 1, spm s) Unten)
             else State (schale s) (stageN s) (Mypos (pm s) Unten)
    jkey s = if dm s == Links
             then State (schale s) (stageN s) (Mypos (fpm s, spm s - 1) Links)
             else State (schale s) (stageN s) (Mypos (pm s) Links)
    lkey s = if dm s == Richtig
             then State (schale s) (stageN s) (Mypos (fpm s, spm s + 1) Richtig)
             else State (schale s) (stageN s) (Mypos (pm s) Richtig)
    rkey s = State ((rTxt . board) $ stage 1) 1 (start $ stage 1)
    skey (State s g (Mypos (x, y) d))
      | d == Oben    = let
        s' = (ch 3 (x-5,y+1) . ch 3 (x-5,y+2) . ch 2 (x-4,y+1) . ch 2 (x-4,y+2)) s in
        State s' g (Mypos (x,y) d)
      | d == Unten   = let
        s' = (ch 3 (x+2,y+1) . ch 3 (x+2,y+2) . ch 2 (x+1,y+1) . ch 2 (x+1,y+2)) s in
        State s' g (Mypos (x,y) d)
      | d == Links   = let
        s' = (ch 3 (x-1,y-2) . ch 3 (x-2,y-2) . ch 2 (x-1,y-1) . ch 2 (x-2,y-1)) s in
        State s' g (Mypos (x,y) d)
      | d == Richtig = let
        s' = (ch 3 (x-1,y+5) . ch 3 (x-2,y+5) . ch 2 (x-1,y+4) . ch 2 (x-2,y+4)) s in
        State s' g (Mypos (x,y) d)

timer :: IORef State -> IO ()
timer state = do
  state $~! nextstate where
  nextstate s
    | (not . null) $ intersect ((goal . stage . stageN) s) (fly $
      mypos s) = State ((rTxt . board . stage) $
      stageN s + 1) (stageN s + 1) (start $ stage $ stageN s + 1)
    | or $ map (\t -> (schale s) R.! t == 3) (fly $
      mypos s) = State ((rTxt . board . stage . stageN) s) (stageN s) (start $ stage $ stageN s)
    | otherwise = State (nextgen $ schale s) (stageN s) (mypos s)

timerProc :: IO a -> TimerCallback
timerProc m = m >> addTimerCallback 54 (timerProc m)

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  loadIdentity

idle :: IdleCallback
idle = postRedisplay Nothing

----------------------------------------------------------------------------------------------------
rTxt :: String -> Schale
rTxt s = R.fromListUnboxed (R.Z R.:. w R.:. h :: R.DIM2) (readTxt s) where
  readTxt [] = []
  readTxt (x:xs)
    | x == '@' = 3 : readTxt xs
    | x == '+' = 2 : readTxt xs
    | x == ':' = 1 : readTxt xs
    | x == '_' = 0 : readTxt xs

box :: R.DIM2 -> [Vertex2 GLdouble]
box (R.Z R.:. x R.:. y) = [
  Vertex2 (h'' * (y' - h')) (w'' * (w' - x')),
  Vertex2 (h'' * (y' - h')) (w'' * (w' - x' - 1)),
  Vertex2 (h'' * (y' - h' + 1)) (w'' * (w' - x' - 1)),
  Vertex2 (h'' * (y' - h' + 1)) (w'' * (w' - x'))] where
  frI = fromIntegral
  (wid, hei) = (frI w, frI h)
  (x', y', w'', h'', w', h') = (frI x, frI y, 2/wid, 2/hei, wid/2, hei/2)

fly :: Mypos -> [R.DIM2]
fly (Mypos (x, y) d)
  | d == Oben = map (wrap . toDIM2) [
                (x-3, y+1), (x-3, y+2),
    (x-2,   y), (x-2, y+1), (x-2, y+2), (x-2, y+3),
    (x-1,   y), (x-1, y+1), (x-1, y+2), (x-1, y+3),
    (  x,   y),                         (  x, y+3)]
  | d == Unten = map (wrap . toDIM2) [
    (x-3,   y),                         (x-3, y+3),
    (x-2,   y), (x-2, y+1), (x-2, y+2), (x-2, y+3),
    (x-1,   y), (x-1, y+1), (x-1, y+2), (x-1, y+3),
                (  x, y+1), (  x, y+2)]
  | d == Links = map (wrap . toDIM2) [
                (x-3, y+1), (x-3, y+2), (x-3, y+3),
    (x-2,   y), (x-2, y+1), (x-2, y+2),
    (x-1,   y), (x-1, y+1), (x-1, y+2),
                (  x, y+1), (  x, y+2), (  x, y+3)]
  | d == Richtig = map (wrap . toDIM2) [
    (x-3,   y), (x-3, y+1), (x-3, y+2),
                (x-2, y+1), (x-2, y+2), (x-2, y+3),
                (x-1, y+1), (x-1, y+2), (x-1, y+3),
    (  x,   y), (  x, y+1), (  x, y+2)] where
  toDIM2 (x, y) = R.Z R.:. x R.:. y
  wrap (R.Z R.:. x R.:. y) = R.Z R.:. (mod x w) R.:. (mod y h)

----------------------------------------------------------------------------------------------------
nextgen :: Schale -> Schale
nextgen s = R.fromListUnboxed (R.Z R.:. w R.:. h :: R.DIM2) (map generate allCell) where
  allCell = map (\(x,y) -> R.Z R.:. x R.:. y) [(x,y) | x <- [0..w-1], y <- [0..h-1]]
  neighbs (R.Z R.:. x R.:. y) = map wrap [
    R.Z R.:. x-1 R.:. y+1, R.Z R.:. x R.:. y+1, R.Z R.:. x+1 R.:. y+1,
    R.Z R.:. x-1 R.:.   y,                      R.Z R.:. x+1 R.:.   y,
    R.Z R.:. x-1 R.:. y-1, R.Z R.:. x R.:. y-1, R.Z R.:. x+1 R.:. y-1]
  wrap (R.Z R.:. x R.:. y) = R.Z R.:. (mod x w) R.:. (mod y h)
  sumAlive x = (length . filter (== 3)) $ map (s R.!) (neighbs x)
  generate x
    | s R.! x == 0 && sumAlive x == 2 = 3
    | s R.! x == 1 = 0
    | s R.! x == 2 = 1
    | s R.! x == 3 && sumAlive x `notElem` [3,4,5] = 2
    | otherwise = s R.! x

