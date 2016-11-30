module UI.NCurses.Simple where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State

import Data.Foldable 

import qualified UI.NCurses as N
import qualified UI.NCurses.Types as NT
import qualified UI.NCurses.Menu as M

type RelPos = (Double, Integer)

data Window = Window
    { w_win :: N.Window
    , w_yo  :: RelPos
    , w_xo  :: RelPos
    , w_yf  :: RelPos
    , w_xf  :: RelPos
    } deriving (Show)

instance Eq Window where
    w == w' = (w_win w) == (w_win w')

instance Ord Window where
    compare w w' = compare (w_win w) (w_win w')

data Menu = Menu
    { m_menu    :: M.Menu
    , m_win     :: Window
    , m_subwin  :: Window
    , m_title   :: String
    } deriving Show

instance Eq Menu where
    m == n = (m_menu m) == (m_menu n)

instance Ord Menu where
    compare m n = compare (m_menu m) (m_menu n)

data RunTime = RunTime
    { windows   :: [Window]
    , subwins   :: [Window]
    , menus     :: [Menu]
    , height    :: Integer
    , width     :: Integer 
    } deriving (Show)

type Curses = StateT RunTime N.Curses

runCurses :: Curses a -> IO a
runCurses op = N.runCurses $ do
    (h,w) <- N.screenSize
    N.setEcho False
    N.setCursorMode N.CursorInvisible
    (a,s) <- runStateT op (RunTime [] [] [] h w)
    {- N.tryCurses $ -}
    do
        traverse M.freeMenu $ m_menu <$> menus s
        traverse N.closeWindow $ w_win <$> subwins s
        traverse N.closeWindow $ w_win <$> windows s
    return a

newWindow :: RelPos -> RelPos -> RelPos -> RelPos -> Curses Window
newWindow yo xo yf xf = do
    rt @ RunTime { windows = ws, height=h, width=w } <- get
    let y' = multRP yo h
        x' = multRP xo w
        h' = (multRP yf h)-y'
        w' = (multRP xf w)-x'
    win <- lift $ N.newWindow h' w' y' x'
    let win' = Window win yo xo yf xf
    put rt { windows = win':ws }
    return win'

newSubWindow :: Window -> RelPos -> RelPos -> RelPos -> RelPos -> Curses Window
newSubWindow win yo xo yf xf = do
    rt @ RunTime { subwins=ws, height=h, width=w } <- get
    let y' = multRP yo h
        x' = multRP xo w
        h' = (multRP yf h)-y'
        w' = (multRP xf w)-x'
    win <- lift $ N.subWindow (w_win win) h' w' y' x'
    let win' = Window win yo xo yf xf
    put rt { subwins = win':ws }
    return win'
    
deleteWindow :: Window -> Curses ()
deleteWindow win = do
    rt @ RunTime { windows = ws } <- get
    put rt { windows = filter (/=win) ws }
    lift $ N.closeWindow $ w_win win

adjustWindow :: Window -> Curses ()
adjustWindow win = do
    rt @ RunTime { windows = ws, height = h', width = w' } <- get
    let y = multRP (w_yo win) h'
        x = multRP (w_xo win) w'
        h = (multRP (w_yf win) h')-y
        w = (multRP (w_xf win) w')-x
    lift $ N.updateWindow (w_win win) $ do
        N.resizeWindow h w
        N.moveWindow y x

newMenu :: String -> RelPos -> RelPos -> RelPos -> RelPos -> Curses Menu
newMenu t yo xo yf xf = do
    rt @ RunTime { menus = ms } <- get
    w <- newWindow yo xo yf xf
    sw <- newSubWindow w ((+1) <$> yo) ((+1) <$> xo) 
        ((subtract 1) <$> yf) ((subtract 1) <$> xf)
    m <- lift $ do
        m' <- M.newMenu []
        M.setWin m' (w_win w)
        M.setSubWin m' (w_win sw)
        return m'
    let menu = Menu m w sw t
    put rt { menus = menu:ms }
    return menu
setItems :: Menu -> [(String, String)] -> Curses ()
setItems m xs = do
    items <- lift $ traverse (uncurry M.newItem) xs
    lift $ M.setItems (m_menu m) items

postMenu :: Menu -> Curses ()
postMenu menu = do
    let w = m_win menu
        t = m_title menu
    lift $ M.postMenu (m_menu menu)
    lift $ N.updateWindow (w_win w) $ do
        N.moveCursor 0 2
        N.drawBox Nothing Nothing
        N.drawString t

adjust :: Curses ()
adjust = do
    rt @ RunTime {windows=ws, height=h, width=w} <- get
    (h',w') <- lift $ N.screenSize
    when (h'/=h || w'/=w) $ do
        put rt {height=h', width=w'}
        traverse_ adjustWindow ws
    
-- util

mult :: Integral a => Double -> a -> a
mult d i = round $ d * fromIntegral i

multRP :: RelPos -> Integer -> Integer
multRP (d,i) j = i+d`mult`j

test = runCurses $ do
    dft <- lift N.defaultWindow
    lift $ N.getEvent dft (Just 10)
    -- st <- get
    w <- newWindow (0,3) (0,3) (1,-3) (1,-3)
    m <- newMenu "test" (0,1) (0,1) (0,20) (0,20)
    setItems m [("1", "_"), ("2asd", "_ashd"), ("c","_")]
    lift $ N.updateWindow (w_win w) $ do  
        N.drawBox Nothing Nothing
    lift $ N.render
    postMenu m
    lift N.render
    flip mplus (return ()) $ forever $ do
        ev <- lift $ N.getEvent (w_win $ m_win m) (Just 100)
        case ev of
            Just (N.EventCharacter 'q') -> liftIO mzero
            Just N.EventResized -> do
                adjust
                lift $ N.updateWindow (w_win w) $ N.drawBox Nothing Nothing
            _ -> return ()       
    get

test2 = runCurses $ do
    menu <- newMenu "test2" (0,1) (0,1) (0,10) (0,20)
    w <- lift $ M.menuWindow (m_menu menu)
    setItems menu [("100", ""), ("200",""), ("ccc","")]
    lift $ do
        M.postMenu (m_menu menu)
        N.refresh
    lift $ N.refresh
    flip mplus (return ()) $ forever $ do
        ev <- lift $ N.getEvent w (Just 100)
        case ev of
            Just (N.EventCharacter 'q') -> liftIO mzero
            Just N.EventResized -> do
                adjust
                lift $ N.updateWindow w $ N.drawBox Nothing Nothing
                -- lift $ N.render
                liftIO $ print 1
            _ -> return ()       
    get
    
test3 = runCurses $ do
    w <- newWindow (0,0) (0,0) (1,0) (1,0)
    w' <- newSubWindow w (0,1) (0,1) (1,-1) (1,-1)
    -- sw <- newSubWindow w (0,0) (0,0) (1,0) (1,0)
    return (w')
    -- get
