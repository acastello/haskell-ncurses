{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DoAndIfThenElse #-}

module UI.NCurses.Menu where

import Data.IORef
import Data.Traversable

import Control.Monad
import Control.Exception (throwIO)

import Foreign
import Foreign.C

import UI.NCurses ()
import UI.NCurses.Types

#ifdef HSNCURSES_NARROW_HEADER
#   include <menu.h>
#else
#   include <ncursesw/menu.h>
#endif

{# pointer *WINDOW as Window nocode #}
{# pointer *ITEM as Item_ nocode #}
newtype Item_ = Item_ { itemPtr_ :: Ptr Item_ }
    deriving (Storable, Show, Eq)

data Item = Item
    { itemPtr   :: Item_
    , itemName  :: CString
    , itemDesc  :: CString
    }

data APtr a = APtr
    { a_ptr :: Ptr a
    , a_n   :: Int
    , a_sz  :: Int
    }
    deriving Show

instance Eq (APtr a) where
    (APtr p1 _ _) == (APtr p2 _ _) = p1 == p2

addAPtr :: (Storable a, Traversable t) => APtr a -> t a -> IO (APtr a)
addAPtr (APtr ptr n sz) xs = do
    let l = length xs
    newptr <- if l+n>=sz then 
            reallocArray ptr (2*sz+l)
        else
            return ptr
    sequence_ . snd $ mapAccumL (\i x -> (i+1,pokeElemOff newptr i x)) n xs
    poke (castPtr $ newptr `advancePtr` (n+l)) (0 :: Word)
    return $ APtr newptr (n+l) (2*sz+l)

newAPtr :: (Storable a, Traversable t) => t a -> IO (APtr a)
newAPtr xs = do
    let l = length xs
        sz = 64+l
    ptr <- mallocArray l
    sequence_ . snd $ mapAccumL (\i x -> (i+1,pokeElemOff ptr i x)) 0 xs
    poke (castPtr $ ptr `advancePtr` l) (0 :: Word)
    return $ APtr ptr l sz

freeAPtr :: APtr a -> IO ()
freeAPtr (APtr ptr _ _) = do
    free ptr 

emptyAPtr :: (Storable a) => APtr a -> IO (APtr a)
emptyAPtr (APtr ptr _ sz) = do
    poke (castPtr ptr) (0 :: Word)
    return (APtr ptr 0 sz)

newItem :: String -> String -> Curses Item 
newItem name desc = Curses $ do
    nam <- newCString name 
    des <- newCString desc
    item <- {# call new_item #} nam des
    when ((itemPtr_ item)==nullPtr) $ 
        throwIO (CursesException "newItem: new_item() returned NULL")
    return $ Item item nam des 

freeItem :: Item -> Curses ()
freeItem (Item item name desc) = Curses $ do
    checkRC "freeItem" =<< {# call free_item #} item
    free name
    free desc

{# pointer *MENU as Menu_ nocode #}
newtype Menu_ = Menu_ { menuPtr_ :: Ptr Menu_ }

data Menu = Menu
    { menuPtr   :: Menu_
    , menuRef :: IORef (APtr Item_)
    } 

newMenu :: Traversable t => t Item -> Curses Menu
newMenu items = Curses $ do
    aptr@(APtr ptr _ _) <- newAPtr (itemPtr <$> items)
    ref <- newIORef aptr
    m <- {# call new_menu #} ptr
    when ((menuPtr_ m) == nullPtr) $
        throwIO (CursesException "newMenu: new_menu() returned NULL")
    return $ Menu m ref

freeMenu :: Menu -> Curses ()
freeMenu (Menu menu iptr) = Curses $ do
    checkRC "freeMenu" =<< {# call free_menu #} menu
    freeAPtr =<< readIORef iptr

emptyMenu :: Menu -> Curses ()
emptyMenu (Menu menu iptr) = do
    aptr <- Curses $ readIORef iptr
    aptr' <- Curses $ newAPtr []
    Curses $ print aptr'
    unpostMenu (Menu menu iptr)
    Curses $ checkRC "setItems" =<< 
        {# call set_menu_items #} menu (a_ptr aptr')
    Curses $ writeIORef iptr aptr'

-- setItems :: Menu -> [Item] -> Curses  ()
-- setItems items = Curses $ do
    -- ptr <- newArray $ (itemPtr <$> items) `mappend` [Item_ nullPtr]
    
setItems :: Traversable t => Menu -> t Item -> Curses ()
setItems (Menu menu ref) xs = Curses $ do
    aptr' <- newAPtr $ itemPtr <$> xs
    checkRC "set_menu_items" =<< {# call set_menu_items #} menu (a_ptr aptr')

addItems :: Traversable t => Menu -> t Item -> Curses ()
addItems (Menu menu ref) xs = Curses $ do
    aptr <- readIORef ref
    aptr' <- addAPtr aptr (itemPtr <$> xs)
    when (aptr /= aptr') $
        checkRC "addItems" =<< {# call set_menu_items #} menu (a_ptr aptr')

postMenu :: Menu -> Curses ()
postMenu (Menu menu _) = Curses $ do
    checkRC "postMenu" =<< {# call post_menu #} menu

unpostMenu :: Menu -> Curses ()
unpostMenu (Menu menu _) = Curses $ do
    checkRC "unpostMenu" =<< {# call unpost_menu #} menu
    
menuMark :: Menu -> Curses String
menuMark m = Curses $ do
    str <- {# call menu_mark #} (menuPtr m)
    when (str == nullPtr) $
        throwIO (CursesException "menuMark: menu_mark() returned NULL")
    peekCString str

setMark :: Menu -> String -> Curses ()
setMark menu str = Curses $ do
    str' <- newCString str
    checkRC "setMenuMark" =<< {# call set_menu_mark #} (menuPtr menu) str'

setSpacing :: Menu -> CInt -> CInt -> CInt -> Curses ()
setSpacing (Menu ptr _) desc rows cols = Curses $ do
    checkRC "setSpacing" =<< {# call set_menu_spacing #} ptr desc rows cols

setWindow :: Menu -> Window -> Curses ()
setWindow (Menu ptr _) win = Curses $ do
    checkRC "setWin" =<< {# call set_menu_win #} ptr win
