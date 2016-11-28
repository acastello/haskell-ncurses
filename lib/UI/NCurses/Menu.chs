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
{# pointer *ITEM as Item nocode #}
newtype Item = Item { itemPtr :: Ptr Item }
    deriving (Storable, Show, Eq)

nullItem = Item nullPtr

newItem :: String -> String -> Curses Item
newItem name desc = Curses $ do
    nam <- newCString name
    des <- newCString desc
    {# call new_item #} nam des

freeItem :: Item -> Curses ()
freeItem item = Curses $ do
    nam <- {# call item_name #} item
    des <- {# call item_description #} item
    checkRC "freeItem" =<< {# call free_item #} item
    free nam
    free des

-- itemValue :: Item -> Curses Bool
-- itemValue item = Curses $ (/=0) <$> {# call item_value #} item

-- setValue :: Item -> Bool -> Curses ()
-- setValue item val = Curses $ checkRC "setValue" =<< 
    -- {# call set_item_value #} item (fromEnum val)

{# pointer *MENU as Menu nocode #}
newtype Menu = Menu { menuPtr :: Ptr Menu }

newMenu :: Traversable t => t Item -> Curses Menu
newMenu items = Curses $ do
    ptr <- mallocItems items
    menu <- {# call new_menu #} ptr
    when ((menuPtr menu) == nullPtr) $
        throwIO (CursesException "newMenu: new_menu() returned NULL")
    return $ menu

freeMenu :: Menu -> Curses ()
freeMenu menu = Curses $ do
    items <- {# call menu_items #} menu
    checkRC "freeMenu" =<< {# call free_menu #} menu
    free items

menuItems :: Menu -> Curses [Item]
menuItems menu = Curses $ do
    ptr <- {# call menu_items #} menu
    if ptr == nullPtr then
        return []
    else
        peekArray0 nullItem ptr

itemCount :: Menu -> Curses CInt
itemCount menu = Curses $ {# call item_count #} menu

setItems :: Traversable t => Menu -> t Item -> Curses ()
setItems menu xs = Curses $ do
    old <- {# call menu_items #} menu
    ptr <- mallocItems xs
    checkRC "set_menu_items" =<< {# call set_menu_items #} menu ptr
    free old

postMenu :: Menu -> Curses ()
postMenu menu = Curses $ do
    checkRC "postMenu" =<< {# call post_menu #} menu

unpostMenu :: Menu -> Curses ()
unpostMenu menu = Curses $ do
    checkRC "unpostMenu" =<< {# call unpost_menu #} menu
    
menuMark :: Menu -> Curses String
menuMark menu = Curses $ do
    str <- {# call menu_mark #} menu
    when (str == nullPtr) $
        throwIO (CursesException "menuMark: menu_mark() returned NULL")
    peekCString str

setMark :: Menu -> String -> Curses ()
setMark menu str = Curses $ do
    str' <- newCString str
    checkRC "setMenuMark" =<< {# call set_menu_mark #} menu str'

setSpacing :: Menu -> CInt -> CInt -> CInt -> Curses ()
setSpacing menu desc rows cols = Curses $ do
    checkRC "setSpacing" =<< {# call set_menu_spacing #} menu desc rows cols

menuWindow :: Menu -> Curses Window
menuWindow = Curses . {# call menu_win #}

setWindow :: Menu -> Window -> Curses ()
setWindow menu win = Curses $ do
    checkRC "setWin" =<< {# call set_menu_win #} menu win

menuDriver :: Menu -> CInt -> Curses CInt
menuDriver menu code = Curses $ {# call menu_driver #} menu code

currentItem :: Menu -> Curses Item
currentItem menu = Curses $ {# call current_item #} menu

setItem :: Menu -> Item -> Curses ()
setItem menu item = Curses $ checkRC "setItem" =<< {# call set_current_item #} menu item


-- defines

{#enum define MenuOpts
    { 

-- utils

mallocItems :: Traversable t => t Item -> IO (Ptr Item)
mallocItems items = do
    let l = length items
    ptr <- mallocArray (l+1)
    pokeTrav ptr items
    pokeElemOff ptr l nullItem
    return ptr

pokeTrav :: (Storable a, Traversable t) => Ptr a -> t a -> IO ()
pokeTrav ptr items = sequence_ . snd $ mapAccumL
    (\i x -> (i+1, pokeElemOff ptr i x)) 0 items
