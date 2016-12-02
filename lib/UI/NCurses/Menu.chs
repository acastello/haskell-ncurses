{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DoAndIfThenElse #-}

module UI.NCurses.Menu where

import Data.Char
import Data.Foldable
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
    deriving (Storable, Show, Eq, Ord)

nullItem = Item nullPtr

newItem :: String -> String -> Curses Item
newItem "" desc = newItem " " desc
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
    deriving (Storable, Show, Eq, Ord)

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
setMark menu str = Curses $ withCString str $ 
    \str' -> checkRC "setMenuMark" =<< {# call set_menu_mark #} menu str'

setSpacing :: Menu -> CInt -> CInt -> CInt -> Curses ()
setSpacing menu desc rows cols = Curses $ do
    checkRC "setSpacing" =<< {# call set_menu_spacing #} menu desc rows cols

menuWindow :: Menu -> Curses Window
menuWindow = Curses . {# call menu_win #}

setWin :: Menu -> Window -> Curses () 
setWin menu win = Curses $ 
    checkRC "setWin" =<< {# call set_menu_win #} menu win

setSubWin :: Menu -> Window -> Curses ()
setSubWin menu win = Curses $ 
    checkRC "setSubWin" =<< {# call set_menu_sub #} menu win

menuDriver :: Menu -> CInt -> Curses CInt
menuDriver menu code = Curses $ {# call menu_driver #} menu code

request :: Menu -> Request -> Curses ()
request menu req = (Curses . checkRC "request") =<< 
    menuDriver menu (fe req)

search :: Menu -> Char -> Curses ()
search menu c = (Curses . checkRC "search") =<< 
    menuDriver menu (fromIntegral $ 0xff .&. (ord c))

searchs :: Menu -> String -> Curses ()
searchs menu = traverse_ (search menu)

currentItem :: Menu -> Curses Item
currentItem menu = Curses $ {# call current_item #} menu

setCurrent :: Menu -> Item -> Curses ()
setCurrent menu item = Curses $ checkRC "setItem" =<< {# call set_current_item #} menu item

menuOpts :: Menu -> Curses [MenuOpt]
menuOpts menu = Curses $ b2l <$> {# call menu_opts #} menu

setOpts :: Menu -> [MenuOpt] -> Curses ()
setOpts menu opts = Curses $ checkRC "setOpts" =<<
    {# call set_menu_opts #} menu (l2b opts)

enableOpts :: Menu -> [MenuOpt] -> Curses ()
enableOpts menu opts = Curses $ checkRC "enableOpts" =<<
    {# call menu_opts_on #} menu (l2b opts)

disableOpts :: Menu -> [MenuOpt] -> Curses ()
disableOpts menu opts = Curses $ checkRC "disableOpts" =<<
    {# call menu_opts_off #} menu (l2b opts)

menuFormat :: Menu -> Curses (CInt, CInt)
menuFormat menu = Curses $ do
    p1 <- malloc
    p2 <- malloc
    {# call menu_format #} menu p1 p2
    i1 <- peek p1
    i2 <- peek p2
    free p1
    free p2
    return (i1,i2)

setFormat :: Menu -> CInt -> CInt -> Curses ()
setFormat menu rows cols = Curses $ checkRC "setFormat" =<<
    {# call set_menu_format #} menu rows cols



-- defines

{# enum define MenuOpt
    { O_ONEVALUE as OneValue
    , O_SHOWDESC as ShowDesc
    , O_ROWMAJOR as RowMajor
    , O_IGNORECASE as IgnoreCase
    , O_SHOWMATCH as ShowMatch
    , O_NONCYCLIC as NonCyclic
    } deriving (Show, Eq, Ord) #} 

{# enum define ItemOpt
    { O_SELECTABLE as Selectable 
    } deriving (Show, Eq, Ord) #}

{# enum define Request
    { REQ_LEFT_ITEM     as LeftItem
    , REQ_RIGHT_ITEM    as RightItem
    , REQ_UP_ITEM       as UpItem
    , REQ_DOWN_ITEM     as DownItem
    , REQ_SCR_ULINE     as UpLine
    , REQ_SCR_DLINE     as DownLine
    , REQ_SCR_DPAGE     as DownPage
    , REQ_SCR_UPAGE     as UpPage
    , REQ_FIRST_ITEM    as FirstItem
    , REQ_LAST_ITEM     as LastItem
    , REQ_NEXT_ITEM     as NextItem
    , REQ_PREV_ITEM     as PrevItem
    , REQ_TOGGLE_ITEM   as ToggleItem
    , REQ_CLEAR_PATTERN as ClearPattern
    , REQ_BACK_PATTERN  as BackPattern
    , REQ_NEXT_MATCH    as NextMatch
    , REQ_PREV_MATCH    as PrevMatch
    } deriving (Show, Eq, Ord) #}

-- utils

l2b :: (Integral b, Enum a) => [a] -> b
l2b xs = fromIntegral $ foldl (.|.) zeroBits (fromEnum <$> xs)

b2l :: (FiniteBits a, Enum b) => a -> [b]
b2l b = toEnum <$> bits where
    bits = bit <$> (filter (testBit b) [0..(finiteBitSize b)])

fe :: Enum a => a -> CInt
fe = fromIntegral . fromEnum

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
