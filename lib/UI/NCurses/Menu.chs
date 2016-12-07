{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DoAndIfThenElse #-}

module UI.NCurses.Menu where

import Data.Char
import Data.Foldable
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Traversable

import Control.Monad
import Control.Exception (throwIO, try)

import Foreign hiding (void)
import Foreign.C

import UI.NCurses 
import UI.NCurses.Types

#include "cbits/mavericks-c2hs-workaround.h"

#define NCURSES_ENABLE_STDBOOL_H 0
#ifndef _XOPEN_SOURCE_EXTENDED
#   define _XOPEN_SOURCE_EXTENDED
#endif
#define NCURSES_NOMACROS
#ifdef HSNCURSES_NARROW_HEADER
#   include <menu.h>
#else
#   include <ncursesw/menu.h>
#endif

#include "cbits/hsncurses-shim.h"

{# pointer *WINDOW as Window nocode #}
{# pointer *ITEM as Item nocode #}
{# pointer *cchar_t as CCharT nocode #}
{# pointer *wchar_t as CWString nocode #}

newtype Item = Item { itemPtr :: Ptr Item }
    deriving (Storable, Show, Eq, Ord)

instance UserData Item where
    getDataPtr = Curses . {# call item_userptr #}
    setDataPtr e v = Curses $ checkRC "setUserPtr" =<< 
        {# call set_item_userptr #} e v

nullItem = Item nullPtr

newItem :: String -> String -> Curses Item
newItem "" desc = newItem " " desc
newItem name desc = Curses $ do
    nam <- newCString name
    des <- newCString desc
    {# call new_item #} nam des

newItemWith :: String -> String -> a -> Curses Item
newItemWith nam des v = do
    item <- newItem nam des
    setData item v
    return item

freeItem :: Item -> Curses ()
freeItem item = Curses $ do
    nam <- {# call item_name #} item
    des <- {# call item_description #} item
    sptr <- {# call item_userptr #} item
    checkRC "freeItem" =<< {# call free_item #} item
    unless (sptr==nullPtr) $ freeStablePtr $ castPtrToStablePtr sptr
    free nam
    free des

itemName :: Item -> Curses String
itemName item = Curses $ do
    ptr <- {# call item_name #} item
    if ptr == nullPtr then
        return ""
    else
        peekCString ptr

itemDesc :: Item -> Curses String
itemDesc item = Curses $ do
    ptr <- {# call item_description #} item
    if ptr == nullPtr then
        return ""
    else
        peekCString ptr

itemIndex :: Item -> Curses CInt
itemIndex item = Curses $ {# call item_index #} item


itemValue :: Item -> Curses Bool
itemValue item = Curses $ cToBool <$> {# call item_value #} item

setValue :: Item -> Bool -> Curses ()
setValue item val = Curses $ checkRC "setValue" =<< 
    {# call set_item_value #} item (cFromBool val)

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

menuData :: Menu -> Curses [a]
menuData menu = fmap catMaybes $
    menuItems menu >>= traverse getDataMaybe 

itemCount :: Menu -> Curses CInt
itemCount menu = Curses $ {# call item_count #} menu

setItems :: Traversable t => Menu -> t Item -> Curses ()
setItems menu xs = Curses $ do
    old <- {# call menu_items #} menu
    ptr <- mallocItems xs
    checkRC "set_menu_items" =<< {# call set_menu_items #} menu ptr
    unless (old==nullPtr) $ peekArray0 nullItem old 
        >>= unCurses . traverse_ freeItem
    free old

overrideIndex :: Menu -> Int -> Item -> Curses ()
overrideIndex menu i item = Curses $ do
    ptr <- {# call menu_items #} menu
    iptr <- peekElemOff ptr i
    pokeElemOff ptr i item
    {# call set_menu_items #} menu ptr
    unCurses $ freeItem iptr

addItems :: Traversable t => Menu -> t Item -> Curses ()
addItems menu items = Curses $ do
    ptr <- {# call menu_items #} menu
    n <- fromIntegral <$> {# call item_count #} menu
    let n' = length items
    if n+n' == 0 then
        return ()
    else do
        ptr' <- mallocArray (n+n'+1)
        copyArray ptr' ptr n
        pokeTrav (advancePtr ptr' n) items
        pokeElemOff ptr' (n+n') nullItem
        checkRC "addItems" =<< {# call set_menu_items #} menu ptr'
        free ptr

swapIndices :: Menu -> Int -> Int -> Curses ()
swapIndices menu i j = Curses $ do
    ptr <- {# call menu_items #} menu
    n <- fromIntegral <$> {# call item_count #} menu
    when (j>=n || i>=n) $ throwIO $ CursesException "out of bounds"
    it <- peekElemOff ptr i
    it' <- peekElemOff ptr j
    pokeElemOff ptr i it'
    pokeElemOff ptr j it
    checkRC "swapIndices" =<< {# call set_menu_items #} menu ptr

moveToIndex :: Menu -> Int -> Int -> Curses ()
moveToIndex menu i j = Curses $ do
    ptr <- {# call menu_items #} menu
    it <- peekElemOff ptr i
    let adv = signum (j-i)
    


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

menuWin :: Menu -> Curses Window
menuWin = Curses . {# call menu_win #}

menuSubWin :: Menu -> Curses Window
menuSubWin = Curses . {# call menu_sub #}

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

clickMenu :: Menu -> CInt -> CInt -> Curses ()
clickMenu menu y x = do
    win <- menuSubWin menu
    (y',_) <- updateWindow win windowPosition
    top <- topRow menu
    n <- itemCount menu
    let index = (y-fromIntegral y')+top
    when (index<n && index>=0) $ do
        setIndex menu index
        updateWindow win (return ())
        render  

search :: Menu -> Char -> Curses ()
search menu c = (Curses . checkRC "search") =<< 
    menuDriver menu (fromIntegral $ 0xff .&. (ord c))

searchs :: Menu -> String -> Curses ()
searchs menu = traverse_ (search menu)

topRow :: Menu -> Curses CInt
topRow = Curses . {# call top_row #}
    
setTopRow :: Menu -> CInt -> Curses ()
setTopRow menu i = Curses $
    checkRC "setTopRow" =<< {# call set_top_row #} menu i

getIndex :: Menu -> Curses CInt
getIndex menu = Curses $ do
    {# call item_index #} =<< {# call current_item #} menu

setIndex :: Menu -> CInt -> Curses ()
setIndex menu i = Curses $ do
    ptr <- {# call menu_items #} menu
    item <- peekElemOff ptr (fromIntegral i)
    checkRC "setIndex" =<< {# call set_current_item #} menu item

currentItem :: Menu -> Curses Item
currentItem menu = Curses $ {# call current_item #} menu

setCurrent :: Menu -> Item -> Curses ()
setCurrent menu item = Curses $ checkRC "setItem" =<< {# call set_current_item #} menu item

currentData :: Menu -> Curses (Maybe a)
currentData menu = do
    item <- currentItem menu
    if item==Item nullPtr then
        return Nothing
    else
        getDataMaybe item

setForeground :: Menu -> [Attribute] -> Curses ()
setForeground menu attrs = Curses $ checkRC "setForeground" =<< 
    {# call set_menu_fore #} menu (foldl (\i j -> i .|. attrToInt j) 0 attrs)

setUnselectable :: Menu -> [Attribute] -> Curses ()
setUnselectable menu attrs = Curses $ checkRC "setUnselectable" =<<
    {# call set_menu_grey #} menu (foldl (\i j -> i .|. attrToInt j) 0 attrs)

setBackground :: Menu -> [Attribute] -> Curses ()
setBackground menu attrs = Curses $ checkRC "setBackground" =<<
    {# call set_menu_back #} menu (foldl (\i j -> i .|. attrToInt j) 0 attrs)

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

isSelectable :: Item -> Curses Bool
isSelectable it = Curses $ 
    (=={# const O_SELECTABLE #}) <$> {# call item_opts #} it

selectable :: Item -> Bool -> Curses ()
selectable item st = Curses $ do
    checkRC "selectable" =<< {# call set_item_opts #} item 
        (if st then {# const O_SELECTABLE #} else 0)


-- defines

{# enum define MenuOpt
    { O_ONEVALUE as OneValue
    , O_SHOWDESC as ShowDesc
    , O_ROWMAJOR as RowMajor
    , O_IGNORECASE as IgnoreCase
    , O_SHOWMATCH as ShowMatch
    , O_NONCYCLIC as NonCyclic
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
    if (l==0) then 
        return nullPtr
    else do 
        ptr <- mallocArray (l+1)
        pokeTrav ptr items
        pokeElemOff ptr l nullItem
        return ptr

pokeTrav :: (Storable a, Traversable t) => Ptr a -> t a -> IO ()
pokeTrav ptr items = sequence_ . snd $ mapAccumL
    (\i x -> (i+1, pokeElemOff ptr i x)) 0 items

