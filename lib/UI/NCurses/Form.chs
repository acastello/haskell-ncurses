{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DoAndIfThenElse #-}

module UI.NCurses.Form where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Traversable

import Foreign  
import Foreign.C

import UI.NCurses.Types
import UI.NCurses ()

#ifdef HSNCURSES_NARROW_HEADER
#   include <form.h>
#else
#   include <ncursesw/form.h>
#endif

{# pointer *WINDOW as Window nocode #}

{# pointer *FIELD as Field nocode #}
newtype Field = Field { fieldPtr :: Ptr Field }
    deriving (Show, Storable)

instance UserData Field where
    getDataPtr = Curses . {# call field_userptr #}
    setDataPtr e v = Curses $ checkRC "setUserPtr" =<<
        {# call set_field_userptr #} e v

newField :: CInt -> CInt -> CInt -> CInt -> Curses Field
newField h w y x = Curses $ do
    ptr <- {# call new_field #} h w y x 0 0
    when (fieldPtr ptr == nullPtr) $ throwIO $ CursesException "new_field() returned NULL"
    return ptr

freeField :: Field -> Curses ()
freeField fp = Curses $ checkRC "freeField" =<<
    {# call free_field #} fp

fieldContents :: Field -> Curses String
fieldContents fi = Curses $ do
    str <- {# call field_buffer #} fi 0
    peekCString str

setFieldSize :: Field -> CInt -> Curses ()
setFieldSize field n = Curses $ checkRC "setFieldSize" =<<
    {# call set_max_field #} field n

setChanged :: Field -> Curses ()
setChanged field = Curses $ checkRC "setChanged" =<<
    set_field_status field (cFromBool True)

setContents :: Field -> String -> Curses ()
setContents fi str = Curses $ do
    ptr <- newCString str
    checkRC "setContents" =<< {# call set_field_buffer #} fi 0 ptr

setBackground :: Field -> [Attribute] -> Curses ()
setBackground fi attrs = Curses $ checkRC "setBackground" =<<
    {# call set_field_back #} fi (foldl (\i j -> i .|. attrToInt j) 0 attrs) 

setForeground :: Field -> [Attribute] -> Curses ()
setForeground fi attrs = Curses $ checkRC "setForeground" =<<
    {# call set_field_fore #} fi (foldl (\i j -> i .|. attrToInt j) 0 attrs) 

setOpts :: Field -> [FieldOpt] -> Curses ()
setOpts field opts = Curses $ checkRC "setOpts" =<<
    {# call set_field_opts #} field (l2b opts)

enableOpts :: Field -> [FieldOpt] -> Curses ()
enableOpts field opts = Curses $ checkRC "enableOpts" =<<
    {# call field_opts_on #} field (l2b opts)

disableOpts :: Field -> [FieldOpt] -> Curses ()
disableOpts field opts = Curses $ checkRC "disableOpts" =<<
    {# call field_opts_off #} field (l2b opts)

fieldOpts :: Field -> Curses [FieldOpt]
fieldOpts field = Curses $ b2l <$> {# call field_opts #} field

{# pointer *FORM as Form nocode #}
newtype Form = Form { formPtr :: Ptr Form }

newForm :: [Field] -> Curses Form
newForm fs = Curses $ do
    fptr <- mallocFields fs
    ptr <- {# call new_form #} fptr
    when (formPtr ptr == nullPtr) $ throwIO $ CursesException "new_form() return NULL"
    return ptr

freeForm :: Form -> Curses ()
freeForm fo = Curses $ checkRC "freeFrom" =<< {# call free_form #} fo

postForm :: Form -> Curses ()
postForm fo = Curses $ checkRC "postForm" =<< {# call post_form #} fo

unpostForm :: Form -> Curses ()
unpostForm fo = Curses $ checkRC "postForm" =<< {# call unpost_form #} fo

setWin :: Form -> Window -> Curses ()
setWin fo win = Curses $ checkRC "setWin" =<< {# call set_form_win #} fo win

setSubWin :: Form -> Window -> Curses ()
setSubWin fo wi = Curses $ checkRC "setSubWin" =<< {# call set_form_sub #} fo wi


-- defines

{# enum define FieldOpt
    { O_ACTIVE as Active
    , O_AUTOSKIP as AutoSkip
    , O_BLANK as Blank
    , O_EDIT as Edit
    , O_NULLOK as NullOk
    , O_PASSOK as PassOk
    , O_PUBLIC as Public
    , O_STATIC as Static
    , O_VISIBLE as Visible
    , O_WRAP as Wrap
    } deriving (Show, Eq, Ord) #}

--
-- utils
--

l2b :: (Integral b, Enum a) => [a] -> b
l2b xs = fromIntegral $ foldl (.|.) zeroBits (fromEnum <$> xs)

b2l :: (FiniteBits a, Enum b) => a -> [b]
b2l b = toEnum <$> bits where
    bits = bit <$> (filter (testBit b) [0..(finiteBitSize b)])

fe :: Enum a => a -> CInt
fe = fromIntegral . fromEnum

mallocFields :: [Field] -> IO (Ptr Field)
mallocFields [] = return nullPtr
mallocFields fs = do
    let l = length fs
    ptr <- mallocArray (l+1)
    pokeTrav ptr fs
    pokeElemOff ptr l (Field nullPtr)
    return ptr
    
pokeTrav :: (Storable a, Traversable t) => Ptr a -> t a -> IO ()
pokeTrav ptr items = sequence_ . snd $ mapAccumL
    (\i x -> (i+1, pokeElemOff ptr i x)) 0 items


-- manual imports

foreign import ccall "set_field_status"
    set_field_status :: Field -> CInt -> IO CInt
