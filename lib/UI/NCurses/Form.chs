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

setContents :: Field -> String -> Curses ()
setContents fi str = Curses $ do
    ptr <- newCString str
    checkRC "setContents" =<< {# call set_field_buffer #} fi 0 ptr

setForeground :: Field -> [Attribute] -> Curses ()
setForeground fi attrs = Curses $ checkRC "setForeground" =<<
    {# call set_field_fore #} fi (foldl (\i j -> i .|. attrToInt j) 0 attrs) 

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

--
-- utils
--

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
