{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DoAndIfThenElse #-}

module UI.NCurses.Form where

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

newField :: Curses Field
newField = Curses $ do
    ptr <- {# call new_field #}
    when (fieldPtr ptr == nullPtr) $ throwIO $ CursesException "new_field() returned NULL"
    return ptr

{# pointer *FORM as Form nocode #}
newtype Form = Form { formPtr :: Ptr Form }

newForm :: [Field] -> Curses Form
newForm fs = Curses $ do
    fptr <- mallocFields fs
    ptr <- {# call new_form #} fptr
    when (formPtr ptr == null) $ throwIO $ CursesException "new_form() return NULL"

--
-- utils
--
mallocFields :: [Field] -> IO (Ptr Field)
mallocFields fs = do
    let l = length fs
    ptr <- mallocArray (l+1)
    pokeTrav ptr items
    pokeElemOff ptr l (Field nullPtr)
    return ptr
    

pokeTrav :: (Storable a, Traversable t) => Ptr a -> t a -> IO ()
pokeTrav ptr items = sequence_ . snd $ mapAccumL
    (\i x -> (i+1, pokeElemOff ptr i x)) 0 items
