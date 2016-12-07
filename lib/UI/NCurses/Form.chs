{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DoAndIfThenElse #-}

module UI.NCurses.Form where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Char
import Data.Traversable

import Foreign hiding (void)
import Foreign.C

import UI.NCurses.Types
import UI.NCurses ()

#ifndef NCURSES_WIDECHAR
#   define NCURSES_WIDECHAR 1
#endif

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

formWin :: Form -> Curses Window
formWin form = Curses $ do  
    ptr <- {# call form_win #} form
    when (ptr == Window nullPtr) $ throwIO $ 
        CursesException "form_win() returned NULL"
    return ptr

request :: Form -> Request -> Curses ()
request form req = Curses $ checkRC "request" =<< 
    {# call form_driver #} form (fe req)

write :: Form -> Char -> Curses ()
write form char = void $ formDriver form 0 (fromIntegral $ ord char)

formDriver :: Form -> CInt -> CInt -> Curses CInt
formDriver form code char = Curses $ {# call form_driver_w #} form code char


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

{# enum define Request
    { REQ_BEG_FIELD     as BeginField
    , REQ_BEG_LINE      as BeginLine
    , REQ_CLR_EOF       as ClearEOF
    , REQ_CLR_EOL       as ClearEOL
    , REQ_CLR_FIELD     as ClearField
    , REQ_DEL_CHAR      as DeleteChar
    , REQ_DEL_LINE      as DeleteLine
    , REQ_DEL_PREV      as DeletePrev
    , REQ_DEL_WORD      as DeleteWord
    , REQ_DOWN_CHAR     as DownChar
    , REQ_DOWN_FIELD    as DownField
    , REQ_END_FIELD     as EndField
    , REQ_END_LINE      as EndLine
    , REQ_FIRST_FIELD   as FirstField
    , REQ_FIRST_PAGE    as FirstPage
    , REQ_INS_CHAR      as InsertChar
    , REQ_INS_LINE      as InsertLine
    , REQ_INS_MODE      as InsertMode
    , REQ_LAST_FIELD    as LastField
    , REQ_LAST_PAGE     as LastPage
    , REQ_LEFT_CHAR     as LeftChar
    , REQ_LEFT_FIELD    as LeftField
    , REQ_NEW_LINE      as NewLine
    , REQ_NEXT_CHAR     as NextChar
    , REQ_NEXT_CHOICE   as NextChoice 
    , REQ_NEXT_FIELD    as NextField
    , REQ_NEXT_LINE     as NextLine
    , REQ_NEXT_PAGE     as NextPage
    , REQ_NEXT_WORD     as NextWord
    , REQ_OVL_MODE      as OverlayMode
    , REQ_PREV_CHAR     as PrevChar
    , REQ_PREV_CHOICE   as PrevChoice
    , REQ_PREV_FIELD    as PrevField
    , REQ_PREV_LINE     as PrevLine
    , REQ_PREV_PAGE     as PrevPage
    , REQ_PREV_WORD     as PrevWord
    , REQ_RIGHT_CHAR    as RightChar
    , REQ_RIGHT_FIELD   as RightField
    , REQ_SCR_BCHAR     as BackChar
    , REQ_SCR_BHPAGE    as BackHalfPage
    , REQ_SCR_BLINE     as BackLine
    , REQ_SCR_BPAGE     as BackPage
    , REQ_SCR_FCHAR     as FwdChar
    , REQ_SCR_FHPAGE    as FwdHalfPage
    , REQ_SCR_FLINE     as FwdLine
    , REQ_SCR_FPAGE     as FwdPage
    , REQ_SCR_HBHALF    as HBackHalfLine
    , REQ_SCR_HBLINE    as HBackLine
    , REQ_SCR_HFHALF    as HFwdHalfLine
    , REQ_SCR_HFLINE    as HFwdLine
    , REQ_SFIRST_FIELD  as SortedFirst
    , REQ_SLAST_FIELD   as SortedLast
    , REQ_SNEXT_FIELD   as SortedNext
    , REQ_SPREV_FIELD   as SortedPrev
    , REQ_UP_CHAR       as UpChar
    , REQ_UP_FIELD      as UpField
    , REQ_VALIDATION    as Validate
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
