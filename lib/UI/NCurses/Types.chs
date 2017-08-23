{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module UI.NCurses.Types where

import qualified Control.Applicative as A
import           Control.Exception 
import           Control.Monad 
import           Control.Monad.Fix (MonadFix, mfix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT, MonadTrans)
import           Data.Bits
import           Data.Char (ord)
import           Data.List (foldl')
import           Data.Typeable
import           Foreign 
import           Foreign.C     

import qualified UI.NCurses.Enums as E

#include "cbits/mavericks-c2hs-workaround.h"

-- Note: c2hs has a hard time with the ncurses macros, and will choke on
-- waddwstr() if NCURSES_NOMACROS is not defined prior to including
-- ncurses.h in this file.
--
-- Transitive includes from hsncurses-shim.h are not sufficient.
#define NCURSES_ENABLE_STDBOOL_H 0
#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS
#ifdef HSNCURSES_NARROW_HEADER
#include <ncurses.h>
#else
#include <ncursesw/ncurses.h>
#endif

#include "cbits/hsncurses-shim.h"

{# pointer *WINDOW as Window nocode #}
{# pointer *cchar_t as CCharT newtype #}
{# pointer *wchar_t as CWString nocode #}

type AttrT = {# type attr_t #}
type MMaskT = {# type mmask_t #}
type CHType = {# type chtype #}

class UserData a where
    getData :: a -> Curses b
    getData e = do
        ptr <- getDataPtr e
        when (ptr==nullPtr) $ Curses $ throwIO $ CursesException "no userptr"
        Curses $ deRefStablePtr $ castPtrToStablePtr ptr
    setData :: a -> b -> Curses ()
    setData e v = do
        ptr <- getDataPtr e
        when (ptr/=nullPtr) $Curses $freeStablePtr $ castPtrToStablePtr ptr
        ptr' <- Curses $ newStablePtr v
        setDataPtr e $ (castStablePtrToPtr ptr')
    getDataMaybe :: a -> Curses (Maybe b)
    getDataMaybe e = fmap (either (\(CursesException _) -> Nothing) Just) $
        Curses $ try (unCurses $ getData e)
    getDataPtr  :: a -> Curses (Ptr ())
    setDataPtr  :: a -> Ptr () -> Curses ()

-- | A small wrapper around 'IO', to ensure the @ncurses@ library is
-- initialized while running.
newtype Curses a = Curses { unCurses :: IO a }

instance Monad Curses where
    return = Curses . return
    m >>= f = Curses (unCurses m >>= unCurses . f)

instance MonadFix Curses where
    mfix f = Curses (mfix (unCurses . f))

instance A.Alternative Curses where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Curses where
    mzero = Curses $ throwIO (CursesException "mzero")
    m `mplus` n = Curses $ catch (unCurses m) 
        $ \(SomeException _)->unCurses n

instance MonadIO Curses where
    liftIO = Curses

instance Functor Curses where
    fmap = liftM

instance A.Applicative Curses where
    pure = return
    (<*>) = ap

newtype Update a = Update { unUpdate :: ReaderT Window Curses a } 

instance Monad Update where
    return = Update . return
    m >>= f = Update (unUpdate m >>= unUpdate . f)

instance MonadFix Update where
    mfix f = Update (mfix (unUpdate . f))

instance MonadIO Update where
    liftIO = Update . liftIO 

instance Functor Update where
    fmap = liftM

instance A.Applicative Update where
    pure = return
    (<*>) = ap

newtype Window = Window { windowPtr :: Ptr Window }
    deriving (Show, Eq, Ord)

newtype CursesException = CursesException String
    deriving (Show, Typeable)

instance Exception CursesException

-- | A glyph is a character, typically spacing, combined with a set of
-- attributes.
data Glyph = Glyph
    { glyphCharacter :: Char
    , glyphAttributes :: [Attribute]
    }
    deriving (Show, Eq)

withMaybeGlyph :: Maybe Glyph -> (CCharT -> IO a) -> IO a
withMaybeGlyph Nothing io = io (CCharT nullPtr)
withMaybeGlyph (Just g) io = withGlyph g io

withGlyph :: Glyph -> (CCharT -> IO a) -> IO a
withGlyph (Glyph char attrs) io =
    let cAttrs = foldl' (\acc a -> acc .|. attrToInt a) 0 attrs in
    withCWStringLen [char] $ \(cChars, cCharsLen) ->
    allocaBytes {# sizeof cchar_t #} $ \pBuf -> do
    {# call hsncurses_init_cchar_t #} (CCharT pBuf) cAttrs cChars (fromIntegral cCharsLen)
    io (CCharT pBuf)

withGlyph' :: Glyph -> (CHType -> IO a) -> IO a
withGlyph' (Glyph char attrs) = 
    ($ foldl' (\acc a -> acc .|. attrToInt a) (fromIntegral $ ord char) attrs)

data Attribute
    = AttributeColor ColorID -- ^ A_COLOR
    | AttributeStandout -- ^ A_STANDOUT
    | AttributeUnderline -- ^ A_UNDERLINE
    | AttributeReverse -- ^ A_REVERSE
    | AttributeBlink -- ^ A_BLINK
    | AttributeDim -- ^ A_DIM
    | AttributeBold -- ^ A_BOLD
    | AttributeAltCharset -- ^ A_ALTCHARSET
    | AttributeInvisible -- ^ A_INVISIBLE
    | AttributeProtect -- ^ A_PROTECT
    | AttributeHorizontal -- ^ A_HORIZONTAL
    | AttributeLeft -- ^ A_LEFT
    | AttributeLow -- ^ A_LOW
    | AttributeRight -- ^ A_RIGHT
    | AttributeTop -- ^ A_TOP
    | AttributeVertical -- ^ A_VERTICAL
    deriving (Show, Eq)

attrEnum :: E.Attribute -> AttrT
attrEnum = fromInteger . E.fromEnum

attrToInt :: Attribute -> AttrT
attrToInt x = case x of
    AttributeStandout    -> attrEnum E.WA_STANDOUT
    AttributeUnderline   -> attrEnum E.WA_UNDERLINE
    AttributeReverse     -> attrEnum E.WA_REVERSE
    AttributeBlink       -> attrEnum E.WA_BLINK
    AttributeDim         -> attrEnum E.WA_DIM
    AttributeBold        -> attrEnum E.WA_BOLD
    AttributeAltCharset  -> attrEnum E.WA_ALTCHARSET
    AttributeInvisible   -> attrEnum E.WA_INVIS
    AttributeProtect     -> attrEnum E.WA_PROTECT
    AttributeHorizontal  -> attrEnum E.WA_HORIZONTAL
    AttributeLeft        -> attrEnum E.WA_LEFT
    AttributeLow         -> attrEnum E.WA_LOW
    AttributeRight       -> attrEnum E.WA_RIGHT
    AttributeTop         -> attrEnum E.WA_TOP
    AttributeVertical    -> attrEnum E.WA_VERTICAL

    -- Colors get special handling: the function COLOR_PAIR converts an
    -- NCURSES_PAIRS_T to an attr_t.
    AttributeColor (ColorID cid) -> fromIntegral ({# call pure unsafe COLOR_PAIR as c_COLOR_PAIR #} (fromIntegral cid))

data Color
    = ColorBlack
    | ColorRed
    | ColorGreen
    | ColorYellow
    | ColorBlue
    | ColorMagenta
    | ColorCyan
    | ColorWhite

    -- | An unspecified default terminal color, for terminals that support
    -- ISO/IEC 6429 escape sequences (or equivalent).
    --
    -- This is most useful for terminals with translucent backgrounds.
    | ColorDefault

    -- | A color outside of the standard COLOR_* enum space, for terminals
    -- that support more than eight colors.
    --
    -- Color-related functions may fail if a Color is provided that cannot
    -- be supported by the current terminal. Users are responsible for
    -- checking 'maxColor' when using extended colors.
    | Color Int16
    deriving (Show, Eq)

-- Get the maximum 'Color' supported by the current terminal.
maxColor :: Curses Int
maxColor = Curses $ do
    count <- fromIntegral <$> peek c_COLORS
    return (count - 1)

foreign import ccall "static &COLORS"
    c_COLORS :: Ptr CInt


-- | A wrapper around 'Int' to ensure clients don&#x2019;t use an
-- uninitialized color in an attribute.
newtype ColorID = ColorID CShort
    deriving (Show, Eq)

colorEnum :: E.Color -> CShort
colorEnum = fromInteger . E.fromEnum

colorToShort :: Color -> CShort
colorToShort x = case x of
    Color n      -> CShort n
    ColorBlack   -> colorEnum E.COLOR_BLACK
    ColorRed     -> colorEnum E.COLOR_RED
    ColorGreen   -> colorEnum E.COLOR_GREEN
    ColorYellow  -> colorEnum E.COLOR_YELLOW
    ColorBlue    -> colorEnum E.COLOR_BLUE
    ColorMagenta -> colorEnum E.COLOR_MAGENTA
    ColorCyan    -> colorEnum E.COLOR_CYAN
    ColorWhite   -> colorEnum E.COLOR_WHITE
    ColorDefault -> colorEnum E.COLOR_DEFAULT

checkRC :: String -> CInt -> IO ()
checkRC name rc = if toInteger rc == E.fromEnum E.ERR
    then throwIO (CursesException (name ++ ": rc == ERR"))
    else return ()

cToBool :: Integral a => a -> Bool
cToBool 0 = False
cToBool _ = True

cFromBool :: Integral a => Bool -> a
cFromBool False = 0
cFromBool True = 1

