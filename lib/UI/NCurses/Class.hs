{-# LANGUAGE DefaultSignatures, TypeFamilies #-}

module UI.NCurses.Class where

import qualified Data.Text as T
import UI.NCurses (Window, Update, ColorID, CursorMode, Color, Pad, Event, Attribute, Glyph, OverlayMode)
import qualified UI.NCurses as C
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.Except
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateL
import qualified Control.Monad.State.Strict as StateS
import qualified Control.Monad.Writer.Lazy as WriterL
import qualified Control.Monad.Writer.Strict as WriterS
import Data.Composition

class Monad m => MonadCurses m where
    defaultWindow :: m Window
    newWindow :: Integer -> Integer -> Integer -> Integer -> m Window
    closeWindow :: Window -> m ()
    cloneWindow :: Window -> m Window
    updateWindow :: Window -> Update a -> m a
    newPad :: Integer -> Integer -> m Pad
    closePad :: Pad -> m ()
    updatePad :: Pad -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Update a -> m a
    maxColor :: m Integer
    supportsColor :: m Bool
    canDefineColor :: m Bool
    maxColorID :: m Integer
    getCursor :: Window -> m (Integer, Integer)
    render :: m ()
    defineColor :: Color -> Integer -> Integer -> Integer -> m ()
    queryColor :: Color -> m (Integer, Integer, Integer)
    newColorID :: Color -> Color -> Integer -> m ColorID
    setColorID :: Color -> Color -> ColorID -> m ()
    getEvent :: Window -> Maybe Integer -> m (Maybe Event)
    setCursorMode :: CursorMode -> m CursorMode
    baudrate :: m Integer
    beep :: m ()
    setRaw :: Bool -> m ()
    setCBreak :: Bool -> m ()
    setEcho :: Bool -> m ()
    enclosed :: Window -> Integer -> Integer -> m Bool
    setKeypad :: Window -> Bool -> m ()
    resizeTerminal :: Integer -> Integer -> m ()

    -- Overrides to let monad transformers wrap this
    default defaultWindow :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m Window
    defaultWindow = lift defaultWindow
    default newWindow :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Integer -> Integer -> Integer -> Integer -> m Window
    newWindow = lift .*** newWindow
    default closeWindow :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> m ()
    closeWindow = lift . closeWindow
    default cloneWindow :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> m Window
    cloneWindow = lift . cloneWindow
    default updateWindow :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> Update a -> m a
    updateWindow = lift .* updateWindow
    default newPad :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Integer -> Integer -> m Pad
    newPad = lift .* newPad
    default closePad :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Pad -> m ()
    closePad = lift . closePad
    default updatePad :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Pad -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Update a -> m a
    updatePad = lift .******* updatePad
    default maxColor :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m Integer
    maxColor = lift maxColor
    default supportsColor :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m Bool
    supportsColor = lift supportsColor
    default canDefineColor :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m Bool
    canDefineColor = lift canDefineColor
    default maxColorID :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m Integer
    maxColorID = lift maxColorID
    default getCursor :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> m (Integer, Integer)
    getCursor = lift . getCursor
    default render :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m ()
    render = lift render
    default defineColor :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Color -> Integer -> Integer -> Integer -> m ()
    defineColor = lift .*** defineColor
    default queryColor :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Color -> m (Integer, Integer, Integer)
    queryColor = lift . queryColor
    default newColorID :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Color -> Color -> Integer -> m ColorID
    newColorID = lift .** newColorID
    default setColorID :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Color -> Color -> ColorID -> m ()
    setColorID = lift .** setColorID
    default getEvent :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> Maybe Integer -> m (Maybe Event)
    getEvent = lift .* getEvent
    default setCursorMode :: (MonadTrans t, MonadCurses m1, m ~ t m1) => CursorMode -> m CursorMode
    setCursorMode = lift . setCursorMode
    default baudrate :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m Integer
    baudrate = lift baudrate
    default beep :: (MonadTrans t, MonadCurses m1, m ~ t m1) => m ()
    beep = lift beep
    default setRaw :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Bool -> m ()
    setRaw = lift . setRaw
    default setCBreak :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Bool -> m ()
    setCBreak = lift . setCBreak
    default setEcho :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Bool -> m ()
    setEcho = lift . setEcho
    default enclosed :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> Integer -> Integer -> m Bool
    enclosed = lift .** enclosed
    default setKeypad :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Window -> Bool -> m ()
    setKeypad = lift .* setKeypad
    default resizeTerminal :: (MonadTrans t, MonadCurses m1, m ~ t m1) => Integer -> Integer -> m ()
    resizeTerminal = lift .* resizeTerminal

instance MonadCurses C.Curses where
    defaultWindow = C.defaultWindow
    newWindow = C.newWindow
    closeWindow = C.closeWindow
    cloneWindow = C.cloneWindow
    updateWindow = C.updateWindow
    newPad = C.newPad
    closePad = C.closePad
    updatePad = C.updatePad
    maxColor = C.maxColor
    supportsColor = C.supportsColor
    canDefineColor = C.canDefineColor
    maxColorID = C.maxColorID
    getCursor = C.getCursor
    render = C.render
    defineColor = C.defineColor
    queryColor = C.queryColor
    newColorID = C.newColorID
    setColorID = C.setColorID
    getEvent = C.getEvent
    setCursorMode = C.setCursorMode
    baudrate = C.baudrate
    beep = C.beep
    setRaw = C.setRaw
    setCBreak = C.setCBreak
    setEcho = C.setEcho
    enclosed = C.enclosed
    setKeypad = C.setKeypad
    resizeTerminal = C.resizeTerminal

instance MonadCurses m => MonadCurses (ContT r m)
instance MonadCurses m => MonadCurses (ExceptT e m)
instance (MonadCurses m, Monoid w) => MonadCurses (RWSL.RWST r w s m)
instance (MonadCurses m, Monoid w) => MonadCurses (RWSS.RWST r w s m)
instance MonadCurses m => MonadCurses (ReaderT r m)
instance MonadCurses m => MonadCurses (StateL.StateT s m)
instance MonadCurses m => MonadCurses (StateS.StateT s m)
instance (MonadCurses m, Monoid w) => MonadCurses (WriterL.WriterT w m)
instance (MonadCurses m, Monoid w) => MonadCurses (WriterS.WriterT w m)

class Monad m => MonadUpdate m where
    moveWindow :: Integer -> Integer -> m ()
    windowPosition :: m (Integer, Integer)
    resizeWindow :: Integer -> Integer -> m ()
    windowSize :: m (Integer, Integer)
    overlay :: Window -> OverlayMode -> m ()
    copyWindow :: Window -> OverlayMode -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m ()
    moveCursor :: Integer -> Integer -> m ()
    cursorPosition :: m (Integer, Integer)
    setColor :: ColorID -> m ()
    drawString :: String -> m ()
    drawText :: T.Text -> m()
    drawGlyph :: Glyph -> m ()
    drawBorder :: Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> m ()
    drawBox :: Maybe Glyph -> Maybe Glyph -> m ()
    drawLineH :: Maybe Glyph -> Integer -> m ()
    drawLineV :: Maybe Glyph -> Integer -> m ()
    clear :: m ()
    clearLine :: m ()
    setBackground :: Glyph -> m ()
    setAttribute :: Attribute -> Bool -> m ()
    setAttributes :: [Attribute] -> m ()
    setTouched :: Bool -> m ()
    setRowsTouched :: Bool -> Integer -> Integer -> m ()
    
    default moveWindow :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Integer -> Integer -> m ()
    moveWindow = lift .* moveWindow
    default windowPosition :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => m (Integer, Integer)
    windowPosition = lift windowPosition
    default resizeWindow :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Integer -> Integer -> m ()
    resizeWindow = lift .* resizeWindow
    default windowSize :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => m (Integer, Integer)
    windowSize = lift windowSize
    default overlay :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Window -> OverlayMode -> m ()
    overlay = lift .* overlay
    default copyWindow :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Window -> OverlayMode -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> m ()
    copyWindow = lift .******* copyWindow
    default moveCursor :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Integer -> Integer -> m ()
    moveCursor = lift .* moveCursor
    default cursorPosition :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => m (Integer, Integer)
    cursorPosition = lift cursorPosition
    default setColor :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => ColorID -> m ()
    setColor = lift . setColor
    default drawString :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => String -> m ()
    drawString = lift . drawString
    default drawText :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => T.Text -> m()
    drawText = lift . drawText
    default drawGlyph :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Glyph -> m ()
    drawGlyph = lift . drawGlyph
    default drawBorder :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> Maybe Glyph -> m ()
    drawBorder = lift .******* drawBorder
    default drawBox :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Maybe Glyph -> Maybe Glyph -> m ()
    drawBox = lift .* drawBox
    default drawLineH :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Maybe Glyph -> Integer -> m ()
    drawLineH = lift .* drawLineH
    default drawLineV :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Maybe Glyph -> Integer -> m ()
    drawLineV = lift .* drawLineV
    default clear :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => m ()
    clear = lift clear
    default clearLine :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => m ()
    clearLine = lift clearLine
    default setBackground :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Glyph -> m ()
    setBackground = lift . setBackground
    default setAttribute :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Attribute -> Bool -> m ()
    setAttribute = lift .* setAttribute
    default setAttributes :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => [Attribute] -> m ()
    setAttributes = lift . setAttributes
    default setTouched :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Bool -> m ()
    setTouched = lift . setTouched
    default setRowsTouched :: (MonadTrans t, MonadUpdate m1, m ~ t m1) => Bool -> Integer -> Integer -> m ()
    setRowsTouched = lift .** setRowsTouched
    
instance MonadUpdate Update where
    moveWindow = C.moveWindow
    windowPosition = C.windowPosition
    resizeWindow = C.resizeWindow
    windowSize = C.windowSize
    overlay = C.overlay
    copyWindow = C.copyWindow
    moveCursor = C.moveCursor
    cursorPosition = C.cursorPosition
    setColor = C.setColor
    drawString = C.drawString
    drawText = C.drawText
    drawGlyph = C.drawGlyph
    drawBorder = C.drawBorder
    drawBox = C.drawBox
    drawLineH = C.drawLineH
    drawLineV = C.drawLineV
    clear = C.clear
    clearLine = C.clearLine
    setBackground = C.setBackground
    setAttribute = C.setAttribute
    setAttributes = C.setAttributes
    setTouched = C.setTouched
    setRowsTouched = C.setRowsTouched

instance MonadUpdate m => MonadUpdate (ContT r m)
instance MonadUpdate m => MonadUpdate (ExceptT e m)
instance (MonadUpdate m, Monoid w) => MonadUpdate (RWSL.RWST r w s m)
instance (MonadUpdate m, Monoid w) => MonadUpdate (RWSS.RWST r w s m)
instance MonadUpdate m => MonadUpdate (ReaderT r m)
instance MonadUpdate m => MonadUpdate (StateL.StateT s m)
instance MonadUpdate m => MonadUpdate (StateS.StateT s m)
instance (MonadUpdate m, Monoid w) => MonadUpdate (WriterL.WriterT w m)
instance (MonadUpdate m, Monoid w) => MonadUpdate (WriterS.WriterT w m)