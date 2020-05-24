module UI.NCurses.Class where

import qualified Data.Text as T
import UI.NCurses (Window, Update, ColorID, CursorMode, CursesException, Color, Pad, Event, Attribute, Glyph, OverlayMode)
import qualified UI.NCurses as C

class Monad m => MonadCurses m where
    runCurses :: m a -> IO a
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
    tryCurses :: m a -> m (Either CursesException a)
    catchCurses :: m a -> (CursesException -> m a) -> m a
    throwCurses :: CursesException -> m a
    baudrate :: m Integer
    beep :: m ()
    setRaw :: Bool -> m ()
    setCBreak :: Bool -> m ()
    setEcho :: Bool -> m ()
    enclosed :: Window -> Integer -> Integer -> m Bool
    setKeypad :: Window -> Bool -> m ()
    resizeTerminal :: Integer -> Integer -> m ()

instance MonadCurses C.Curses where
    runCurses = C.runCurses
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
    tryCurses = C.tryCurses
    catchCurses = C.catchCurses
    throwCurses = C.throwCurses
    baudrate = C.baudrate
    beep = C.beep
    setRaw = C.setRaw
    setCBreak = C.setCBreak
    setEcho = C.setEcho
    enclosed = C.enclosed
    setKeypad = C.setKeypad
    resizeTerminal = C.resizeTerminal

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