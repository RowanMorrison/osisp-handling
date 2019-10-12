module Main  where

import Lib (safeHead)
import Graphics.Win32
import System.Win32.DLL (getModuleHandle)
import System.Environment (getArgs)

import Data.Bits ((.&.), shift)
import Data.Maybe (isJust, fromJust)
import Data.Int (Int32)
import Control.Monad (when)

newtype Velocity  = Velocity (Int32, Int32) deriving Show

wM_MOUSEWHEEL :: WindowMessage 
wM_MOUSEWHEEL = 0x020A

stdRectangle :: RECT
stdRectangle = (10, 10, 30, 30)

stdVelocity :: Velocity 
stdVelocity = Velocity (7, 0)

getPath :: IO (String)
getPath = do
    args <- getArgs
    return $ maybe "./u.bmp" id (safeHead args)

main :: IO ()
main = do
    let className = mkClassName "Handling Class"
    hInstance <- getModuleHandle Nothing
    wBrush <- getStockBrush wHITE_BRUSH
    curArrow <- loadCursor Nothing iDC_ARROW
    let wc = ( cS_DBLCLKS -- GetWindowLongPtr GWLP_USERDATA
             , hInstance
             , Nothing
             , Just curArrow
             , Just wBrush
             , Nothing
             , className  
             )
    mAtom <- registerClass wc
    when (isJust mAtom) $ do
        path <- getPath
        hBitMap <- loadImage nullHINSTANCE path iMAGE_BITMAP 0 0 lR_LOADFROMFILE -- you may die
        hWnd <- createWindow
                className 
                "Handling"
                wS_OVERLAPPEDWINDOW
                (Just cW_USEDEFAULT)
                (Just 0)
                (Just cW_USEDEFAULT)
                (Just 0)
                Nothing
                Nothing
                hInstance
                (wndProc stdVelocity stdRectangle stdDraw hBitMap)   -- SetWindowLongPointer GWLP_USERDATA

        setWinTimer hWnd 0{-tid-} 35{-msec-}

        showWindow hWnd sW_SHOWNORMAL
        updateWindow hWnd
        allocaMessage $ messagePump

        
        unregisterClass className hInstance

wndProc :: Velocity -> RECT -> (HDC -> RECT -> IO ()) -> HANDLE ->
            HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT 
wndProc v r onPaint hBitMap
        hWnd msg wParam lParam 
        | msg == wM_TIMER = do
            (v', r') <- onTimer hWnd v r
            invalidateRect (Just hWnd) Nothing True
            setWindowClosure hWnd (wndProc v' r' onPaint hBitMap)
            return 0

        | msg == wM_PAINT = allocaPAINTSTRUCT $ \ lpps -> do
            hdc <- beginPaint hWnd lpps
            onPaint hdc r 
            endPaint hWnd lpps
            return 0

        | msg == wM_DESTROY = do
            killTimer (Just hWnd) 0
            deleteBitmap hBitMap
            postQuitMessage 0
            return 0

        | msg == wM_MOUSEWHEEL = do 
            let r' = onWheel wParam r
            setWindowClosure hWnd (wndProc v r' onPaint hBitMap)
            return 0 

        | msg == wM_LBUTTONDBLCLK = do 
            setWindowClosure hWnd (wndProc v r (rDraw hBitMap) hBitMap)
            return 0

        | msg == wM_RBUTTONDBLCLK = do
            messageBox (Just hWnd) (show r) "Hey!" mB_OK
            return 0


        | msg == wM_KEYDOWN && isJust mv = do
            setWindowClosure hWnd (wndProc (fromJust mv) r onPaint hBitMap)
            return 0

        | otherwise = defWindowProc (Just hWnd) msg wParam lParam
        where 
            mv = onKey wParam lParam v

messagePump :: LPMSG -> IO ()
messagePump lpMsg = do 
    fContinue <- getMessage lpMsg Nothing
    when fContinue $ do
        translateMessage lpMsg 
        dispatchMessage lpMsg 
        messagePump lpMsg       

stdDraw :: HDC -> RECT -> IO ()
stdDraw hdc r = do
    bBrush <- getStockBrush bLACK_BRUSH
    fillRect hdc r bBrush  
    deleteBrush bBrush
    
rDraw :: HANDLE -> HDC -> RECT -> IO ()
rDraw hBitMap hdc (x1, y1, x2, y2) = do    
    memDC <- createCompatibleDC Nothing
    oldBitMap <- selectBitmap memDC hBitMap
    (_, aw, ah, _, _, _, _) <- getBitmapInfo hBitMap
    stretchBlt hdc x1 y1 (x2 - x1) (y2 - y1) memDC 0 0 aw ah sRCCOPY
    selectBitmap memDC oldBitMap
    deleteDC memDC  

onTimer :: HWND -> Velocity -> RECT -> IO (Velocity, RECT)
onTimer hWnd v r = do 
    (xmin, ymin, xmax, ymax) <- getWindowRect hWnd
    return $ moveRectangle (acceleration v) r (0, 0, xmax - xmin, ymax - ymin)

onKey :: WPARAM -> LPARAM -> Velocity -> Maybe Velocity
onKey wParam lParam (Velocity (dx, dy))  
    | wParam == 0x53 = Just $ Velocity (dx, dy + n * m)
    | wParam == 0x57 = Just $ Velocity (dx, dy - n * m)
    | wParam == 0x41 = Just $ Velocity (dx - n * m, dy)
    | wParam == 0x44 = Just $ Velocity (dx + n * m, dy)
    | otherwise      = Nothing
    where 
        n = 5
        m = fromIntegral (lParam .&. 65535)
      
onWheel :: WPARAM -> RECT -> RECT 
onWheel wParam (x1, y1, x2, y2) = 
    if minV > x2 - x1  
        then (x1, y1, x1 + minV, y1 + minV)
        else (x1, y1, x2 + d, y2 + d)
    where 
        w = x2 - x1 
        h = y2 - y1
        d = fromIntegral wParam `shift` (-16) `div` 120
        minV = 5

acceleration :: Velocity -> Velocity 
acceleration (Velocity (vx, vy)) = Velocity (vx, vy + 2) 

moveRectangle :: Velocity -> RECT -> RECT -> (Velocity, RECT)
moveRectangle v r (xmin, ymin, xmax, ymax) = movX . movY $ (v, r) 
    where 
        movX :: (Velocity, RECT) -> (Velocity, RECT)
        movX (Velocity (dx, dy), (x1, y1, x2, y2))
            | x2 + dx + 14 >= xmax = (Velocity (-dx, dy), (xmax - w - 14, y1, xmax - 14, y2))
            | x1 + dx < xmin       = (Velocity (-dx, dy), (xmin, y1, w, y2))
            | otherwise            = (Velocity (dx, dy), (x1 + dx, y1, x2 + dx, y2))
            where 
                w = x2 - x1
        movY :: (Velocity, RECT) -> (Velocity, RECT)
        movY (Velocity (dx, dy), (x1, y1, x2, y2))
            | y2 + dy + 39 >= ymax = (Velocity (dx, -dy), (x1, ymax - h - 39, x2, ymax - 39))
            | y1 + dy < 0          = (Velocity (dx, -dy), (x1, ymin, x2, h))
            | otherwise            = (Velocity (dx, dy), (x1, y1 + dy, x2, y2 + dy))
            where 
                h = y2 - y1
