-- | A helper module to lay elements out in columns or rows 
module Graphics.Diagrams.Positioned.Layout
    ( pboxColumnLayout
    , pboxRowLayout
    )
   where

-- External imports
import Data.List (mapAccumL)

-- Internal imports
import Graphics.Diagrams.Types
import Graphics.Diagrams.Positioned.PositionedDiagram

-- | Arranges boxes in a column
pboxColumnLayout :: Float -> Float -> HAlign -> [PBox] -> [PBox]
pboxColumnLayout base w align bs =
   snd $ mapAccumL (pboxColumnLayoutP boxSep width align) base bs
 where width = max w (fst (pboxListSize bs))

-- | Arranges one box in a column, returning the new top and the modified box
pboxColumnLayoutP :: Float -> Float -> HAlign -> Float -> PBox -> (Float, PBox)
pboxColumnLayoutP left maxWidth align top b = (top', b')
 where top' = top + boxSep + snd (pboxSize b)
       b'   = case b of
                PBox n k _ sz@(w,_) c         -> PBox n k (left + newX w, top) sz c
                PGroupBox n _ sz@(w,_) gs c e -> PGroupBox n (left + newX w, top) sz gs c e

       -- Align
       newX w = case align of
                 HLeft   -> 0
                 HCenter -> (maxWidth - w) / 2
                 HRight  -> maxWidth - w

-- | Arranges boxes in a row
pboxRowLayout :: VAlign -> [PBox] -> [PBox]
pboxRowLayout align bs = snd $ mapAccumL (pboxRowLayoutP height align) 0 bs
 where height = snd $ pboxListSize bs

-- | Arranges one box in a row, returning the new left and the modified box
pboxRowLayoutP :: Float -> VAlign -> Float -> PBox -> (Float, PBox)
pboxRowLayoutP maxHeight  align  left b = (left', b')
  where left' = left + boxSep + 40 + fst (pboxSize b)
        b'    = case b of
                 PBox n k _ sz@(_,h) c         -> PBox n k (left, newY h) sz c
                 PGroupBox n _ sz@(_,h) gs c e -> PGroupBox n (left, newY h) sz gs c e
        
        -- Align
        newY h = case align of
                  VTop    -> maxHeight - h
                  VCenter -> (maxHeight - h) / 2
                  VBottom -> maxHeight - h
