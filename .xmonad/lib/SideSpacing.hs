-- File: SideSpacing.hs
-- Based on Xmonad.Layout.Spacing created by Brent Yorgey
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module SideSpacing (sideSpacing) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier

-- | Surround all windows by a certain number of pixels of blank space.
sideSpacing :: Int -> l a -> ModifiedLayout SideSpacing l a
sideSpacing p = ModifiedLayout (SideSpacing p)

data SideSpacing a = SideSpacing Int deriving (Show, Read)

instance LayoutModifier SideSpacing a where

    pureModifier (SideSpacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    modifierDescription (SideSpacing p) = "Side Spacing " ++ show p

shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect p (Rectangle x y w h) = Rectangle (x+fi p) (y) (w-2 * fi p) (h)
