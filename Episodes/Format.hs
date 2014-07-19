{-# LANGUAGE OverloadedStrings #-}

module Episodes.Format (
    formatMonth
) where


import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import qualified Data.Time.Calendar as C


-- Format month.
formatMonth :: C.Day -> TL.Text
formatMonth d = TF.format "{}-{}" (yt, mt)
    where
        yt = TF.left 4 '0' y
        mt = TF.left 2 '0' m
        (y, m, _) = C.toGregorian d

