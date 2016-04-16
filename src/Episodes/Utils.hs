{-# LANGUAGE OverloadedStrings #-}

-- | Utils sit between all Episodes and external libraries.
-- Utils can't use neither Foundation, nor DB, not even Common.
-- DB and Common can use utils.
module Episodes.Utils (
    hashPassword
) where


import Prelude
import Crypto.PBKDF.ByteString (sha1PBKDF2)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BSB64


-- | Hash password using Episodes hashing.
hashPassword :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashPassword salt password = hashBS
    where
        rounds = 400
        hashlen = 24
        -- pbkdf2.py salt is $p5k2$$<raw-salt>
        pbkdf2Salt = BS.intercalate "$" ["", "p5k2", "", salt]
        passHashBytes = sha1PBKDF2 password pbkdf2Salt rounds hashlen
        _trhc c = case c of
            '+' -> '.'
            _ -> c
        hashBS = BS.intercalate "$" [
                "",
                "p5k2",
                "",
                salt,
                BS.map _trhc $ BSB64.encode passHashBytes
            ]
