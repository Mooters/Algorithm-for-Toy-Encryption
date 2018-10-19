-- CPSC 312 - 2018 - 

-- Key Bit Size Repair --

module Repair where

import qualified Data.ByteString as B
import qualified Crypto.Data.Padding (pad, unpad, Format(PKCS7))

dummy_key = B.pack "key"

repair key
    | B.length key == 16    = key
    | B.length key == 32    = key
    | otherwise             = pad (PKCS7 ) key


    

