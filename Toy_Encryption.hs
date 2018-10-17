-- CPSC 312 - 2018 - 

-- Algorithm for Toy Encryption, Symmetrical Key, AES256 Cipher, System.Random Generated Salt --

module Toy_Encryption where

    import System.Random
    import Crypto.Simple.CBC (encrypt, decrypt)
    import Crypto.Hash
    import qualified Data.ByteString as BYTE
    import Data.ByteString (ByteString)
    import qualified Data.ByteString.Char8 as WRD

    teststring = WRD.pack "Username"
    bytestring = BYTE.unpack teststring

    salt = []
    pre_key = _
    byte_key = _
    key = _

    -- Main --
    
    mainEncrypt msg = do
        salt <- generateSalt
        pre_key <- generateKey
        byte_key <- WRD.pack pre_key
        key <- hashingSHA1 bytekey
        msg <- WRD.pack msg
        return encryptMsg key msg

    mainDecrypt msg = do
        msg <- WRD.pack msg
        return decryptMsg key msg
    
    -- Salt Generation --

    generateSalt = do
        g <- newStdGen
        return take 64 $ (randomRs ('0','9') g)

    -- Key Generation --

    generateKey = do
         return teststring ++ generateSalt

    hashingSHA1 :: ByteString -> IO ()
    hashingSHA1 key = do
        -- putStrLn $ "  sha1(" ++ show key ++ ") = " ++ show (hashWith SHA1   key)
        return (hashWith SHA1   key)

    -- Encryption --

    encryptMsg key msg =
        encrypt key msg

    -- Decryption --

    decryptMsg key msg =
        decrypt key msg

