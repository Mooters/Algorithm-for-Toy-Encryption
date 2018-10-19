-- CPSC 312 - 2018 - 

-- Algorithm for Toy Encryption, Symmetrical Key, AES256 Cipher, System.Random Generated Salt --

module Toy_Encryption where

    import System.Random
    import Crypto.Simple.CBC (encrypt, decrypt)
    import Crypto.Hash
    import qualified Data.ByteString as BYTE
    import Data.ByteString (ByteString)
    import Data.ByteString.Char8 (pack)
    import Control.Monad

    salt        = generateSalt
    
    pre_key     = "pre-key"
    byte_key    = pack "byte"

    pre_msg     = "Secret!"
    byte_msg    = pack "Secret Message!"

    result_byte = pack "byte"
    result      = "Secret!"

    -- Main --
    
    mainEncrypt msg = do
        let salt = generateSalt
        let pre_key = generateKey
        let byte_key = pack pre_key
        let byte_msg = pack msg
        encryptMsg byte_key byte_msg

    mainDecrypt encrypted = do
        return BYTE.unpack (decryptMsg byte_key encrypted)
    
    -- Salt Generation --

    generateSalt :: IO [Char]
    generateSalt = do
        g <- newStdGen
        return (take 64 $ (randomRs ('0','9') g))

    -- Key Generation --

    generateKey =
        liftM (pre_key ++) salt

    -- Encryption --

    encryptMsg :: ByteString -> ByteString -> IO ByteString
    encryptMsg key1 msg1 =
        encrypt key1 msg1

    -- Decryption --

    decryptMsg :: ByteString -> ByteString -> IO ByteString
    decryptMsg key2 msg2 =
        decrypt key2 msg2

    -- SHA256 Encryption --

    hashingSHA256 :: ByteString -> IO()
    hashingSHA256 byte_msg = do
        putStrLn $ "sha256( " ++ show byte_msg ++ " ) = " ++ show (hashWith SHA256 byte_msg)

