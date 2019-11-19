module JsonParser
(
    getJSON,
    getTransations,
    getData
) where

import qualified Data.ByteString.Lazy as B  
import Data.Aeson
import Tipos

jsonFile :: FilePath
jsonFile = "data/transacoes.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getTransations :: IO (Either String [Transacao])
getTransations = (eitherDecode <$> getJSON) :: IO (Either String [Transacao])


extract (Right e) = e
extract (Left e) = error e


getData d = extract d



-- test :: Either String [Transacao] -> Transacao
-- test xs = head xs;;;;;;;//////////