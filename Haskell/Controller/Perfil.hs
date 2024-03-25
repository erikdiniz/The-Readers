{-# LANGUAGE DeriveGeneric #-}
module Controller.Perfil where

import Controller.Usuario
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Unsafe

data Perfil = Perfil {
    nome :: String,
    biografia :: String,
    qtdSeguidores :: Int,
    qtdLivrosLidos :: Int
} deriving (Show, Generic)

