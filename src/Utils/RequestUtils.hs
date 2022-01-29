{-# LANGUAGE OverloadedStrings #-}


module Utils.RequestUtils where

import qualified Data.ByteString.Char8 as S8
import qualified Network.HTTP.Types as HttpTypes
import Data.CaseInsensitive
import System.Environment

getOneHeader :: [HttpTypes.Header] -> String -> S8.ByteString
getOneHeader headers headerName =
  case Prelude.filter (\n -> fst n == (Data.CaseInsensitive.mk (S8.pack headerName) :: CI S8.ByteString)) headers of
    [header] -> snd header
    _ -> ""
httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing

getRestUrl :: IO String
getRestUrl = head <$> getArgs
