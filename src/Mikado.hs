{-# LANGUAGE LambdaCase #-}

module Mikado
  ( runMikado
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap, edge, empty, overlays)
import Algebra.Graph.Export.Dot (Attribute(..), Style(..), defaultStyle, export)
import Control.Monad (when)
import Data.Maybe (catMaybes)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath ((-<.>), takeFileName)
import System.Process (system)
import Text.Printf (printf)

runMikado :: IO ()
runMikado = do
  files <- getArgs
  imageFPs <- catMaybes <$> mapM processMikadoFile files
  print $ imageFPs

data OutputFormat
  = PNG
  | SVG

suffix :: OutputFormat -> String
suffix =
  \case
    PNG -> "png"
    SVG -> "svg"

processMikadoFile :: FilePath -> IO (Maybe FilePath)
processMikadoFile fp = do
  let outp = SVG
  gr <- readMikado fp
  let dot = export (mikadoStyle gr) gr
  let dotFP = takeFileName fp -<.> "dot"
  let imgFP = takeFileName fp -<.> suffix outp
  writeFile dotFP dot
  let cmd = printf "dot -o %s -T%s %s" imgFP (suffix outp) dotFP
  ec <- system cmd
  case ec of
    ExitSuccess -> do
      when False $ removeFile dotFP
      pure $ Just imgFP
    ExitFailure i ->
      error $ printf "command %s failed with exit code %d" (show cmd) i

mikadoStyle :: AdjacencyMap String -> Style String String
mikadoStyle _ =
  sty {graphAttributes = ("size" := sizeVal : graphAttributes sty)}
  where
    sty = defaultStyle id
    sizeVal :: String
    sizeVal = printf "%f,%f" (10 :: Float) (7.5 :: Float)

readMikado :: FilePath -> IO (AdjacencyMap String)
readMikado fp = do
  eRes <- parseMikado <$> readFile fp
  case eRes of
    Left msg -> error msg
    Right gr -> pure gr

-- TODO This /can't/ fail right now because my syntax is too simple.
-- But maybe in the future...
parseMikado :: String -> Either String (AdjacencyMap String)
parseMikado src = overlays <$> traverse parseMikadoLine ls
  where
    ls :: [String]
    ls = lines src

parseMikadoLine :: String -> Either String (AdjacencyMap String)
parseMikadoLine line =
  case words line of
    [] -> pure empty
    ("DONE":_) -> pure empty -- TODO This isn't right: we want to keep
                             -- but darken
    (src:dsts) -> pure $ overlays [edge src dst | dst <- dsts]
