{-# LANGUAGE LambdaCase #-}

module Mikado
  ( runMikado
  ) where

import Algebra.Graph.AdjacencyMap
  ( AdjacencyMap
  , adjacencyMap
  , edge
  , empty
  , overlays
  , transpose
  , vertex
  )
import Algebra.Graph.Export.Dot (Attribute(..), Style(..), defaultStyle, export)
import Control.Monad (forM_, when)
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
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
  forM_ imageFPs $ \fp -> system ("open " ++ fp)

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
  doneGr <- readMikado fp
  let dot = export (mikadoStyle doneGr) (snd doneGr)
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

mikadoStyle :: (S.Set String, AdjacencyMap String) -> Style String String
mikadoStyle (dones, gr) =
  sty
    { graphAttributes =
        ("size" := sizeVal : "rankdir" := "BT" : graphAttributes sty)
    , vertexAttributes = va
    }
  where
    sty = defaultStyle id
    sizeVal :: String
    sizeVal = printf "%f,%f" (10 :: Float) (7.5 :: Float)
    isRipe :: String -> Bool
    isRipe src = succs `S.isSubsetOf` dones
      where
        m = adjacencyMap gr
        succs = m M.! src
    isRoot :: String -> Bool
    isRoot goal = null succs
      where
        gr' = transpose gr
        m' = adjacencyMap gr'
        succs = m' M.! goal
    va :: String -> [Attribute String]
    va goal
      | goal `S.member` dones =
        ["color" := "gray50", "fillcolor" := "gray75", "style" := "filled"]
      | isRipe goal =
        ["color" := "green", "fillcolor" := "chartreuse", "style" := "filled"]
      | isRoot goal = ["shape" := "doubleoctagon"]
      | otherwise = []

readMikado :: FilePath -> IO (S.Set String, AdjacencyMap String)
readMikado fp = do
  eRes <- parseMikado <$> readFile fp
  case eRes of
    Left msg -> error msg
    Right gr -> pure gr

-- TODO This /can't/ fail right now because my syntax is too simple.
-- But maybe in the future...
parseMikado :: String -> Either String (S.Set String, AdjacencyMap String)
parseMikado src = (,) <$> done' <*> notDone'
  where
    (doneLines, notDoneLines) = partition isDone $ lines src
      where
        isDone line =
          case words line of
            ("DONE":_) -> True
            _ -> False
    notDone' :: Either String (AdjacencyMap String)
    notDone' = overlays <$> traverse parseNotDoneLine notDoneLines
    done' :: Either String (S.Set String)
    done' = S.unions <$> traverse parseDoneLine doneLines

parseDoneLine :: String -> Either String (S.Set String)
parseDoneLine line =
  case words line of
    ("DONE":goals) -> pure $ S.fromList goals
    _ -> Left ("This shouldn't happen: non-DONE done line. " ++ show line)

parseNotDoneLine :: String -> Either String (AdjacencyMap String)
parseNotDoneLine line =
  pure $
  case words line of
    [] -> empty
    [src] -> vertex src
    (src:dsts) -> overlays [edge src dst | dst <- dsts]
