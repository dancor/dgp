module Main where

import Control.Monad
import Data.List
import Data.Maybe
import FUtil
import HSH
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.TagSoup
import qualified Data.Set as Set

data Options = Options {
  optNum :: Int,
  optLevel :: Int
  }

defaultOptions :: Options
defaultOptions = Options {
  optNum = 10,
  optLevel = 10
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "n" ["num"]
    (ReqArg (\ n o -> o {optNum = read n}) "NUM")
    "do NUM problems (default 10)",
  Option "l" ["level"]
    (ReqArg (\ l o -> o {optLevel = read l}) "LEVEL")
    "level (30 is 30k, 25, 20, 18, 15, 12, 10, 8,\n\
    \7, .., 1, -1 is 1dan, -2, .. -6)"
  ]

levToDiff :: (Num t, Num t1) => t -> t1
levToDiff n = case n of
  30 -> 0
  25 -> 1
  20 -> 2
  18 -> 3
  15 -> 4
  12 -> 5
  8 -> 6
  7 -> 7
  6 -> 8
  5 -> 9
  4 -> 10
  3 -> 11
  2 -> 12
  1 -> 13
  -1 -> 14
  -2 -> 15
  -3 -> 16
  -4 -> 17
  -5 -> 18
  -6 -> 19
  _ -> error "unknown level"

getTags :: Int -> IO [Tag]
getTags lev = do
  let 
    d = levToDiff lev
    params = [
      ("doit", "1"),
      ("num", "0"),
      ("genre", ""),
      ("diff0", show d),
      ("diff1", show (d + 1)),
      ("orderby", "recent"),
      ("status", "any")
      ]
  s <- run ("wget", ["http://goproblems.com/problems.php3", "-O", "-",
    "--post-data", intercalate "&" $ map (\ (k, v) -> k ++ "=" ++ v) params])
  return $ parseTags s

readFileStrict :: FilePath -> IO String
readFileStrict f = do
  c <- readFile f
  length c `seq` return c

doErr :: String -> a
doErr err = let
  usage = "usage: dgp [options]"
  in error $ err ++ usageInfo usage options

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  let
    (optsPre, moreArgs, errs) = getOpt Permute options args
    opts = foldl (flip id) defaultOptions optsPre
    dir = home </> ".dgp"
    cached = True
    lev = optLevel opts
    f = dir </> "lev." ++ show lev
    fDid = dir </> "did"
  unless (null errs) . doErr $ concat errs
  unless (null moreArgs) $ doErr "Unexpected arguments\n"
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  cached' <- if cached then doesFileExist f else return False
  c <- if cached' then readFile f else do
    t <- getTags lev
    let 
      dropToTable = dropWhile (not . isTagOpenName "table")
      aToUrl (TagOpen "a" attrs) = snd $ head attrs
      urls = map aToUrl . head . stripe 2 . filter (isTagOpenName "a") . 
        takeWhile (not . isTagCloseName "table") .
        dropToTable . drop 1 . dropToTable
      c = unlines . map (takeWhile (/= '&') . drop 1 . dropWhile (/= '=')) $
        urls t
    writeFile f c
    return c
  let
    doOne c = do
      unlessM (doesFileExist fDid) $ writeFile fDid ""
      probsDid <- fmap (Set.fromList . lines) $ readFileStrict fDid
      let
        probs = Set.fromList $ lines c 
        probsUndid = probs `Set.difference` probsDid
        prob = head $ Set.toList probsUndid
        probsDid' = Set.insert prob probsDid 
      writeFile fDid . unlines $ Set.toList probsDid'
      runIO ("ff", ["http://goproblems.com/prob.php3?id=" ++ prob])
  sequence_ . 
    intersperse (putStrLn "press enter when done.." >> getLine >> return ()) .
    replicate (optNum opts) $ doOne c
