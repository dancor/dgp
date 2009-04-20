module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import Data.Unamb
import FUtil
import HSH
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.HTML.TagSoup
import qualified Data.Set as Set

data Options = Options {
  optNum :: Int,
  optLevel :: Int,
  optCache :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
  optNum = 10,
  optLevel = 10,
  optCache = True
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "n" ["num"]
    (ReqArg (\ n o -> o {optNum = read n}) "NUM")
    "do NUM problems (default 10)",
  Option "l" ["level"]
    (ReqArg (\ l o -> o {optLevel = read l}) "LEVEL")
    "level (30 is 30k, 25, 20, 18, 15, 12, 10, 8,\n\
    \7, .., 1, -1 is 1dan, -2, .. -6)",
  Option "c" ["clear-cache"]
    (NoArg (\ o -> o {optCache = False}))
    "update the saved problem cache for this level"
  ]

levToDiff :: (Num t, Num t1) => t -> t1
levToDiff n = case n of
  30 -> 0
  25 -> 1
  20 -> 2
  18 -> 3
  15 -> 4
  12 -> 5
  10 -> 6
  8 -> 7
  7 -> 8
  6 -> 9
  5 -> 10
  4 -> 11
  3 -> 12
  2 -> 13
  1 -> 14
  -1 -> 15
  -2 -> 16
  -3 -> 17
  -4 -> 18
  -5 -> 19
  -6 -> 20
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
    cached = optCache opts
    lev = optLevel opts
    f = dir </> "lev." ++ show lev
    fDid = dir </> "did"
    dgpWait = dir </> "wait"
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
    doOne = do
      unlessM (doesFileExist fDid) $ writeFile fDid ""
      probsDid <- fmap (Set.fromList . lines) $ readFileStrict fDid
      let
        probs = Set.fromList $ lines c
        probsUndid = probs `Set.difference` probsDid
      case Set.toList probsUndid of
        [] -> do
          return False
        prob:_ -> do
          writeFile fDid . unlines . Set.toList $ Set.insert prob probsDid
          runIO ("ff", ["http://goproblems.com/prob.php3?id=" ++ prob])
          return True
    doN n i = when (n > 0) . ifM doOne (when (n > 1) $ i >> doN (n - 1) i) .
      print $ "all problems done when there were " ++ show n ++ " left to do."

  -- kill any pre-existing hotkey presses
  -- this is a hack and makes a "end of file" error print
  t <- forkIO $ withFile dgpWait ReadMode (\ h -> forever $
    hGetLine h >> threadDelay 5000)
  threadDelay 500000
  killThread t

  doN (optNum opts) $ --race
    --(putStrLn "press enter when done.." >> getLine >> return ())
    withFile dgpWait ReadMode hGetLine
