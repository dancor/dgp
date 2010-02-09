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
  optCache :: Bool,
  optRemoteVCS :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
  optNum = 10,
  optLevel = 10,
  optCache = True,
  optRemoteVCS = True
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
    "update the saved problem cache for this level",
  Option "R" ["no-remote-vcs"]
    (NoArg (\ o -> o {optRemoteVCS = False}))
    "don't push/pull if ~/.dgp/ is version-controlled"
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
      ("genre[]", "elementary"),
      ("genre[]", "life and death"),
      ("genre[]", "joseki"),
      ("genre[]", "fuseki"),
      ("genre[]", "tesuji"),
      ("genre[]", "best move"),
      ("genre[]", "endgame"),
      ("diff0", show d),
      ("diff1", show (d + 1)),
      ("orderby", "recent"),
      ("status[]", "untried"),
      ("status[]", "unsolved"),
      ("status[]", "solved")
      ]
  s <- run ("wget", ["http://goproblems.com/problems.php3", "-O", "-",
    "--post-data", intercalate "&" $ map (\ (k, v) -> k ++ "=" ++ v) params])
  return $ parseTags s

doErr :: String -> a
doErr err = let
  usage = "usage: dgp [options]"
  in error $ err ++ usageInfo usage options

doOne :: FilePath -> String -> IO Bool
doOne fDid c = do
  unlessM (doesFileExist fDid) $ writeFile fDid ""
  probsDid <- fmap (Set.fromList . lines) $ readFileStrict fDid
  let
    probs = Set.fromList $ lines c
    probsUndid = probs `Set.difference` probsDid
  print $ "probs count: " ++ show (length $ Set.toList probs)
  print $ "did count: " ++ show (length $ Set.toList probsDid)
  print $ "undid count: " ++ show (length $ Set.toList probsUndid)
  case Set.toList probsUndid of
    [] -> do
      return False
    prob:_ -> do
      writeFile fDid . unlines . Set.toList $ Set.insert prob probsDid
      let
        url = "http://goproblems.com/prob.php3?id=" ++ prob
      print url
      runIO ("ff", ["--firefox", url])
      return True

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
  unlessM (doesFileExist $ dgpWait) $ run ("mkfifo", [dgpWait])
  haveGit <- doesDirectoryExist $ dir </> ".git"
  when (haveGit && optRemoteVCS opts) . inCd dir $ run "git pull && git push"
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
    doN n i = when (n > 0) .
      ifM (doOne fDid c) (when (n > 1) $ i >> doN (n - 1) i) .
      print $ "all problems done when there were " ++ show n ++ " left to do."

  -- ReadWriteMode not ReadMode so that unblocking fifo doesn't immediately EOF
  -- http://www.haskell.org/haskellwiki/GHC:FAQ#When_I_open_a_FIFO_.28named_pipe.29_and_try_to_read_from_it.2C_I_get_EOF_immediately.
  hFifo <- openFile dgpWait ReadWriteMode
  -- kill any pre-existing hotkey presses
  -- this is a hack and makes a "end of file" error print
  t <- forkIO . forever $ hGetLine hFifo >> threadDelay 5000
  threadDelay 500000
  killThread t
  -- reopen necessary for some reason
  hClose hFifo
  hFifo <- openFile dgpWait ReadWriteMode

  doN (optNum opts) $ race
    (putStrLn "press enter when done.." >> getLine >> return ())
    (hGetLine hFifo >> return ())
  hClose hFifo

  when haveGit . inCd dir $ run "git commit -am 'dgp autosave'"
  when (haveGit && optRemoteVCS opts) . inCd dir $ run "git pull && git push"
