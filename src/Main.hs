module Main where

import Data.List
import Data.Maybe
import FUtil
import HSH
import System.Directory
import System.Environment
import Text.HTML.TagSoup
import qualified Data.Set as Set

-- todo getopt

levToDiff :: (Num t, Num t1) => t -> t1
levToDiff 12 = 5
levToDiff 10 = 6

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

main :: IO ()
main = do
  args <- getArgs
  let
    cached = True
    num = 10
    lev = read . fromMaybe "10" $ listToMaybe args
    f = "lev." ++ show lev
    fDid = "did"
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
    replicate num $ doOne c
