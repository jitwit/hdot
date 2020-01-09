{-# language LambdaCase #-}

module Main where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.Export.Dot
import Data.Set (Set,fromList,member)
import Data.Tree
import Data.List

import System.FilePath
import System.Directory
import System.Process

type Extension = String
type ModuleName = String

module_of_path :: FilePath -> FilePath -> ModuleName
module_of_path base = clean where
  clean = intercalate "." . splitDirectories . dropExtension . drop (1+length base)

reifyDir :: Extension -> FilePath -> IO [FilePath]
reifyDir ext path = doesDirectoryExist path >>= \case
  False -> pure $ if isExtensionOf ext path then [path] else []
  True -> do
    files <- listDirectory path
    let children = [ path </> f | f <- files, head (takeFileName f) /= '.' ]
    concat <$> traverse (reifyDir ext) children

haskellModules :: FilePath -> IO [ModuleName]
haskellModules project = map (module_of_path project) <$> reifyDir "hs" project

moduleImports :: FilePath -> IO [ModuleName]
moduleImports file = getImports <$> readFile file where
  getImports s = [ parseImport l | l <- lines s, isPrefixOf "import" l ]
  parseImport l = case words l of
    "import":"qualified":m:_ -> m
    "import":m:_ -> m
    _ -> error $ "oops " ++ l

moduleDeps :: FilePath -> FilePath -> IO [(ModuleName,ModuleName)]
moduleDeps project file = do
  deps <- moduleImports file
  let target = module_of_path project file
  pure [ (d,target) | d <- deps ]

-- haskellProjectGraph :: FilePath ->
haskellProjectGraph project = do
  files <- reifyDir "hs" project
  deps <- concat <$> mapM (moduleDeps project) files
  let local_modules = module_of_path project <$> files
      loc = fromList local_modules
      g = edges deps
      g' = -- transitiveClosure $
        induce (`member`loc) g
  print g'
  writeFile "alga" $ exportAsIs g'
  callCommand "dot -Tpdf < alga > alga.pdf && open alga.pdf"

main :: IO ()
main = haskellProjectGraph "../alga/src"
