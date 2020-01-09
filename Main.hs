{-# language LambdaCase #-}

module Main where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.Export.Dot
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

getModulesFrom :: FilePath -> IO [(ModuleName,ModuleName)]
getModulesFrom base = do
  files <- reifyDir "hs" base
  concat <$> traverse (moduleDeps base) files

getGraph :: [FilePath] -> IO (AdjacencyMap ModuleName)
getGraph paths = edges . concat <$> traverse getModulesFrom paths

--module_edges :: FilePath -> FilePath -> IO [(ModuleName,ModuleName)]
--module_edges base path = do
--  files <- reifyDir "hs" path
--  concat <$> mapM (moduleDeps base) files

--module_graph :: FilePath -> [FilePath] -> IO (AdjacencyMap ModuleName)
--module_graph project paths = edges . concat <$> traverse (module_edges project) paths
  
-- haskellProjectGraph :: FilePath ->
haskellProjectGraph project paths = do
  g <- getGraph paths
  let out = project -<.> "svg"
      cmd = unwords ["dot","-Tsvg","<",project,">",out,"&&","open",out]
--  files <- reifyDir "hs" project
--  deps <- concat <$> mapM (moduleDeps project) files
--  let local_modules = module_of_path project <$> files
--      g = edges deps
--      g' = -- transitiveClosure $
--        induce (`elem`local_modules) g
  writeFile project $ exportAsIs g
  callCommand cmd

main :: IO ()
main = haskellProjectGraph "alga" ["../alga/src"]
