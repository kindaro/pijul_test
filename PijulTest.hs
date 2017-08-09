#! /usr/bin/env stack
{- stack
    script
        --resolver lts-9.0 
        --package split
        --package directory
        --package process
        --package filepath
 -}

module PijulTest where

import Control.Monad
import Data.List
import Data.List.Split
import System.Directory
import System.FilePath
import System.Process
import System.IO

type Handles = ( Maybe Handle
               , Maybe Handle
               , Maybe Handle
               , ProcessHandle)

heroes = ["Alicia", "Roberto"]

poem = "poem.md"

local :: FilePath -> CreateProcess -> CreateProcess
local loc proc = proc { cwd = Just loc, std_out = Inherit, std_err = Inherit }

forHero :: FilePath -> CreateProcess -> IO Handles
forHero hero = createProcess . local hero

forHeroes proc = forM heroes (`forHero` proc)

commit hero verse = do
    forCurrentHeroProc "pijul" ["pull", "--all", ".." </> head (heroes \\ [hero])]
    let file = hero </> poem
    firstVerse <- not <$> doesFileExist file
    case firstVerse of
        True -> do
            appendFile file ""
            forCurrentHeroProc "pijul" ["add", poem]
            forCurrentHeroProc "pijul" ["record", "--all", "--name", "Initial commit."]
            return ()
        False -> appendFile file "\n"
    appendFile file verse
    appendFile file "\n"
    forCurrentHeroProc "pijul" ["record", "--all", "--name", "Another verse."]
    where
    forCurrentHeroProc :: String -> [String] -> IO Handles
    forCurrentHeroProc command args = forHero hero $ proc command args

main = do
    mapM_ (createDirectoryIfMissing False) heroes
    forHeroes $ proc "pijul" ["init"]
    verses <- splitOn "\n\n" <$> readFile poem
    let actions = zipWith commit (cycle heroes) verses
    sequence_ actions

    putStrLn "done!"
