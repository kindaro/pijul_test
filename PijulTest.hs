#! /usr/bin/env stack
{- stack
    script
        --resolver lts-9.0 
        --package split
        --package directory
        --package process
        --package filepath
        --package tuple
 -}

module PijulTest where

import Control.Monad
import Control.Exception
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple.Select
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO

type Handles = ( Maybe Handle
               , Maybe Handle
               , Maybe Handle
               , ProcessHandle)

heroes = ["Alicia", "Roberto"]

poem = "poem.md"

local :: FilePath -> CreateProcess -> CreateProcess
local loc proc = proc { cwd = Just loc, std_out = Inherit, std_err = Inherit }

-- Thanks to stackoverflow.com/a/15849658/2108477
withHiddenTerminalInput :: IO a -> IO a
withHiddenTerminalInput io = bracket
   (do prevBuff <- hGetBuffering stdin
       prevEcho <- hGetEcho stdin

       hSetBuffering stdin NoBuffering
       hSetEcho stdin False

       return (prevBuff, prevEcho)
   )

   (\(prevBuff, prevEcho) -> do
       hSetBuffering stdin prevBuff
       hSetEcho stdin prevEcho
   )

   (const io)

askPermission :: String -> IO () -> IO () -> IO ()
askPermission message yes no = do
    putStrLn message
    allowance <- withHiddenTerminalInput getChar
    case allowance of
        'y' -> yes
        _ -> no

forHero :: FilePath -> CreateProcess -> IO ()
forHero hero proc = do
    handles <- createProcess $ local hero proc
    status <- waitForProcess (sel4 handles)
    case status of
        ExitSuccess -> return ()
        ExitFailure code -> askPermission
            ("Process " ++ show (cmdspec proc) ++ " returned " ++ show code ++ "! Proceed anyway?")
            (return ())
            (error $ "Process " ++ show (cmdspec proc) ++ " returned " ++ show code ++ ". Exiting.")

forSomeHeroes someHeroes proc = forM someHeroes (`forHero` proc) 
forHeroes proc = forSomeHeroes heroes proc

anotherHero hero = fromJust $ do
    index <- elemIndex hero heroes
    if index < length heroes - 1
    then return (heroes !! (index + 1))
    else return hero

commit hero verse = askPermission
    (hero ++ " is about to put another verse down. Let do? (y/N)")
        proceed leave
    where
    proceed = do
        forM
            (heroes \\ [hero])
            (\someoneElse -> forCurrentHeroProc "pijul" ["pull", "--all", ".." </> someoneElse])
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
        return ()

    leave = do
        putStrLn "Enough of these dull poems!"
        error "God's will against us."

    forCurrentHeroProc :: String -> [String] -> IO ()
    forCurrentHeroProc command args = forHero hero $ proc command args

main = do
    mapM_ (createDirectoryIfMissing False) heroes
    forHeroes $ proc "pijul" ["init"]
    verses <- splitOn "\n\n" <$> readFile poem
    let actions = zipWith commit (cycle heroes) verses
    sequence_ actions
    let lastHero = (fst . last $ zip (cycle heroes) verses)
    let remainingHeroes = heroes \\ [lastHero]
    forSomeHeroes remainingHeroes $ proc "pijul" ["pull", "--all", ".." </> lastHero]

    putStrLn "Done!"
