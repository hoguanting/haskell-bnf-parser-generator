{-# LANGUAGE TypeApplications #-}

import Assignment (bnfParser, generateHaskellCode)
import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, void, when)
import Data.Bool (bool)

import Instances (ParseResult (Error, Result), parse)
import System.FilePath
    ( joinPath
    , replaceExtension
    , splitDirectories
    , splitFileName
    , takeBaseName
    , takeFileName
    , (</>)
    )

import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removeFile
    , renameFile
    )
import System.FilePath.Glob (glob)
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Error (isDoesNotExistError)
import System.Process
    ( StdStream (UseHandle)
    , createProcess
    , proc
    , std_out
    , waitForProcess
    )

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile = do
    putStrLn $ "Starting " ++ inputFile
    result <- try (readFile inputFile) :: IO (Either IOException String)
    case result of
        Left e
            | isDoesNotExistError e -> putStrLn $ "File not found: " ++ inputFile
            | otherwise -> putStrLn $ "Error reading file " ++ inputFile ++ ": " ++ show e
        Right contents -> do
            let parsed = parse bnfParser contents
            case parsed of
                Result _ adt -> do
                    let prettyOutput = generateHaskellCode adt
                    writeFile outputFile prettyOutput
                    putStrLn $ "Processed: " ++ inputFile
                _ -> putStrLn $ "Invalid ParseResult for file " ++ inputFile

mkFolderIfNotExists :: FilePath -> IO ()
mkFolderIfNotExists = createDirectoryIfMissing True

genOutputName :: FilePath -> String -> String -> FilePath
genOutputName outputFolder extension file = replaceExtension (replaceFolderAtIndex file 1 outputFolder) extension

replaceFolderAtIndex :: FilePath -> Int -> String -> FilePath
replaceFolderAtIndex filePath index newFolder =
    joinPath (replaceAtIndex index newFolder (splitDirectories dirPath))
        </> fileName
  where
    -- Split the file path into directory and file components
    dirPath = fst $ splitFileName filePath
    fileName = takeFileName filePath

    -- Replace the folder at the specified index using zipWith
    replaceAtIndex :: Int -> String -> [FilePath] -> [FilePath]
    replaceAtIndex idx newVal = zipWith (\i folder -> if i == idx then newVal else folder) [0 ..]

checkADTShow :: IO ()
checkADTShow = do
    let markdownStr = "<number>     ::= [int]"
    putStrLn "Testing ADT's Show instance:"
    putStrLn $ "Input string: " ++ markdownStr
    putStrLn $ case parse bnfParser markdownStr of
        Error _ -> "Parse error"
        Result _ adt -> "Result: " ++ show adt

-- Helper to safely remove a file
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile file = void $ try @IOException (removeFile file)

-- Function to run git diff and write output to a file
runGitDiffToFile :: FilePath -> FilePath -> FilePath -> IO ()
runGitDiffToFile file1 file2 outputFile = do
    withFile outputFile WriteMode $ \handle -> do
        (_, _, _, processHandle) <-
            createProcess
                (proc "git" ["--no-pager", "diff", "--no-index", "--no-ext-diff", file1, file2])
                    { std_out = UseHandle handle
                    }
        _ <- waitForProcess processHandle
        return ()

-- Function to get the "size" of a diff file (number of lines)
getDiffLineCount :: FilePath -> IO Int
getDiffLineCount filePath = do
    result <- try (readFile filePath) :: IO (Either IOException String)
    case result of
        Left e
            | isDoesNotExistError e -> return maxBound -- Treat as largest possible diff if file doesn't exist
            | otherwise -> ioError e
        Right contents -> return . length . lines $ contents

-- Represents the current best diff found: (size, path to diff file)
type BestDiff = (Int, FilePath)

-- Helper function to compare a new diff with the current best
compareAndUpdateBestDiff
    :: FilePath -- base name for temp diff files
    -> FilePath -- actual output file
    -> BestDiff -- current best diff (size, path)
    -> FilePath -- current expected file to compare against
    -> IO BestDiff -- returns the new best diff
compareAndUpdateBestDiff baseDiffPath actualOutputFile (currentBestSize, currentBestPath) expectedFile = do
    let tempDiffFile = baseDiffPath ++ "_" ++ takeBaseName expectedFile ++ ".tmpdiff"

    runGitDiffToFile expectedFile actualOutputFile tempDiffFile
    candidateDiffSize <- getDiffLineCount tempDiffFile

    let isBetter = candidateDiffSize < currentBestSize

    unless isBetter $ safeRemoveFile tempDiffFile

    return $
        bool
            (currentBestSize, currentBestPath)
            (candidateDiffSize, tempDiffFile)
            isBetter

-- Function to compare an actual output against all expected outputs and find the best diff
findBestDiffForInput :: FilePath -> FilePath -> [FilePath] -> IO ()
findBestDiffForInput inputFile actualOutputFile allExpectedOutputFiles = do
    let baseName = takeBaseName inputFile
    let bestDiffFileNamePrefix = "./examples/diff_output/" ++ baseName
    let finalBestDiffFile = bestDiffFileNamePrefix ++ ".diff"

    putStrLn $ "Finding smallest diff for: " ++ inputFile

    let initialBest = (maxBound :: Int, "")

    (finalSize, bestTempPath) <-
        foldM
            (compareAndUpdateBestDiff bestDiffFileNamePrefix actualOutputFile)
            initialBest
            allExpectedOutputFiles

    when (finalSize /= maxBound) $ do
        exists <- doesFileExist bestTempPath
        when exists $ do
            safeRemoveFile finalBestDiffFile
            renameFile bestTempPath finalBestDiffFile

    tempFiles <- glob $ bestDiffFileNamePrefix ++ "_*.tmpdiff"
    let toDelete = filter (/= bestTempPath) tempFiles
    mapM_ safeRemoveFile toDelete

main :: IO ()
main = do
    checkADTShow

    putStrLn ""
    putStrLn "Testing example inputs:"

    inputFiles <- glob "examples/input/*.bnf"

    mkFolderIfNotExists "./examples/output"
    mkFolderIfNotExists "./examples/diff_output"

    mapM_
        ( \inputFile -> do
            let actualOutputFile = genOutputName "output" "hs" inputFile

            -- Process the input file to generate its actual output
            processFile inputFile actualOutputFile

            let baseName = takeBaseName inputFile
            expectedOutputFiles <- glob $ "examples/expected_output/" ++ baseName ++ "/*.hs"

            -- Find the best diff for this processed input against all expected outputs
            findBestDiffForInput inputFile actualOutputFile expectedOutputFiles
        )
        inputFiles

    putStrLn "\nAll processing complete."
