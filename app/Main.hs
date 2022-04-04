{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Lib (getResult)
import Data.List (intercalate)
import Options.Applicative
import Data.Semigroup
import System.Directory
import Control.Monad (when, unless)
import Data.Either (partitionEithers)
import qualified Text.PortableLines  as PL (lines)

data ProgArgs = ProgArgs { inputFile :: FilePath,
                           outputFile :: FilePath } deriving Show

progArgs :: Parser ProgArgs
progArgs = ProgArgs
        <$> argument str (metavar "INPUT_FILE" <> help "Input csv file with unix line endings")
        <*> argument str (metavar "OUTPUT_FILE" <> help "Output filename")



opts :: ParserInfo ProgArgs
opts = info ( progArgs <**> helper)
    ( fullDesc
    <> progDesc "Get genomic location for variants given as gene exon change and amino acid"
    <> header "Convert to genomic location Broad supplied return of results files" )

main :: IO ()
main = do
        ProgArgs inF outF <- execParser opts
        inputFileExists <- doesFileExist inF
        when inputFileExists $ do
           res <- readFile inF
           let
              (header':rest) =  PL.lines . Prelude.filter (/= '"') $ res
              header = intercalate "," [header',"spid", "chrom", "pos", "ref", "alt", "bcftoolsQuery"]
           results' <- mapM getResult rest
           let 
              (failures,results) = partitionEithers results'
              errorMsg = unlines $ "The following failures were encountered" : map show  failures
              successMsg = "All variants were found in clinvar Database"
           writeFile outF  . unlines $ header:results
           if null failures then putStrLn successMsg else putStrLn errorMsg


