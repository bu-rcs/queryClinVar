{-# LANGUAGE OverloadedStrings #-}
{-|
 Module      : Aux.VcfFields
 Description : Auxiliary to deal with VcfFields provides a FromJSON instance and helper functions
 License : GPL-3
 This module is for internal use of the clinVarDB query program.
 -}
module Aux.VcfFields 
where


import Data.Aeson

-- |Data structure to hold VCF information
data VcfFields = VcfFields {
                 chrom :: String,
                 pos :: Int,
                 ref :: String,
                 alt :: String } deriving Show

instance FromJSON VcfFields where 
  parseJSON = withObject "VcfField" $ \obj -> do
                dat <- obj .: "data"  
                chrom' <-  dat  .: "chrom"
                pos' <-   dat .: "pos"
                ref'  <-  dat .: "ref"
                alt' <- dat .: "alt"
                return $ VcfFields chrom' pos' ref' alt'
 



-- | Create a String array from a vcf field
formatVcfInfo :: VcfFields -> [String]
formatVcfInfo x = res
             where
               res = [extractChromFromNCstring . chrom $ x,
                      show . pos $ x,
                      ref x,
                      alt x,
                      bcfQueryStringFrom x ]


{- | 
  Helper Function to get chromosome from NC String
  This refers to the sequence name strings that are used by NCBI
  as for chromosomes they start with the letters NC.
  This is probably not stable.
 -}
extractChromFromNCstring :: String -> String
extractChromFromNCstring x = format res 
                            where
                              res = takeWhile (/= '.') . dropWhile(=='0') . tail . dropWhile (/='_') $ x
                              format "23" = "X"
                              format "24" = "Y"
                              format y = y

{- | 
 Create a chrosome region query string
 suitable to be used with bcftools, tabix, e.t.c
 Note that "chr" prefix is appropriate for build 38
 but might not be for other builds.
 -}
bcfQueryStringFrom :: VcfFields -> String
bcfQueryStringFrom x = concat [chrom', ":", posStart, "-", posStart]
                where
                  chrom' = "chr" ++ (extractChromFromNCstring . chrom $ x)
                  posStart = show . pos $ x
