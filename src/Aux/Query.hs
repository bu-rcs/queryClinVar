{-|
 Module      : Aux.Query
 Description : Auxiliary library to work with Queries and Query Errors
 License : GPL-3
 This module is for internal use of the clinVarDB query program and collects 
 Query data and helper functions.
 -}
module Aux.Query
where

import Data.List (intercalate, isPrefixOf, isInfixOf)

-- | Query information that are extracted from input String
data Query = Query {gene :: String,
                    cdnaVar :: String,
                    protVar :: String} deriving Show

-- | Possible query errors
data QueryError = IdNotFound Query | MultipleResults Query | NoResults Query deriving Show

-- | Data to hold spdi and title from summary database
data SummaryInfo = SummaryInfo {spdi :: String, title :: String}


-- | Input String to Query 
toQuery :: String -> Query
toQuery x = Query geneName cDnaVar prot
       where
          (geneName :_:var:_) = words . Prelude.filter (/= '"' ) . takeWhile (/= ',') $ x
          cDnaVar = "c." ++ var
          prot = tail .dropWhile (/=',') $ x

{- | 
   Obtain cdna variant information from title
   Fragile operation
   -}
titleToCdnaVar :: String -> String
titleToCdnaVar = takeWhile (/=' '). tail . dropWhile (/= ':')



-- | Check if a title matches a query
titleMatchesQuery :: Query -> SummaryInfo -> Bool 
titleMatchesQuery q (SummaryInfo _ t) 
       |length x < length  (cdnaVar q) = x `isPrefixOf` cdnaVar q
       | otherwise = cdnaVar q `isPrefixOf` x
       where 
         x = titleToCdnaVar t



