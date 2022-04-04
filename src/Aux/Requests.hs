{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
 Module      : Aux.Requests
 Description : Auxiliary library to work with requests to clinvar web api
 License : GPL-3
 This module is for internal use of the clinVarDB query program and has
 the urls and auxiliary function for generating requests.
 More info can be found looking for Entrez Programming Utilities (E-utilities)
 and the https://www.ncbi.nlm.nih.gov/books/NBK25501/
 -}

module Aux.Requests where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Types
import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.Vector as V
import Aux.Query (Query(..))



-- | Base url for ncbi eutils requests 
baseUrl :: Url 'Https
baseUrl = https "eutils.ncbi.nlm.nih.gov" /: 
             "entrez" /:
             "eutils" 

-- | Url for ncbi searches
searchUrl :: Url 'Https
searchUrl = baseUrl  /: "esearch.fcgi"

-- | Url for ncbi summary results
summaryUrl :: Url 'Https
summaryUrl = baseUrl /: "esummary.fcgi" 


-- | Variation service url
varServiceBaseUrl :: Url 'Https
varServiceBaseUrl  = https "api.ncbi.nlm.nih.gov" /: "variation" /: "v0" /: "spdi"

-- | Url for lookup of an spdi string with the variation service
urlForSpdi :: String -> Url 'Https
urlForSpdi x = varServiceBaseUrl /: T.pack x /: "vcf_fields" 



useClinVarDB :: Option 'Https
useClinVarDB = "db" =: ("clinvar"::String)


returnJson :: Option 'Https
returnJson = "retmode" =: ("json":: String)

-- | Parameters to query the clinvar database and return json results
mainParams :: Option 'Https
mainParams = useClinVarDB <> returnJson

-- | Get ids from esearch result value
getIds :: Value -> Parser [String]
getIds = withObject "esearchresult" $ \v ->  
           v.: "esearchresult" >>=  (.: "idlist")


-- | Create an option for an id search
searchParams :: String -> Option 'Https
searchParams x = mainParams <> ("id" =: x)


-- | Get spdi and title information from a search result
getLocation :: String -> Value -> Parser (String, String)
getLocation x = withObject "result" $ \o -> do
                r' <- o .: "result"
                r  <- r' .: fromString x
                vs <- r .: "variation_set"
                title <- r .: "title"
                spdi <- V.head vs .: "canonical_spdi"
                return (spdi, title)

-- | Transform a query to parameters for esearch
queryToParams :: Query -> Option 'Https
queryToParams (Query gn var prot) = mainParams <> ("term" =: query) 
                              where
                                geneQuery = gn++"[gene]"
                                query = unwords [ geneQuery, "AND", var]


