module Lib
    ( getResult
    ) where

import Aux.VcfFields (VcfFields, formatVcfInfo)
import Aux.Query 
import Aux.Requests
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson.Types
import Network.HTTP.Req
import Data.Maybe (fromJust, catMaybes)
import Data.List (intercalate, isPrefixOf, isInfixOf)
import System.Process


{- | Query Function
     Takes an string and produces teh final result or a QueryError -}
getResult :: String -> IO (Either QueryError String)
getResult x =  do
                 spid <-  getSpdisForQuery $ toQuery x
                 let
                   toString sp v = intercalate "," (x:sp:formatVcfInfo v)
                   res = (\x -> toString x <$> getVCFFieldsForSpdi  x) <$> spid 
                 case res of
                  Left z -> return $ Left z
                  Right y -> Right <$> y 

{- | 
  From a Query generate a result string or output a Query error.
  First get possible ids for the query.
  Get summary for each id and filter those responses using the title.
  If we can narrow to one response we have our results otherwise
  we return the appropriate Query error.
 -}
getSpdisForQuery :: Query -> IO (Either QueryError String)
getSpdisForQuery q = do
                ids <- delayedReq requestIdsFor q
                case ids of 
                     Left x -> return $ Left x 
                     Right xs -> do
                                 res <- catMaybes <$> mapM (delayedReq requestSummaryForId ) xs 
                                 let 
                                    shortList = filter (titleMatchesQuery q) res
                                 return $ case shortList of 
                                     [y] -> Right $ spdi y
                                     (_:_:_) -> Left $ MultipleResults q
                                     [] -> Left $ NoResults q


{- | Global sleep to slow down requests
    The ncbi server has a request rating limit and we are being rather conservative with our
    requests
 -}
wait = system "sleep 1s"

-- | Auxiliary function that delays requests
delayedReq :: (a -> Req b) -> (a -> IO b)
delayedReq f x = runReq defaultHttpConfig $ liftIO wait >> f x


-- | Use the esearch ncbi utils to get ids corresponding to the current query
requestIdsFor :: Query -> Req (Either QueryError [String])
requestIdsFor q = do
          r <- req 
               GET 
               searchUrl
               NoReqBody
               jsonResponse
               (queryToParams q)
          let  resultObject = responseBody r
               ids = parseMaybe getIds resultObject
          return $ case ids of
             Nothing -> Left $ IdNotFound q
             Just x -> Right x


-- | Use the esummary ncbi utils to get spdi id and title from id
requestSummaryForId :: String -> Req (Maybe SummaryInfo)
requestSummaryForId id = do 
                g <- req
                     GET
                     summaryUrl
                     NoReqBody
                     jsonResponse
                     (searchParams id)         
                let
                   resultFinal = responseBody g
                   parsedResult = parseMaybe  (getLocation id) resultFinal 
                   finalResult = parsedResult >>= \(x,y) -> return $ SummaryInfo x y  
                return finalResult



-- | Use the esummary ncbi utils to get spdi id and title from id
requestVCFFieldsForSpdi :: String -> Req VcfFields
requestVCFFieldsForSpdi x  = do
                      r <- req 
                            GET
                            (urlForSpdi x)
                            NoReqBody
                            jsonResponse
                            mempty
                      return $ responseBody r


-- | Request to get the VCF fields informations using an spdi 
getVCFFieldsForSpdi :: String -> IO VcfFields
getVCFFieldsForSpdi x = runReq  defaultHttpConfig $ requestVCFFieldsForSpdi x


