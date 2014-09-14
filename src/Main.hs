{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import           Snap.Http.Server
import           Snap.Core
import           System.IO
import           Snap.Util.FileServe (serveFile)

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif

main :: IO ()
main = do
  quickHttpServe defaultHandler

defaultHandler :: Snap ()
defaultHandler = do
  uri <- getsRequest rqPathInfo
  if or $ map (uri ==) ["index.html"]
    then 
      serveFile ("static/" ++ B.unpack uri)
    else
      routeUri uri

routeUri :: B.ByteString -> Snap ()
routeUri uri = go uri handlers
  where 
    handlers = [("/search", handleSearch), ("/movie", handleMovie), ("/graph", handleGraph)]
    go uri ((path, handler) : xs) = if uri `B.isPrefixOf` path
      then handler
      else go uri xs

handleSearch :: Snap ()
handleSearch = 
  where
    query :: Query '[Str, Str, Number]
    query = [cypher| 
      MATCH (movie:Movie) 
      WHERE movie.title =~ {0} 
      RETURN movie.title as title, movie.tagline as tagline, movie.released as released|]

handleMovie :: Snap ()
handleMovie = 
  where
    query :: Query '[Str, Str, Str, Str]
    query = [cypher| 
      MATCH (movie:Movie {title:{0}})
      OPTIONAL MATCH (movie)<-[r]-(person:Person)
      WITH movie.title as title,
           collect({name:person.name,
                    job:head(split(lower(type(r)),'_')),
                    role:r.roles}) as cast 
      LIMIT 1
      UNWIND cast as c 
      RETURN title, c.name as name, c.job as job, c.role as role|]

handleGraph :: Snap ()
handleGraph = 
  where
    query :: Query '[Str, Collection Str]
    query = [cypher|
      MATCH (m:Movie)<-[:ACTED_IN]-(a:Person)
      RETURN m.title as movie, collect(a.name) as cast
      LIMIT {0}|]
