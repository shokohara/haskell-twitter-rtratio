module Main where
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Authenticate.OAuth
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit

myOAuth :: OAuth
myOAuth = newOAuth { oauthServerName = "api.twitter.com"
                   , oauthConsumerKey = "Your Consumer Key"
                   , oauthConsumerSecret = "Your Cosumer Secret"
                   }

myCred :: Credential
myCred = newCredential "Your Access Token" "Your Access Token Secret"

data Tweet = Tweet { text :: Text } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

getTweets :: String -> IO (Either String [Tweet])
getTweets name = do
    req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
    m <- newManager tlsManagerSettings
    res <- do
            signedreq <- signOAuth myOAuth myCred req
            httpLbs signedreq m
    return $ eitherDecode $ responseBody res

main :: IO ()
main = do
    ets <- getTweets "ishiy1993"
    case ets of
      Left err -> putStrLn err
      Right ts -> mapM_ T.putStrLn . map text $ take 5 ts

