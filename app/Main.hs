{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.IORef
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.HTTP.Req
import System.Random

numConsidered, numTaken, maxSize, interval :: Int
numConsidered = 50
numTaken = 2
maxSize = numTaken * 100
interval = 1000000 * 7200

hnApi :: Url Https
hnApi = https "hacker-news.firebaseio.com" /: "v0"

mastodonApi :: Text -> Url Https
mastodonApi host = https host /: "api" /: "v1" /: "statuses"

type M = ReaderT Env IO

data Env = Env
    { alreadyPosted :: IORef (S.Set Int)
    , hostInstance :: Text
    , token :: Text
    }

evictInsert :: Hn -> M ()
evictInsert Hn{postId = n} = do
    env <- ask
    liftIO $ modifyIORef (alreadyPosted env) $ S.insert n
    l <- liftIO $ S.size <$> readIORef (alreadyPosted env)
    when (l > maxSize) $
        liftIO $ modifyIORef (alreadyPosted env) $ S.drop 1

data Hn = Hn
    { postId :: Int
    , title :: Text
    , score :: Int
    , author :: Text
    } deriving (Eq, Ord, Show)

instance FromJSON Hn where
    parseJSON = withObject "Hn" $ \v -> Hn
        <$> v .: "id"
        <*> v .: "title"
        <*> v .: "score"
        <*> v .: "by"

toUrl :: Hn -> Text
toUrl Hn{postId = pid} = "https://news.ycombinator.com/item?id=" <> pidStr
    where
    pidStr = T.pack $ show pid

toStatus :: Hn -> Text
toStatus hn = T.unlines
    [ title hn
    , toUrl hn
    , "Submitted by: " <> author hn <> "; Score: " <> (T.pack . show $ score hn)
    ]

weightedSelect :: [Hn] -> M Hn
weightedSelect hns = do
    acc0 <- liftIO $ randomRIO (0, total - 1)
    let sel = foldr go (Right acc0) hns
    case sel of
        Left item -> evictInsert item >> return item
        Right acc -> liftIO $ throwIO $ ErrorCall $
            "weightedSelect failed: " ++
            "most likely no posts were passed or the total score was 0"
    where
    total = sum $ map score hns

    go hn (Left item) = Left item
    go hn (Right acc) = if acc < score hn
        then Left hn
        else Right (acc - score hn)

getTopPosts :: M [Hn]
getTopPosts = do
    env <- ask
    s <- liftIO $ readIORef (alreadyPosted env)
    ids <- runReq defaultHttpConfig $
        req GET (hnApi /: "topstories.json") NoReqBody jsonResponse mempty
    fmap catMaybes . mapM getHnById . take numConsidered 
        . filter (not . flip S.member s) $ responseBody ids

getHnById :: Int -> M (Maybe Hn)
getHnById pid = liftIO . handle handler . runReq defaultHttpConfig $ do
    post <- req GET (hnApi /: "item" /: json) NoReqBody jsonResponse mempty
    return $ responseBody post
    where
    json = (T.pack $ show pid) <> ".json"

    handler :: SomeException -> IO (Maybe Hn)
    handler e = return Nothing

takeBest :: [Hn] -> M Hn
takeBest posts =
    let best'm = listToMaybe posts
    in case best'm of
        Just best -> evictInsert best >> return best
        Nothing -> liftIO $ throwIO $ ErrorCall $
            "takeBest failed: " ++
            "most likely no posts were passed or all posts on the front page " ++
            "have already been posted"

getPostsToToot :: M (Hn, Hn)
getPostsToToot = do
    posts <- getTopPosts
    best <- takeBest posts
    rand <- weightedSelect posts
    return (best, rand)

tootHn :: Hn -> M ()
tootHn hn = do
    env <- ask
    let 
        header = oAuth2Bearer $ T.encodeUtf8 (token env)
        reqData = object $ 
            ["status" .= toStatus hn, "visibility" .= ("public" :: Text)] 
    runReq defaultHttpConfig $ do
        resp <- req POST (mastodonApi $ hostInstance env) (ReqBodyJson reqData) jsonResponse header
        liftIO $ print (responseBody resp :: Value)

main :: IO ()
main = do
    s <- newIORef S.empty 
    hi <- T.getLine
    token <- T.getLine
    let env = Env s hi token
    forever $ handle handler $ runReaderT action env
    where
    action = do
        (a, b) <- getPostsToToot
        tootHn a
        tootHn b
        liftIO $ threadDelay interval

    handler :: SomeException -> IO ()
    handler e = print e
