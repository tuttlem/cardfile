module Main where

import Network.Wai.Handler.Warp         (run)
import System.Environment               (lookupEnv)

import Config (Config(..), Environment(..), setLogger)
import Api    (app)

main :: IO ()
main = do
    env     <- lookupSetting "ENV" Development
    port    <- lookupSetting "PORT" 8081

    let cfg = Config { getEnv = env }
        logger = setLogger env

    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing  -> def
                       Just a   -> read a
