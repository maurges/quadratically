module Handle
(
    handleClient,
    KnowBase,
    KnowBaseLock
) where

import Prelude hiding (lookup)

import System.IO
import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe (isJust)
import Data.Map.Strict (member, (!), Map, empty, insert, foldlWithKey', lookup, insertWith, foldl')

import Validate


type KnowBase = Map UserName UserInfo
type KnowBaseLock = MVar KnowBase


type UserName = String
data UserInfo = UserInfo {userKey :: Maybe String, userShout :: Maybe String}
    deriving (Show, Read)


handleClient :: Handle -> KnowBaseLock -> Integer -> IO ()
handleClient handle db_lock index = do
    printBanner handle
    continue <- authenticateUser handle db_lock
    case continue of
        Just name -> mainLoop name handle db_lock
        Nothing -> return ()


printBanner :: Handle -> IO ()
printBanner handle = hPutStrLn handle "Welcome to the Electronic Farts private server! Do you feel the sense of pride and accomplishment?"


authenticateUser :: Handle -> KnowBaseLock -> IO (Maybe UserName)
authenticateUser handle db_lock = do
    hPutStrLn handle "Enter your name: "
    name <- hGetLine handle
    db <- readMVar db_lock
    if name `member` db
        then
            authoriseUser handle db_lock name >>= \r ->
                return $ if r then Just name else Nothing
        else
            registerUser handle db_lock name >>= \r ->
                return $ if r then Just name else Nothing


authoriseUser :: Handle -> KnowBaseLock -> UserName -> IO Bool
authoriseUser handle db_lock name =
    hasRegisteredKey name db_lock >>= \does ->
        if does
        then checkUserKey handle db_lock name
        else inviteToBuy handle db_lock name


registerUser :: Handle -> KnowBaseLock -> UserName -> IO Bool
registerUser handle db_lock name = do
    db <- takeMVar db_lock
    if name `member` db
    then (hPutStrLn handle "Someone already registered while you were waiting!"
        >> putMVar db_lock db >> return False)
    else putMVar db_lock (createUser name db) >> alert_succ >> return True
    where alert_succ = hPutStrLn handle "Successfully registered! Now you are entitled to access to the server" 


createUser :: UserName -> KnowBase -> KnowBase
createUser name = insert name (UserInfo {userKey = Nothing, userShout = Nothing})


hasRegisteredKey :: UserName -> KnowBaseLock -> IO Bool
hasRegisteredKey name db_lock =
    on_database <$> readMVar db_lock
    where on_database db = isJust $ userKey =<< lookup name db


inviteToBuy :: Handle -> KnowBaseLock -> UserName -> IO Bool
inviteToBuy handle _ _ = hPutStrLn handle "Remember to purchase the key in our web store to get access to the premium functions" >> return True


checkUserKey :: Handle -> KnowBaseLock -> UserName -> IO Bool
checkUserKey handle db_lock name = do
    hPutStrLn handle "As the key is activated on this account, please prove your ownership of it by providing the key"
    key <- hGetLine handle
    db <- readMVar db_lock
    if validKey name key
    then hPutStrLn handle "Successfully logged in" >> return True
    else hPutStrLn handle "Wrong key!" >> return False


userHasKey :: KnowBaseLock -> UserName -> IO Bool
userHasKey db_lock name = do
    db <- readMVar db_lock
    case (userKey $ db ! name) of
        Nothing -> return False
        Just _ -> return True


validKey :: UserName -> String -> Bool
validKey = flip validateKey


uniqueKey :: String -> KnowBaseLock -> IO (Maybe UserName)
uniqueKey key db_lock = do
    db <- takeMVar db_lock
    let includes = foldlWithKey' is_key Nothing db
    putMVar db_lock db
    return includes
    where is_key Nothing name k =
            if userKey k == Just key
                then Just name
                else Nothing
          is_key x _ _ = x


mainLoop :: UserName -> Handle -> KnowBaseLock -> IO ()
mainLoop username handle db_lock = do
    hPutStr handle "> "
    command_str <- hGetLine handle
    let command:args = words command_str
    command_status <- handleCommand username command args db_lock handle
    if command_status
    then mainLoop username handle db_lock
    else return ()


handleCommand :: UserName -> String -> [String]-> KnowBaseLock -> Handle  -> IO Bool

handleCommand _ "quit" _ _ handle = hPutStrLn handle quit_str >> return False
    where quit_str = "Goodbye, come back soon"

handleCommand _ "help" _ _ handle = hPutStrLn handle help_str >> return True
    where help_str = "Availible commands: help quit claim proclaim list see"
handleCommand _ "halp" _ _ handle = hPutStrLn handle help_str >> return True
    where help_str = "Availible commands: help quit claim proclaim list see"

handleCommand name "see" _ l h = seeShout name l h
handleCommand name "claim" (key:_) l h = claimKey name key l h
handleCommand name "proclaim" message l h = proclaimMsg name (unwords message) l h
handleCommand _ "list" _ l h = listUsers l h
handleCommand _ "zakladka" _ l h = zakladka l h

handleCommand _ _ _ _ handle = hPutStrLn handle no_command_str >> return True
    where no_command_str = "No such command or wrong number of arguments"


claimKey :: UserName -> String -> KnowBaseLock -> Handle -> IO Bool
claimKey name key db_lock handle = do
    if validKey name key
        then do u <- uniqueKey key db_lock
                case u of
                  Nothing -> add_key name key db_lock >> hPutStrLn handle success
                  Just name -> hPutStrLn handle (non_unique name)
        else hPutStrLn handle invalid
    return True
    where
        success = "Successfully claimed the key! Now premium functions are availible!"
        --
        invalid = "Can't claim the key as it is invalid"
        --
        non_unique name = "Can't claim the key as it is already claimed by " ++ name
        --
        add_key name key db_lock = do
            db <- takeMVar db_lock
            putMVar db_lock $ updateUserKey db name key



proclaimMsg :: UserName -> String -> KnowBaseLock -> Handle -> IO Bool
proclaimMsg username msg db_lock handle = do
    has_key <- userHasKey db_lock username
    if has_key
        then add_message username msg db_lock >> hPutStrLn handle (success msg)
        else hPutStrLn handle failure
    return True
    where
        success msg = "You proclaim: " ++ msg
        --
        failure = "You can't write the message, as you are not premium!"
        --
        add_message name msg db_lock = do
            db <- takeMVar db_lock
            putMVar db_lock $ updateUserShout db name msg


listUsers :: KnowBaseLock -> Handle -> IO Bool
listUsers db_lock handle =
    (hPutStrLn handle . foldlWithKey' pretty "" =<< readMVar db_lock)
    >> return True
    where
        pretty :: String -> UserName -> UserInfo -> String
        pretty s name _ = name ++ "\n" ++ s


zakladka :: KnowBaseLock -> Handle -> IO Bool
zakladka db_lock handle =
    (hPutStrLn handle . foldlWithKey' pretty "" =<< readMVar db_lock)
    >> return True
    where
        pretty :: String -> UserName -> UserInfo -> String
        pretty s name (UserInfo {userKey = key}) =
            case key of
                Nothing -> s
                Just jkey -> name ++ ": " ++ jkey ++ "\n" ++ s


seeShout :: UserName -> KnowBaseLock -> Handle -> IO Bool
seeShout name db_lock handle = do
    db <- readMVar db_lock
    let Just info = lookup name db
    let shout = userShout info
    case shout of
        Just s -> hPutStrLn handle $ "You shouted: " ++ s
        Nothing -> hPutStrLn handle "You have no shoutout!"
    return True


updateUserKey :: KnowBase -> UserName -> String -> KnowBase
updateUserKey db name key =
    insertWith replace name empty_data db where
        replace _ (UserInfo {userShout = s}) = UserInfo {userShout = s, userKey = Just key}
        --
        empty_data = UserInfo {userShout = Nothing, userKey = Nothing}

updateUserShout :: KnowBase -> UserName -> String -> KnowBase
updateUserShout db name shout =
    insertWith replace name empty_data db where
        replace _ (UserInfo {userKey = k}) = UserInfo {userKey = k, userShout = Just shout}
        --
        empty_data = UserInfo {userShout = Nothing, userKey = Nothing}
