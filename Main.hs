-- https://www.haskell.org/hoogle

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (throwTo)
import Network.Socket
import System.IO
import System.Exit          (ExitCode(ExitSuccess))
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import System.Directory     (renameFile)
import Data.Maybe           (listToMaybe)
import Data.Map.Strict      (member, (!), Map, empty, insert, foldlWithKey')

import Handle


databaseName :: FilePath
databaseName = "database.db"

databaseTemp :: FilePath
databaseTemp = "tempdb.db"

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


main :: IO ()
main = do
    db <- maybeCreateDB <$> loadDB
    db_lock <- newMVar db
    db_lock `seq` putStrLn "Database loaded"
    forkIO $ saveDBLoop db_lock
    --
    main_tid <- myThreadId
    res <- placeHandlers db_lock main_tid
    res `seq` putStrLn "Handlers set"
    --
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet server_port iNADDR_ANY)
    listen_result <- listen sock 2
    listen_result `seq` putStrLn "Listening on port 1488"
    --
    let start_index = 0
    mainLoop sock db_lock start_index
    where server_port = 1488


loadDB :: IO (Maybe (KnowBase))
loadDB = maybeRead <$> readFile databaseName

maybeCreateDB :: Maybe (KnowBase) -> KnowBase
maybeCreateDB Nothing = empty
maybeCreateDB (Just x) = x


saveDBLoop :: KnowBaseLock -> IO ()
saveDBLoop db_lock = do
    threadDelay write_delay
    saveDB db_lock
    saveDBLoop db_lock
    --
    where
        write_delay = 30 * 1000000 -- 30 seconds


saveDB db_lock = do
    database <- takeMVar db_lock
    --
    res <- withFile databaseTemp WriteMode (write_all database)
    res `seq` renameFile databaseTemp databaseName
    --
    res <- putMVar db_lock database
    res `seq` putStrLn "Database written"
    --
    where
        write_all :: KnowBase -> Handle -> IO ()
        write_all db handle = hPutStr handle (show db)


placeHandlers :: KnowBaseLock -> ThreadId  -> IO Handler
placeHandlers db_lock tid = do
    installHandler sigINT  (Catch $ handleExit db_lock tid) Nothing
    installHandler sigTERM (Catch $ handleExit db_lock tid) Nothing
    
    

mainLoop :: Socket -> KnowBaseLock -> Integer -> IO ()
mainLoop sock db_lock index = do
    conn <- accept sock
    putStrLn "Got a connection"
    forkIO $ handleConn conn db_lock index
    mainLoop sock db_lock (index + 1)


handleConn :: (Socket, SockAddr) -> KnowBaseLock -> Integer -> IO ()
handleConn (sock, addr) db_lock index = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    --
    handleClient handle db_lock index
    --
    hClose handle
    putStrLn "Connection closed"


handleExit :: KnowBaseLock -> ThreadId -> IO ()
handleExit db_lock tid =
    saveDB db_lock >> putStrLn "Exiting..."
    >> throwTo tid ExitSuccess
