import System.IO
import System.Environment (getArgs)
import Network (listenOn, accept, Socket, withSocketsDo, PortID(..))
import Control.Concurrent
import Control.Monad
import Directory (doesFileExist)

makeAnswer str = do
    if (take 5 str) == "GET /"
        then do
            name <- return ((drop 5 (take ((length str) - 10) str)))
            flag <- doesFileExist name
            if flag
                then do
                    h <- openFile name ReadMode
                    content <- hGetContents h
                    seq content (hClose h)
                    return ("HTTP/1.1 200 OK\r\nContent-Length: " ++ (show (length content)) ++ "\r\n\r\n" ++ content)
                else return "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404: Not Found"
        else return "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\n400: Bad Request"

main = withSocketsDo $ do
    [portStr] <- getArgs
    let port = fromIntegral (read portStr :: Int)
    socket <- listenOn (PortNumber port)
    forever $ do
        (h, _, _) <- accept socket
        forkIO $ do
            hSetBuffering h LineBuffering
            r <- hGetLine h
            hSetBuffering h NoBuffering
            (makeAnswer r) >>= (hPutStr h)
            hClose h
