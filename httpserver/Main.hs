import System.Environment
import System.IO
import Network
import Control.Concurrent (forkIO)
import Control.Monad
import Prelude
import System.Directory

makeResponse req = do
  if (take 5 req) == "GET /"
    then do
      let file = drop 5 $ take ((length req) - 10) req
      exists <- doesFileExist file
      if exists
        then do
          content <- readFile file
          return ("HTTP/1.1 200 OK\r\nContent-Length: " ++ (show (length content)) ++ "\r\n\r\n" ++content)
	else return "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404: Not Found"
    else return "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\n400: Bad Request"

server :: PortNumber -> IO()
server port = withSocketsDo $ do 
  socket <- listenOn(PortNumber port)
  forever $ do  
    (sockh, _, _) <- accept socket
    forkIO $ do 
      hSetBuffering sockh LineBuffering
      s <- hGetLine sockh
      hSetBuffering sockh NoBuffering
      response <- makeResponse s
      hPutStr sockh response
      hClose sockh


main :: IO()
main = do
  portNums <- getArgs
  let port | (length portNums) > 1 = error "Too much arguments"
           | otherwise            = fromIntegral $ read $ head portNums
  if (port < 0)
    then print "Illegal port number"
    else server (fromIntegral port)  
