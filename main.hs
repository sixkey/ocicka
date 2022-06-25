import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import System.Environment
import System.IO
import Control.Monad.RWS as R
import Control.Monad.Trans.State as S
import Data.Maybe
import Control.Concurrent
import Data.Time.Clock
import Data.Heap as H
import Data.Bifunctor
import Data.Map as M
import Data.List
import System.Process
import Control.Monad.Random

data Interval = Hours Integer 
              | Seconds Integer 
              | Minutes Integer 
              | Days Integer 
              | Weeks Integer 
              | Months Integer deriving Show

data MessageList = List [ String ]
                 | Var String deriving Show

data Message = Message String
             | OneOf MessageList deriving Show

data Action = Every Interval Action  
            | After Interval Action 
            | Ping Message
            | Sequence [ Action ] 
            deriving Show 

-- generic parsing

pPosInt:: Parser Integer
pPosInt = read <$> many1 digit 

manspaces :: Parser ()
manspaces = do 
    many1 space
    return ()

inbetween :: Char -> Char -> Parser a -> Parser a
inbetween a b = between ( char a ) ( char b )

pList :: Char -> Char -> Char -> Parser a -> Parser [ a ]
pList start end del element = 
        inbetween start end  
                ( (:) 
                  <$> ( spaces *> element <* spaces ) 
                  <*> many ( char del *> spaces *> element <* spaces ) ) 

-- ocicka parsing

constructorOfSuffix :: Char -> Integer -> Interval
constructorOfSuffix 's' = Seconds
constructorOfSuffix 'm' = Minutes
constructorOfSuffix 'h' = Hours
constructorOfSuffix 'D' = Days
constructorOfSuffix 'W' = Weeks
constructorOfSuffix 'M' = Months

pInterval :: Parser Interval
pInterval = do 
    number <- pPosInt
    c <- oneOf "smhDWM"
    return $ constructorOfSuffix c number


pMessageLiteral :: Parser String
pMessageLiteral = inbetween '"' '"' $ many ( noneOf "\"" ) 

pTextMessage :: Parser Message
pTextMessage = Message <$> pMessageLiteral

pMessageListLiteral :: Parser MessageList
pMessageListLiteral = List <$> pList '[' ']' ',' pMessageLiteral

pMessageListVariable :: Parser MessageList
pMessageListVariable = Var <$> ( (:) <$> letter <*> many1 alphaNum )

pMessageList :: Parser MessageList
pMessageList = pMessageListLiteral <|> pMessageListVariable 

pOneOf :: Parser Message
pOneOf = do  
    string "oneof" 
    manspaces
    OneOf <$> pMessageList

pMessage :: Parser Message
pMessage = pOneOf <|> pTextMessage

pPing :: Parser Action
pPing = do 
    string "ping"
    manspaces
    Ping <$> pMessage

pEvery :: Parser Action
pEvery = do 
    string "every" <* manspaces
    Every <$> pInterval <* manspaces <*> pAction

pAfter :: Parser Action
pAfter = do 
    string "after" <* manspaces
    After <$> pInterval <* manspaces <*> pAction

pAction :: Parser Action
pAction = pEvery <|> pAfter <|> pPing <|> pSequence 

pSequence :: Parser Action
pSequence = Sequence <$> pList '(' ')' ',' pAction

pFile = pAction

-- collecting jobs

data Job = Job Integer Integer ( IO () ) 

instance Eq Job where
    Job aid _ _ == Job bid _ _ = aid == bid

instance Show Job where
    show ( Job id repeat _ ) = "Job " ++ show id ++ " " ++ show repeat

data JobBox = JobBox Integer Job deriving ( Show, Eq )
type JobCollector a = RWS ( Integer, Integer ) [ JobBox ] Integer a -- r - ( delay in seconds, repeat in seconds )

instance Ord JobBox where
    JobBox a _ <= JobBox b _ = a <= b

microsecondsOfInterval ( Seconds s ) = 1000000 * s
microsecondsOfInterval ( Minutes m ) = 1000000 * 60 * m
microsecondsOfInterval ( Hours h ) = 1000000 * 60 * 60 * h
microsecondsOfInterval ( Days d ) = 1000000 * 24 * 60 * 60 * d

addJob :: IO () -> JobCollector () 
addJob work = do 
    ( delay, repeat ) <- ask
    id <- R.get
    tell [ JobBox delay ( Job id repeat work ) ]
    R.modify (+1)

notifySendCommand :: ( Show a ) => a -> Maybe Integer -> String
notifySendCommand message limit = 
    "notify-send" ++ 
        ( case limit of 
             Just time -> " -t " ++ show ( time * 1000 ) ++ " "
             Nothing -> "" )
    ++ "'" ++ show message ++ "'"

notifySend :: ( Show a ) => a -> Maybe Integer -> IO ()
notifySend message limit = do 
    spawnCommand $ notifySendCommand message limit
    return ()

randomElement :: ( MonadRandom m ) => [ a ] -> m a
randomElement xs = do 
    let n = length xs in do
        i <- getRandomR ( 0, n - 1 )
        return ( xs !! i )

ping :: Message -> Map String [ String ] -> IO ()
ping ( Message msg ) map = print msg 
ping ( OneOf ( List l ) ) map = randomElement l >>= print
ping ( OneOf ( Var v ) ) map = print $ M.lookup v map 

collect :: Action -> JobCollector ()
collect ( Ping message ) = addJob ( ping message M.empty )

collect ( Every interval action ) = do 
    local ( \ ( delay, repeat ) -> ( delay, microsecondsOfInterval interval ) ) ( collect action )
collect ( After interval action ) = do 
    local ( \ ( delay, repeat ) -> ( delay + microsecondsOfInterval interval, repeat ) ) ( collect action )
collect ( Sequence actions ) = mapM_ collect actions

processAction ( Left error ) = do
    print error
    return []
processAction ( Right action ) = return $ snd $ execRWS ( collect action ) ( 0, -1 ) 0

type Time = Integer
type Scheduler a = StateT ( Time, Heap JobBox ) IO a

getUTCMicros :: IO Integer
getUTCMicros = flip div 1000000 . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime

optionalDelay :: Int -> IO ()
optionalDelay 0 = return ()
optionalDelay n = do 
    --print n 
    threadDelay n

schedule :: Scheduler ()
schedule = do 
    ( startTime, jobs ) <- S.get
    if H.null jobs then return () 
       else do
        let minJob@( JobBox delay job@( Job _ repeat work ) ) = H.minimum jobs in
            do -- lift ( print minJob )
               currentTime <- lift getUTCMicros
               lift . optionalDelay . fromIntegral $ 
                   delay - ( currentTime - startTime )
               currenterTime <- flip (-) startTime <$> lift getUTCMicros
               lift work
               S.modify $ second  
                   ( if repeat > 0 then H.insert ( JobBox ( currentTime + delay - startTime + repeat )job ) . H.deleteMin
                                   else H.deleteMin ) 
               schedule

main :: IO ()
main = do 
    args <- getArgs
    actions <- R.mapM ( parseFromFile pFile ) args
    jobs <- concat <$> R.mapM processAction actions
    currentTime <- getUTCMicros
    runStateT schedule ( currentTime, H.fromList jobs )
    return ()
