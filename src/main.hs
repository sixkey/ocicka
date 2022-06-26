import Text.Parsec as P
import Text.Parsec.String as PS
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
import Data.Functor
import Data.Map as M
import Data.List
import System.Process
import Control.Monad.Random
import Options.Applicative as O

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

data ActionType = Run | Ping deriving Show

data Action = Every Interval Action  
            | After Interval Action 
            | Atom ActionType Message
            | Sequence [ Action ] 
            | Define String [ String ] 
            deriving Show 

-- generic parsing

pPosInt:: PS.Parser Integer
pPosInt = read <$> P.many1 digit 

manspaces :: PS.Parser ()
manspaces = do 
    P.many1 space
    return ()

inbetween :: Char -> Char -> PS.Parser a -> PS.Parser a
inbetween a b = between ( char a ) ( char b )

pList :: Char -> Char -> Char -> PS.Parser a -> PS.Parser [ a ]
pList start end del element = 
        inbetween start end  
                ( (:) 
                  <$> ( spaces *> element <* spaces ) 
                  <*> P.many ( char del *> spaces *> element <* spaces ) ) 

-- ocicka parsing

constructorOfSuffix :: Char -> Integer -> Interval
constructorOfSuffix 's' = Seconds
constructorOfSuffix 'm' = Minutes
constructorOfSuffix 'h' = Hours
constructorOfSuffix 'D' = Days
constructorOfSuffix 'W' = Weeks
constructorOfSuffix 'M' = Months

pInterval :: PS.Parser Interval
pInterval = do 
    number <- pPosInt
    c <- oneOf "smhDWM"
    return $ constructorOfSuffix c number


pMessageLiteral :: PS.Parser String
pMessageLiteral = inbetween '"' '"' $ P.many ( noneOf "\"" ) 

pTextMessage :: PS.Parser Message
pTextMessage = Message <$> pMessageLiteral

pMessageListLiteral :: PS.Parser MessageList
pMessageListLiteral = List <$> pList '[' ']' ',' pMessageLiteral

pMessageListVariable :: PS.Parser MessageList
pMessageListVariable = Var <$> ( (:) <$> letter <*> P.many1 alphaNum )

pMessageList :: PS.Parser MessageList
pMessageList = pMessageListLiteral P.<|> pMessageListVariable 

pOneOf :: PS.Parser Message
pOneOf = do  
    string "oneof" 
    manspaces
    OneOf <$> pMessageList

pMessage :: PS.Parser Message
pMessage = pOneOf P.<|> pTextMessage

pAtom :: PS.Parser Action
pAtom = do 
    actionType <- ( string "ping" >> return Ping ) P.<|> ( string "run" >> return Run )
    manspaces
    Atom actionType <$> pMessage

pEvery :: PS.Parser Action
pEvery = do 
    string "every" <* manspaces
    Every <$> pInterval <* manspaces <*> pAction

pAfter :: PS.Parser Action
pAfter = do 
    string "after" <* manspaces
    After <$> pInterval <* manspaces <*> pAction

pAction :: PS.Parser Action
pAction = pEvery P.<|> pAfter P.<|> pAtom P.<|> pSequence 

pSequence :: PS.Parser Action
pSequence = Sequence <$> pList '(' ')' ',' pAction

pMessageBlock :: PS.Parser Action
pMessageBlock = do 
    string "--"
    manspaces
    name <- P.many1 letter    
    char '\n' 
    lines <- P.many ( (:) <$> noneOf "-" <*> P.many ( noneOf "\n" ) <* char '\n' )
    string "--"
    return $ Define name lines

pFile = spaces *> P.many ( ( pAction P.<|> pMessageBlock ) <* spaces ) <* eof 

-- collecting jobs

data Job = Job Integer Integer ( IO () ) 

instance Eq Job where
    Job aid _ _ == Job bid _ _ = aid == bid

instance Show Job where
    show ( Job id repeat _ ) = "Job " ++ show id ++ " " ++ show repeat

data JobBox = JobBox Integer Job deriving ( Show, Eq )
type JobCollector a = RWS ( Integer, Integer ) [ JobBox ] ( Integer, Map String [ String ] ) a -- r - ( delay in seconds, repeat in seconds )

instance Ord JobBox where
    JobBox a _ <= JobBox b _ = a <= b

microsecondsOfInterval ( Seconds s ) = 1000000 * s
microsecondsOfInterval ( Minutes m ) = 1000000 * 60 * m
microsecondsOfInterval ( Hours h ) = 1000000 * 60 * 60 * h
microsecondsOfInterval ( Days d ) = 1000000 * 24 * 60 * 60 * d

addJob :: IO () -> JobCollector () 
addJob work = do 
    ( delay, repeat ) <- ask
    ( id, _ ) <- R.get
    tell [ JobBox delay ( Job id repeat work ) ]
    R.modify $ first (+1)

notifySendCommand :: ( Show a ) => a -> Maybe Integer -> String
notifySendCommand message limit = 
    "notify-send" ++ 
        ( case limit of 
             Just time -> " -t " ++ show ( time * 1000 ) ++ " "
             Nothing -> "" )
    ++ "'" ++ show message ++ "'"

notifySend :: ( Show a ) => Maybe Integer -> a -> IO ()
notifySend limit message = do 
    spawnCommand $ notifySendCommand message limit
    return ()

randomElement :: ( MonadRandom m ) => [ a ] -> m a
randomElement xs = do 
    let n = length xs in do
        i <- getRandomR ( 0, n - 1 )
        return ( xs !! i )

getMessage :: Message -> Map String [ String ] -> IO String
getMessage msg map = 
    case msg of 
        Message msg -> return msg 
        OneOf ( List l ) -> randomElement l 
        OneOf ( Var v ) -> randomElement ( fromJust $ M.lookup v map ) 

newtype PlainString = PlainString String

instance Show PlainString where  
    show ( PlainString a ) = a

getAction :: ActionType -> String -> IO ()
getAction Ping s = notifySend ( Just 10 ) ( PlainString s )
getAction Run s = void . runCommand $ s

collect :: Action -> JobCollector ()
collect ( Atom actionType message ) = do 
    map <- snd <$> R.get 
    addJob ( getMessage message map >>= getAction actionType )
collect ( Every interval action ) = 
    local ( second . const $ microsecondsOfInterval interval ) 
          ( collect action )
collect ( After interval action ) = 
    local ( first ( + microsecondsOfInterval interval ) ) 
          ( collect action )
collect ( Sequence actions ) = mapM_ collect actions
collect ( Define name messages ) = R.modify $ second ( M.insert name messages )

processAction ( Left error ) = print error >> return []
processAction ( Right actions ) = return . snd $ execRWS ( R.mapM collect actions ) 
                                                         ( 0, -1 ) 
                                                         ( 0, M.empty )

type Time = Integer
type Scheduler a = StateT ( Time, Heap JobBox ) IO a

getUTCMicros :: IO Integer
getUTCMicros = flip div 1000000 . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime

optionalDelay :: Int -> IO ()
optionalDelay n  
    | n <= 0 = return ()
    | otherwise = threadDelay n

schedule :: Scheduler ()
schedule = do 
    ( startTime, jobs ) <- S.get
    if H.null jobs 
       then return () 
       else do
        let minJob@( JobBox delay job@( Job _ repeat work ) ) = H.minimum jobs in
            do currentTime <- lift getUTCMicros
               lift . optionalDelay . fromIntegral $ 
                   delay - ( currentTime - startTime )
               currenterTime <- lift getUTCMicros
               lift work
               S.modify $ second  
                   ( if repeat > 0 
                        then H.insert ( JobBox ( delay + repeat ) job ) . H.deleteMin
                        else H.deleteMin ) 
               schedule

aOffset :: O.Parser Integer
aOffset = O.option auto 
    ( long "startoffset" 
   <> metavar "START_OFFSET" 
   <> value 0
   <> help "starting offset in seconds" )

aFiles :: O.Parser [ String ]
aFiles = O.some ( argument str ( metavar "FILES..." ) )

data Options = Options {
    offset :: Integer, 
    files :: [ String ]
}

aOpts :: O.Parser Options
aOpts = Options <$> aOffset <*> aFiles

options :: O.ParserInfo Options
options = info ( aOpts <**> helper )
    ( fullDesc 
   <> progDesc "Run ocicka notification scheduler"
   <> header "what" )

main :: IO ()
main = do
    args <- O.execParser options
    actions <- R.mapM ( parseFromFile pFile ) ( files args )
    jobs <- concat <$> R.mapM processAction actions
    currentTime <- getUTCMicros
    runStateT schedule ( currentTime - offset args * 1000000, H.fromList jobs )
    return ()
