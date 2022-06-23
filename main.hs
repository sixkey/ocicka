import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Cron.Schedule
import Control.Monad.RWS
import System.Cron.Types
import Data.Maybe

data Interval = Hours Int 
              | Minutes Int 
              | Days Int 
              | Weeks Int 
              | Months Int deriving Show

data Action = Every Interval Action  
            | After Interval Action 
            | Ping String
            | Sequence [ Action ] 
            deriving Show 

pPosInt:: Parser Int
pPosInt = read <$> many1 digit 

manspaces :: Parser ()
manspaces = do 
    many1 space
    return ()

pInterval :: Parser Interval
pInterval = do 
    number <- pPosInt
    c <- oneOf "hmDWM"
    return $ 
        if c == 'h' then Hours number
        else if c == 'm' then Minutes number
        else if c == 'D' then Days number
        else if c == 'W' then Weeks number
        else Months number

pPing :: Parser Action
pPing = do 
    string "ping"
    manspaces
    char '\"'
    string <- many ( noneOf "\"" )
    char '\"'
    return $ Ping string 

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
pSequence = do 
    Sequence <$> 
        between ( char '(' ) ( char ')' ) 
                ( (:) 
                  <$> ( spaces *> pAction <* spaces ) 
                  <*> many ( char ',' *> spaces *> pAction <* spaces ) ) 

type JobCollector a = RWS CronSchedule [ Job ] () a

everyUpdateField n field =
    let ns = case field of 
                Field baseField -> 
                    StepField' . fromJust $ mkStepField baseField n
                ListField _ -> undefined
                StepField' sf -> StepField' . fromJust $ mkStepField ( sfField sf ) n in 
    ns

everyUpdate ( Minutes m ) schedule = 
    let ns = everyUpdateField m ( minuteSpec . minute $ schedule ) in 
    schedule { minute = fromJust $ mkMinuteSpec ns }
everyUpdate ( Hours m ) schedule = 
    let ns = everyUpdateField m ( hourSpec . hour $ schedule ) in 
    schedule { hour = fromJust $ mkHourSpec ns }
everyUpdate ( Days m ) schedule = 
    let ns = everyUpdateField m ( dayOfWeekSpec . dayOfWeek $ schedule ) in 
    schedule { dayOfWeek = fromJust $ mkDayOfWeekSpec ns }
everyUpdate ( Months m ) schedule = 
    let ns = everyUpdateField m ( monthSpec . month $ schedule ) in 
    schedule { month = fromJust $ mkMonthSpec ns }

afterUpdate interval schedule = undefined

collect :: Action -> JobCollector ()
collect ( Sequence actions ) = mapM_ collect actions
collect ( Ping message ) = do schedule <- ask 
                              tell [ Job schedule ( print message ) ]
collect ( After interval action ) = local ( afterUpdate interval ) $ collect action
collect ( Every interval action ) = do schedule <- ask
                                       local ( everyUpdate interval ) $ collect action

pFile = pAction

processAction ( Left _ ) = return ()
processAction ( Right action ) = 
    let collector = collect action in
    let ( rs, rw ) = execRWS collector everyMinute () in 
    print rw

main :: IO ()
main = do 
    args <- getArgs
    clauses <- mapM ( parseFromFile pFile ) args
    mapM_ processAction clauses
