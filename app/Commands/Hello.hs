-- ~\~ language=Haskell filename=app/Commands/Hello.hs
-- ~\~ begin <<README.md|hello>>[0]
module Commands.Hello where

import RIO
import qualified Commands.Common as Common
import Options.Applicative
    ( Parser, long, short, help, metavar, value
    , option, auto, (<**>), helper)

data Args = Args
    { name :: Text
    }

parseArgs :: Parser Args
parseArgs = Args <$> option auto ( long "name" 
                                <> short 'n' 
                                <> metavar "NAME"
                                <> value "World"
                                <> help "Name of sender." )
                <**> helper
-- ~\~ end
-- ~\~ begin <<README.md|hello>>[1]
run :: (HasLogFunc env) => Common.Args Args -> RIO env ()
-- ~\~ end
-- ~\~ begin <<README.md|hello>>[2]
run args = do
    logInfo "Printing message"
    Common.print msg
    where msg = "Hello, " <> name subArgs <> "!\n"
          subArgs = Common.subArgs args
-- ~\~ end
