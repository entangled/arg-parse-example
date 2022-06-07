-- ~\~ language=Haskell filename=app/Commands/Goodbye.hs
-- ~\~ begin <<README.md|app/Commands/Goodbye.hs>>[0]
module Commands.Goodbye where

import RIO
import qualified Commands.Common as Common
import Options.Applicative
    ( Parser, long, short, help, str, metavar
    , argument, strOption, (<**>), helper )

data Args = Args
    { name :: Text
    , from :: Maybe Text
    }

parseArgs :: Parser Args
parseArgs = Args <$> argument str ( metavar "NAME" <> help "Name of sender." )
                 <*> optional ( strOption ( long "from"
                                         <> short 'f' 
                                         <> metavar "FROM"
                                         <> help "Identity of sender." ) )
                <**> helper

run :: (HasLogFunc env) => Common.Args Args -> RIO env ()
run args = do
    logInfo "Printing message"
    Common.print msg
    where msg = "Goodbye " <> name subArgs
             <> maybe "" (", from " <>) (from subArgs) <> "!\n"
          subArgs = Common.subArgs args
-- ~\~ end
