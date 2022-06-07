-- ~\~ language=Haskell filename=app/Main.hs
-- ~\~ begin <<README.md|main>>[0]
module Main where

import RIO
import Options.Applicative
    ( Parser, switch, long, short, help, subparser
    , command, info, progDesc, execParser
    , (<**>), helper, fullDesc, header )
import qualified Commands.Common as Common
import qualified Commands.Hello as Hello
import qualified Commands.Goodbye as Goodbye

data SubCommand
    = CmdHello Hello.Args
    | CmdGoodbye Goodbye.Args

parseArgs :: Parser (Common.Args SubCommand)
parseArgs = Common.Args
    <$> switch (long "version" <> short 'v' <> help "Show version.")
    <*> switch (long "verbose" <> short 'V' <> help "Be verbose.")
    <*> subparser
        (  command "hello"   (info (CmdHello <$> Hello.parseArgs)
                                   (progDesc "Say Hello"))
        <> command "goodbye" (info (CmdGoodbye <$> Goodbye.parseArgs)
                                   (progDesc "Say Goodbye"))
        )
-- ~\~ end
-- ~\~ begin <<README.md|main>>[1]
data App = App 
    { logFunc' :: LogFunc
    }

instance HasLogFunc App where
    logFuncL = lens logFunc' (\x y -> x { logFunc' = y })
-- ~\~ end
-- ~\~ begin <<README.md|main>>[2]
run :: Common.Args SubCommand -> IO ()
run args = do
    logOptions <- setLogUseTime True
              <$> logOptionsHandle stderr (Common.verboseFlag args)
    withLogFunc logOptions $ \lf -> do
        runRIO (App lf) $ do
            logDebug "Running sub-command"
            case Common.subArgs args of
                CmdHello   x -> Hello.run   (args { Common.subArgs = x })
                CmdGoodbye x -> Goodbye.run (args { Common.subArgs = x })
            logDebug "Until next time"
-- ~\~ end
-- ~\~ begin <<README.md|main>>[3]
main :: IO ()
main = run =<< execParser opts
    where opts = info (parseArgs <**> helper)
                    ( fullDesc
                   <> progDesc "Print a greeting."
                   <> header "hello - a test for optparse-applicative, with sub-commands" )
-- ~\~ end
