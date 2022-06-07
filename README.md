---
title: Structuring a larger command-line interface in Haskell
subtitle: a small foray into optparse-applicative
author: Johan Hidding
include-after: Powered by [Entangled](https://entangled.github.io) and [Pandoc](https://pandoc.org)
---

I've been using `optparse-applicative` to do my command-line parsing in Haskell. At some point, an application may outgrow a certain size, where you want to structure the command-line parsing and handling into several modules, such that each module can handle a single sub-command.

Let's build a small greeter program to see how to make this work. We get a project structure as follows:

```
▾ app/
  ▾ Commands/
      Common.hs
      Goodbye.hs
      Hello.hs
    Main.hs
```

Here, module `Commands.Common` contains options that are used by all other sub-commands, and `Commands.Goodbye` and `Commands.Hello` are the two actual sub-commands.

There is a catch-all solution, namely parsing your options to `IO ()` and then `join` the resulting IO action after parsing. Somehow, this solution vibes me the wrong way.

## The `Common` module
Now, we suppose that the options in the `Common` section may also effect code in `Goodbye` and `Hello` in such a way that each sub-command needs to have the common information at hand. This means that `Hello` and `Goodbye` import from `Common` but we need a argument sum-datatype that contains either `Hello` or `Goodbye` specific arguments. Note however, that circular imports are not possible in Haskell. We'll have to abstract out the command-specific bits fromm `Common`.

``` {.haskell file=app/Commands/Common.hs}
module Commands.Common where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as Text

data Args a = Args
    { versionFlag :: Bool
    , verboseFlag :: Bool
    , subArgs :: a }

print :: (MonadIO m) => Text -> m ()
print text = B.hPutStr stdout (Text.encodeUtf8 text)
```

Notice, I'm using `RIO` instead of `Prelude`. Now for the `Hello` part, I'd like to define an additional `Args` type that contains just the arguments that are specific to that sub-command.

## Defining a sub-command

``` {.haskell file=app/Commands/Hello.hs #hello}
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
```

So far, so good. Now, what will by the type of the `run` function in this module? I'm currently settling on 

``` {.haskell #hello}
run :: (HasLogFunc env) => Common.Args Args -> RIO env ()
```

with the following implementation that doesn't really matter.

``` {.haskell #hello}
run args = do
    logInfo "Printing message"
    Common.print msg
    where msg = "Hello, " <> name subArgs <> "!\n"
          subArgs = Common.subArgs args
```

I've considered parsing to a `Common.Args a`, where `a` is the sum-type that should then be defined in `Main`. Then we need a `HasArgs a` data class, for retrieving the desired arguments. The problem here, is that the `HasArgs` class should have a getter method that returns a `Maybe`, since the actual sub-command is not captured by the type system. The current solution has the advantage of being simple enough.

A very similar afair in `Goodbye`:

``` {.haskell file=app/Commands/Goodbye.hs}
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
```

## The `Main` module
The `Main` module contains the plumming for setting up the `RIO` environment, and then dispatches to the correct sub-command.

``` {.haskell file=app/Main.hs #main}
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
```

The environment structure will only contain the logger function here.

``` {.haskell #main}
data App = App 
    { logFunc' :: LogFunc
    }

instance HasLogFunc App where
    logFuncL = lens logFunc' (\x y -> x { logFunc' = y })
```

To dispatch to the correct sub-command runner, we replace the `Common.Args SubCommand` record with a `Common.Args Hello.Args` (or `Goodbye` equivalent). This is done using record update syntax.

``` {.haskell #main}
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
```

The remaining `main` function, parses arguments and passes those on to the `run` function.

``` {.haskell #main}
main :: IO ()
main = run =<< execParser opts
    where opts = info (parseArgs <**> helper)
                    ( fullDesc
                   <> progDesc "Print a greeting."
                   <> header "hello - a test for optparse-applicative, with sub-commands" )
```



