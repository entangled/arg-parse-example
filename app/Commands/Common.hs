-- ~\~ language=Haskell filename=app/Commands/Common.hs
-- ~\~ begin <<README.md|app/Commands/Common.hs>>[0]
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
-- ~\~ end
