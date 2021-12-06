{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Shell
  ( readLazy,
    readStrict,
    writeUtf8TextLazy,
    writeUtf8TextStrict,
    (|*),
    (|$),
  )
where

import Control.Category ((.))
import Control.Monad ((>>=))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as ByteString.Strict (putStr)
import Data.ByteString.Lazy qualified as ByteString.Lazy (ByteString, putStr)
import Data.Function (($))
import Data.Text qualified as Text.Strict (Text, unpack)
import Data.Text.Encoding as Text.Strict (encodeUtf8)
import Data.Text.Lazy qualified as Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding as Text.Lazy (encodeUtf8)
import System.IO (IO)
import System.Process.Typed
  ( ProcessConfig,
    byteStringInput,
    readProcessStdout_,
    setStdin,
  )
import Text.Read qualified

readLazy :: forall c. Text.Read.Read c => Text.Lazy.Text -> c
readLazy = Text.Read.read . Text.Lazy.unpack

readStrict :: forall c. Text.Read.Read c => Text.Strict.Text -> c
readStrict = Text.Read.read . Text.Strict.unpack

writeUtf8TextLazy :: Text.Lazy.Text -> IO ()
writeUtf8TextLazy = ByteString.Lazy.putStr . Text.Lazy.encodeUtf8

writeUtf8TextStrict :: Text.Strict.Text -> IO ()
writeUtf8TextStrict = ByteString.Strict.putStr . Text.Strict.encodeUtf8

infixl 9 |$

-- |
-- >>> "echo foo bar" |$ "tr [a-z] [A-Z]" |* "wc" <&> decodeUtf8
-- "      1       2       8\n"
(|$) ::
  MonadIO m =>
  ProcessConfig stdin stdoutIgnored1 stderr1 ->
  ProcessConfig stdin0 stdoutIgnored2 stderr2 ->
  m ByteString.Lazy.ByteString
cmd1 |$ cmd2 = readProcessStdout_ cmd1 >>= \x -> readProcessStdout_ $ setStdin (byteStringInput x) cmd2

infixl 9 |*

-- |
-- >>> "echo foo bar" |$ "tr [a-z] [A-Z]" |* "wc" <&> decodeUtf8
-- "      1       2       8\n"
(|*) ::
  MonadIO m =>
  m ByteString.Lazy.ByteString ->
  ProcessConfig stdin0 stdoutIgnored stderr ->
  m ByteString.Lazy.ByteString
input |* cmd = input >>= \x -> readProcessStdout_ $ setStdin (byteStringInput x) cmd
