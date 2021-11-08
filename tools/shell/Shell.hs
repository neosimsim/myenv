{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Shell (read, read', (|$), (|*)) where

import Control.Category ((.))
import Control.Monad ((>>=))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Function (($))
import Data.Text qualified as Strict
import Data.Text.Lazy as Text.Lazy (Text, unpack)
import System.Process.Typed
  ( ProcessConfig,
    byteStringInput,
    readProcessStdout_,
    setStdin,
  )
import Text.Read qualified

read :: forall c. Text.Read.Read c => Text -> c
read = Text.Read.read . unpack

read' :: forall c. Text.Read.Read c => Strict.Text -> c
read' = Text.Read.read . Strict.unpack

infixl 9 |$

-- |
-- >>> "echo foo bar" |$ "tr [a-z] [A-Z]" |* "wc" <&> decodeUtf8
-- "      1       2       8\n"
(|$) ::
  MonadIO m =>
  ProcessConfig stdin stdoutIgnored1 stderr1 ->
  ProcessConfig stdin0 stdoutIgnored2 stderr2 ->
  m ByteString
cmd1 |$ cmd2 = readProcessStdout_ cmd1 >>= \x -> readProcessStdout_ $ setStdin (byteStringInput x) cmd2

infixl 9 |*

-- |
-- >>> "echo foo bar" |$ "tr [a-z] [A-Z]" |* "wc" <&> decodeUtf8
-- "      1       2       8\n"
(|*) ::
  MonadIO m =>
  m ByteString ->
  ProcessConfig stdin0 stdoutIgnored stderr ->
  m ByteString
input |* cmd = input >>= \x -> readProcessStdout_ $ setStdin (byteStringInput x) cmd
