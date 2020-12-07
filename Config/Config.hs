module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail.Types


config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
