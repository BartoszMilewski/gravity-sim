module JsonOpt where
import Data.Aeson.TH

-- Options for Json must be defined in a separate module

options0610 :: Options
options0610 = defaultOptions
          { sumEncoding           = ObjectWithSingleField
          , allNullaryToStringTag = False
          }

