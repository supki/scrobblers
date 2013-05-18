{-# LANGUAGE OverloadedStrings #-}

import Scrobbler


main :: IO ()
main = scrobbler Credentials
  { apiKey     = "__YOUR_API_KEY__"
  , sessionKey = "__YOUR_SESSION_KEY__"
  , secret     = "__YOUR_SECRET__"
  }
