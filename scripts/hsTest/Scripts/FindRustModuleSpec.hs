{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scripts.FindRustModuleSpec (spec) where

import Test.Hspec
import Scripts.FindRustModule

spec :: Spec
spec = do
  describe "moduleFiles" $ do
    it "works in module definitions" $ do
      moduleFiles "signals" `shouldContain` ["signals.rs"]
      moduleFiles "signals" `shouldContain` ["signals/mod.rs"]
      moduleFiles "signals" `shouldContain` ["*/signals.rs"]
      moduleFiles "signals" `shouldContain` ["*/signals/mod.rs"]
    it "works on absolute module names" $ do
      moduleFiles "controller_api::battery::signals" `shouldContain` ["battery/signals.rs"]
      moduleFiles "controller_api::battery::signals" `shouldContain` ["battery/signals/mod.rs"]
    it "works on self" $ do
      moduleFiles "self::battery::signals" `shouldContain` ["battery/signals.rs"]
      moduleFiles "self::battery::signals" `shouldContain` ["battery/signals/mod.rs"]
    it "works on super" $ do
      moduleFiles "super::battery::signals" `shouldContain` ["battery/signals.rs"]
      moduleFiles "super::battery::signals" `shouldContain` ["battery/signals/mod.rs"]
      moduleFiles "super::battery::signals" `shouldContain` ["../battery/signals.rs"]
      moduleFiles "super::battery::signals" `shouldContain` ["../battery/signals/mod.rs"]
    it "works on crate" $ do
      moduleFiles "crate::battery::signals" `shouldContain` ["battery/signals.rs"]
      moduleFiles "crate::battery::signals" `shouldContain` ["battery/signals/mod.rs"]
    it "works on relative module names" $ do
      moduleFiles "battery::signals" `shouldBe` moduleFiles "self::battery::signals"
