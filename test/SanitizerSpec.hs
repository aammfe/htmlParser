{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SanitizerSpec (spec) where



import Test.Hspec (describe, it, shouldBe, Spec )
import Parser (parseHtml)
import Sanitizer (senitize)
import Html



spec :: Spec
spec = do
  describe "Parsing Tests" $ do   
    it "totally remove script" $ do
      let r = fmap senitize . parseHtml . ValidHtml $ "<script>doSomethingEvil();</script>"
      r `shouldBe` Right []
   
    it "totally remove the callbacks" $ do
      let r = fmap senitize . parseHtml . ValidHtml $ "<body onload=alert('XSS')></body>"
      r `shouldBe` Right [ManuallyClosingTag (TagName "body") [] []]
   
    it "remove invalid src, href, dynsrc" $ do
      let r = fmap senitize . parseHtml . ValidHtml $ "<img dynsrc=\"https://plus.unsplash.com/premium_photo\" src=\"javascript:alert('XS');\" href=\"javascript:alert('XS');\">"
      r `shouldBe` Right [SelfClosingTag (TagName "img") [Attribute {attrName = AttributeName "dynsrc", attrValue = Just (AttributeValue Str "https://plus.unsplash.com/premium_photo")}]]