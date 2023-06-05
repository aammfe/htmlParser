{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}


module Html(Html(..), ValidHtml(..), TagName(..), 
            TextContent(..), AttributeName(..), AttributeTextualValue(..), 
            Attribute(..), AttributeValue(..), selfClosingTags ) where

import Data.Text (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import           Test.QuickCheck.Gen       (Gen, elements, resize, suchThat)
import           Control.Monad             (replicateM)
import           Data.String               (IsString (fromString))


data ValidHtml = ValidHtml Text
    deriving (Show, Eq)


data TagName = TagName Text
    deriving (Eq,Show)

data TextContent = TextContent Text
    deriving (Show, Eq)

instance Arbitrary TextContent where
    arbitrary = TextContent <$> genText

data Html = SelfClosingTag TagName [Attribute]
          | ManuallyClosingTag TagName [Attribute] [Html]
          | JustText TextContent
          | Script [Attribute] TextContent
          | Style [Attribute] TextContent
    deriving (Show, Eq)


instance Arbitrary Html where
    arbitrary = do 
        x :: Int <- elements [1,2,3,4,5]
        case x of
            1 -> SelfClosingTag <$> (TagName <$> elements selfClosingTags) <*> arbitrary
            2 -> ManuallyClosingTag <$> (TagName <$> elements manuallyClosingTags) <*> arbitrary <*> arbitrary
            3 -> JustText <$> arbitrary
            4 -> Script <$> arbitrary <*> arbitrary
            _ -> Style <$> arbitrary <*> arbitrary

manuallyClosingTags :: [Text]
manuallyClosingTags = ["p" , "lu", "li", "dt", "dd", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote", "pre", "fieldset", "legend", "textarea", "figure", "figcaption", "details", "summary", "menuitem", "optgroup", "option", "colgroup"]



data AttributeName = AttributeName Text
    deriving (Show, Eq)


instance Arbitrary AttributeName where
    arbitrary = AttributeName <$> genText


data AttributeTextualValue = AttributeTextualValue Text
    deriving (Show, Eq)


instance Arbitrary AttributeTextualValue where
    arbitrary = AttributeTextualValue <$> genText


data Attribute = Attribute { attrName :: AttributeName, attrValue :: AttributeValue }
    deriving (Show, Eq)


instance Arbitrary Attribute where
    arbitrary = Attribute <$> arbitrary <*> arbitrary


data AttributeValue = Str AttributeTextualValue | Dou Double | Integ Integer | NoValue
    deriving (Show,Eq)


instance Arbitrary AttributeValue where
    arbitrary = do 
        x :: Int <- elements [1,2,3,4]
        case x of
            1 -> Str <$> arbitrary
            2 -> Dou <$> arbitrary
            3 -> Integ <$> arbitrary
            _ -> return NoValue


genText :: Gen Text
genText = do
    l <- resize 10 $ arbitrary `suchThat` (\i -> i > 0)
    let g = elements $ ['A'.. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']
    fromString <$> replicateM l g


selfClosingTags :: [Text]
selfClosingTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]