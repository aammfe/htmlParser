{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}


module Html(Tag(..), ValidHtml(..), TagName(..),
            TextContent(..), AttributeName(..), AttributeValue(..),
            Attribute(..), AttributeValueType(..), selfClosingTags ) where

import Data.Text (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import           Test.QuickCheck.Gen       (Gen, elements, resize, suchThat)
import           Control.Monad             (replicateM)
import           Data.String               (IsString (fromString))


newtype ValidHtml = ValidHtml Text
    deriving (Show, Eq)


newtype TagName = TagName Text
    deriving (Eq,Show, Ord)

newtype TextContent = TextContent Text
    deriving (Show, Eq)

instance Arbitrary TextContent where
    arbitrary = TextContent <$> genText

data Tag = SelfClosingTag TagName [Attribute]
          | ManuallyClosingTag TagName [Attribute] [Tag]
          | JustText TextContent
          | Script [Attribute] TextContent
          | Style [Attribute] TextContent
          | Comment TextContent
    deriving (Show, Eq)


instance Arbitrary Tag where
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



newtype AttributeName = AttributeName Text
    deriving (Show, Eq, Ord)


instance Arbitrary AttributeName where
    arbitrary = AttributeName <$> genText


data AttributeValueType = LiteralStr | Str 
    deriving (Show,Eq)

instance Arbitrary AttributeValueType where
    arbitrary = elements [LiteralStr, Str]

data AttributeValue = AttributeValue AttributeValueType Text
    deriving (Show, Eq)


instance Arbitrary AttributeValue where
    arbitrary = AttributeValue <$> arbitrary <*> genText


data Attribute = Attribute { attrName :: AttributeName, attrValue ::  Maybe AttributeValue }
    deriving (Show, Eq)


instance Arbitrary Attribute where
    arbitrary = Attribute <$> arbitrary <*> arbitrary


genText :: Gen Text
genText = do
    l <- resize 10 $ arbitrary `suchThat` (> 0)
    let g = elements $ ['A'.. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']
    fromString <$> replicateM l g


selfClosingTags :: [Text]
selfClosingTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]