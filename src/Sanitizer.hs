{-# LANGUAGE OverloadedStrings #-}


module Sanitizer where

import Html
    ( AttributeName(..),
      TagName(..),
      AttributeValue(..),
      Attribute(..))
import Data.Set (Set(), member, fromList)
import Data.Text(isPrefixOf,toLower, Text, unpack, pack)
import Network.URI (uriScheme, parseURIReference, URI (..), isAllowedInURI, escapeURIString)
import Codec.Binary.UTF8.String ( encodeString )


import Parser
import Text.Parsec


senitize :: [Tag] -> [Tag]
senitize = map senitizeTag . filter isSafeTag


isSafeTag :: Tag -> Bool
isSafeTag (Script _ _)
    = False
isSafeTag (SelfClosingTag n _)
    = not . isForbiddenTagName $ n
isSafeTag (ManuallyClosingTag n _ _)
    = not . isForbiddenTagName $ n
isSafeTag (JustText _)
    = True
isSafeTag (Style _ _)
    = True

senitizeTag :: Tag -> Tag
senitizeTag (Script _ _)  = error "not possible"
senitizeTag (SelfClosingTag n ats) = SelfClosingTag n (filter isAttributeSafe ats)
senitizeTag (ManuallyClosingTag n ats b) = ManuallyClosingTag n (filter isAttributeSafe ats) (senitize b)
senitizeTag t = t

isForbiddenTagName :: TagName -> Bool
isForbiddenTagName x = member x forbiddenTagNames


forbiddenTagNames :: Set TagName
forbiddenTagNames = fromList $ TagName <$> ["script", "iframe", "embed", "object"]


isAttributeSafe :: Attribute -> Bool
isAttributeSafe (Attribute n (Just v))
  | isCallBackAttribute n = False
  | isUrlAttributes n     = isSafeURI v
  | otherwise             = True
isAttributeSafe _         = True


isUrlAttributes :: AttributeName -> Bool
isUrlAttributes n = member n urlAttributes


urlAttributes :: Set AttributeName
urlAttributes = fromList $ AttributeName <$> ["url", "href", "action"]


isCallBackAttribute :: AttributeName -> Bool
isCallBackAttribute (AttributeName x) = "on" `isPrefixOf` toLower x


safeURISchemes :: Set Text
safeURISchemes = fromList [ "ftp", "http", "https"]


escapeURI :: String -> String
escapeURI = escapeURIString isAllowedInURI . encodeString


isSafeURI :: AttributeValue -> Bool
isSafeURI (AttributeValue _ u) =
  case parseURIReference . escapeURI . unpack $ u of
     Just p  -> null (uriScheme p) ||
                member (toLower . pack . init . uriScheme $ p) safeURISchemes
     Nothing -> False



totallySkipScript :: Either ParseError [Tag]
totallySkipScript = senitize  <$> parseHtml (ValidHtml "<script>doSomethingEvil();</script>")

totallySkipCallBackTags :: Either ParseError [Tag]
totallySkipCallBackTags = senitize <$> parseHtml (ValidHtml "<body onload=alert('XSS')></body>")


i :: Either ParseError [Tag]
i = senitize <$> parseHtml (ValidHtml "<input class=\"site-search-input\" placeholder=\"\" style=\"background: url(\&quot;https://www.google.com/cse/static/images/1x/en/branding.png\&quot;) left 9px top 50% no-repeat rgb(255, 255, 255);\">")


