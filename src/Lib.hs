{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib (parseHtml, Html(..), ValidHtml(..)) where

import Text.Parsec (parse, try, anyChar, manyTill,eof, noneOf, lookAhead, ParseError, letter, alphaNum, oneOf)
import qualified Text.Parsec.Token as PT
import Text.Parsec.Token      (GenLanguageDef(..))  
import Data.Functor.Identity (Identity)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (char)
import qualified Text.Parsec.Char as Char
import Control.Monad (void, join)
import Control.Applicative (Alternative(..), asum)
import Data.Text (Text, unpack, pack)
import Html


parseHtml :: ValidHtml -> Either ParseError [Html]
parseHtml (ValidHtml content) = parse (htmlParser eof) "" content


config :: forall u . PT.GenTokenParser Text u Identity
config = PT.makeTokenParser htmlDef
        where htmlDef = PT.LanguageDef 
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter htmlDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }


lexeme :: Parser a -> Parser a
lexeme = PT.lexeme config


identifier :: Parser Text
identifier = pack <$> try (some $ noneOf "<> /=\n\r\t\\&$")


integer :: Parser Integer
integer = PT.integer config


stringLiteral :: Parser Text
stringLiteral = pack <$> PT.stringLiteral config


string :: Text -> Parser Text
string x = pack <$> (Char.string . unpack $ x)


float :: Parser Double
float = PT.float config


htmlParser :: Parser () -> Parser [Html]
htmlParser till = manyTill p till where
    p = styleTag <|> scriptTag <|> selfClosingTag <|> manuallyClosingTag <|> justText till


scriptTag :: Parser Html
scriptTag = do
    try . void . lexeme . string $ "<script"
    attrs <- many . lexeme $ attribute
    void . lexeme $ char '>'

    let end = lexeme . string $ "</script>"
    content <- manyTill anyChar (lookAhead . try $ end)
    void end
    return . Script attrs . TextContent . pack $ content


styleTag :: Parser Html
styleTag = do
    try . void . lexeme . string $ "<style"
    attrs <- many . lexeme $ attribute
    void . lexeme $ char '>'

    let end = lexeme . string $ "</style>"
    content <- manyTill anyChar (lookAhead . try $ end)
    void end
    return . Style attrs . TextContent . pack $ content


justText :: Parser () -> Parser Html
justText till' = do
    let till = lookAhead $ (till' <|> (void . char $ '<') <|> eof)
    content <- lexeme $ pack <$> manyTill anyChar till
    return . JustText . TextContent $ content


selfClosingTagName :: Parser Text
selfClosingTagName = asum $ (try . string) <$> selfClosingTags


selfClosingTag :: Parser Html
selfClosingTag = do
    tagName <- try $ do
        void . lexeme $ char '<'
        lexeme selfClosingTagName
    attrs <- many . lexeme $ attribute
    void . lexeme $ (void . string $ "/>") <|> (void . char $ '>')
    return . SelfClosingTag (TagName tagName) $ attrs


manuallyClosingTag :: Parser Html
manuallyClosingTag = do
    (tagName, attrs) <- try $ do
        void . lexeme $ char '<'
        tagName <- lexeme identifier
        attrs <- many . lexeme $ attribute
        void . lexeme $ char '>'
        return (tagName, attrs)

    let closeTag = void . lexeme $ string ("</" <> tagName <> ">")
    let tryCloseTag = lookAhead . try $ closeTag
    body <- manyTill (lexeme . htmlParser $ tryCloseTag) tryCloseTag

    void . lexeme $ closeTag

    return . ManuallyClosingTag (TagName tagName) attrs $ join body


attribute :: Parser Attribute
attribute = do
   name <- lexeme identifier
   value <- lexeme $ lexeme valuedAttr <|> return NoValue
   return $ Attribute (AttributeName name) value
   where valuedAttr = do
            void . char $ '='
            lexeme $ (Str . AttributeTextualValue <$> stringLiteral) <|> try (Dou <$> float) <|> (Integ <$> integer)