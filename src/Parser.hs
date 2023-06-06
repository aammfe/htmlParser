{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Parser (parseHtml, Tag(..), ValidHtml(..)) where

import Text.Parsec (parse, try, anyChar, manyTill,eof, noneOf, lookAhead, ParseError, letter, alphaNum, oneOf, )--parserTrace)
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


parseHtml :: ValidHtml -> Either ParseError [Tag]
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


string :: Text -> Parser Text
string x = pack <$> (Char.string . unpack $ x)


stringLiteral :: Parser Text
stringLiteral = pack <$> PT.stringLiteral config


htmlParser :: Parser () -> Parser [Tag]
htmlParser till = manyTill p till where
    p = styleTag <|> scriptTag <|> selfClosingTag <|> manuallyClosingTag <|> justText till


scriptTag :: Parser Tag
scriptTag = do
  --  parserTrace "scriptTag"
    try . void . lexeme . string $ "<script"
    attrs <- many . lexeme $ attribute
    void . lexeme $ char '>'

    let end = lexeme . string $ "</script>"
    content <- manyTill anyChar (lookAhead . try $ end)
    void end
    return . Script attrs . TextContent . pack $ content


styleTag :: Parser Tag
styleTag = do
  --  parserTrace "styleTag"
    try . void . lexeme . string $ "<style"
    attrs <- many . lexeme $ attribute
    void . lexeme $ char '>'

    let end = lexeme . string $ "</style>"
    content <- manyTill anyChar (lookAhead . try $ end)
    void end
    return . Style attrs . TextContent . pack $ content


justText :: Parser () -> Parser Tag
justText till' = do
  --  parserTrace "justText"
    let till = lookAhead (till' <|> (void . char $ '<') <|> eof)
    content <- lexeme $ pack <$> manyTill anyChar till
    return . JustText . TextContent $ content


selfClosingTagName :: Parser Text
selfClosingTagName = asum $ try . string <$> selfClosingTags


selfClosingTag :: Parser Tag
selfClosingTag = do
 --   parserTrace "selfClosingTag"
    tagName <- try $ do
        void . lexeme $ char '<'
        lexeme selfClosingTagName
    attrs <- many . lexeme $ attribute
    void . lexeme $ (void . string $ "/>") <|> (void . char $ '>')
    return . SelfClosingTag (TagName tagName) $ attrs


manuallyClosingTag :: Parser Tag
manuallyClosingTag = do
 --   parserTrace "manuallyClosingTag"
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
  -- parserTrace "attribute"
   name <- lexeme identifier
   value <- lexeme $ lexeme valuedAttr <|> return Nothing
   return $ Attribute (AttributeName name) value
   where valuedAttr = do
            void . char $ '='
            lexeme $ lit <|> anyValueOtherThenStrLit
         lit = Just . AttributeValue Str <$> stringLiteral
         anyValueOtherThenStrLit = Just . AttributeValue LiteralStr . pack <$> (manyTill anyChar . try . lookAhead .oneOf $ " >")