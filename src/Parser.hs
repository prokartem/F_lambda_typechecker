module Parser where

import Control.Applicative((<*))
import Text.Parsec ( alphaNum, char, letter, oneOf, (<|>), Stream, ParsecT, (<?>), eof, parse, ParseError )
import Text.Parsec.Token
    ( GenLanguageDef(commentStart, commentEnd, identStart, identLetter,
                     opStart, opLetter, reservedOpNames, reservedNames),
      makeTokenParser,
      GenTokenParser(TokenParser, parens, identifier, reservedOp,
                     reserved, semiSep1, whiteSpace) )
import Text.Parsec.Language ( emptyDef )
import qualified GHC.Exts as GHC.Types
import Data.Functor.Identity ( Identity )
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft, AssocRight, AssocNone), Operator (Prefix, Infix))
import Text.Parsec.String (Parser)

-- Syntax in ADT --

data Type =
    Type { tName :: String }
    | Arrow { ty1 :: Type, ty2 :: Type }
    | Forall {varName :: String, body :: Type}
    deriving (Eq, Show)

data Term =
    LVar { v :: String }
    | LAbs { absV :: String, ty :: Type, term :: Term }
    | LApp { te1 :: Term, te2 :: Term }
    | LTAbs { absT :: String, te :: Term }
    | LTApp { tyAbs :: Term, ttype :: Type }
    -- new kind of terms. for parsing purposes only
    | TermType Type
    deriving Show

-- Language --

def :: GenLanguageDef String st Identity
def = emptyDef{ commentStart = "{-"
              , commentEnd   = "-}"
              , identStart   = letter
              , identLetter  = alphaNum
              , opStart      = oneOf "-$@"
              , opLetter     = oneOf ">"
              , reservedOpNames = ["->", "$"]
              , reservedNames   = ["lambda", ":", ".",
                                   "Lambda", "Forall", "[", "]"]
            }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp  = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

-- Parsers --

typeParser :: Parser Type
typeParser = buildExpressionParser table typeExpr
    where
        table = [ [Infix (m_reserved "->" >> return Arrow) AssocRight ] ]
        typeExpr =
            -- type Forall ?
            do { m_reserved "Forall"
               ; x <- m_identifier
               ; m_reserved "."
               ; Forall x <$> typeParser
               }
            -- type Arrow ?
            <|> m_parens typeParser
            -- simple Type 
            <|> fmap Type m_identifier

termParser :: Parser Term
termParser = buildExpressionParser table termExpr
    where
        -- this function decides: is it Application or Type-Application
        typeOrRegularApplication :: Term -> Term -> Term
        typeOrRegularApplication term1 term2 = case term2 of
            TermType ttype -> LTApp term1 ttype
            _              -> LApp term1 term2

        table = [ [Infix (m_reserved "$" >> return typeOrRegularApplication) AssocLeft ] ]
        termExpr =
            -- lambda-type-abstraction ?
            do { m_reserved "Lambda"
                -- parse name of variable
                ; x <- m_identifier
                ; m_reserved "."
                -- create LAbs and parse lambda-term
                ; LTAbs x <$> termParser
                }
            -- lambda-abstraction ?
            <|> do { m_reserved "lambda"
                    -- parse name of variable
                    ; x <- m_identifier
                    ; m_reserved ":"
                    -- parse type of variable
                    ; ty <- typeParser
                    ; m_reserved "."
                    -- create LAbs and parse lambda-term
                    ; LAbs x ty <$> termParser
                    }
            -- term-type ?
            <|> do { m_reserved "["
                    ; ttype <- typeParser
                    ; m_reserved "]"
                    ; return $ TermType ttype
                    }
            -- lambda-term ?
            <|> m_parens termParser
            -- lambda-variable
            <|> fmap LVar m_identifier

lambdaParser :: Parser [Term]
lambdaParser = m_whiteSpace >> m_semiSep1 termParser <* eof

