module ElmFormat.AST.BinaryOperatorPrecedence (parseElm0_19, parsePrecedence, Tree(..), Precedence(..), Associativity(..)) where

import ElmFormat.AST.Shared
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)


data Tree op e
    = Leaf e
    | Branch op (Tree op e) (Tree op e)
    deriving (Eq, Show, Functor)

data Precedence
    = Precedence
        { precedence :: Int
        , associativity :: Associativity
        }

data Associativity
    = LeftAssociate
    | RightAssociate
    | NonAssociate
    deriving (Eq)


elm0_19 :: Ord ns => Map (Ref ns) Precedence
elm0_19 =
    Map.fromList $ fmap (\(op, p, a) -> (OpRef $ SymbolIdentifier op, Precedence p a))
        -- From https://github.com/elm/core/blob/1.0.5/src/Basics.elm#L68-L89
        [ ( "<|", 0, RightAssociate )
        , ( "|>", 0, LeftAssociate )
        , ( "||", 2, RightAssociate )
        , ( "&&", 3, RightAssociate )
        , ( "==", 4, NonAssociate )
        , ( "/=", 4, NonAssociate )
        , ( "<", 4, NonAssociate )
        , ( ">", 4, NonAssociate )
        , ( "<=", 4, NonAssociate )
        , ( ">=", 4, NonAssociate )
        , ( "++", 5, RightAssociate )
        , ( "+", 6, LeftAssociate )
        , ( "-", 6, LeftAssociate )
        , ( "*", 7, LeftAssociate )
        , ( "/", 7, LeftAssociate )
        , ( "//", 7, LeftAssociate )
        , ( "^", 8, RightAssociate )
        , ( "<<", 9, LeftAssociate )
        , ( ">>", 9, RightAssociate )

        -- From https://github.com/elm/core/blob/1.0.5/src/List.elm#L41
        , ( "::", 5, RightAssociate )

        -- From https://github.com/elm/url/blob/1.0.0/src/Url/Parser.elm#L46-L50
        , ( "</>", 7, RightAssociate )
        , ( "<?>", 8, LeftAssociate )

        -- From https://github.com/elm/parser/blob/1.1.0/src/Parser.elm#L55-L59
        , ( "|=", 5, LeftAssociate )
        , ( "|.", 6, LeftAssociate )
        ]

parseElm0_19 :: (Ord ns, Show ns) => e -> List (Ref ns, e) -> Either Text (Tree (Ref ns) e)
parseElm0_19 = parsePrecedence elm0_19


parsePrecedence :: (Ord op, Show op) => Map op Precedence -> e -> List (op, e) -> Either Text (Tree op e)
parsePrecedence precedenceMap first = parsePrecedence' precedenceMap (Leaf first) []

-- https://en.wikipedia.org/wiki/Shunting-yard_algorithm
parsePrecedence' :: (Ord op, Show op) => Map op Precedence -> Tree op e -> List (Tree op e, op) -> List (op, e) -> Either Text (Tree op e)
parsePrecedence' precedenceMap b ((a, op) : rest) [] =
    parsePrecedence' precedenceMap (Branch op a b) rest []
parsePrecedence' _ last [] [] = Right last
parsePrecedence' precedenceMap prev stack ((op, next) : rest) =
    let
        prec o = fromMaybe (error ("operator is not defined: " <> show o)) $ Map.lookup o precedenceMap
    in
    case stack of
        (a, opPrev) : restStack
          | precedence (prec opPrev) > precedence (prec op)
            || (precedence (prec opPrev) == precedence (prec op)
                && associativity (prec op) == LeftAssociate
                && associativity (prec opPrev) == LeftAssociate)
          ->
            parsePrecedence' precedenceMap
                (Branch opPrev a prev)
                restStack
                ((op, next) : rest)

        (_, opPrev) : _
          | precedence (prec opPrev) == precedence (prec op)
            && (associativity (prec op) == NonAssociate
                || associativity (prec opPrev) /= associativity (prec op))
          ->
            Left "conflicting associativity"

        _ ->
            parsePrecedence' precedenceMap
                (Leaf next)
                ((prev, op) : stack)
                rest
