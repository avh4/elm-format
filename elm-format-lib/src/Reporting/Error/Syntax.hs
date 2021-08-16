-- This module is based on `Reporting.Error.Syntax` in the Elm compiler
-- https://github.com/elm/compiler/blob/94715a520f499591ac6901c8c822bc87cd1af24f/compiler/src/Reporting/Error/Syntax.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Syntax
  ( Error(..)
--  , toReport
--  --
--  , Module(..)
--  , Exposing(..)
--  --
--  , Decl(..)
--  , DeclType(..)
--  , TypeAlias(..)
--  , CustomType(..)
--  , DeclDef(..)
--  , Port(..)
--  --
--  , Expr(..)
--  , Record(..)
--  , Tuple(..)
--  , List(..)
--  , Func(..)
--  , Case(..)
--  , If(..)
--  , Let(..)
--  , Def(..)
--  , Destruct(..)
--  --
--  , Pattern(..)
--  , PRecord(..)
--  , PTuple(..)
--  , PList(..)
--  --
--  , Type(..)
--  , TRecord(..)
--  , TTuple(..)
  --
  , Char(..)
  , String(..)
  , Escape(..)
  , Number(..)
  --
--  , Space(..)
--  , toSpaceReport
  --
  , newline
  , freshLine
  , whitespace
  , keyword
  )
  where


import Prelude hiding (Char, String)
import qualified Prelude as Prelude

--import qualified Data.Char as Char
--import qualified Data.Name as Name
import Data.Word (Word16)
import Data.Int (Int64)
--import Numeric (showHex)

--import qualified Elm.ModuleName as ModuleName
--import Parse.Primitives (Row, Col)
import qualified Parse.ParsecAdapter.Message
--import Parse.Symbol (BadOperator(..))
--import qualified Reporting.Annotation as A
--import qualified Reporting.Doc as D
--import qualified Reporting.Report as Report
--import qualified Reporting.Render.Code as Code



-- ALL SYNTAX ERRORS


data Error
    = Parse [Parse.ParsecAdapter.Message.Message]
--  = ModuleNameUnspecified ModuleName.Raw
--  | ModuleNameMismatch ModuleName.Raw (A.Located ModuleName.Raw)
--  | UnexpectedPort A.Region
--  | NoPorts A.Region
--  | NoPortsInPackage (A.Located Name.Name)
--  | NoPortModulesInPackage A.Region
--  | NoEffectsOutsideKernel A.Region
--  | ParseError Module
    deriving (Eq, Show)


instance Show Parse.ParsecAdapter.Message.Message where
   show _ = "<Text.Parsec.Error.Message>"



-- MODULE


--data Module
--  = ModuleSpace Space Row Col
--  | ModuleBadEnd Row Col
--  --
--  | ModuleProblem Row Col
--  | ModuleName Row Col
--  | ModuleExposing Exposing Row Col
--  --
--  | PortModuleProblem Row Col
--  | PortModuleName Row Col
--  | PortModuleExposing Exposing Row Col
--  --
--  | Effect Row Col
--  --
--  | FreshLine Row Col
--  --
--  | ImportStart Row Col
--  | ImportName Row Col
--  | ImportAs Row Col
--  | ImportAlias Row Col
--  | ImportExposing Row Col
--  | ImportExposingList Exposing Row Col
--  | ImportEnd Row Col -- different based on col=1 or if greater
--  --
--  | ImportIndentName Row Col
--  | ImportIndentAlias Row Col
--  | ImportIndentExposingList Row Col
--  --
--  | Infix Row Col
--  --
--  | Declarations Decl Row Col


--data Exposing
--  = ExposingSpace Space Row Col
--  | ExposingStart Row Col
--  | ExposingValue Row Col
--  | ExposingOperator Row Col
--  | ExposingOperatorReserved BadOperator Row Col
--  | ExposingOperatorRightParen Row Col
--  | ExposingTypePrivacy Row Col
--  | ExposingEnd Row Col
--  --
--  | ExposingIndentEnd Row Col
--  | ExposingIndentValue Row Col



-- DECLARATIONS


--data Decl
--  = DeclStart Row Col
--  | DeclSpace Space Row Col
--  --
--  | Port Port Row Col
--  | DeclType DeclType Row Col
--  | DeclDef Name.Name DeclDef Row Col
--  --
--  | DeclFreshLineAfterDocComment Row Col


--data DeclDef
--  = DeclDefSpace Space Row Col
--  | DeclDefEquals Row Col
--  | DeclDefType Type Row Col
--  | DeclDefArg Pattern Row Col
--  | DeclDefBody Expr Row Col
--  | DeclDefNameRepeat Row Col
--  | DeclDefNameMatch Name.Name Row Col
--  --
--  | DeclDefIndentType Row Col
--  | DeclDefIndentEquals Row Col
--  | DeclDefIndentBody Row Col


--data Port
--  = PortSpace Space Row Col
--  | PortName Row Col
--  | PortColon Row Col
--  | PortType Type Row Col
--  | PortIndentName Row Col
--  | PortIndentColon Row Col
--  | PortIndentType Row Col



-- TYPE DECLARATIONS


--data DeclType
--  = DT_Space Space Row Col
--  | DT_Name Row Col
--  | DT_Alias TypeAlias Row Col
--  | DT_Union CustomType Row Col
--  --
--  | DT_IndentName Row Col


--data TypeAlias
--  = AliasSpace Space Row Col
--  | AliasName Row Col
--  | AliasEquals Row Col
--  | AliasBody Type Row Col
--  --
--  | AliasIndentEquals Row Col
--  | AliasIndentBody Row Col


--data CustomType
--  = CT_Space Space Row Col
--  | CT_Name Row Col
--  | CT_Equals Row Col
--  | CT_Bar Row Col
--  | CT_Variant Row Col
--  | CT_VariantArg Type Row Col
--  --
--  | CT_IndentEquals Row Col
--  | CT_IndentBar Row Col
--  | CT_IndentAfterBar Row Col
--  | CT_IndentAfterEquals Row Col



---- EXPRESSIONS


--data Expr
--  = Let Let Row Col
--  | Case Case Row Col
--  | If If Row Col
--  | List List Row Col
--  | Record Record Row Col
--  | Tuple Tuple Row Col
--  | Func Func Row Col
--  --
--  | Dot Row Col
--  | Access Row Col
--  | OperatorRight Name.Name Row Col
--  | OperatorReserved BadOperator Row Col
--  --
--  | Start Row Col
--  | Char Char Row Col
--  | String String Row Col
--  | Number Number Row Col
--  | Space Space Row Col
--  | EndlessShader Row Col
--  | ShaderProblem [Char.Char] Row Col
--  | IndentOperatorRight Name.Name Row Col


--data Record
--  = RecordOpen Row Col
--  | RecordEnd Row Col
--  | RecordField Row Col
--  | RecordEquals Row Col
--  | RecordExpr Expr Row Col
--  | RecordSpace Space Row Col
--  --
--  | RecordIndentOpen Row Col
--  | RecordIndentEnd Row Col
--  | RecordIndentField Row Col
--  | RecordIndentEquals Row Col
--  | RecordIndentExpr Row Col


--data Tuple
--  = TupleExpr Expr Row Col
--  | TupleSpace Space Row Col
--  | TupleEnd Row Col
--  | TupleOperatorClose Row Col
--  | TupleOperatorReserved BadOperator Row Col
--  --
--  | TupleIndentExpr1 Row Col
--  | TupleIndentExprN Row Col
--  | TupleIndentEnd Row Col


--data List
--  = ListSpace Space Row Col
--  | ListOpen Row Col
--  | ListExpr Expr Row Col
--  | ListEnd Row Col
--  --
--  | ListIndentOpen Row Col
--  | ListIndentEnd Row Col
--  | ListIndentExpr Row Col


--data Func
--  = FuncSpace Space Row Col
--  | FuncArg Pattern Row Col
--  | FuncBody Expr Row Col
--  | FuncArrow Row Col
--  --
--  | FuncIndentArg Row Col
--  | FuncIndentArrow Row Col
--  | FuncIndentBody Row Col


--data Case
--  = CaseSpace Space Row Col
--  | CaseOf Row Col
--  | CasePattern Pattern Row Col
--  | CaseArrow Row Col
--  | CaseExpr Expr Row Col
--  | CaseBranch Expr Row Col
--  --
--  | CaseIndentOf Row Col
--  | CaseIndentExpr Row Col
--  | CaseIndentPattern Row Col
--  | CaseIndentArrow Row Col
--  | CaseIndentBranch Row Col
--  | CasePatternAlignment Word16 Row Col


--data If
--  = IfSpace Space Row Col
--  | IfThen Row Col
--  | IfElse Row Col
--  | IfElseBranchStart Row Col
--  --
--  | IfCondition Expr Row Col
--  | IfThenBranch Expr Row Col
--  | IfElseBranch Expr Row Col
--  --
--  | IfIndentCondition Row Col
--  | IfIndentThen Row Col
--  | IfIndentThenBranch Row Col
--  | IfIndentElseBranch Row Col
--  | IfIndentElse Row Col


--data Let
--  = LetSpace Space Row Col
--  | LetIn Row Col
--  | LetDefAlignment Word16 Row Col
--  | LetDefName Row Col
--  | LetDef Name.Name Def Row Col
--  | LetDestruct Destruct Row Col
--  | LetBody Expr Row Col
--  | LetIndentDef Row Col
--  | LetIndentIn Row Col
--  | LetIndentBody Row Col


--data Def
--  = DefSpace Space Row Col
--  | DefType Type Row Col
--  | DefNameRepeat Row Col
--  | DefNameMatch Name.Name Row Col
--  | DefArg Pattern Row Col
--  | DefEquals Row Col
--  | DefBody Expr Row Col
--  | DefIndentEquals Row Col
--  | DefIndentType Row Col
--  | DefIndentBody Row Col
--  | DefAlignment Word16 Row Col


--data Destruct
--  = DestructSpace Space Row Col
--  | DestructPattern Pattern Row Col
--  | DestructEquals Row Col
--  | DestructBody Expr Row Col
--  | DestructIndentEquals Row Col
--  | DestructIndentBody Row Col



-- PATTERNS


--data Pattern
--  = PRecord PRecord Row Col
--  | PTuple PTuple Row Col
--  | PList PList Row Col
--  --
--  | PStart Row Col
--  | PChar Char Row Col
--  | PString String Row Col
--  | PNumber Number Row Col
--  | PFloat Word16 Row Col
--  | PAlias Row Col
--  | PWildcardNotVar Name.Name Int Row Col
--  | PSpace Space Row Col
--  --
--  | PIndentStart Row Col
--  | PIndentAlias Row Col


--data PRecord
--  = PRecordOpen Row Col
--  | PRecordEnd Row Col
--  | PRecordField Row Col
--  | PRecordSpace Space Row Col
--  --
--  | PRecordIndentOpen Row Col
--  | PRecordIndentEnd Row Col
--  | PRecordIndentField Row Col


--data PTuple
--  = PTupleOpen Row Col
--  | PTupleEnd Row Col
--  | PTupleExpr Pattern Row Col
--  | PTupleSpace Space Row Col
--  --
--  | PTupleIndentEnd Row Col
--  | PTupleIndentExpr1 Row Col
--  | PTupleIndentExprN Row Col


--data PList
--  = PListOpen Row Col
--  | PListEnd Row Col
--  | PListExpr Pattern Row Col
--  | PListSpace Space Row Col
--  --
--  | PListIndentOpen Row Col
--  | PListIndentEnd Row Col
--  | PListIndentExpr Row Col



-- TYPES


--data Type
--  = TRecord TRecord Row Col
--  | TTuple TTuple Row Col
--  --
--  | TStart Row Col
--  | TSpace Space Row Col
--  --
--  | TIndentStart Row Col


--data TRecord
--  = TRecordOpen Row Col
--  | TRecordEnd Row Col
--  --
--  | TRecordField Row Col
--  | TRecordColon Row Col
--  | TRecordType Type Row Col
--  --
--  | TRecordSpace Space Row Col
--  --
--  | TRecordIndentOpen Row Col
--  | TRecordIndentField Row Col
--  | TRecordIndentColon Row Col
--  | TRecordIndentType Row Col
--  | TRecordIndentEnd Row Col


--data TTuple
--  = TTupleOpen Row Col
--  | TTupleEnd Row Col
--  | TTupleType Type Row Col
--  | TTupleSpace Space Row Col
--  --
--  | TTupleIndentType1 Row Col
--  | TTupleIndentTypeN Row Col
--  | TTupleIndentEnd Row Col



-- LITERALS


data Char
  = CharEndless
  | CharEscape Escape
  | CharNotString Word16
  deriving Show


data String
  = StringEndless_Single
  | StringEndless_Multi
  | StringEscape Escape
  deriving Show


data Escape
  = EscapeUnknown
  | BadUnicodeFormat Word16
  | BadUnicodeCode Word16
  | BadUnicodeLength Word16 Int Int
  deriving Show


data Number
  = NumberEnd
  | NumberDot Int64
  | NumberHexDigit
  | NumberNoLeadingZero
  deriving Show



-- TAGGING PARSE ERRORS


newline :: Prelude.String
newline = "NEWLINE"


freshLine :: Prelude.String
freshLine = "FRESH_LINE"


whitespace :: Prelude.String
whitespace = "WHITESPACE"


keyword :: Prelude.String -> Prelude.String
keyword kwd =
  "KEYWORD=" ++ kwd



-- MISC


--data Space
--  = HasTab
--  | EndlessMultiComment
