module ElmFormat.AST.PublicAST.Reference (Reference(..),mkReference,toRef) where

import ElmFormat.AST.PublicAST.Core
import qualified Data.Text as Text


data Reference
    = ExternalReference
        { module_ :: ModuleName
        , identifier :: Ref ()
        }
    | VariableReference
        { name :: Ref ()
        }

mkReference :: Ref [UppercaseIdentifier] -> Reference
mkReference = \case
    VarRef [] var ->
        VariableReference $ VarRef () var

    VarRef namespace var ->
        ExternalReference
            (ModuleName namespace)
            (VarRef () var)

    TagRef [] tag ->
        VariableReference $ TagRef () tag

    TagRef namespace tag ->
        ExternalReference
            (ModuleName namespace)
            (TagRef () tag)

    OpRef sym ->
        VariableReference $ OpRef sym

toRef :: Reference -> Ref [UppercaseIdentifier]
toRef = \case
    VariableReference (VarRef () var) ->
        VarRef [] var

    VariableReference (TagRef () tag) ->
        TagRef [] tag

    VariableReference (OpRef sym) ->
        OpRef sym


instance ToJSON Reference where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Reference where
    toPairs = \case
        ExternalReference module_ identifier ->
            mconcat
                [ type_ "ExternalReference"
                , "module" .= module_
                , "identifier" .= identifier
                ]

        VariableReference name ->
            mconcat
                [ type_ "VariableReference"
                , "name" .= name
                ]

instance FromJSON Reference where
    parseJSON = withObject "Reference" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "VariableReference" -> do
                name <- obj .: "name"
                case refFromText name of
                    Nothing ->
                        fail ("invalid Reference name: " <> Text.unpack name)

                    Just ref ->
                        return $ VariableReference ref

            _ ->
                fail ("unexpected Reference tag: " <> tag)
