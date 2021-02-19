module ElmFormat.AST.PublicAST.Reference (Reference(..),mkReference) where

import ElmFormat.AST.PublicAST.Core


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


