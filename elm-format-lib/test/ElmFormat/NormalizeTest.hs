{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module ElmFormat.NormalizeTest where

import AST.V0_16

import qualified Data.Indexed as I
import qualified ElmFormat.Normalize as Normalize
import Test.Tasty.Hspec
import Control.Monad.Identity (Identity(..))


-- type ResultList =
--     Compose List (Either String)

-- instance Semigroup (ResultList a) where
--     (Compose a) <> (Compose b) = Compose (a <> b)

-- instance Monad ResultList where
--     -- (Compose es) >>= f = Compose $ es >>= either (pure . Left) (getCompose . f)
--     (Compose []) >>= _ =
--         Compose []
--     (Compose (Left x : rest)) >>= f =
--         Compose (Left x : getCompose (Compose rest >>= f))
--     (Compose (Right a : rest)) >>= f =
--         let
--             rest' = getCompose (Compose rest >>= f)
--         in
--         Compose (getCompose (f a) >>= (: rest'))

-- err :: String -> ASTNS ResultList () nk
-- err = I.Fix . Compose . pure . Left

-- ok :: ASTNS1 ResultList () nk -> ASTNS ResultList () nk
-- ok = I.Fix . Compose . pure . Right

-- oks :: List (ASTNS1 ResultList () nk) -> ASTNS ResultList () nk
-- oks = I.Fix . Compose . fmap Right

lc :: String -> LowercaseIdentifier
lc = LowercaseIdentifier

c :: List String -> a -> C1 'BeforeTerm a
c comments =
    C (BlockComment . pure <$> comments)


spec_spec :: Spec
spec_spec = describe "ElmFormat.Normalize" $ do
    describe "remove parens within comments in function application" $
        let
            ml = FAJoinFirst JoinAll
        in do
        -- [ testCase "ResultList" $
        --   let
        --       var n = VarExpr $ VarRef () $ lc n
        --       f = ok $ var "f"
        --   in
        --   Normalize.deep
        --     (ok $ App f
        --         [ c ["!"] (oks [var "a", Parens $ C ([BlockComment ["$"]], []) $ oks [var "b", var "c"]])
        --         , c ["@"] (err "x")
        --         ]
        --         ml )
        --     @=?
        --     oks
        --         [ App f [ c ["!"] (ok $ var "a"), c ["@"] (err "x")] ml
        --         , App f [ c ["!","$"] (oks [var "b", var "c"]), c ["@"] (err "x")] ml
        --         ]
        -- , testCase "ResultList monad makes sense" $
        --     liftM2 (,)
        --         (Compose [Right "a", Left "y"])
        --         (Compose [Left "x", Right "b"])
        --     @=?
        --     Compose
        --         [ Right $ Compose [ Right "a", Left "x"]
        --         , Right $ Compose [ Right "b", Left "x"]
        --         ]

        it "List" $
            let
                var n = VarExpr $ I.Fix2 [VarRef_ $ VarRef () $ lc n]
                f = I.Fix2 [var "f"]
            in do
            Normalize.deepMonad
                (I.Fix2 [App f
                    [ c ["!"] (I.Fix2 [var "a", Parens $ C ([BlockComment ["$"]], []) (I.Fix2 [var "b", var "c"])])
                    , c ["@"] (I.Fix2 [var "x", var "z"])
                    ]
                    ml ])
                `shouldBe`
                I.Fix2
                    [ App f [ c ["!"] (I.Fix2 [var "a"]), c ["@"] (I.Fix2 [var "x"])] ml
                    , App f [ c ["!"] (I.Fix2 [var "a"]), c ["@"] (I.Fix2 [var "z"])] ml
                    , App f [ c ["!","$"] (I.Fix2 [var "b"]), c ["@"] (I.Fix2 [var "x"])] ml
                    , App f [ c ["!","$"] (I.Fix2 [var "b"]), c ["@"] (I.Fix2 [var "z"])] ml
                    , App f [ c ["!","$"] (I.Fix2 [var "c"]), c ["@"] (I.Fix2 [var "x"])] ml
                    , App f [ c ["!","$"] (I.Fix2 [var "c"]), c ["@"] (I.Fix2 [var "z"])] ml
                    ]

        -- it "Either" $
        --     let
        --         var n = I.Fix2 $ Right $ VarExpr $ I.Fix2 $ Right $ VarRef_ $ VarRef () $ lc n
        --         f = var "f"
        --     in do
        --     Normalize.deepSemigroup
        --         (I.Fix2 $ Right $ App f
        --             [ c ["!"] (I.Fix2 $ Right $ Parens $ C ([BlockComment ["$"]], []) (var "b"))
        --             , c ["@"] (I.Fix2 $ Left ("#$%" :: String))
        --             ]
        --             ml)
        --         `shouldBe`
        --         I.Fix2 (Right $
        --             App f
        --                 [ c ["!","$"] (var "b")
        --                 , c ["@"] (I.Fix2 $ Left "#$%")
        --                 ]
        --                 ml
        --             )

        -- it "Maybe" $
        --     let
        --         var n = I.Fix2 $ Just $ VarExpr $ I.Fix2 $ Just $ VarRef_ $ VarRef () $ lc n
        --         f = var "f"
        --     in do
        --     Normalize.deepAlternative
        --         (I.Fix2 $ Just $ App f
        --             [ c ["!"] (I.Fix2 $ Just $ Parens $ C ([BlockComment ["$"]], []) (var "b"))
        --             , c ["@"] (I.Fix2 Nothing)
        --             ]
        --             ml)
        --         `shouldBe`
        --         I.Fix2 (Just $
        --             App f
        --                 [ c ["!","$"] (var "b")
        --                 , c ["@"] (I.Fix2 Nothing)
        --                 ]
        --                 ml
        --             )

        it "Identity" $
            let
                var n = I.Fix2 $ Identity $ VarExpr $ I.Fix2 $ Identity $ VarRef_ $ VarRef () $ lc n
                f = var "f"
            in
            Normalize.deepMonad
                (I.Fix2 $ Identity $ App f
                    [ c ["!"] (I.Fix2 $ Identity $ Parens $ C ([BlockComment ["$"]], []) (var "b"))
                    , c ["@"] (var "c")
                    ]
                    ml)
                `shouldBe`
                I.Fix2 (Identity $
                    App f
                        [ c ["!","$"] (var "b")
                        , c ["@"] (var "c")
                        ]
                        ml
                    )
