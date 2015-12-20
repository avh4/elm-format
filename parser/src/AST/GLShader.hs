{-# OPTIONS_GHC -Wall #-}
module AST.GLShader where

import qualified Data.Map as Map


data GLTipe
    = Int
    | Float
    | V2
    | V3
    | V4
    | M4
    | Texture
    deriving (Eq, Show)


glTipeName :: GLTipe -> String
glTipeName glTipe =
    case glTipe of
      Int     -> "Int"
      Float   -> "Float"
      V2      -> "Math.Vector2.Vec2"
      V3      -> "Math.Vector3.Vec3"
      V4      -> "Math.Vector4.Vec4"
      M4      -> "Math.Matrix4.Mat4"
      Texture -> "WebGL.Texture"


data GLShaderTipe = GLShaderTipe
    { attribute :: Map.Map String GLTipe
    , uniform :: Map.Map String GLTipe
    , varying :: Map.Map String GLTipe
    }
    deriving (Eq, Show)
