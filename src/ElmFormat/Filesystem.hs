module ElmFormat.Filesystem where

import Control.Monad.Free
import ElmFormat.FileStore
import ElmVersion
import System.FilePath ((</>), takeDirectory)

import qualified Data.Maybe as Maybe
import qualified System.FilePath as FilePath


data ElmFile
    = ElmFile
        { version :: ElmVersion
        , path :: FilePath
        } deriving (Show)


collectFiles :: Monad m => (a -> m [a]) -> a -> m [a]
collectFiles children root =
    do
        xs <- children root
        subChildren <- mapM (collectFiles children) xs
        return $ root : concat subChildren


listDir :: FileStore f => ElmFile -> Free f [ElmFile]
listDir (ElmFile upwardVersion path) =
    do
        version <- findDirectoryElmVersion path SingleDirectory
        let version' = Maybe.fromMaybe upwardVersion version
        fmap (ElmFile version' . (path </>)) <$> listDirectory path


doesDirectoryExist :: FileStore f => FilePath -> Free f Bool
doesDirectoryExist path =
    do
        fileType <- stat path
        case fileType of
            IsDirectory -> return True
            _ -> return False


fileList :: FileStore f => ElmFile -> Free f [ElmFile]
fileList =
    let
        children dir =
            if isSkippable dir then
                return []
            else
                do
                    directory <- doesDirectoryExist (path dir)
                    if directory then listDir dir else return []
    in
        collectFiles children


isSkippable :: ElmFile -> Bool
isSkippable (ElmFile _ path) =
    or
        [ hasFilename "elm-stuff" path
        , hasFilename "node_modules" path
        , hasFilename ".git" path
        ]

hasExtension :: String -> ElmFile -> Bool
hasExtension ext (ElmFile _ path) =
    ext == FilePath.takeExtension path


findAllElmFiles :: FileStore f => ElmFile -> Free f [ElmFile]
findAllElmFiles inputFile =
    filter (hasExtension ".elm") <$> fileList inputFile


findElmVersion :: FileStore f => FilePath -> Free f (Maybe ElmVersion)
findElmVersion path =
    do
        fileType <- stat path
        case fileType of
            IsFile ->
                do
                    absoluteDir <- makeAbsolute $ takeDirectory path
                    findDirectoryElmVersion absoluteDir Recursive

            IsDirectory ->
                do
                    absoluteDir <- makeAbsolute path
                    findDirectoryElmVersion absoluteDir Recursive

            _ ->
                return Nothing


data ElmVersionFinder
    = SingleDirectory
    | Recursive


findDirectoryElmVersion :: FileStore f => FilePath -> ElmVersionFinder -> Free f (Maybe ElmVersion)
findDirectoryElmVersion dir finder =
    do
        let upwardDir = takeDirectory dir
        elmJson <- stat (dir </> "elm.json")
        case elmJson of
            IsFile ->
                return $ Just Elm_0_19

            _ ->
                do
                    elmPackageJson <- stat (dir </> "elm-package.json")
                    case (elmPackageJson, finder) of
                        (IsFile, _) ->
                            return $ Just Elm_0_18

                        (_, Recursive) | dir /= upwardDir ->
                            findDirectoryElmVersion upwardDir Recursive

                        _ ->
                            return Nothing


hasFilename :: String -> FilePath -> Bool
hasFilename name path =
    name == FilePath.takeFileName path
