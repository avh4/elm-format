module CommandLine.Program (ProgramResult(..), ProgramIO, run, failed, CommandLine.Program.error, showUsage, liftEither, liftM, liftME) where

-- Common handling for command line programs

import Prelude ()
import Relude hiding (putStrLn, exitSuccess, exitFailure)

import ElmFormat.World
import System.Exit (ExitCode(..))

import qualified Options.Applicative as OptParse


data ProgramResult err a
    = ShowUsage
    | ProgramError err
    | ProgramSuccess a
    | ProgramFailed
    deriving (Functor)

instance Applicative (ProgramResult err) where
    pure a = ProgramSuccess a
    liftA2 f (ProgramSuccess a) (ProgramSuccess b) = ProgramSuccess (f a b)
    liftA2 _ (ProgramError err) _ = ProgramError err
    liftA2 _ ProgramFailed _ = ProgramFailed
    liftA2 _ ShowUsage _ = ShowUsage
    liftA2 _ _ (ProgramError err) = ProgramError err
    liftA2 _ _ ProgramFailed = ProgramFailed
    liftA2 _ _ ShowUsage = ShowUsage

instance Monad (ProgramResult err) where
    ShowUsage >>= _ = ShowUsage
    ProgramFailed >>= _ = ProgramFailed
    (ProgramError err) >>= _ = ProgramError err
    (ProgramSuccess a) >>= f = f a


newtype ProgramIO m x a =
    ProgramIO (m (ProgramResult x a))
    deriving (Functor)

instance Monad m => Applicative (ProgramIO m x) where
    pure a = ProgramIO $ return $ pure a
    liftA2 f (ProgramIO a) (ProgramIO b) =
        ProgramIO $ do
            a' <- a
            b' <- b
            return $ liftA2 f a' b'

instance Monad m => Monad (ProgramIO m x) where
    (ProgramIO m) >>= f = ProgramIO (m >>= f')
        where
            f' r =
                case r of
                    ShowUsage -> return $ ShowUsage
                    ProgramError x -> return $ ProgramError x
                    ProgramFailed -> return $ ProgramFailed
                    ProgramSuccess a -> (\(ProgramIO z) -> z) $ f a

failed :: Applicative m => ProgramIO m x a
failed = ProgramIO $ pure $ ProgramFailed

error :: Applicative m => x -> ProgramIO m x a
error x = ProgramIO $ pure $ ProgramError x

showUsage :: Applicative m => ProgramIO m x a
showUsage = ProgramIO $ pure $ ShowUsage

liftEither :: Applicative m => Either x a -> ProgramIO m x a
liftEither (Left x) = ProgramIO $ pure $ ProgramError x
liftEither (Right a) = ProgramIO $ pure $ ProgramSuccess a

liftM :: Functor m => m a -> ProgramIO m x a
liftM m = ProgramIO (ProgramSuccess <$> m)

liftME :: Monad m => m (Either x a) -> ProgramIO m x a
liftME m = ProgramIO (m >>= ((\(ProgramIO z) -> z) . liftEither))


run ::
    World m =>
    ([String] -> OptParse.ParserResult flags)
    -> (err -> String)
    -> (flags -> ProgramIO m err ())
    -> [String]
    -> m ()
run parseFlags formatError run' args =
    do
        flags <- handleParseResult $ parseFlags args
        case flags of
            Nothing -> return ()
            Just flags' ->
                do
                    result <- (\(ProgramIO m) -> m) $ run' flags'
                    case result of
                        ShowUsage ->
                            (handleParseResult $ parseFlags ["--help"])
                                -- TODO: handleParseResult is exitSuccess, so we never get to exitFailure
                                >> exitFailure

                        ProgramError err ->
                            putStrLnStderr (formatError err)
                                >> exitFailure

                        ProgramSuccess () ->
                            exitSuccess

                        ProgramFailed ->
                            exitFailure


{-| copied from Options.Applicative -}
handleParseResult :: World m => OptParse.ParserResult a -> m (Maybe a)
handleParseResult (OptParse.Success a) = return (Just a)
handleParseResult (OptParse.Failure failure) = do
    progn <- getProgName
    let (msg, exit) = OptParse.renderFailure failure progn
    case exit of
        ExitSuccess -> putStrLn msg *> exitSuccess *> return Nothing
        _           -> putStrLnStderr msg *> exitFailure *> return Nothing
handleParseResult (OptParse.CompletionInvoked _) =
    -- do
    --     progn <- getProgName
    --     msg <- OptParse.execCompletion compl progn
    --     putStr msg
    --     const undefined <$> exitSuccess
    Relude.error "Shell completion not yet implemented"
