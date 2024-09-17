{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Exception (bracket, SomeException, assert)
import           Codec (decode, ForceFormat(..))
import           Data.Aeson (Value,Result(Success), encode)
import           Data.Aeson.Diff (Config(Config), diff', diff, patch)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import           Options.Applicative (fullDesc, info, execParser, helper, metavar, progDesc, argument, help, value, long, option, short, switch)
import           Options.Applicative.Types (Parser, readerAsk)
import           System.IO (Handle, IOMode(ReadMode, WriteMode), hClose, openFile, stdin, stdout)
import           GHC.Conc.Sync (setUncaughtExceptionHandler)
import           System.Exit
import           Data.Aeson.Patch (isRem, isTst, patchOperations)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson.Patch (isRem, isTst, patchOperations)

type File = Maybe FilePath

-- new comment
-- | Command-line options.
data DiffOptions = DiffOptions
    { optionTst  :: Bool
    , optionOut  :: File
    , optionFrom :: File
    , optionTo   :: File
    , optionYaml :: Bool
    }

data Configuration = Configuration
    { cfgOptions :: DiffOptions
    , cfgTst  :: Bool
    , cfgOut  :: Handle
    , cfgFrom :: Handle
    , cfgTo   :: Handle
    }

optionParser :: Parser DiffOptions
optionParser = DiffOptions
    <$> switch
        (  long "test-before-remove"
        <> short 'T'
        <> help "Include a test before each remove."
        )
    <*> option fileP
        (  long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Write patch to file OUTPUT."
        <> value Nothing
        )
    <*> argument fileP
        (  metavar "FROM"
        )
    <*> argument fileP
        (  metavar "TO"
        )
    <*> switch
        (  long "yaml"
        <> help "Use yaml decoding and encoding."
        )
  where
    fileP = do
        s <- readerAsk
        return $ case s of
            "-" -> Nothing
            _ -> Just s

jsonFile :: Handle -> ForceFormat -> File -> IO Value
jsonFile fp mformat mfilepath = do
    s <- BS.hGetContents fp
    case decode mformat mfilepath (BSL.fromStrict s) of
        Nothing ->
          case mformat of
            ForceYaml ->        error "Could not parse as YAML"
            AutodetectFormat -> error "Could not parse file. Make sure the file contents and the extension correspond: i.e. use '.json' for JSON files."
        Just v -> return v

run :: DiffOptions -> IO ()
run opt = bracket (load opt) close process
  where
    openr :: Maybe FilePath -> IO Handle
    openr Nothing = return stdin
    openr (Just p) = openFile p ReadMode

    openw :: Maybe FilePath -> IO Handle
    openw Nothing = return stdout
    openw (Just p) = openFile p WriteMode

    load :: DiffOptions -> IO Configuration
    load options@DiffOptions{..} =
        Configuration
            <$> pure  options
            <*> pure  optionTst
            <*> openw optionOut
            <*> openr optionFrom
            <*> openr optionTo

    close :: Configuration -> IO ()
    close Configuration{..} = do
        hClose cfgOut
        hClose cfgFrom
        hClose cfgTo

-- | Extracting and applying a patch is an identity.
diffApply
    :: Value
    -> Value
    -> Bool
diffApply f t =
    let p = diff f t
    in (Success t == patch p f) ||
       error ("BAD PATCH\n" <> BL.unpack (encode p) <> "\n"
                            <> result "<failure>" (BL.unpack . encode <$> patch p f))

result :: a -> Result a -> a
result _ (Success a) = a
result a _             = a

-- | Patch extracted from identical documents should be mempty.
prop_diff_id
    :: Value
    -> Bool
prop_diff_id v =
    diff v v == mempty

-- | Extract and apply a patch (between wellformed JSON documents).
prop_diff_documents
    :: Value
    -> Value
    -> Bool
prop_diff_documents f t =
    diffApply f t

-- | Check that 'Rem' always preceded by a 'Tst'.
prop_tst_before_rem
  :: Value
  -> Value
  -> Bool
prop_tst_before_rem f t =
  let ops = zip [1..] (patchOperations $ diff' (Config True) f t)
      rs = map fst . filter (isRem . snd) $ ops
      ts = map fst . filter (isTst . snd) $ ops
      minusOneInTs :: Integer -> Bool
      minusOneInTs r = (r - 1) `elem` ts
  in (length rs <= length ts) && all minusOneInTs rs

process :: Configuration -> IO ()
process Configuration{..} = do
    let mformat = if optionYaml cfgOptions then ForceYaml else AutodetectFormat
    json_from <- jsonFile cfgFrom mformat (optionFrom cfgOptions)
    json_to <- jsonFile cfgTo mformat (optionTo cfgOptions)
    let c = Config cfgTst
    assert (prop_diff_id json_from) (return ())
    assert (prop_diff_documents json_from json_to) (return ())
    assert (prop_tst_before_rem json_from json_to) (return ())

exitOnException ::  SomeException -> IO ()
exitOnException _ = exitWith (ExitFailure 2)

main :: IO ()
main = do
    setUncaughtExceptionHandler exitOnException
    execParser opts >>= run
  where
    opts = info (helper <*> optionParser)
     (  fullDesc
     <> progDesc "Generate a patch between two JSON documents.")
