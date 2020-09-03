import Control.Monad
import Data.Maybe
import Evdev
import Evdev.Codes
import Evdev.Stream
import qualified Streamly.Prelude as S
import System.Environment
import System.Process

main :: IO ()
main = do
    cmd <- fromMaybe "xfce4-popup-whiskermenu" . listToMaybe <$> getArgs
    void $ S.foldlM' (processEvent cmd) False $ S.map (eventData . snd) $ readEventsMany $ allDevices <> newDevices

processEvent :: String -> Bool -> EventData -> IO Bool
processEvent cmd interfered ev = do
    when (not interfered && isSuperReleased ev) $ callCommand cmd
    return $ case ev of
        KeyEvent KeyLeftmeta Pressed -> False
        KeyEvent KeyLeftmeta _ -> interfered
        KeyEvent _ _ -> True
        _ -> interfered
  where
    isSuperReleased = \case
        KeyEvent KeyLeftmeta Released -> True
        _ -> False
