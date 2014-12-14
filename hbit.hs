import Control.Monad
import Data.IORef
import Data.Functor

import Paths
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Configuration
------------------------------------------------------------------------------}
bars  = 4
beats = 4
defaultBpm = 120

bpm2ms :: Int -> Int
bpm2ms bpm = ceiling $ 1000*60 / fromIntegral bpm

drums = words "kick1 snare1 hihat1"

loadInstrumentSample name = return $ "static/" ++ name ++ ".wav"

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- start the server with specified config
main :: IO ()
main = do
    startGUI defaultConfig { tpStatic = Just "static/"} setup


-- everytime the server is reached, this function is called
setup :: Window -> UI ()
setup w = void $ do
    return w # set title "hBit"
    
    elBpm  <- UI.input # set value (show defaultBpm) -- set bpm in input elem
    elTick <- UI.span                                -- show tick in span elem
    let status = grid [[UI.string "BPM:" , element elBpm] 
                      ,[UI.string "Beat:", element elTick]]
    (kit, elInstruments) <- mkDrumKit
    out <- UI.span # set text "Keycode: "
    wrap <- UI.div #. "wrap"
         # set (attr "tabindex") "1"
         #+ (status : map element elInstruments)
         #+[element out]
    getBody w #+ [element wrap]
    -- add list of elements to body
    timer <- UI.timer # set UI.interval (bpm2ms defaultBpm)
    eBeat <- accumE (0::Int) $
        (\beat -> (beat + 1) `mod` (beats * bars)) <$ UI.tick timer
    onEvent eBeat $ \beat -> do
        -- display beat count
        element elTick # set text (show $ beat + 1)
        -- play corresponding sounds
        sequence_ $ map (!! beat) kit
    
    -- allow user to set BPM
    on UI.keydown elBpm $ \keycode -> when (keycode == 13) $ void $ do
        bpm <- read <$> get value elBpm
        return timer # set UI.interval (bpm2ms bpm)

    on UI.keydown   wrap $ \c ->
      --element out # set text ("Keycode: " ++ show c)
      return $ somefunc c
      
    -- star the timer
    UI.start timer
{- -}
somefunc :: UI.KeyCode -> UI ()
somefunc c = do
  elPianoKey1 <- UI.audio # set UI.src "static/tom1.wav"
  case c of
    50        -> do
      runFunction $ ffi "%1.pause()" elPianoKey1
      runFunction $ ffi "%1.currentTime = 0" elPianoKey1 
      runFunction $ ffi "%1.play()" elPianoKey1
    otherwise -> do
      runFunction $ ffi "%1.pause()" elPianoKey1
      runFunction $ ffi "%1.currentTime = 0" elPianoKey1 
      runFunction $ ffi "%1.play()" elPianoKey1
      
type Kit        = [Instrument]
type Instrument = [Beat]
type Beat       = UI ()         -- play the corresponding sound

mkDrumKit :: UI (Kit, [Element])
mkDrumKit = unzip <$> mapM mkInstrument drums

mkInstrument :: String -> UI (Instrument, Element)
mkInstrument name = do
    elCheckboxes <-
        sequence $ replicate bars  $
        sequence $ replicate beats $
            UI.input # set UI.type_ "checkbox"

    url     <- loadInstrumentSample name
    elAudio <- UI.audio # set (attr "preload") "1" # set UI.src url

    let play box = do
            checked <- get UI.checked box
            when checked $ do
                runFunction $ ffi "%1.pause()" elAudio
                runFunction $ ffi "%1.currentTime = 0" elAudio 
                runFunction $ ffi "%1.play()" elAudio
        beats    = map play . concat $ elCheckboxes
        elGroups = [UI.span #. "bar" #+ map element bar | bar <- elCheckboxes]
    
    elInstrument <- UI.div #. "instrument"
        #+ (element elAudio : UI.string name : elGroups)
    
    return (beats, elInstrument)



