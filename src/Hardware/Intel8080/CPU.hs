{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Intel8080.CPU where

import Clash.Prelude hiding (lift)

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import Hardware.Intel8080.MicroCPU

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens hiding (Index)
import Data.Maybe (fromMaybe)
import Data.Wedge

import Barbies
import Barbies.Bare
import Data.Barbie.TH
import qualified Language.Haskell.TH.Syntax as TH
import Debug.Trace

data Phase
    = Init
    | Halted
    | Fetching Bool
    | Executing Bool Value (Index MicroLen)
    deriving (Show, Generic, NFDataX)

declareBareB [d|
  data CPUIn = CPUIn
    { dataIn :: Maybe Value
    , interruptRequest :: Bool
    } |]

data CPUState = CPUState
    { _phase :: Phase
    , _interrupted :: Bool
    , _addrLatch :: Maybe Addr
    , _microState :: MicroState
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''CPUState

initState :: Addr -> CPUState
initState pc0 = CPUState
    { _phase = Init
    , _interrupted = False
    , _addrLatch = Nothing
    , _microState = mkMicroState pc0
    }

declareBareB [d|
  data CPUOut = CPUOut
      { _addrOut :: Either Port Addr
      , _dataOut :: Maybe Value
      , _interruptAck :: Bool
      , _halted :: Bool
      } |]
makeLenses ''CPUOut

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{_microState = MicroState{..}, ..} = CPUOut{..}
  where
    _addrOut = Right $ fromMaybe _addrBuf _addrLatch
    _dataOut = Nothing
    _interruptAck = False
    _halted = case _phase of
        Halted -> True
        _ -> False

type M = MaybeT (CPUM CPUState CPUOut)

traceState :: (Show a) => M a -> M a
traceState act = do
    s <- zoom microState $ gets debugState
    x <- act
    trace (unlines [s, show x]) $ return x

latchInterrupt :: Pure CPUIn -> M Bool
latchInterrupt CPUIn{..} = do
    allowed <- use (microState.allowInterrupts)
    when (interruptRequest && allowed) $ interrupted .= True
    use interrupted

acceptInterrupt :: M ()
acceptInterrupt = do
    -- trace (show ("Interrupt accepted", pc)) $ return ()
    microState.allowInterrupts .= False
    addrLatch .= Nothing
    interrupted .= False
    interruptAck .:= True

readByte :: Pure CPUIn -> M Value
readByte CPUIn{..} = do
    x <- MaybeT . return $ dataIn
    addrLatch .= Nothing
    return x

cpu :: Pure CPUIn -> CPUM CPUState CPUOut ()
cpu inp@CPUIn{..} = void . runMaybeT $ do
    interrupted <- latchInterrupt inp

    use phase >>= \case
        Halted -> when interrupted $ do
            acceptInterrupt
            phase .= Fetching True
        Init -> do
            fetchNext
        Fetching False | interrupted -> do
            acceptInterrupt
            phase .= Fetching True
        Fetching interrupting -> do
            instr <- readByte inp
            unless interrupting $ microState.pc += 1
            let (setup, _) = microcodeFor instr
            load <- addressing (wedgeRight setup)
            phase .= Executing load instr 0
        Executing load instr i -> do
            when load $ assign (microState.valueBuf) =<< readByte inp
            exec instr i

fetchNext :: M ()
fetchNext = do
    latchAddr =<< use (microState.pc)
    phase .= Fetching False

exec :: Value -> Index MicroLen -> M ()
exec instr i = do
    let (uop, teardown) = snd (microcodeFor instr) !! i
    -- traceShow (i, uop, teardown) $ return ()
    runExceptT (zoom microState $ uexec uop) >>= \case
        Left GotoNext -> do
            fetchNext
        Left GotoHalt -> do
            phase .= Halted
        Right () -> do
            load <- addressing teardown
            maybe fetchNext (assign phase . Executing load instr) $ succIdx i

addressing :: Wedge OutAddr InAddr -> M Bool
addressing Nowhere = return False
addressing (Here write) = do
    doWrite =<< zoom microState (outAddr write)
    return False
addressing (There read) = do
    doRead =<< zoom microState (inAddr read)
    return True

doWrite :: Either Port Addr -> M ()
doWrite target = do
    addrOut .:= target
    value <- use (microState.valueBuf)
    dataOut .:= Just value

doRead :: Either Port Addr -> M ()
doRead target = either tellPort latchAddr target

tellPort :: Value -> M ()
tellPort port = do
    addrOut .:= Left port

latchAddr :: Addr -> M ()
latchAddr addr = do
    addrLatch .= Just addr
    addrOut .:= Right addr

microcodeFor :: Value -> Microcode
microcodeFor = asyncRom $(TH.lift $ map (microcode . decodeInstr . bitCoerce) $ indicesI @256)
