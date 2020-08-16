{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Hardware.Intel8080.Sim where

import Hardware.Intel8080
import Hardware.Intel8080.CPU

import Clash.Prelude hiding (lift)

import RetroClash.Barbies
import RetroClash.CPU

import Control.Monad.State
import Data.Foldable (traverse_, for_)
import Control.Lens hiding (index)

data IRQ
    = NewIRQ Value
    | QueuedIRQ Value

data World m = World
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , inPort :: Port -> m Value
    , outPort :: Port -> Value -> m Value
    }

initInput :: Pure CPUIn
initInput = CPUIn
    { dataIn = Nothing
    , interruptRequest = False
    }

world :: (Monad m) => World m -> Pure CPUOut -> StateT (Maybe IRQ) m (Pure CPUIn)
world World{..} CPUOut{..} = do
    dataIn <- Just <$> read
    unless _portSelect $ lift $ traverse_ (writeMem _addrOut) _dataOut

    interruptRequest <- get >>= \case
        Just (NewIRQ op) -> do
            put $ Just $ QueuedIRQ op
            return True
        _ -> return False

    return CPUIn{..}
  where
    read | _portSelect = lift $ maybe (inPort port) (outPort port) _dataOut
         | _interruptAck = get >>= \case
            Just (QueuedIRQ op) -> do
                put Nothing
                return op
            _ -> return 0x00
         | otherwise = lift $ readMem _addrOut

    port = truncateB _addrOut

sim :: (Monad m) => (CPUState -> World m) -> StateT (Pure CPUIn, CPUState, Maybe IRQ) m ()
sim mkWorld = do
    inp <- use _1
    s <- use _2

    let (out, s') = runState (cpuMachine inp) s
    inp' <- zoom _3 $ world (mkWorld s) out
    _1 .= inp'
    _2 .= s'

interrupt :: (Monad m) => Unsigned 3 -> StateT (Maybe IRQ) m ()
interrupt v = put $ Just $ NewIRQ rst
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)