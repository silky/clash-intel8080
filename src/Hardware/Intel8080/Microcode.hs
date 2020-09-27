{-# LANGUAGE RankNTypes #-}
module Hardware.Intel8080.Microcode where

import Clash.Prelude

import Hardware.Intel8080
import Hardware.Intel8080.Steps
import qualified Language.Haskell.TH.Lift as TH

data InAddr
    = FromPtr
    | FromPort
    | IncrPC
    | IncrSP
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, TH.Lift)

data OutAddr
    = ToPtr
    | ToPort
    | DecrSP
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, TH.Lift)

data UpdateAC
    = SetAC
    | KeepAC
    deriving (Show, Eq, Generic, NFDataX, TH.Lift)

data UpdateC
    = SetC
    | KeepC
    deriving (Show, Eq, Generic, NFDataX, TH.Lift)

data MicroInstr
    = Get Reg
    | Set Reg
    | FromPC
    | FromAddrBuf
    | ToAddrBuf
    | Get2 RegPair
    | Swap2 RegPair
    | Jump
    | When (Maybe Cond)
    | Compute ALUArg ALU UpdateC UpdateAC
    | ComputeSR (Either ShiftRotate ShiftRotate)
    | Compute2 ALU2 UpdateC
    | Compute0 Flag ALU0
    | UpdateFlags
    | Rst (Unsigned 3)
    | SetInt Bool
    | FixupBCD
    deriving (Show, Generic, NFDataX, TH.Lift)

data ALUArg
    = RegA
    | Const01
    | ConstFF
    deriving (Show, Generic, NFDataX, TH.Lift)

data ALU2
    = Inc2
    | Dec2
    | AddHL
    deriving (Show, Generic, NFDataX, TH.Lift)

data ALU0
    = Complement0
    | ConstTrue0
    deriving (Show, Generic, NFDataX, TH.Lift)

type MicroSteps = Steps InAddr MicroInstr OutAddr

imm2 =
    step (IJust IncrPC) ToAddrBuf INothing >++>
    step (IJust IncrPC) ToAddrBuf INothing

push2 =
    step INothing FromAddrBuf (IJust DecrSP) >++>
    step INothing FromAddrBuf (IJust DecrSP)

pushPC =
    step INothing FromPC (IJust DecrSP) >++>
    step INothing FromPC (IJust DecrSP)

pop2 =
    step (IJust IncrSP) ToAddrBuf INothing >++>
    step (IJust IncrSP) ToAddrBuf INothing

type MicroOp = (MicroInstr, Maybe (Either OutAddr InAddr))
type MicroLen = 6
type Microcode = (Maybe InAddr, Vec MicroLen MicroOp)

mc :: (KnownNat k, (1 + n + k) ~ MicroLen) => MicroSteps (1 + n) pre post -> Microcode
mc ops = let (first, ops') = stepsOf ops
         in (first, ops' ++ repeat (When Nothing, Nothing))

evalSrc
    :: (KnownNat k, KnownNat k', ((1 + n) + k) ~ MicroLen, ((1 + (1 + n)) + k') ~ MicroLen)
    => RHS
    -> (forall pre. IMaybe pre InAddr -> MicroSteps (1 + n) pre post)
    -> Microcode
evalSrc src k = case src of
    Imm -> mc $
        k (IJust IncrPC)
    LHS (Reg r) -> mc $
        step INothing (Get r) INothing >++>
        k INothing
    LHS (Addr rr) -> mc $
        step INothing (Get2 rr) INothing >++>
        k (IJust FromPtr)

microcode :: Instr -> Microcode
microcode NOP = mc $ step INothing (When Nothing) INothing
-- microcode HLT = mc _
microcode (INT b) = mc $ step INothing (SetInt b) INothing
microcode CMA = mc $
    step INothing (Get RA)                                   INothing >++>
    step INothing (Compute ConstFF (Sub False) KeepC KeepAC) INothing >++>
    step INothing (Set RA)                                   INothing
microcode CMC = mc $
    step INothing (Compute0 FC Complement0) INothing
microcode STC = mc $
    step INothing (Compute0 FC ConstTrue0) INothing
microcode (ALU fun src) = evalSrc src $ \read ->
    step read     (Compute RegA fun SetC SetAC) INothing >++>
    step INothing UpdateFlags                   INothing >++>
    step INothing (Set RA)                      INothing
microcode (CMP src) = evalSrc src $ \read ->
    step read     (Compute RegA (Sub False) SetC SetAC) INothing >++>
    step INothing UpdateFlags                           INothing
microcode (SHROT sr) = mc $
    step INothing (Get RA)       INothing >++>
    step INothing (ComputeSR sr) INothing >++>
    step INothing (Set RA)       INothing
microcode (RST irq) = mc $
    pushPC >++>
    step INothing (Rst irq) INothing
microcode JMP = mc $
    imm2 >++>
    step INothing Jump INothing
microcode (JMPIf cond) = mc $
    imm2 >++>
    step INothing (When $ Just cond) INothing >++>
    step INothing Jump               INothing
microcode CALL = mc $
    imm2 >++>
    pushPC >++>
    step INothing Jump INothing
microcode (CALLIf cond) = mc $
    imm2 >++>
    step INothing (When $ Just cond) INothing >++>
    pushPC                                    >++>
    step INothing Jump               INothing
microcode RET = mc $
    pop2 >++>
    step INothing Jump INothing
microcode (RETIf cond) = mc $
    step INothing (When $ Just cond) INothing >++>
    pop2 >++>
    step INothing Jump               INothing
microcode LDA = mc $
    imm2 >++>
    step (IJust FromPtr) (Set RA) INothing
microcode STA = mc $
    imm2 >++>
    step INothing (Get RA) (IJust ToPtr)
microcode (DCX rr) = mc $
    step INothing (Get2 rr)             INothing >++>
    step INothing (Compute2 Dec2 KeepC) INothing >++>
    step INothing (Swap2 rr)            INothing
microcode (INX rr) = mc $
    step INothing (Get2 rr)             INothing >++>
    step INothing (Compute2 Inc2 KeepC) INothing >++>
    step INothing (Swap2 rr)            INothing
microcode (DAD rr) = mc $
    step INothing (Get2 rr)             INothing >++>
    step INothing (Compute2 AddHL SetC) INothing >++>
    step INothing (Swap2 RHL)           INothing
microcode (INR (Addr rr)) = mc $
    step INothing        (Get2 rr)                                 INothing >++>
    step (IJust FromPtr) (Compute Const01 (Add False) KeepC SetAC) INothing >++>
    step INothing        UpdateFlags                               (IJust ToPtr)
microcode (INR (Reg r)) = mc $
    step INothing (Get r)                                   INothing >++>
    step INothing (Compute Const01 (Add False) KeepC SetAC) INothing >++>
    step INothing UpdateFlags                               INothing >++>
    step INothing (Set r)                                   INothing
microcode (DCR (Addr rr)) = mc $
    step INothing        (Get2 rr)                                 INothing >++>
    step (IJust FromPtr) (Compute ConstFF (Add False) KeepC SetAC) INothing >++>
    step INothing        UpdateFlags                               (IJust ToPtr)
microcode (DCR (Reg r)) = mc $
    step INothing (Get r)                                   INothing >++>
    step INothing (Compute ConstFF (Add False) KeepC SetAC) INothing >++>
    step INothing UpdateFlags                               INothing >++>
    step INothing (Set r)                                   INothing
microcode DAA = mc $
    step INothing (Get RA)    INothing >++>
    step INothing FixupBCD    INothing >++>
    step INothing UpdateFlags INothing >++>
    step INothing (Set RA)    INothing
microcode (LXI rr) = mc $
    imm2 >++>
    step INothing (Swap2 rr) INothing
microcode PCHL = mc $
    step INothing (Get2 RHL) INothing >++>
    step INothing Jump       INothing
microcode SPHL = mc $
    step INothing (Get2 RHL) INothing >++>
    step INothing (Swap2 SP) INothing
microcode LHLD = mc $
    imm2 >++>
    step (IJust FromPtr) (Set RL)              INothing >++>
    step INothing        (Compute2 Inc2 KeepC) INothing >++>
    step (IJust FromPtr) (Set RH)              INothing
microcode SHLD = mc $
    imm2 >++>
    step INothing (Get RL)              (IJust ToPtr) >++>
    step INothing (Compute2 Inc2 KeepC) INothing      >++>
    step INothing (Get RH)              (IJust ToPtr)
microcode XTHL = mc $
    pop2 >++>
    step INothing (Swap2 RHL) INothing >++>
    push2
microcode (PUSH rr) = mc $
    step INothing (Get2 rr) INothing >++>
    push2
microcode (POP rr) = mc $
    pop2 >++>
    step INothing (Swap2 rr) INothing
microcode (MOV (Reg r) src) = evalSrc src $ \read ->
    step read (Set r) INothing
microcode (MOV (Addr rr) src) = evalSrc src $ \read ->
    step read (Get2 rr) (IJust ToPtr)
microcode XCHG = mc $
    step INothing (Get2 RHL)  INothing >++>
    step INothing (Swap2 RDE) INothing >++>
    step INothing (Swap2 RHL) INothing
microcode IN = mc $
    step (IJust IncrPC)   ToAddrBuf INothing >++>
    step (IJust FromPort) (Set RA)  INothing
microcode OUT = mc $
    step (IJust IncrPC) ToAddrBuf INothing >++>
    step INothing       (Get RA)  (IJust ToPort)
-- microcode instr = errorX $ show instr
