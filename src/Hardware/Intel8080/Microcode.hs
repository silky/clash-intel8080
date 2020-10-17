{-# LANGUAGE RankNTypes #-}
module Hardware.Intel8080.Microcode where

import Clash.Prelude

import Hardware.Intel8080
import Hardware.Intel8080.Steps
import Data.Wedge

data InAddr
    = FromPtr
    | FromPort
    | IncrPC
    | IncrSP
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, Lift)

data OutAddr
    = ToPtr
    | ToPort
    | DecrSP
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, Lift)

data UpdateAC
    = SetAC
    | KeepAC
    deriving (Show, Eq, Generic, NFDataX, Lift)

data UpdateC
    = SetC
    | KeepC
    deriving (Show, Eq, Generic, NFDataX, Lift)

data MicroInstr
    = FromReg Reg
    | ToReg Reg
    | FromPC
    | FromAddrBuf
    | ToAddrBuf
    | Get2 RegPair
    | Swap2 RegPair
    | Jump
    | When (Maybe Cond)
    | Compute ALU ALUArg UpdateC UpdateAC
    | ComputeSR (Either ShiftRotate ShiftRotate)
    | Compute2 ALU2
    | Compute0 Flag ALU0
    | UpdateFlags
    | Rst (Unsigned 3)
    | SetInt Bool
    | FixupBCD
    | Halt
    deriving (Show, Generic, NFDataX, Lift)

data ALUArg
    = RegA
    | AddrLo
    | Const01
    | ConstFF
    deriving (Show, Generic, NFDataX, Lift)

data ALU2
    = Inc2
    | Dec2
    deriving (Show, Generic, NFDataX, Lift)

data ALU0
    = Complement0
    | ConstTrue0
    | ConstFalse0
    deriving (Show, Generic, NFDataX, Lift)

type MicroSteps = Steps InAddr MicroInstr OutAddr

imm2 :: MicroSteps 2 True False
imm2 =
    step (IJust IncrPC) ToAddrBuf INothing >++>
    step (IJust IncrPC) ToAddrBuf INothing

push2 :: MicroSteps 2 False True
push2 =
    step INothing FromAddrBuf (IJust DecrSP) >++>
    step INothing FromAddrBuf (IJust DecrSP)

pushPC :: MicroSteps 2 False True
pushPC =
    step INothing FromPC (IJust DecrSP) >++>
    step INothing FromPC (IJust DecrSP)

pop2 :: MicroSteps 2 True False
pop2 =
    step (IJust IncrSP) ToAddrBuf INothing >++>
    step (IJust IncrSP) ToAddrBuf INothing

type MicroOp = (MicroInstr, Wedge OutAddr InAddr)
type MicroLen = 8
type Microcode = (Maybe InAddr, Vec MicroLen MicroOp)

mc :: (KnownNat k, (1 + n + k) ~ MicroLen) => MicroSteps (1 + n) pre post -> Microcode
mc ops = let (first, ops') = stepsOf ops
         in (first, ops' ++ repeat (When Nothing, Nowhere))

evalSrc
    :: (KnownNat k, KnownNat k', ((1 + n) + k) ~ MicroLen, ((1 + (1 + n)) + k') ~ MicroLen)
    => RHS
    -> (forall pre. IMaybe pre InAddr -> MicroSteps (1 + n) pre post)
    -> Microcode
evalSrc src k = case src of
    Imm -> mc $
        k (IJust IncrPC)
    LHS (Reg r) -> mc $
        step INothing (FromReg r) INothing >++>
        k INothing
    LHS (Addr rr) -> mc $
        step INothing (Get2 rr) INothing >++>
        k (IJust FromPtr)

microcode :: Instr -> Microcode
microcode NOP = mc $ step INothing (When Nothing) INothing
microcode (INT b) = mc $ step INothing (SetInt b) INothing
microcode CMA = mc $
    step INothing (FromReg RA)                               INothing >++>
    step INothing (Compute (Sub False) ConstFF KeepC KeepAC) INothing >++>
    step INothing (ToReg RA)                                 INothing
microcode CMC = mc $
    step INothing (Compute0 FC Complement0) INothing
microcode STC = mc $
    step INothing (Compute0 FC ConstTrue0) INothing
microcode (ALU fun src) = evalSrc src $ \read ->
    step read     (Compute fun RegA SetC SetAC) INothing >++>
    step INothing UpdateFlags                   INothing >++>
    step INothing (ToReg RA)                    INothing
microcode (CMP src) = evalSrc src $ \read ->
    step read     (Compute (Sub False) RegA SetC SetAC) INothing >++>
    step INothing UpdateFlags                           INothing
microcode (SHROT sr) = mc $
    step INothing (FromReg RA)   INothing >++>
    step INothing (ComputeSR sr) INothing >++>
    step INothing (ToReg RA)     INothing
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
    step (IJust FromPtr) (ToReg RA) INothing
microcode STA = mc $
    imm2 >++>
    step INothing (FromReg RA) (IJust ToPtr)
microcode (DCX rr) = mc $
    step INothing (Get2 rr)       INothing >++>
    step INothing (Compute2 Dec2) INothing >++>
    step INothing (Swap2 rr)      INothing
microcode (INX rr) = mc $
    step INothing (Get2 rr)       INothing >++>
    step INothing (Compute2 Inc2) INothing >++>
    step INothing (Swap2 rr)      INothing
microcode (DAD rr) = mc $
    step INothing (Get2 rr)            INothing                     >++>
    step INothing (FromReg RL)         INothing                     >++>
    step INothing (Compute (Add False) AddrLo SetC KeepAC) INothing >++>
    step INothing ToAddrBuf            INothing                     >++>
    step INothing (FromReg RH)         INothing                     >++>
    step INothing (Compute (Add True)  AddrLo SetC KeepAC) INothing >++>
    step INothing ToAddrBuf            INothing                     >++>
    step INothing (Swap2 RHL)          INothing
microcode (INR (Addr rr)) = mc $
    step INothing        (Get2 rr)                                 INothing >++>
    step (IJust FromPtr) (Compute (Add False) Const01 KeepC SetAC) INothing >++>
    step INothing        UpdateFlags                               (IJust ToPtr)
microcode (INR (Reg r)) = mc $
    step INothing (FromReg r)                               INothing >++>
    step INothing (Compute (Add False) Const01 KeepC SetAC) INothing >++>
    step INothing UpdateFlags                               INothing >++>
    step INothing (ToReg r)                                 INothing
microcode (DCR (Addr rr)) = mc $
    step INothing        (Get2 rr)                                 INothing >++>
    step (IJust FromPtr) (Compute (Add False) ConstFF KeepC SetAC) INothing >++>
    step INothing        UpdateFlags                               (IJust ToPtr)
microcode (DCR (Reg r)) = mc $
    step INothing (FromReg r)                               INothing >++>
    step INothing (Compute (Add False) ConstFF KeepC SetAC) INothing >++>
    step INothing UpdateFlags                               INothing >++>
    step INothing (ToReg r)                                 INothing
microcode DAA = mc $
    step INothing (FromReg RA) INothing >++>
    step INothing FixupBCD     INothing >++>
    step INothing UpdateFlags  INothing >++>
    step INothing (ToReg RA)   INothing
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
    step (IJust FromPtr) (ToReg RL)      INothing >++>
    step INothing        (Compute2 Inc2) INothing >++>
    step (IJust FromPtr) (ToReg RH)      INothing
microcode SHLD = mc $
    imm2 >++>
    step INothing (FromReg RL)    (IJust ToPtr) >++>
    step INothing (Compute2 Inc2) INothing      >++>
    step INothing (FromReg RH)    (IJust ToPtr)
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
    step read (ToReg r) INothing
microcode (MOV (Addr rr) src) = evalSrc src $ \read ->
    step read (Get2 rr) (IJust ToPtr)
microcode XCHG = mc $
    step INothing (Get2 RHL)  INothing >++>
    step INothing (Swap2 RDE) INothing >++>
    step INothing (Swap2 RHL) INothing
microcode IN = mc $
    step (IJust IncrPC)   ToAddrBuf  INothing >++>
    step (IJust FromPort) (ToReg RA) INothing
microcode OUT = mc $
    step (IJust IncrPC) ToAddrBuf    INothing >++>
    step INothing       (FromReg RA) (IJust ToPort)
microcode HLT = mc $
    step INothing Halt INothing
-- microcode instr = errorX $ show instr
