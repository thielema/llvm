{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Core.CodeGenMonad(
    -- * Module code generation
    CodeGenModule, runCodeGenModule, genMSym, getModule,
    GlobalMappings(..), addGlobalMapping, getGlobalMappings,
    -- * Function code generation
    CodeGenFunction, runCodeGenFunction, liftCodeGenModule, genFSym, getFunction, getBuilder, getFunctionModule, getExterns, putExterns,
    -- * Reexport
    liftIO
    ) where
import Data.Typeable
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, get, gets, put, modify, )
import Control.Monad.IO.Class (MonadIO, liftIO, )
import Control.Monad.Fix (MonadFix, )
import Control.Applicative (Applicative, )

import Foreign.Ptr (Ptr, )

import LLVM.Core.Util(Module, Builder, Function)

--------------------------------------

data CGMState = CGMState {
    cgm_module :: Module,
    cgm_externs :: [(String, Function)],
    cgm_global_mappings :: [(Function, Ptr ())],
    cgm_next :: !Int
    }
    deriving (Show, Typeable)
newtype CodeGenModule a = CGM (StateT CGMState IO a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, Typeable)

genMSym :: String -> CodeGenModule String
genMSym prefix = do
    s <- CGM get
    let n = cgm_next s
    CGM $ put (s { cgm_next = n + 1 })
    return $ "_" ++ prefix ++ show n

getModule :: CodeGenModule Module
getModule = CGM $ gets cgm_module

runCodeGenModule :: Module -> CodeGenModule a -> IO a
runCodeGenModule m (CGM body) = do
    let cgm = CGMState { cgm_module = m, cgm_next = 1, cgm_externs = [], cgm_global_mappings = [] }
    evalStateT body cgm

--------------------------------------

data CGFState r = CGFState {
    cgf_module :: CGMState,
    cgf_builder :: Builder,
    cgf_function :: Function,
    cgf_next :: !Int
    }
    deriving (Show, Typeable)

newtype CodeGenFunction r a = CGF (StateT (CGFState r) IO a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, Typeable)

genFSym :: CodeGenFunction a String
genFSym = do
    s <- CGF get
    let n = cgf_next s
    CGF $ put (s { cgf_next = n + 1 })
    return $ "_L" ++ show n

getFunction :: CodeGenFunction a Function
getFunction = CGF $ gets cgf_function

getBuilder :: CodeGenFunction a Builder
getBuilder = CGF $ gets cgf_builder

getFunctionModule :: CodeGenFunction a Module
getFunctionModule = CGF $ gets (cgm_module . cgf_module)

getExterns :: CodeGenFunction a [(String, Function)]
getExterns = CGF $ gets (cgm_externs . cgf_module)

putExterns :: [(String, Function)] -> CodeGenFunction a ()
putExterns es = do
    cgf <- CGF get
    let cgm' = (cgf_module cgf) { cgm_externs = es }
    CGF $ put (cgf { cgf_module = cgm' })

addGlobalMapping ::
    Function -> Ptr () -> CodeGenModule ()
addGlobalMapping value func = CGM $ modify $ \cgm ->
        cgm { cgm_global_mappings =
                 (value,func) : cgm_global_mappings cgm }

newtype GlobalMappings =
   GlobalMappings [(Function, Ptr ())]

{- |
Get a list created by calls to 'staticFunction'
that must be passed to the execution engine
via 'LLVM.ExecutionEngine.addGlobalMappings'.
-}
getGlobalMappings ::
    CodeGenModule GlobalMappings
getGlobalMappings =
    CGM $ gets (GlobalMappings . cgm_global_mappings)

runCodeGenFunction :: Builder -> Function -> CodeGenFunction r a -> CodeGenModule a
runCodeGenFunction bld fn (CGF body) = do
    cgm <- CGM get
    let cgf = CGFState { cgf_module = cgm,
                         cgf_builder = bld,
                         cgf_function = fn,
                         cgf_next = 1 }
    (a, cgf') <- liftIO $ runStateT body cgf
    CGM $ put (cgf_module cgf')
    return a

--------------------------------------

-- | Allows you to define part of a module while in the middle of defining a function.
liftCodeGenModule :: CodeGenModule a -> CodeGenFunction r a
liftCodeGenModule (CGM act) = do
    cgf <- CGF get
    (a, cgm') <- liftIO $ runStateT act (cgf_module cgf)
    CGF $ put (cgf { cgf_module = cgm' })
    return a
