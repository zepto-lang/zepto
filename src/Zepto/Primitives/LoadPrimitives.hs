module Zepto.Primitives.LoadPrimitives where

import Control.Monad.Except (throwError, liftIO)
import Unsafe.Coerce (unsafeCoerce)

import qualified GHC
import qualified GHC.Paths (libdir)
import qualified DynFlags

import Zepto.Types

loadNative :: [LispVal] -> IOThrowsError LispVal
loadNative [SimpleVal (String file)] = do
  result <- liftIO $ GHC.defaultErrorHandler DynFlags.defaultFatalMessager
                                         DynFlags.defaultFlushOut . GHC.runGhc (Just GHC.Paths.libdir) $ do
    dynflags <- GHC.getSessionDynFlags
    _ <- GHC.setSessionDynFlags dynflags
    target <- GHC.guessTarget (file ++ ".hs") Nothing
    GHC.addTarget target
    r <- GHC.load GHC.LoadAllTargets
    case r of
       GHC.Failed -> return $ Nothing
       GHC.Succeeded -> do
           _ <- GHC.findModule (GHC.mkModuleName file) Nothing
           GHC.setContext
             [ GHC.IIDecl $
               (GHC.simpleImportDecl . GHC.mkModuleName $ file)
               {GHC.ideclQualified = True}
             ]
           fetched <- GHC.compileExpr (file ++ ".exports")
           let res = (unsafeCoerce fetched :: [(String, [LispVal] -> IOThrowsError LispVal, String)])
           return $ Just res
  case result of
    Just res -> return $ List $ map def res
    Nothing -> return $ fromSimple $ Bool False
    where def (name, fun, doc) = List [fromSimple $ String name,
                                       fromSimple $ String doc,
                                       IOFunc name fun]
loadNative [x] = throwError $ TypeMismatch "string" x
loadNative x = throwError $ NumArgs 1 x
