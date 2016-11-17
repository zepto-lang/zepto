module Zepto.Primitives.LoadPrimitives where

import Control.Monad.Except (throwError, liftIO)
import Unsafe.Coerce (unsafeCoerce)

import qualified GHC
import qualified GHC.Paths (libdir)
import qualified DynFlags

import Zepto.Types

loadNativeDoc :: String
loadNativeDoc = "load a native (i.e. Haskell) library.\n\
  It is mostly internal and <fun>load</fun> abstracts over it.\n\
\n\
  params:\n\
    - name: the module and file name\n\
  complexity: depends on the complexity of the library\n\
  returns: a list of lists of the form [name<string>, documentation<string>, primitive<function>]"

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
           _ <- GHC.findModule (GHC.mkModuleName moduleName) Nothing
           GHC.setContext
             [ GHC.IIDecl $
               (GHC.simpleImportDecl . GHC.mkModuleName $ moduleName)
               {GHC.ideclQualified = True}
             ]
           fetched <- GHC.compileExpr (moduleName ++ ".exports")
           let res = (unsafeCoerce fetched :: [(String, [LispVal] -> IOThrowsError LispVal, String)])
           return $ Just res
  case result of
    Just res -> return $ List $ map def res
    Nothing -> return $ fromSimple $ Bool False
  where def (name, fun, doc) = List [fromSimple $ String name,
                                     fromSimple $ String doc,
                                     IOFunc name fun]
        moduleName = last $ splitSlash file
        splitSlash s =  case dropWhile (== '/') s of
                      "" -> []
                      s' -> w : splitSlash s''
                            where (w, s'') = break (== '/') s'
loadNative [x] = throwError $ TypeMismatch "string" x
loadNative x = throwError $ NumArgs 1 x
