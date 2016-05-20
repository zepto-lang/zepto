module Zepto.Prompt( runRepl
                    , runSingleStatement
                    , evalAndPrint
                    , runFile
                    ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Control.Monad

import Paths_zepto
import Zepto.Types
import Zepto.Primitives
import Zepto.Variables

-- | adds primitive bindings to an empty environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip extendEnv (fmap (makeFunc IOFunc) ioPrimitives ++
                                fmap (makeFunc PrimitiveFunc) primitives ++
                                fmap (makeFunc EvalFunc) evalPrimitives)
                where makeFunc constructor (var, func, _) = ((vnamespace, var), constructor var func)

-- | evaluate a line of code and print it
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn . (++) "=> "

stdlib :: Env -> IO String
stdlib env = do
    _ <- loadFun env "zepto-stdlib/foldl.zp"
    x <- mapM (\x -> loadFile env x) ["zepto-stdlib/let.zp",
                                  "zepto-stdlib/extra.zp",
                                  "zepto-stdlib/logical.zp",
                                  "zepto-stdlib/util.zp",
                                  "zepto-stdlib/math.zp",
                                  "zepto-stdlib/zplist.zp",
                                  "zepto-stdlib/zpstring.zp",
                                  "zepto-stdlib/definitions.zp",
                                  "zepto-stdlib/io.zp",
                                  "zepto-stdlib/pairs.zp",
                                  "zepto-stdlib/zphash.zp",
                                  "zepto-stdlib/zpgenerics.zp",
                                  "zepto-stdlib/zpvector.zp",
                                  "zepto-stdlib/zpcollections.zp",
                                  "zepto/load.zp",
                                  "zepto-stdlib/module.zp"]
    return $ last x

loadFun :: Env -> String -> IO String
loadFun env f = do
      file <- getDataFileName f
      evalString env $ "(eval (list:car (macro-expand (parse \"" ++ file ++ "\"))))"

-- | load a file into an environment
loadFile :: Env -> String -> IO String
loadFile env f = do
      file <- getDataFileName f
      evalString env $ "(begin (define env (current-env)) " ++
                               "(foldl (lambda (_ el) (eval el env)) " ++
                                       "[] " ++
                                       "(macro-expand (parse \"" ++ file ++ "\") env)))"

-- | run a single statement
runSingleStatement :: String -> IO ()
runSingleStatement statement = do
        env <- primitiveBindings
        _   <- stdlib env
        evalString env statement >>= putStrLn

-- | run a file
runFile :: [String] -> IO ()
runFile args = do
        env <- primitiveBindings >>= flip extendEnv[((vnamespace, "zepto:args"),
                                                    List $ fromSimple . String <$> drop 1 args)]
                                 >>= flip extendEnv[((vnamespace, "zepto:name"),
                                                    fromSimple $ String $ head args)]
        _   <- stdlib env
        smt  <- runIOThrows (liftM show $ eval env (nullCont env)
          (List [fromSimple (Atom "load"), fromSimple $ String $ head args]))
        putStrLn smt


-- | run the REPL
runRepl :: IO ()
runRepl = do
        repl <- getDataFileName "zepto/run-repl.zp"
        runFile [repl]
