module Zepto.Macro(macroEval) where
import Control.Monad.Except

import Zepto.Types
import Zepto.Variables

-- | evaluates a macro
macroEval :: Env -> LispVal -> IOThrowsError LispVal
macroEval env (List [SimpleVal (Atom "define-syntax"), SimpleVal (Atom keyword), syntaxRules@(List (SimpleVal (Atom "syntax-rules") : (List _ : _)))]) = do
  _ <- defineNamespacedVar env mnamespace keyword syntaxRules
  return $ fromSimple $ Nil ""
macroEval env (List (x@(List _) : xs)) = do
  first <- macroEval env x
  rest <- mapM (macroEval env) xs
  return $ List $ first : rest
macroEval env lisp@(List (SimpleVal (Atom x) : xs)) = do
  isDefined <- liftIO $ isNamespacedBound env mnamespace x
  if isDefined
     then do
       (List (SimpleVal (Atom "syntax-rules") : (List identifiers : rules))) <- getNamespacedVar env mnamespace x
       macroEval env =<< macroTransform env (List identifiers) rules lisp
     else do
       rest <- mapM (macroEval env) xs
       return $ List $ fromSimple (Atom x) : rest
macroEval _ lisp@_ = return lisp
macroTransform :: Env -> LispVal -> [LispVal] -> LispVal -> IOThrowsError LispVal
macroTransform env identifiers (rule@(List _) : rs) input = do
  localEnv <- liftIO nullEnv
  result <- matchRule env identifiers localEnv rule input
  case result of
    SimpleVal (Nil _) -> macroTransform env identifiers rs input
    _ -> return result
macroTransform _ _ _ input = throwError $ BadSpecialForm "Input does not match a macro pattern" input

macroElementMatchesMany :: LispVal -> Bool
macroElementMatchesMany (List (_ : ps)) =
  not (null ps) &&
     (case head ps of
                SimpleVal (Atom "...") -> True
                _ -> False)
macroElementMatchesMany _ = False

matchRule :: Env -> LispVal -> Env -> LispVal -> LispVal -> IOThrowsError LispVal
matchRule _ identifiers localEnv (List [pattern, template]) (List inputVar) = do
   let is = tail inputVar
   let p = case pattern of
              DottedList ds d -> case ds of
                                  (SimpleVal (Atom l) : ls) -> List [fromSimple (Atom l), DottedList ls d]
                                  _ -> pattern
              _ -> pattern
   case p of
      List (SimpleVal (Atom _) : ps) -> do
        match <- loadLocal localEnv identifiers (List ps) (List is) False False
        case match of
           SimpleVal (Bool False) -> return $ fromSimple $ Nil ""
           _ -> transformRule localEnv 0 (List []) template (List [])
      _ -> throwError $ BadSpecialForm "Malformed rule in syntax-rules" p
matchRule _ _ _ rule input = throwError $
                              BadSpecialForm "Malformed rule in syntax-rules" $
                              List [fromSimple (Atom "rule: "), rule, fromSimple (Atom "input: "), input]

loadLocal :: Env -> LispVal -> LispVal -> LispVal -> Bool -> Bool -> IOThrowsError LispVal
loadLocal localEnv identifiers pattern input hasEllipsis outerHasEllipsis =
  case (pattern, input) of
       (DottedList ps p, DottedList is i) -> do
         result <- loadLocal localEnv identifiers (List ps) (List is) False outerHasEllipsis
         case result of
            SimpleVal (Bool True) -> loadLocal localEnv identifiers p i False outerHasEllipsis
            _ -> return $ SimpleVal (Bool False)
       (List (p : ps), List (i : is)) -> do
         let localHasEllipsis = macroElementMatchesMany pattern
         status <- checkLocal localEnv identifiers (localHasEllipsis || outerHasEllipsis) p i
         case status of
              SimpleVal (Bool False) -> if localHasEllipsis
                                then
                                    loadLocal localEnv identifiers (List $ tail ps) (List (i : is)) False outerHasEllipsis
                                else return $ fromSimple $ Bool False
              _ -> if localHasEllipsis
                      then loadLocal localEnv identifiers pattern (List is) True outerHasEllipsis
                      else loadLocal localEnv identifiers (List ps) (List is) False outerHasEllipsis
       (List [], List []) -> return $ fromSimple $ Bool True
       (List (_ : ps), List []) -> do
                                 _ <- initializePatternVars localEnv "list" identifiers pattern
                                 if macroElementMatchesMany pattern && (length ps == 1)
                                           then return $ fromSimple $ Bool True
                                           else return $ fromSimple $ Bool False
       (List [], _) -> return $ fromSimple $ Bool False
       (_, _) -> checkLocal localEnv identifiers (hasEllipsis || outerHasEllipsis) pattern input

checkLocal :: Env -> LispVal -> Bool -> LispVal -> LispVal -> IOThrowsError LispVal
checkLocal _ _ _ (SimpleVal (Bool pattern)) (SimpleVal (Bool input)) = return $ fromSimple $ Bool $ pattern == input
checkLocal _ _ _ (SimpleVal (Number pattern)) (SimpleVal (Number input)) = return $ fromSimple $ Bool $ pattern == input
checkLocal _ _ _ (SimpleVal (String pattern)) (SimpleVal (String input)) = return $ fromSimple $ Bool $ pattern == input
checkLocal _ _ _ (SimpleVal (Character pattern)) (SimpleVal (Character input)) = return $ fromSimple $ Bool $ pattern == input
checkLocal localEnv identifiers hasEllipsis (SimpleVal (Atom pattern)) input =
  if hasEllipsis
     then do isDefined <- liftIO $ isBound localEnv pattern
             isIdent <- findAtom (fromSimple (Atom pattern)) identifiers
             case isIdent of
                SimpleVal (Bool True) ->
                    case input of
                        SimpleVal (Atom inpt) ->
                            if pattern == inpt
                               then do
                                 _ <- addPatternVar isDefined $ fromSimple $ Atom pattern
                                 return $ fromSimple $ Bool True
                               else return $ fromSimple $ Bool False
                        _ -> return $ fromSimple $ Bool False
                _ -> do _ <- addPatternVar isDefined input
                        return $ fromSimple $ Bool True
     else do
         isIdent <- findAtom (fromSimple (Atom pattern)) identifiers
         case isIdent of
            SimpleVal (Bool True) ->
                case input of
                    SimpleVal (Atom inpt) ->
                        if pattern == inpt
                           then do _ <- defineVar localEnv pattern input
                                   return $ fromSimple $ Bool True
                           else return $ fromSimple $ Bool False
                    _ -> return $ fromSimple $ Bool False
            _ -> do _ <- defineVar localEnv pattern input
                    return $ fromSimple $ Bool True
    where
      addPatternVar isDefined val =
             if isDefined
                then do v <- getVar localEnv pattern
                        case v of
                          (List vs) -> setVar localEnv pattern (List $ vs ++ [val])
                          _ -> throwError $ Default "Unexpected error in checkLocal (Atom)"
                else defineVar localEnv pattern (List [val])
checkLocal localEnv identifiers hasEllipsis pattern@(DottedList _ _) input@(DottedList _ _) =
  loadLocal localEnv identifiers pattern input False hasEllipsis
checkLocal localEnv identifiers hasEllipsis pattern@(DottedList ps p) input@(List (i : is)) =
  if length ps == length is
     then loadLocal localEnv identifiers (List $ ps ++ [p]) input False hasEllipsis
     else loadLocal localEnv identifiers pattern (DottedList (i : is) (List [])) False hasEllipsis
checkLocal localEnv identifiers hasEllipsis pattern@(List _) input@(List _) =
  loadLocal localEnv identifiers pattern input False hasEllipsis
checkLocal _ _ _ _ _ = return $ fromSimple $ Bool False

transformRule :: Env -> Int -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
transformRule localEnv ellipsisIndex (List result) transform@(List (List l : ts)) (List ellipsisList) =
  if macroElementMatchesMany transform
     then do
             curT <- transformRule localEnv (ellipsisIndex + 1) (List []) (List l) (List result)
             case curT of
               SimpleVal (Nil _) -> if ellipsisIndex == 0
                           then transformRule localEnv 0 (List result) (List $ tail ts) (List [])
                           else transformRule localEnv 0 (List $ ellipsisList ++ result) (List $ tail ts) (List [])
               List [SimpleVal (Nil _), List _] -> transformRule localEnv 0 (List result) (List $ tail ts) (List [])
               List _ -> transformRule localEnv (ellipsisIndex + 1) (List $ result ++ [curT]) transform (List ellipsisList)
               _ -> throwError $ Default "Unexpected error"
     else do
             lst <- transformRule localEnv ellipsisIndex (List []) (List l) (List ellipsisList)
             case lst of
                  List [SimpleVal (Nil _), _] -> return lst
                  List _ -> transformRule localEnv ellipsisIndex (List $ result ++ [lst]) (List ts) (List ellipsisList)
                  SimpleVal (Nil _) -> return lst
                  _ -> throwError $ BadSpecialForm "Macro transform error" $ List [lst, List l, fromSimple $ Number $ NumI $ toInteger ellipsisIndex]
transformRule localEnv ellipsisIndex (List result) transform@(List (dl@(DottedList _ _) : ts)) (List ellipsisList) =
  if macroElementMatchesMany transform
     then do
             curT <- transformDottedList localEnv (ellipsisIndex + 1) (List []) (List [dl]) (List result)
             case curT of
               SimpleVal (Nil _) -> if ellipsisIndex == 0
                           then transformRule localEnv 0 (List result) (List $ tail ts) (List [])
                           else transformRule localEnv 0 (List $ ellipsisList ++ result) (List $ tail ts) (List [])
               List [SimpleVal (Nil _), List _] -> transformRule localEnv 0 (List result) (List $ tail ts) (List [])
               List t -> transformRule localEnv (ellipsisIndex + 1) (List $ result ++ t) transform (List ellipsisList)
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformDottedList localEnv ellipsisIndex (List []) (List [dl]) (List ellipsisList)
             case lst of
                  List [SimpleVal (Nil _), List _] -> return lst
                  List l -> transformRule localEnv ellipsisIndex (List $ result ++ l) (List ts) (List ellipsisList)
                  SimpleVal (Nil _) -> return lst
                  _ -> throwError $
                        BadSpecialForm "transformRule: Macro transform error" $
                        List [List ellipsisList, lst, List [dl], fromSimple $ Number $ NumI $ toInteger ellipsisIndex]
transformRule localEnv ellipsisIndex (List result) transform@(List (SimpleVal (Atom a):ts)) unused = do
  let hasEllipsis = macroElementMatchesMany transform
  isDefined <- liftIO $ isBound localEnv a
  if hasEllipsis
     then if isDefined
             then do
                  var <- getVar localEnv a
                  case var of
                    List v -> transformRule localEnv ellipsisIndex (List $ result ++ v) (List $ tail ts) unused
                    v@_ -> transformRule localEnv ellipsisIndex (List $ result ++ [v]) (List $ tail ts) unused
             else
                  transformRule localEnv ellipsisIndex (List result) (List $ tail ts) unused
     else do t <- if isDefined
                     then do var <- getVar localEnv a
                             if ellipsisIndex > 0
                                then case var of
                                          List v -> if length v > (ellipsisIndex - 1)
                                                       then return $ v !! (ellipsisIndex - 1)
                                                       else return $ fromSimple $ Nil ""
                                          _ -> throwError $ Default "Unexpected error in transformRule"
                                else return var
                     else return $ fromSimple $ Atom a
             case t of
               SimpleVal (Nil _) -> return t
               _ -> transformRule localEnv ellipsisIndex (List $ result ++ [t]) (List ts) unused
transformRule localEnv ellipsisIndex (List result) (List (t : ts)) (List ellipsisList) =
  transformRule localEnv ellipsisIndex (List $ result ++ [t]) (List ts) (List ellipsisList)
transformRule _ _ result@(List _) (List []) _ = return result
transformRule _ ellipsisIndex result transform unused =
  throwError $ BadSpecialForm "An error occurred during macro transform" $
   List [fromSimple $ Number $ NumI $ toInteger ellipsisIndex, result, transform, unused]

transformDottedList :: Env -> Int -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
transformDottedList localEnv ellipsisIndex (List result) (List (DottedList ds d : ts)) (List ellipsisList) = do
          lsto <- transformRule localEnv ellipsisIndex (List []) (List ds) (List ellipsisList)
          case lsto of
            List lst -> do
                r <- transformRule localEnv ellipsisIndex (List []) (List [d]) (List ellipsisList)
                case r of
                    List [List []] -> transformRule localEnv ellipsisIndex (List $ result ++ [List lst]) (List ts) (List ellipsisList)
                    List [rst] -> do
                        src <- lookupPatternVarSrc localEnv $ List ds
                        case src of
                            SimpleVal (String "pair") -> transformRule localEnv ellipsisIndex (List $ result ++ [DottedList lst rst]) (List ts) (List ellipsisList)
                            _ -> transformRule localEnv ellipsisIndex (List $ result ++ [List $ lst ++ [rst]]) (List ts) (List ellipsisList)
                    _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
            SimpleVal (Nil _) -> return $ List [(fromSimple (Nil "")), List ellipsisList]
            _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
transformDottedList _ _ _ _ _ = throwError $ Default "Unexpected error in transformDottedList"

findAtom :: LispVal -> LispVal -> IOThrowsError LispVal
findAtom (SimpleVal (Atom target)) (List (SimpleVal (Atom a) : as)) =
  if target == a
     then return $ fromSimple $ Bool True
     else findAtom (fromSimple (Atom target)) (List as)
findAtom _ (List (badtype : _)) = throwError $ TypeMismatch "symbol" badtype
findAtom _ _ = return $ fromSimple $ Bool False

initializePatternVars :: Env -> String -> LispVal -> LispVal -> IOThrowsError LispVal
initializePatternVars localEnv src identifiers pattern@(List _) =
    case pattern of
        List (p : ps) -> do _ <- initializePatternVars localEnv src identifiers p
                            initializePatternVars localEnv src identifiers $ List ps
        List [] -> return $ fromSimple $ Bool True
        _ -> return $ fromSimple $ Bool True
initializePatternVars localEnv src identifiers (DottedList ps p) = do
    _ <- initializePatternVars localEnv src identifiers $ List ps
    initializePatternVars localEnv src identifiers p
initializePatternVars localEnv src identifiers (SimpleVal (Atom pattern)) =
    do _ <- defineNamespacedVar localEnv mnamespace pattern $ fromSimple $ String src
       isDefined <- liftIO $ isBound localEnv pattern
       found <- findAtom (SimpleVal (Atom pattern)) identifiers
       case found of
            SimpleVal (Bool False) -> if not isDefined
                                        then defineVar localEnv pattern (List [])
                                        else return $ fromSimple $ Bool True
            _ -> return $ fromSimple $ Bool True
initializePatternVars _ _ _ _ = return $ fromSimple $ Bool True

lookupPatternVarSrc :: Env -> LispVal -> IOThrowsError LispVal
lookupPatternVarSrc localEnv pattern@(List _) =
    case pattern of
        List (p : ps) -> do result <- lookupPatternVarSrc localEnv p
                            case result of
                              SimpleVal (Bool False) -> lookupPatternVarSrc localEnv $ List ps
                              _ -> return result
        List [] -> return $ fromSimple $ Bool False
        _ -> return $ fromSimple $ Bool False
lookupPatternVarSrc localEnv (DottedList ps p) = do
    result <- lookupPatternVarSrc localEnv $ List ps
    case result of
        SimpleVal (Bool False) -> lookupPatternVarSrc localEnv p
        _ -> return result
lookupPatternVarSrc localEnv (SimpleVal (Atom pattern)) =
    do isDefined <- liftIO $ isNamespacedBound localEnv mnamespace pattern
       if isDefined then getNamespacedVar localEnv mnamespace pattern
                    else return $ fromSimple $ Bool False
lookupPatternVarSrc _ _ =
    return $ fromSimple $ Bool False
