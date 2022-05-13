{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
module Plugin.Effect.TH where

import Control.Applicative
import Control.Monad

import Data.Tuple.Solo

import FastString
import GHC (mkModule, mkModuleName)
import GhcPlugins (DefUnitId(DefUnitId), UnitId (DefiniteUnitId), InstalledUnitId (InstalledUnitId))
import Lexeme

import Generics.SYB

import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax (Name(..), NameFlavour(..), mkNameG_v, OccName (OccName), PkgName (PkgName), ModName (ModName), qRecover, trueName, falseName)


import Plugin.Lifted
import Plugin.Effect.Monad
import Plugin.Trans.TysWiredIn
import Plugin.Trans.Import (lookupSupportedBuiltInModule)

lookupBuiltin :: String -> Maybe String
lookupBuiltin "[]" = Just "NilFL"
lookupBuiltin ":"  = Just "ConsFL"
lookupBuiltin "()" = Just "UnitFL"
lookupBuiltin s | Just n <- tupleConArity s = Just $ "Tuple" ++ show n ++ "FL"
                | otherwise                 = Nothing
  where tupleConArity ('(':rest) = Just $ succ $ length $ takeWhile (== ',') rest
        tupleConArity _          = Nothing

liftTHName :: Name -> Name
liftTHName (Name (OccName str) flav) = Name withSuffix flav'
  where
    withSuffix | Just str' <- lookupBuiltin str = OccName str'
               | isLexVarSym (mkFastString str) = OccName $ str ++ "#"
               | isLexConSym (mkFastString str) = OccName $ str ++ "#"
               | otherwise                      = OccName $ str ++ "FL"
    flav' = case flav of
      NameS -> NameS
      NameQ mn -> NameQ mn
      NameU n -> NameU n
      NameL n -> NameL n
      NameG ns (PkgName pn) (ModName mn) ->
        let ghcModule = mkModule (DefiniteUnitId (DefUnitId (InstalledUnitId (mkFastString pn)))) (mkModuleName mn)
            (pkgNm', mdlNm') = maybe (pn, mn) (thisPkgName,) $
                               lookupSupportedBuiltInModule ghcModule
        in NameG ns (PkgName pkgNm') (ModName mdlNm')

liftTHNameQ :: Name -> Q Name
liftTHNameQ name = do
  let liftedName = liftTHName name
  reifiable <- qRecover (return False) (reify liftedName >> return True)
  if reifiable
    then return liftedName
    else fail $ "No inverse for " ++ show name

var :: Integer -> a
var _ = error "var is undefined outside of input and output classes"

getFunArity :: Name -> Q Int
getFunArity name = do
  info <- reify name
  (_, _, ty) <- decomposeForallT <$> case info of
    VarI _ ty' _     -> return ty'
    ClassOpI _ ty' _ -> return ty'
    _                -> fail $ show name ++ " is no function or class method"
  return $ arrowArity ty

getConArity :: Name -> Q Int
getConArity name = do
  info <- reify name
  (_, _, ty) <- decomposeForallT <$> case info of
    DataConI _ ty' _ -> return ty'
    _                -> fail $ show name ++ " is no data constructor"
  return $ arrowArity ty

--TODO: use elsewhere
thisPkgName :: String
thisPkgName = case 'toFL of
  Name _ (NameG _ (PkgName s) _) -> s
  _                              -> error "could not deduce package name of the plugin package"

--TODO: lift to q and throw error when length of list is > maxTupleArity
mkLiftedTupleE :: [Exp] -> Exp
mkLiftedTupleE xs  = AppE (VarE 'return) (applyExp liftedTupleConE xs)
  where
    -- TODO does this really work?
    liftedTupleConE = ConE $ mkNameG_v thisPkgName builtInModule tupleConName
    tupleConName | null xs        = "UnitFL"
                 | length xs == 1 = "SoloFL"
                 | otherwise      = "Tuple" ++ show (length xs) ++ "FL"

decomposeForallT :: Type -> ([TyVarBndr], [Type], Type)
decomposeForallT (ForallT bndrs ctxt ty) =
  let (bndrs', ctxt', ty') = decomposeForallT ty
  in (bndrs ++ bndrs', ctxt ++ ctxt', ty')
decomposeForallT ty                      = ([], [], ty)

getTyVarBndrName :: TyVarBndr -> Name
getTyVarBndrName (PlainTV  name  ) = name
getTyVarBndrName (KindedTV name _) = name

mkFreeP :: Exp -> Exp
mkFreeP = AppE (VarE 'Plugin.Effect.Monad.free)

mkIntExp :: Int -> Exp
mkIntExp = LitE . IntegerL . fromIntegral

applyExp :: Exp -> [Exp] -> Exp
applyExp = foldl AppE

applyExp' :: ExpQ -> [ExpQ] -> ExpQ
applyExp' = foldl appE

unapplyExp :: Exp -> (Exp, [Exp])
unapplyExp (AppE e1 e2) = let (e, es) = unapplyExp e1 in (e, es ++ [e2])
unapplyExp e            = (e, [])

genLiftedApply :: Exp -> [Exp] -> ExpQ
genLiftedApply = foldM (\f arg -> newName "v" >>= \vName -> return $ applyExp (VarE '(>>=)) [f, LamE [ConP 'Func [VarP vName]] $ AppE (VarE vName) arg])

mkTupleE :: [Exp] -> Exp
mkTupleE [e] = AppE (ConE 'Solo) e
mkTupleE es  = TupE $ map Just es

mkTupleT :: [Type] -> Type
mkTupleT [ty] = AppT (ConT ''Solo) ty
mkTupleT tys  = applyType (TupleT (length tys)) tys

mkTupleP :: [Pat] -> Pat
mkTupleP [p] = ConP 'Solo [p]
mkTupleP ps' = TupP ps'

mkArrowT :: Type -> Type -> Type
mkArrowT ty1 ty2 = applyType ArrowT [ty1, ty2]

applyType :: Type -> [Type] -> Type
applyType = foldl AppT

arrowUnapply :: Type -> ([Type], Type)
arrowUnapply (AppT (AppT ArrowT ty1) ty2) = (ty1 : tys, ty)
  where (tys, ty) = arrowUnapply ty2
arrowUnapply ty                           = ([], ty)

arrowArity :: Type -> Int
arrowArity = length . fst . arrowUnapply

--------------------------------------------------------------------------------

genLiftedTupleDataDecl :: Int -> Q (Dec, Dec)
genLiftedTupleDataDecl ar = do
  let name = mkName $ "Tuple" ++ show ar ++ "FL"
  mVar <- newName "m"
  tyVarNames <- replicateM ar (newName "a")
  let con = NormalC name $ map (\tyVarName -> (Bang NoSourceUnpackedness NoSourceStrictness, AppT (VarT mVar) (VarT tyVarName))) tyVarNames
  let da = DataD [] name (KindedTV mVar (AppT (AppT ArrowT StarT) StarT) : map PlainTV tyVarNames) Nothing [con] []
  let ki = KiSigD name ((StarT `mkArrowT` StarT) `mkArrowT` foldr mkArrowT StarT (replicate ar StarT))
  return (da, ki)

genLiftedTupleDataDeclAndInstances :: Int -> DecsQ
genLiftedTupleDataDeclAndInstances ar = do
  TyConI originalDataDecl <- reify $ tupleTypeName ar
  (liftedDataDecl, kiSig) <- genLiftedTupleDataDecl ar
  instances <- genInstances originalDataDecl liftedDataDecl
  return $ liftedDataDecl : kiSig : instances

--------------------------------------------------------------------------------

data ConInfo = ConInfo { conName :: Name, conArgs :: [Type] }

conArity :: ConInfo -> Int
conArity = length . conArgs

extractConInfo :: (Type -> Type) -> Con -> ConInfo
extractConInfo f (NormalC n btys) = ConInfo n (map (f . snd) btys)
extractConInfo _ _ = error "mkConInfo: unsupported"
--TODO: missing constructors

data TcInfo = TcInfo { tcName :: Name, tcVarNames :: [Name], tcConInfos :: [ConInfo] }

tcArity :: TcInfo -> Int
tcArity = length . tcVarNames

extractTcInfo :: (Type -> Type) -> Dec -> TcInfo
extractTcInfo f (DataD _ tcNm tyVars _ cons _) = TcInfo tcNm (map getTyVarBndrName tyVars) (map (extractConInfo f) cons)
extractTcInfo f (NewtypeD _ tcNm tyVars _ con _) = TcInfo tcNm (map getTyVarBndrName tyVars) [extractConInfo f con]
extractTcInfo _ _ = error "extractTcInfo: unsupported"

renameTcInfo :: String -> TcInfo -> TcInfo
renameTcInfo suffix (TcInfo tcNm vs cis) = TcInfo tcNm (map rename vs) (map renameConInfo cis)
  where renameConInfo (ConInfo conNm tys) = ConInfo conNm (map renameType tys)
        renameType = everywhere (mkT renameVar)
        renameVar (VarT varNm) = VarT (rename varNm)
        renameVar ty = ty
        rename (Name (OccName str) nf) = Name (OccName (str ++ suffix)) nf

mkNarrowEntry :: Name -> ConInfo -> Exp
mkNarrowEntry idName conInfo = mkTupleE [conExp, mkIntExp (conArity conInfo)]
  where conExp = foldl (\conExp' idOffset -> AppE conExp' (mkFreeP (mkMinus (VarE idName) (mkIntExp idOffset)))) (ConE $ conName conInfo) [0 .. conArity conInfo - 1]

mkPlus :: Exp -> Exp -> Exp
mkPlus e1 e2 = applyExp (VarE '(+)) [e1, e2]

mkMinus :: Exp -> Exp -> Exp
mkMinus e1 e2 = applyExp (VarE '(-)) [e1, e2]

genMTy :: Q Type
genMTy = VarT <$> newName "m"

genLifted :: Type -> Type -> [Type] -> Type -> DecsQ
genLifted originalTc liftedTc liftedTyVars mTy = do
  let genLiftedApp n =
        let relevantVars = take n liftedTyVars
        in TySynInstD $ TySynEqn Nothing (mkLifted mTy (applyType originalTc relevantVars)) (applyType liftedTc (map (mkLifted mTy) relevantVars))
  return $ map genLiftedApp [0 .. length liftedTyVars]

genInstances :: Dec -> Dec -> DecsQ
genInstances (ClassD _ originalName _ _ _) (ClassD _ liftedName liftedTyVarBndrs _ _) =
  let liftedTyVarNames = map getTyVarBndrName liftedTyVarBndrs
      liftedTyVars = map VarT liftedTyVarNames
      originalTc = ConT originalName
      liftedTc = ConT liftedName
  in genLifted originalTc liftedTc liftedTyVars (ConT ''FL)
genInstances (TySynD _ _ _) (TySynD _ _ _) = do
  return []
genInstances originalDataDec liftedDataDec = do

  let originalTcInfo = renameTcInfo "a" $ extractTcInfo id originalDataDec
      originalTc = ConT $ tcName originalTcInfo
      originalTyVarNames = tcVarNames originalTcInfo
      originalTyVars = map VarT originalTyVarNames
      originalTy = applyType (ConT $ tcName originalTcInfo) originalTyVars
      originalConInfos = tcConInfos originalTcInfo
      originalConArgs = concatMap conArgs originalConInfos
  let liftedTcInfo = renameTcInfo "b" $ extractTcInfo innerType liftedDataDec
      liftedTc = AppT (ConT $ tcName liftedTcInfo) mTy
      (mvar, liftedTyVarNames) = case tcVarNames liftedTcInfo of { x:xs -> (x,xs); _ -> error "TH: unexpected unlifted constructor"} -- Discard the monad parameter here
      liftedTyVars = map VarT liftedTyVarNames
      liftedTy = applyType (ConT $ tcName liftedTcInfo) $ ConT ''FL : liftedTyVars
      liftedConInfos = tcConInfos liftedTcInfo
      liftedConArgs = map (replaceMTyVar mvar (ConT ''FL)) $ concatMap conArgs liftedConInfos
      mTy = VarT mvar

  let genHasPrimitiveInfo = do
        let body = NormalB $ ConE 'NoPrimitive
            dec = FunD 'primitiveInfo [Clause [] body []]
            ctxt = map mkHasPrimitiveInfoConstraint liftedConArgs
        return $ InstanceD Nothing ctxt (mkHasPrimitiveInfoConstraint liftedTy) [dec]

      genNarrowable = do
        jName <- newName "j"
        let entries = map (mkNarrowEntry jName) liftedConInfos
            body = NormalB $ ListE entries
            dec = FunD 'narrow [Clause [VarP jName] body []]
            ctxt = map mkHasPrimitiveInfoConstraint liftedConArgs
        return $ InstanceD Nothing ctxt (mkNarrowableConstraint liftedTy) [dec]

      genTo = do
        let genTo' = do
              tf <- newName "tf"
              let genMatch originalConInfo liftedConInfo = do
                    argNames <- replicateM (conArity originalConInfo) (newName "x")
                    let pat = ConP (conName originalConInfo) $ map VarP argNames
                        body = NormalB $ applyExp (ConE $ conName liftedConInfo) $ map (AppE (VarE tf) . VarE) argNames
                    return $ Match pat body []
              matches <- zipWithM genMatch originalConInfos liftedConInfos
              arg <- newName "arg"
              return $ FunD 'toWith [Clause [VarP tf, VarP arg] (NormalB (CaseE (VarE arg) matches)) []]
        let ctxt = map mkToConstraint originalConArgs
        InstanceD Nothing ctxt (mkToConstraint originalTy) <$>
          sequence [genTo']

      genFrom = do
        let genFrom' = do
              ff <- newName "ff"
              let genMatch liftedConInfo originalConInfo = do
                    argNames <- replicateM (conArity liftedConInfo) (newName "x")
                    let pat = ConP (conName liftedConInfo) $ map VarP argNames
                        body = NormalB $ applyExp (ConE $ conName originalConInfo) $ map (AppE (VarE ff) . VarE) argNames
                    return $ Match pat body []
              arg <- newName "arg"
              matches <- zipWithM genMatch liftedConInfos originalConInfos
              return $ FunD 'fromWith [Clause [VarP ff, VarP arg] (NormalB (CaseE (VarE arg) matches)) []]
        let ctxt = map mkFromConstraint originalConArgs
        InstanceD Nothing ctxt (mkFromConstraint originalTy) <$>
          sequence [genFrom']

      genMatchable = do
        let genMatch = do
              let genClause originalConInfo liftedConInfo = do
                    originalArgNames <- replicateM (conArity originalConInfo) (newName "x")
                    liftedArgNames <- replicateM (conArity liftedConInfo) (newName "y")
                    let originalPat = ConP (conName originalConInfo) $ map VarP originalArgNames
                        liftedPat = ConP (conName liftedConInfo) $ map VarP liftedArgNames
                        body = NormalB $ foldr (\e1 e2 -> applyExp (VarE '(>>)) [e1, e2]) (AppE (VarE 'return) (ConE '())) $ zipWith (\originalArgName liftedArgName -> applyExp (VarE 'matchFL) [VarE originalArgName, VarE liftedArgName]) originalArgNames liftedArgNames
                    return $ Clause [originalPat, liftedPat] body []
              clauses <- zipWithM genClause originalConInfos liftedConInfos
              let failClause = Clause [WildP, WildP] (NormalB $ VarE 'Control.Applicative.empty) []
              return $ FunD 'match (clauses ++ [failClause])
        dec <- genMatch
        let ctxt = map mkToConstraint originalConArgs ++
                     map mkMatchableConstraint originalConArgs
        return $ InstanceD Nothing ctxt (mkMatchableConstraint originalTy) [dec]

      genUnifiable = do
        let genUnify = do
              let genClause liftedConInfo = do
                    let liftedConArity = conArity liftedConInfo
                        liftedConName = conName liftedConInfo
                    liftedArgNames1 <- replicateM liftedConArity (newName "x")
                    liftedArgNames2 <- replicateM liftedConArity (newName "y")
                    let liftedPat1 = ConP liftedConName $ map VarP liftedArgNames1
                        liftedPat2 = ConP liftedConName $ map VarP liftedArgNames2
                        body = NormalB $ foldr (\e1 e2 -> applyExp (VarE '(>>)) [e1, e2]) (AppE (VarE 'return) (ConE '())) $ zipWith (\liftedArgName1 liftedArgName2 -> applyExp (VarE 'lazyUnifyFL) [VarE liftedArgName1, VarE liftedArgName2]) liftedArgNames1 liftedArgNames2
                    return $ Clause [liftedPat1, liftedPat2] body []
              clauses <- mapM genClause liftedConInfos
              let failClause = Clause [WildP, WildP] (NormalB $ VarE 'Control.Applicative.empty) []
              return $ FunD 'lazyUnify (clauses ++ [failClause])
        dec <- genUnify
        let ctxt = map mkUnifiableConstraint originalConArgs
        return $ InstanceD Nothing ctxt (mkUnifiableConstraint originalTy) [dec]

      genNormalForm = do
        nfName <- newName "nf"
        let genMatch liftedConInfo = do
              let liftedConName = conName liftedConInfo
                  liftedConArity = conArity liftedConInfo
              liftedArgNames <- replicateM liftedConArity (newName "x")
              freshLiftedArgNames <- replicateM liftedConArity (newName "y")
              let pat = ConP liftedConName $ map VarP liftedArgNames
                  body = NormalB $ foldr (\ (liftedArgName, freshLiftedArgName) e -> applyExp (VarE '(>>=)) [AppE (VarE nfName) (VarE liftedArgName), LamE [VarP freshLiftedArgName] e]) (AppE (VarE 'return) $ AppE (ConE 'Result) $ AppE (VarE 'pure) $ applyExp (ConE liftedConName) $ map VarE freshLiftedArgNames) $ zip liftedArgNames freshLiftedArgNames
              return $ Match pat body []
        matches <- mapM genMatch liftedConInfos
        let body = NormalB (LamCaseE matches)
            dec = FunD 'normalFormWith [Clause [VarP nfName] body []]
            ctxt = map mkNormalFormConstraint originalConArgs
        return $ InstanceD Nothing ctxt (mkNormalFormConstraint originalTy) [dec]

      genShowFree = do
        --TODO: improve for infix declarations
        let genShowsFreePrec' = do
              let genClause originalConInfo = do
                    dName <- newName "d"
                    let originalConArity = conArity originalConInfo
                        originalConName@(Name (OccName originalConOccString) _) = conName originalConInfo
                    originalArgNames <- replicateM originalConArity (newName "x")
                    let originalPat = ConP (conName originalConInfo) $ map VarP originalArgNames
                    let isTuple = originalConName == tupleDataName originalConArity
                        isInfix = head originalConOccString == ':'
                    body <- NormalB <$> if isTuple
                      then [| showString "(" . $(foldl (\qExp originalArgName -> [| $(qExp) . showString "," . showsFree $(return $ VarE originalArgName) |]) [| showsFree $(return $ VarE $ head originalArgNames) |] (tail originalArgNames)) . showString ")" |]
                      else [| showParen ($(return $ ConE $ if originalConArity > 0 then trueName else falseName) && $(return $ VarE dName) > 10) $(foldl (\qExp originalArgExp -> [| $(qExp) . showString " " . showsFreePrec 11 $(return originalArgExp) |]) [| showParen $(return $ ConE $ if isInfix then trueName else falseName) (showString $(return $ LitE $ StringL originalConOccString)) |] (map VarE originalArgNames)) |]
                    return $ Clause [if isTuple then WildP else VarP dName, originalPat] body []
              clauses <- mapM genClause originalConInfos
              return $ FunD 'showsFreePrec' clauses
        dec <- genShowsFreePrec'
        let ctxt = map mkShowFreeConstraint originalConArgs
        return $ InstanceD Nothing ctxt (mkShowFreeConstraint originalTy) [dec]

      genInvertible = do
        let ctxt = [ mkToConstraint originalTy
                   , mkFromConstraint originalTy
                   , mkMatchableConstraint originalTy
                   , mkUnifiableConstraint originalTy
                   , mkNormalFormConstraint originalTy
                   , mkHasPrimitiveInfoConstraint (mkLifted (ConT ''FL) originalTy)
                   , mkShowFreeConstraint originalTy
                   ] ++ map mkInvertibleConstraint originalConArgs
        return $ InstanceD Nothing ctxt (mkInvertibleConstraint originalTy) [] :: Q Dec

  (++) <$> genLifted originalTc liftedTc liftedTyVars mTy
       <*> sequence [genHasPrimitiveInfo, genNarrowable, genTo, genFrom, genMatchable, genUnifiable, genNormalForm, genShowFree, genInvertible]

replaceMTyVar :: Name -> Type -> Type -> Type
replaceMTyVar tvar replacement = go
  where
    go :: Type -> Type
    go ty = case ty of
      AppT ty1 ty2       -> AppT (go ty1) (go ty2)
      VarT n | n == tvar -> replacement
             | otherwise -> VarT n
      _                  -> ty

innerType :: Type -> Type
innerType (AppT (VarT _) ty) = ty
innerType ty = error $ "Plugin.Effect.TH.innerType: incorrect usage on -> " ++ show ty

mkLifted :: Type -> Type -> Type
mkLifted mty ty = applyType (ConT ''Lifted) [mty, ty]

mkHasPrimitiveInfoConstraint :: Type -> Type
mkHasPrimitiveInfoConstraint ty = applyType (ConT ''HasPrimitiveInfo) [ty]

mkNarrowableConstraint :: Type -> Type
mkNarrowableConstraint ty = applyType (ConT ''Narrowable) [ty]

mkToConstraint :: Type -> Type
mkToConstraint ty = applyType (ConT ''To) [ty]

mkFromConstraint :: Type -> Type
mkFromConstraint ty = applyType (ConT ''From) [ty]

mkMatchableConstraint :: Type -> Type
mkMatchableConstraint ty = applyType (ConT ''Matchable) [ty]

mkUnifiableConstraint :: Type -> Type
mkUnifiableConstraint ty = applyType (ConT ''Unifiable) [ty]

mkShowFreeConstraint :: Type -> Type
mkShowFreeConstraint ty = applyType (ConT ''ShowFree) [ty]

mkNormalFormConstraint :: Type -> Type
mkNormalFormConstraint ty = applyType (ConT ''NormalForm) [ty]

mkInvertibleConstraint :: Type -> Type
mkInvertibleConstraint ty = applyType (ConT ''Invertible) [ty]
