{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Plugin.Trans.Import
Description : A Renamer plugin to check if imported modules are compatible
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module implements the Renamer plugin that checks if all imported
modules have been compiled with the plugin or marked as compatible.
-}
module Plugin.Trans.Import (processImportPlugin, supportedBuiltInModules) where

import Prelude hiding ((<>))
import Data.Maybe
import Control.Monad

import GHC.Hs.Extension
import GHC.Hs.Decls
import TcRnTypes
import GhcPlugins
import TcRnMonad
import PrelNames

import Plugin.Effect.Annotation
import Plugin.Trans.Util

-- | A plugin that checks if all imported modules
-- have been compiled with the plugin or marked as compatible.
-- Will produce an error message otherwise.
processImportPlugin :: TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
processImportPlugin env grp = checkImports env >> return (env, grp)

data IfaceLoadResult = LoadSuccess Module
                     | LoadFail Module (Maybe SrcSpan)

-- | A plugin that checks if all imported modules
-- have been compiled with the plugin or marked as compatible.
-- Will produce an error message otherwise.
checkImports :: TcGblEnv -> TcM ()
checkImports env = do
  -- Get the list of all external and home pacckage modules.
  externalRef <- hsc_EPS <$> getTopEnv
  external <- readTcRef externalRef
  home <- hsc_HPT <$> getTopEnv
  -- Get the name of the current compilation unit/module.
  let unit = moduleUnitId (tcg_semantic_mod env)
  -- Get the environment of all external annotations.
  let annEnvExt = eps_ann_env external
  -- Get the annotations for each imported module, except Data.Kind.
  -- Data.Kind is special and allowed in the Plugin
  let mostMdls = filter (not . isSupportedBuiltInModule) $ allImportedMdls env
  let anns = map (uncurry (getAnnFor unit home annEnvExt)) mostMdls
  -- Check if the annotations for every module contain the correct marker
  let lds = map (uncurry3 classifyWithLoadResult) anns
  -- Create an error for each incorrect import
  mapM_ errorOnFailedLoad lds
  -- Fail if at least one error was recorded.
  failIfErrsM

-- If you add a module here, you need to import its related lifted module in the lifted Prelude.
supportedBuiltInModules :: [((String, UnitId), String)]
supportedBuiltInModules =
  [ (("Prelude",                baseUnitId), "Plugin.BuiltIn")
  , (("Data.Kind",              baseUnitId), "Plugin.BuiltIn")
  , (("GHC.Exts",               baseUnitId), "Plugin.BuiltIn")
  , (("GHC.Show",               baseUnitId), "Plugin.BuiltIn")
  , (("Data.Char",              baseUnitId), "Plugin.BuiltIn.Char")
  , (("GHC.Char",               baseUnitId), "Plugin.BuiltIn.Char")
  , (("Data.Functor.Identity",  baseUnitId), "Plugin.BuiltIn.Identity")
  ]

isSupportedBuiltInModule :: (Module, [ImportedBy]) -> Bool
isSupportedBuiltInModule (Module u n, _) =
  n == mkModuleName "Plugin.BuiltIn" || (n, u) `elem` map ((\ (a, b) -> (mkModuleName a, b)) . fst) supportedBuiltInModules

-- | Get any 'NondetTag' module annotations for a given module
-- and the source span of the import declaration, if available.
getAnnFor :: UnitId -> HomePackageTable -> AnnEnv -> Module -> [ImportedBy]
          -> (Module, [InvertTag], Maybe SrcSpan)
getAnnFor unit modinfo annsExt mdl imps
  | unit == moduleUnitId mdl = case lookupHpt modinfo (moduleName mdl) of
      Nothing   -> panicAnyUnsafe "Cannot find info for module" mdl
      Just info -> (mdl, findAnns' (mkAnnEnv (md_anns (hm_details info))), imp)
  | otherwise = (mdl, findAnns' annsExt, imp)
    where
      findAnns' anns = findAnns deserializeWithData anns (ModuleTarget mdl)
      imp = msum (map importSpanMaybe imps)

-- | Get all imported modules.
allImportedMdls :: TcGblEnv -> [(Module, [ImportedBy])]
allImportedMdls = moduleEnvToList . imp_mods . tcg_imports

-- | Get the source span of an import, if available.
importSpanMaybe :: ImportedBy -> Maybe SrcSpan
importSpanMaybe (ImportedByUser i) = Just (imv_span i)
importSpanMaybe ImportedBySystem   = Nothing

-- | Classify a module import as ok or failed.
-- If it is classified as failed, then the span of the import is added as well.
classifyWithLoadResult :: Module -> [InvertTag] -> Maybe SrcSpan
                       -> IfaceLoadResult
classifyWithLoadResult mdl anns mspan =
  if notNull anns
    then LoadSuccess mdl
    else LoadFail mdl mspan

-- | Check if the given load result should provide an error message.
errorOnFailedLoad :: IfaceLoadResult -> TcM ()
errorOnFailedLoad (LoadSuccess _      ) = return ()
errorOnFailedLoad (LoadFail    mdl msp) =
  maybe id setSrcSpan msp $ addErrTc $
    "Module" <+> quotes (ppr mdl) <> impType <>
    "was not compiled with the inversion plugin and cannot be imported."
  where
    impType
      | isNothing msp  = space <> parens "System import" <> space
      | mdl == pRELUDE = space <> parens "Possibly implicit import" <> space
      | otherwise      = space
