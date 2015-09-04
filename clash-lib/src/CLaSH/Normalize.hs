{-# LANGUAGE TemplateHaskell #-}

-- | Turn CoreHW terms into normalized CoreHW Terms
module CLaSH.Normalize where

import           Control.Concurrent.Supply        (Supply)
import           Control.Lens                     ((.=))
import qualified Control.Lens                     as Lens
import           Data.Either                      (partitionEithers)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.List                        (mapAccumL,intersect)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens
import           Unbound.Generics.LocallyNameless (unembed)

import           CLaSH.Core.FreeVars              (termFreeIds)
import           CLaSH.Core.Pretty                (showDoc)
import           CLaSH.Core.Subst                 (substTms)
import           CLaSH.Core.Term                  (Term (..), TmName)
import           CLaSH.Core.Type                  (Type)
import           CLaSH.Core.TyCon                 (TyCon, TyConName)
import           CLaSH.Core.Util                  (collectArgs, mkApps, termType)
import           CLaSH.Core.Var                   (Id,varName)
import           CLaSH.Driver.Types               (CLaSHOpts (..))
import           CLaSH.Netlist.Types              (HWType)
import           CLaSH.Netlist.Util               (splitNormalized)
import           CLaSH.Normalize.Strategy
import           CLaSH.Normalize.Transformations  (bindConstantVar, caseCon,
                                                   reduceConst, topLet )
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Rewrite.Combinators        ((>->),(!->),repeatR,topdownR)
import           CLaSH.Rewrite.Types              (DebugLevel (..), RewriteEnv (..), RewriteState (..),
                                                   bindings, curFun, dbgLevel,
                                                   tcCache, extra)
import           CLaSH.Rewrite.Util               (runRewrite,
                                                   runRewriteSession)
import           CLaSH.Util

-- | Run a NormalizeSession in a given environment
runNormalization :: CLaSHOpts
                 -- ^ Level of debug messages to print
                 -> Supply
                 -- ^ UniqueSupply
                 -> HashMap TmName (Type,Term)
                 -- ^ Global Binders
                 -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                 -- ^ Hardcoded Type -> HWType translator
                 -> HashMap TyConName TyCon
                 -- ^ TyCon cache
                 -> (HashMap TyConName TyCon -> Term -> Term)
                 -- ^ Hardcoded evaluator (delta-reduction)
                 -> NormalizeSession a
                 -- ^ NormalizeSession to run
                 -> a
runNormalization opts supply globals typeTrans tcm eval
  = runRewriteSession rwEnv rwState
  where
    rwEnv     = RewriteEnv
                  (opt_dbgLevel opts)
                  typeTrans
                  tcm
                  eval

    rwState   = RewriteState
                  0
                  globals
                  supply
                  (error $ $(curLoc) ++ "Report as bug: no curFun")
                  0
                  normState

    normState = NormalizeState
                  HashMap.empty
                  Map.empty
                  HashMap.empty
                  (opt_specLimit opts)
                  HashMap.empty
                  (opt_inlineLimit opts)
                  (opt_inlineBelow opts)


normalize :: [TmName]
          -> NormalizeSession (HashMap TmName (Type,Term))
normalize []  = return HashMap.empty
normalize top = do
  (new,topNormalized) <- unzip <$> mapM normalize' top
  newNormalized <- normalize (concat new)
  return (HashMap.union (HashMap.fromList topNormalized) newNormalized)

normalize' :: TmName
           -> NormalizeSession ([TmName],(TmName,(Type,Term)))
normalize' nm = do
  exprM <- HashMap.lookup nm <$> Lens.use bindings
  let nmS = showDoc nm
  case exprM of
    Just (_,tm) -> do
      tmNorm <- makeCached nm (extra.normalized) $ do
                  curFun .= nm
                  tm' <- rewriteExpr ("normalization",normalization) (nmS,tm)
                  tcm <- Lens.view tcCache
                  ty' <- termType tcm tm'
                  return (ty',tm')
      let usedBndrs = Lens.toListOf termFreeIds (snd tmNorm)
      traceIf (nm `elem` usedBndrs)
              ($(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " (:: " ++ showDoc (fst tmNorm) ++ ") remains recursive after normalization:\n" ++ showDoc (snd tmNorm))
              (return ())
      prevNorm <- fmap HashMap.keys $ Lens.use (extra.normalized)
      let toNormalize = filter (`notElem` (nm:prevNorm)) usedBndrs
      return (toNormalize,(nm,tmNorm))
    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " not found"

-- | Rewrite a term according to the provided transformation
rewriteExpr :: (String,NormRewrite) -- ^ Transformation to apply
            -> (String,Term) -- ^ Term to transform
            -> NormalizeSession Term
rewriteExpr (nrwS,nrw) (bndrS,expr) = do
  lvl <- Lens.view dbgLevel
  let before = showDoc expr
  let expr' = traceIf (lvl >= DebugFinal)
                (bndrS ++ " before " ++ nrwS ++ ":\n\n" ++ before ++ "\n")
                expr
  rewritten <- runRewrite nrwS nrw expr'
  let after = showDoc rewritten
  traceIf (lvl >= DebugFinal)
    (bndrS ++ " after " ++ nrwS ++ ":\n\n" ++ after ++ "\n") $
    return rewritten

-- | Check if the call graph (second argument), starting at the @topEnity@
-- (first argument) is non-recursive. Returns the list of normalized terms if
-- call graph is indeed non-recursive, errors otherwise.
checkNonRecursive :: TmName -- ^ @topEntity@
                  -> HashMap TmName (Type,Term) -- ^ List of normalized binders
                  -> HashMap TmName (Type,Term)
checkNonRecursive topEntity norm =
  let cg = callGraph [] norm topEntity
  in  case recursiveComponents cg of
       []  -> norm
       rcs -> error $ $(curLoc) ++ "Callgraph after normalisation contains following recursive cycles: " ++ show rcs

-- | Perform general \"clean up\" of the normalized (non-recursive) function
-- hierarchy. This includes:
--
--   * Inlining functions that simply \"wrap\" another function
cleanupGraph :: TmName
             -> (HashMap TmName (Type,Term))
             -> NormalizeSession (HashMap TmName (Type,Term))
cleanupGraph topEntity norm = do
  let ct = mkCallTree [] norm topEntity
  ctFlat <- flattenCallTree ct
  return (HashMap.fromList $ snd $ callTreeToList [] ctFlat)


data CallTree = CLeaf   (TmName,(Type,Term))
              | CBranch (TmName,(Type,Term)) [CallTree]

mkCallTree :: [TmName] -- ^ Visited
           -> HashMap TmName (Type,Term) -- ^ Global binders
           -> TmName -- ^ Root of the call graph
           -> CallTree
mkCallTree visited bindingMap root = case used of
                            [] -> CLeaf   (root,rootTm)
                            _  -> CBranch (root,rootTm) other
  where
    rootTm = Maybe.fromMaybe (error $ $(curLoc) ++ show root ++ " is not a global binder") $ HashMap.lookup root bindingMap
    used   = Set.toList $ Lens.setOf termFreeIds $ snd rootTm
    other  = map (mkCallTree (root:visited) bindingMap) (filter (`notElem` visited) used)

stripArgs :: [TmName]
          -> [Id]
          -> [Either Term Type]
          -> Maybe [Either Term Type]
stripArgs _      (_:_) []   = Nothing
stripArgs allIds []    args = if any mentionsId args
                                then Nothing
                                else Just args
  where
    mentionsId t = not $ null (either (Lens.toListOf termFreeIds) (const []) t
                              `intersect`
                              allIds)

stripArgs allIds (id_:ids) (Left (Var _ nm):args)
      | varName id_ == nm = stripArgs allIds ids args
      | otherwise         = Nothing
stripArgs _ _ _ = Nothing

flattenNode :: CallTree
            -> NormalizeSession (Either CallTree ((TmName,Term),[CallTree]))
flattenNode c@(CLeaf (nm,(_,e))) = do
  tcm  <- Lens.view tcCache
  norm <- splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (map varName ids) (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),[]))
        Nothing        -> return (Left c)
    _ -> return (Left c)
flattenNode b@(CBranch (nm,(_,e)) us) = do
  tcm  <- Lens.view tcCache
  norm <- splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (map varName ids) (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),us))
        Nothing        -> return (Left b)
    _ -> return (Left b)

flattenCallTree :: CallTree
                -> NormalizeSession CallTree
flattenCallTree c@(CLeaf _) = return c
flattenCallTree (CBranch (nm,(ty,tm)) used) = do
  flattenedUsed   <- mapM flattenCallTree used
  (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
  let (toInline,il_used) = unzip il_ct
  newExpr <- case toInline of
               [] -> return tm
               _  -> rewriteExpr ("bindConstants",(repeatR (topdownR $ (bindConstantVar >-> caseCon >-> reduceConst))) !-> topLet) (showDoc nm, substTms toInline tm)
  return (CBranch (nm,(ty,newExpr)) (newUsed ++ (concat il_used)))

callTreeToList :: [TmName]
               -> CallTree
               -> ([TmName],[(TmName,(Type,Term))])
callTreeToList visited (CLeaf (nm,(ty,tm)))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,(ty,tm))])
callTreeToList visited (CBranch (nm,(ty,tm)) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,(ty,tm)):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used
