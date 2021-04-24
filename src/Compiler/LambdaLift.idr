module Compiler.LambdaLift

import Core.CompileExpr
import Core.Context
import Core.Core
import Core.TT

import Data.List
import Data.Vect
import Data.Maybe

%default covering

mutual
  public export
  -- lazy (lazy reason) represents if a function application is lazy (Just _)
  -- and if so why (eg. Just LInf, Just LLazy)
  data Lifted : List Name -> Type where
       LLocal : {idx : Nat} -> FC -> (0 p : IsVar x idx vars) -> Lifted vars
       -- A known function applied to exactly the right number of arguments,
       -- so the runtime can Just Go
       LAppName : FC -> (lazy : Maybe LazyReason) -> Name -> List (Lifted vars) -> Lifted vars
       -- A known function applied to too few arguments, so the runtime should
       -- make a closure and wait for the remaining arguments
       LUnderApp : FC -> Name -> (missing : Nat) ->
                   (args : List (Lifted vars)) -> Lifted vars
       -- A closure applied to one more argument (so, for example a closure
       -- which is waiting for another argument before it can run).
       -- The runtime should add the argument to the closure and run the result
       -- if it is now fully applied.
       LApp : FC -> (lazy : Maybe LazyReason) -> (closure : Lifted vars) -> (arg : Lifted vars) -> Lifted vars
       LLet : FC -> (x : Name) -> Lifted vars ->
              Lifted (x :: vars) -> Lifted vars
       LCon : FC -> Name -> (tag : Maybe Int) -> List (Lifted vars) -> Lifted vars
       LOp : {arity : _} ->
             FC -> (lazy : Maybe LazyReason) -> PrimFn arity -> Vect arity (Lifted vars) -> Lifted vars
       LExtPrim : FC -> (lazy : Maybe LazyReason) -> (p : Name) -> List (Lifted vars) -> Lifted vars
       LConCase : FC -> Lifted vars ->
                  List (LiftedConAlt vars) ->
                  Maybe (Lifted vars) -> Lifted vars
       LConstCase : FC -> Lifted vars ->
                    List (LiftedConstAlt vars) ->
                    Maybe (Lifted vars) -> Lifted vars
       LPrimVal : FC -> Constant -> Lifted vars
       LErased : FC -> Lifted vars
       LCrash : FC -> String -> Lifted vars

  public export
  data LiftedConAlt : List Name -> Type where
       MkLConAlt : Name -> (tag : Maybe Int) -> (args : List Name) ->
                   Lifted (args ++ vars) -> LiftedConAlt vars

  public export
  data LiftedConstAlt : List Name -> Type where
       MkLConstAlt : Constant -> Lifted vars -> LiftedConstAlt vars

public export
data LiftedDef : Type where
     -- We take the outer scope and the function arguments separately so that
     -- we don't have to reshuffle de Bruijn indices, which is expensive.
     -- This should be compiled as a function which takes 'args' first,
     -- then 'reverse scope'.
     -- (Sorry for the awkward API - it's to do with how the indices are
     -- arranged for the variables, and it oculd be expensive to reshuffle them!
     -- See Compiler.ANF for an example of how they get resolved to names)
     MkLFun : (args : List Name) -> -- function arguments
              (scope : List Name) -> -- outer scope
              Lifted (scope ++ args) -> LiftedDef
     MkLCon : (tag : Maybe Int) -> (arity : Nat) -> (nt : Maybe Nat) -> LiftedDef
     MkLForeign : (ccs : List String) ->
                  (fargs : List CFType) ->
                  CFType ->
                  LiftedDef
     MkLError : Lifted [] -> LiftedDef

showLazy : Maybe LazyReason -> String
showLazy = maybe "" $ (" " ++) . show

mutual
  export
  {vs : _} -> Show (Lifted vs) where
    show (LLocal {idx} _ p) = "!" ++ show (nameAt p)
    show (LAppName fc lazy n args)
        = show n ++ showLazy lazy ++ "(" ++ showSep ", " (map show args) ++ ")"
    show (LUnderApp fc n m args)
        = "<" ++ show n ++ " underapp " ++ show m ++ ">(" ++
          showSep ", " (map show args) ++ ")"
    show (LApp fc lazy c arg)
        = show c ++ showLazy lazy ++ " @ (" ++ show arg ++ ")"
    show (LLet fc x val sc)
        = "%let " ++ show x ++ " = " ++ show val ++ " in " ++ show sc
    show (LCon fc n t args)
        = "%con " ++ show n ++ "(" ++ showSep ", " (map show args) ++ ")"
    show (LOp fc lazy op args)
        = "%op " ++ show op ++ showLazy lazy ++ "(" ++ showSep ", " (toList (map show args)) ++ ")"
    show (LExtPrim fc lazy p args)
        = "%extprim " ++ show p ++ showLazy lazy ++ "(" ++ showSep ", " (map show args) ++ ")"
    show (LConCase fc sc alts def)
        = "%case " ++ show sc ++ " of { "
             ++ showSep "| " (map show alts) ++ " " ++ show def
    show (LConstCase fc sc alts def)
        = "%case " ++ show sc ++ " of { "
             ++ showSep "| " (map show alts) ++ " " ++ show def
    show (LPrimVal _ x) = show x
    show (LErased _) = "___"
    show (LCrash _ x) = "%CRASH(" ++ show x ++ ")"

  export
  {vs : _} -> Show (LiftedConAlt vs) where
    show (MkLConAlt n t args sc)
        = "%conalt " ++ show n ++
             "(" ++ showSep ", " (map show args) ++ ") => " ++ show sc

  export
  {vs : _} -> Show (LiftedConstAlt vs) where
    show (MkLConstAlt c sc)
        = "%constalt(" ++ show c ++ ") => " ++ show sc

export
Show LiftedDef where
  show (MkLFun args scope exp)
      = show args ++ show (reverse scope) ++ ": " ++ show exp
  show (MkLCon tag arity pos)
      = "Constructor tag " ++ show tag ++ " arity " ++ show arity ++
        maybe "" (\n => " (newtype by " ++ show n ++ ")") pos
  show (MkLForeign ccs args ret)
      = "Foreign call " ++ show ccs ++ " " ++
        show args ++ " -> " ++ show ret
  show (MkLError exp) = "Error: " ++ show exp


data Lifts : Type where

record LDefs where
  constructor MkLDefs
  basename : Name -- top level name we're lifting from
  defs : List (Name, LiftedDef) -- new definitions we made
  nextName : Int -- name of next definition to lift

genName : {auto l : Ref Lifts LDefs} ->
          Core Name
genName
    = do ldefs <- get Lifts
         let i = nextName ldefs
         put Lifts (record { nextName = i + 1 } ldefs)
         pure $ mkName (basename ldefs) i
  where
    mkName : Name -> Int -> Name
    mkName (NS ns b) i = NS ns (mkName b i)
    mkName (UN n) i = MN n i
    mkName (DN _ n) i = mkName n i
    mkName (CaseBlock outer inner) i = MN ("case block in " ++ outer ++ " (" ++ show inner ++ ")") i
    mkName (WithBlock outer inner) i = MN ("with block in " ++ outer ++ " (" ++ show inner ++ ")") i
    mkName n i = MN (show n) i

unload : FC -> (lazy : Maybe LazyReason) -> Lifted vars -> List (Lifted vars) -> Core (Lifted vars)
unload fc _ f [] = pure f
-- only outermost LApp must be lazy as rest will be closures
unload fc lazy f (a :: as) = unload fc Nothing (LApp fc lazy f a) as

record Used (vars : List Name) where
  constructor MkUsed
  used : Vect (length vars) Bool

initUsed : {vars : _} -> Core (Used vars)
initUsed {vars} = do
  pure $ MkUsed (replicate (length vars) False)

contractUsed : {vars : _} ->
               (Used (x::vars)) ->
               Core (Used vars)
contractUsed (MkUsed (_::rest)) = pure $ MkUsed rest

contractUsedMany : {vars : _} ->
                   {remove : _} ->
                   (Used (remove ++ vars)) ->
                   Core (Used vars)
contractUsedMany {remove=[]} x = pure x
contractUsedMany {remove=(r::rs)} x = contractUsedMany {remove=rs} !(contractUsed x)

natFin : {vars : _} ->
         (idx : Nat) ->
         (0 prf : IsVar x idx vars) ->
         Fin (length vars)
natFin Z First = FZ
natFin (S x) First impossible
natFin (S x) (Later l) = FS (natFin x l)

markUsed : {vars : _} ->
           (idx : Nat) ->
           {0 prf : IsVar x idx vars} ->
           Used vars ->
           Core (Used vars)
markUsed {vars} {prf} idx (MkUsed us) = do
  let newUsed = replaceAt (natFin idx prf) True us
  pure $ MkUsed newUsed

mergeUsed : {vars : List Name} ->
           Used vars ->
           Used vars ->
           Core (Used vars)
mergeUsed {vars=[]} (MkUsed []) (MkUsed _) = pure $ MkUsed []
mergeUsed {vars=(_::_)} (MkUsed (x::xs)) (MkUsed (y::ys)) = do
  MkUsed rest <- mergeUsed (MkUsed xs) (MkUsed ys)
  pure $ MkUsed ((x || y)::rest)

getUnused : {vars : List Name} ->
            Used vars ->
            Core (Vect (length vars) Bool)
getUnused {vars} (MkUsed uv) = pure $ getUnused' uv
  where
    getUnused' : Vect (length vars) Bool -> Vect (length vars) Bool
    getUnused' v = map not v

foldlCore : (Foldable t) => (f : a -> b -> Core a) -> (init : a) -> t b -> Core a
foldlCore f i = foldl (\ma, b => ma >>= flip f b) (pure i)

total
dropped : (vars : List Name) ->
          (drop : Vect (length vars) Bool) ->
          List Name
dropped [] _ = []
dropped (x::xs) (False::us) = x::(dropped xs us)
dropped (x::xs) (True::us) = dropped xs us

mutual
  makeLam : {auto l : Ref Lifts LDefs} ->
            {vars : _} ->
            {doLazyAnnots : Bool} ->
            {default Nothing lazy : Maybe LazyReason} ->
            FC -> (bound : List Name) ->
            CExp (bound ++ vars) -> Core (Lifted vars)
  makeLam fc bound (CLam _ x sc') = makeLam fc {doLazyAnnots} {lazy} (x :: bound) sc'
  makeLam {vars} fc bound sc
      = do scl <- liftExp {doLazyAnnots} {lazy} sc
           -- Find out which variables aren't used in the new definition, and
           -- do not abstract over them in the new definition.
           scUsedL <- usedVars scl
           unusedContracted <- contractUsedMany {remove=bound} scUsedL
           unused <- getUnused unusedContracted
           let scl' = dropUnused {outer=bound} unused scl
           n <- genName
           ldefs <- get Lifts
           put Lifts (record { defs $= ((n, MkLFun (dropped vars unused) bound scl') ::) } ldefs)
           pure $ LUnderApp fc n (length bound) (allVars fc vars unused)
    where
        allPrfs : (vs : List Name) -> (unused : Vect (length vs) Bool) -> List (Var vs)
        allPrfs [] _ = []
        allPrfs (v :: vs) (False::uvs) = MkVar First :: map weaken (allPrfs vs uvs)
        allPrfs (v :: vs) (True::uvs) = map weaken (allPrfs vs uvs)

        -- apply to all the variables. 'First' will be first in the last, which
        -- is good, because the most recently bound name is the first argument to
        -- the resulting function
        allVars : FC -> (vs : List Name) -> (uvars : Vect (length vs) Bool) -> List (Lifted vs)
        allVars fc vs uvars = map (\ (MkVar p) => LLocal fc p) (allPrfs vs uvars)

-- if doLazyAnnots = True then annotate function application with laziness
-- otherwise use old behaviour (thunk is a function)
  liftExp : {vars : _} ->
            {auto l : Ref Lifts LDefs} ->
            {doLazyAnnots : Bool} ->
            {default Nothing lazy : Maybe LazyReason} ->
            CExp vars -> Core (Lifted vars)
  liftExp (CLocal fc prf) = pure $ LLocal fc prf
  liftExp (CRef fc n) = pure $ LAppName fc lazy n [] -- probably shouldn't happen!
  liftExp (CLam fc x sc) = makeLam {doLazyAnnots} {lazy} fc [x] sc
  liftExp (CLet fc x _ val sc) = pure $ LLet fc x !(liftExp {doLazyAnnots} val) !(liftExp {doLazyAnnots} sc)
  liftExp (CApp fc (CRef _ n) args) -- names are applied exactly in compileExp
      = pure $ LAppName fc lazy n !(traverse (liftExp {doLazyAnnots}) args)
  liftExp (CApp fc f args)
      = unload fc lazy !(liftExp {doLazyAnnots} f) !(traverse (liftExp {doLazyAnnots}) args)
  liftExp (CCon fc n t args) = pure $ LCon fc n t !(traverse (liftExp {doLazyAnnots}) args)
  liftExp (COp fc op args)
      = pure $ LOp fc lazy op !(traverseArgs args)
    where
      traverseArgs : Vect n (CExp vars) -> Core (Vect n (Lifted vars))
      traverseArgs [] = pure []
      traverseArgs (a :: as) = pure $ !(liftExp {doLazyAnnots} a) :: !(traverseArgs as)
  liftExp (CExtPrim fc p args) = pure $ LExtPrim fc lazy p !(traverse (liftExp {doLazyAnnots}) args)
  liftExp (CForce fc lazy tm) = if doLazyAnnots
    then liftExp {doLazyAnnots} {lazy = Nothing} tm
    else liftExp {doLazyAnnots} (CApp fc tm [CErased fc])
  liftExp (CDelay fc lazy tm) = if doLazyAnnots
    then liftExp {doLazyAnnots} {lazy = Just lazy} tm
    else liftExp {doLazyAnnots} (CLam fc (MN "act" 0) (weaken tm))
  liftExp (CConCase fc sc alts def)
      = pure $ LConCase fc !(liftExp {doLazyAnnots} sc) !(traverse (liftConAlt {lazy}) alts)
                           !(traverseOpt (liftExp {doLazyAnnots}) def)
    where
      liftConAlt : {default Nothing lazy : Maybe LazyReason} ->
                   CConAlt vars -> Core (LiftedConAlt vars)
      liftConAlt (MkConAlt n t args sc) = pure $ MkLConAlt n t args !(liftExp {doLazyAnnots} {lazy} sc)
  liftExp (CConstCase fc sc alts def)
      = pure $ LConstCase fc !(liftExp {doLazyAnnots} sc) !(traverse liftConstAlt alts)
                             !(traverseOpt (liftExp {doLazyAnnots}) def)
    where
      liftConstAlt : {default Nothing lazy : Maybe LazyReason} ->
                     CConstAlt vars -> Core (LiftedConstAlt vars)
      liftConstAlt (MkConstAlt c sc) = pure $ MkLConstAlt c !(liftExp {doLazyAnnots} {lazy} sc)
  liftExp (CPrimVal fc c) = pure $ LPrimVal fc c
  liftExp (CErased fc) = pure $ LErased fc
  liftExp (CCrash fc str) = pure $ LCrash fc str

  usedVars : {vars : _} ->
             {auto l : Ref Lifts LDefs} ->
             Lifted vars ->
             Core (Used vars)
  usedVars (LLocal {idx} fc prf) = do
    used <- initUsed {vars}
    markUsed {prf} idx used
  usedVars (LAppName fc lazy n args) = do
    allUsed <- traverse usedVars args
    foldlCore mergeUsed !(initUsed) allUsed
  usedVars (LUnderApp fc n miss args) = do
    allUsed <- traverse usedVars args
    foldlCore mergeUsed !(initUsed) allUsed
  usedVars (LApp fc lazy c arg) = do
    mergeUsed !(usedVars c) !(usedVars arg)
  usedVars (LLet fc x val sc) = do
    valUsed <- usedVars val
    inner <- usedVars sc
    mergeUsed !(contractUsed inner) valUsed
  usedVars (LCon fc n tag args) = do
    allUsed <- traverse usedVars args
    foldlCore mergeUsed !(initUsed) allUsed
  usedVars (LOp fc lazy fn args) = do
    allUsed <- traverseVect usedVars args
    foldlCore mergeUsed !(initUsed) allUsed
  usedVars (LExtPrim fc lazy fn args) = do
    allUsed <- traverse usedVars args
    foldlCore mergeUsed !(initUsed) allUsed
  usedVars (LConCase fc sc alts def) = do
      scUsed <- usedVars sc
      defUsed <- traverseOpt usedVars def
      altsUsed <- traverse usedConAlt alts
      mergedAlts <- foldlCore mergeUsed !(initUsed) altsUsed
      scDefUsed <- mergeUsed scUsed (maybe !(initUsed) id defUsed)
      mergeUsed scDefUsed mergedAlts
    where
      usedConAlt : {default Nothing lazy : Maybe LazyReason} ->
                   LiftedConAlt vars -> Core (Used vars)
      usedConAlt (MkLConAlt n tag args sc) = do
        contractUsedMany {remove=args} !(usedVars sc)

  usedVars (LConstCase fc sc alts def) = do
      scUsed <- usedVars sc
      defUsed <- traverseOpt usedVars def
      altsUsed <- traverse usedConstAlt alts
      mergedAlts <- foldlCore mergeUsed !(initUsed) altsUsed
      scDefUsed <- mergeUsed scUsed (maybe !(initUsed) id defUsed)
      mergeUsed scDefUsed mergedAlts
    where
      usedConstAlt : {default Nothing lazy : Maybe LazyReason} ->
                     LiftedConstAlt vars -> Core (Used vars)
      usedConstAlt (MkLConstAlt c sc) = usedVars sc

  usedVars (LPrimVal _ _) = initUsed
  usedVars (LErased _) = initUsed
  usedVars (LCrash _ _) = initUsed

  dropIdx : {vars : _} ->
            {idx : _} ->
            (outer : List Name) ->
            (unused : Vect (length vars) Bool) ->
            (0 p : IsVar x idx (outer ++ vars)) ->
            Var (outer ++ (dropped vars unused))
  dropIdx [] (False::_) First = MkVar First
  dropIdx [] (True::_) First = assert_total $ idris_crash "referenced var cannot be unused"
  dropIdx [] (False::rest) (Later p) = Var.later $ dropIdx [] rest p
  dropIdx [] (True::rest) (Later p) = dropIdx [] rest p
  dropIdx (_::xs) unused First = MkVar First
  dropIdx (_::xs) unused (Later p) = Var.later $ dropIdx xs unused p

  dropUnused : {vars : _} ->
               {auto l : Ref Lifts LDefs} ->
               {outer : List Name} ->
               (unused : Vect (length vars) Bool) ->
               (l : Lifted (outer ++ vars)) ->
               Lifted (outer ++ (dropped vars unused))
  dropUnused _ (LPrimVal fc val) = LPrimVal fc val
  dropUnused _ (LErased fc) = LErased fc
  dropUnused _ (LCrash fc msg) = LCrash fc msg
  dropUnused {outer} unused (LLocal fc p) =
    let (MkVar p') = dropIdx outer unused p in LLocal fc p'
  dropUnused unused (LCon fc n tag args) =
    let args' = map (dropUnused unused) args in
        LCon fc n tag args'
  dropUnused {outer} unused (LLet fc n val sc) =
    let val' = dropUnused unused val
        sc' = dropUnused {outer=n::outer} (unused) sc in
        LLet fc n val' sc'
  dropUnused unused (LApp fc lazy c arg) =
    let c' = dropUnused unused c
        arg' = dropUnused unused arg in
        LApp fc lazy c' arg'
  dropUnused unused (LOp fc lazy fn args) =
    let args' = map (dropUnused unused) args in
        LOp fc lazy fn args'
  dropUnused unused (LExtPrim fc lazy n args) =
    let args' = map (dropUnused unused) args in
        LExtPrim fc lazy n args'
  dropUnused unused (LAppName fc lazy n args) =
    let args' = map (dropUnused unused) args in
        LAppName fc lazy n args'
  dropUnused unused (LUnderApp fc n miss args) =
    let args' = map (dropUnused unused) args in
        LUnderApp fc n miss args'
  dropUnused {vars} {outer} unused (LConCase fc sc alts def) =
    let alts' = map dropConCase alts in
        LConCase fc (dropUnused unused sc) alts' (map (dropUnused unused) def)
    where
      dropConCase : LiftedConAlt (outer ++ vars) ->
                    LiftedConAlt (outer ++ (dropped vars unused))
      dropConCase (MkLConAlt n t args sc) =
        let droppedSc = dropUnused {vars=vars} {outer=args++outer} unused (rewrite sym $ appendAssociative args outer vars in sc) in
        MkLConAlt n t args (rewrite appendAssociative args outer (dropped vars unused) in droppedSc)
  dropUnused {vars} {outer} unused (LConstCase fc sc alts def) =
    let alts' = map dropConstCase alts in
        LConstCase fc (dropUnused unused sc) alts' (map (dropUnused unused) def)
    where
      dropConstCase : LiftedConstAlt (outer ++ vars) ->
                      LiftedConstAlt (outer ++ (dropped vars unused))
      dropConstCase (MkLConstAlt c val) = MkLConstAlt c (dropUnused unused val)

export
liftBody : {vars : _} -> {doLazyAnnots : Bool} ->
           Name -> CExp vars -> Core (Lifted vars, List (Name, LiftedDef))
liftBody n tm
    = do l <- newRef Lifts (MkLDefs n [] 0)
         tml <- liftExp {doLazyAnnots} {l} tm
         ldata <- get Lifts
         pure (tml, defs ldata)

lambdaLiftDef : (doLazyAnnots : Bool) -> Name -> CDef -> Core (List (Name, LiftedDef))
lambdaLiftDef doLazyAnnots n (MkFun args exp)
    = do (expl, defs) <- liftBody {doLazyAnnots} n exp
         pure ((n, MkLFun args [] expl) :: defs)
lambdaLiftDef _ n (MkCon t a nt) = pure [(n, MkLCon t a nt)]
lambdaLiftDef _ n (MkForeign ccs fargs ty) = pure [(n, MkLForeign ccs fargs ty)]
lambdaLiftDef doLazyAnnots n (MkError exp)
    = do (expl, defs) <- liftBody {doLazyAnnots} n exp
         pure ((n, MkLError expl) :: defs)

-- Return the lambda lifted definitions required for the given name.
-- If the name hasn't been compiled yet (via CompileExpr.compileDef) then
-- this will return an empty list
-- An empty list an error, because on success you will always get at least
-- one definition, the lifted definition for the given name.
export
lambdaLift : {auto c : Ref Ctxt Defs} ->
             (doLazyAnnots : Bool) ->
             Name -> Core (List (Name, LiftedDef))
lambdaLift doLazyAnnots n
    = do defs <- get Ctxt
         Just def <- lookupCtxtExact n (gamma defs) | Nothing => pure []
         let Just cexpr = compexpr def              | Nothing => pure []
         lambdaLiftDef doLazyAnnots n cexpr
