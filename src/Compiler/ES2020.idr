module Compiler.ES2020

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Directory
import Core.Name
import Core.Options
import Core.TT
import Utils.Hex

import Data.NameMap
import Data.Vect
import System
import System.Info

%default covering

jsIdent : String -> String
jsIdent s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c
                  then cast c
                  else "$" ++ asHex (cast {to=Int} c)

export
jsName : Name -> String
jsName (NS ns n) = showSep "_" ns ++ "_" ++ jsName n
jsName (UN n) = jsIdent n
jsName (MN n i) = jsIdent n ++ "_" ++ show i
jsName (PV n d) = "pat__" ++ jsName n
jsName (DN _ n) = jsName n
jsName (RF n) = "rf__" ++ jsIdent n
jsName (Nested (i, x) n) = "n__" ++ show i ++ "_" ++ show x ++ "_" ++ jsName n
jsName (CaseBlock x y) = "case__" ++ show x ++ "_" ++ show y
jsName (WithBlock x y) = "with__" ++ show x ++ "_" ++ show y
jsName (Resolved i) = "fn__" ++ show i

jsString : String -> String
jsString s = "'" ++ (concatMap okchar (unpack s)) ++ "'"
  where
    okchar : Char -> String
    okchar c = if (c >= ' ') && (c /= '\\') && (c /= '"') && (c /= '\'') && (c <= '~')
                  then cast c
                  else "\\u{" ++ asHex (cast {to=Int} c) ++ "}"
binOp : String -> String -> String -> String
binOp o lhs rhs = "(" ++ lhs ++ " " ++ o ++ " " ++ rhs ++ ")"

boundedInt : Int -> String -> String
boundedInt bits e = "(" ++ e ++ " % __jsPrim_int_bound_" ++ show bits ++ ")"

boundedIntOp : Int -> String -> String -> String -> String
boundedIntOp bits o lhs rhs = boundedInt 63 (binOp o lhs rhs)

boolOp : String -> String -> String -> String
boolOp o lhs rhs = "(" ++ binOp o lhs rhs ++ " ? 1 : 0)"

toBigInt : String -> String
toBigInt e = "BigInt(" ++ e ++ ")"

fromBigInt : String -> String
fromBigInt e = "Number(" ++ e ++ ")"

jsOp : PrimFn arity -> Vect arity String -> String
jsOp (Add IntType) [x, y] = boundedIntOp 63 "+" x y
jsOp (Sub IntType) [x, y] = boundedIntOp 63 "-" x y
jsOp (Mul IntType) [x, y] = boundedIntOp 63 "*" x y
jsOp (Div IntType) [x, y] = boundedIntOp 63 "/" x y
jsOp (Mod IntType) [x, y] = boundedIntOp 63 "%" x y
jsOp (Add ty) [x, y] = binOp "+" x y
jsOp (Sub ty) [x, y] = binOp "-" x y
jsOp (Mul ty) [x, y] = binOp "*" x y
jsOp (Div ty) [x, y] = binOp "/" x y
jsOp (Mod ty) [x, y] = binOp "%" x y
jsOp (Neg ty) [x] = "(-(" ++ x ++ "))"
jsOp (ShiftL ty) [x, y] = binOp "<<" x y
jsOp (ShiftR ty) [x, y] = binOp ">>" x y
jsOp (BAnd ty) [x, y] = binOp "&" x y
jsOp (BOr ty) [x, y] = binOp "|" x y
jsOp (BXOr ty) [x, y] = binOp "^" x y
jsOp (LT ty) [x, y] = boolOp "<" x y
jsOp (LTE ty) [x, y] = boolOp "<=" x y
jsOp (EQ ty) [x, y] = boolOp "===" x y
jsOp (GTE ty) [x, y] = boolOp ">=" x y
jsOp (GT ty) [x, y] = boolOp ">" x y
jsOp StrLength [x] = toBigInt $ x ++ ".length"
jsOp StrHead [x] = "(" ++ x ++ ".charAt(0))"
jsOp StrTail [x] = "(" ++ x ++ ".slice(1))"
jsOp StrIndex [x, y] = "(" ++ x ++ ".charAt(" ++ fromBigInt y ++ "))"
jsOp StrCons [x, y] = binOp "+" x y
jsOp StrAppend [x, y] = binOp "+" x y
jsOp StrReverse [x] = "__jsPrim_reverseStr(" ++ x ++ ")"
jsOp StrSubstr [x, start, end] = x ++ ".slice(" ++ fromBigInt start ++ ", " ++ fromBigInt end ++ ")"
jsOp DoubleExp [x] = "Math.exp(" ++ x ++ ")"
jsOp DoubleLog [x] = "Math.log(" ++ x ++ ")"
jsOp DoubleSin [x] = "Math.sin(" ++ x ++ ")"
jsOp DoubleCos [x] = "Math.cos(" ++ x ++ ")"
jsOp DoubleTan [x] = "Math.tan(" ++ x ++ ")"
jsOp DoubleASin [x] = "Math.asin(" ++ x ++ ")"
jsOp DoubleACos [x] = "Math.acos(" ++ x ++ ")"
jsOp DoubleATan [x] = "Math.atan(" ++ x ++ ")"
jsOp DoubleSqrt [x] = "Math.sqrt(" ++ x ++ ")"
jsOp DoubleFloor [x] = "Math.floor(" ++ x ++ ")"
jsOp DoubleCeiling [x] = "Math.ceil(" ++ x ++ ")"
jsOp (Cast IntType CharType) [x] = "String.fromCodePoint(" ++ fromBigInt x ++ ")"
jsOp (Cast IntegerType CharType) [x] = "String.fromCodePoint(" ++ fromBigInt x ++ ")"
jsOp (Cast CharType IntType) [x] = toBigInt $ x ++ ".codePointAt(0)"
jsOp (Cast CharType IntegerType) [x] = toBigInt $ x ++ ".codePointAt(0)"
jsOp (Cast DoubleType IntType) [x] = boundedInt 63 $ "BigInt(Math.floor(" ++ x ++ "))"
jsOp (Cast DoubleType IntegerType) [x] = "BigInt(Math.floor(" ++ x ++ "))"
jsOp (Cast StringType IntType) [x] = boundedInt 63 $ "__jsPrim_integer_of_string(" ++ x ++ ")"
jsOp (Cast StringType IntegerType) [x] = "__jsPrim_integer_of_string(" ++ x ++ ")"
jsOp (Cast IntegerType IntType) [x] = boundedInt 63 x
jsOp (Cast IntType IntegerType) [x] = x
jsOp (Cast ty DoubleType) [x] = "parseFloat(" ++ x ++ ")"
jsOp (Cast ty StringType) [x] = "(''+" ++ x ++ ")" -- this is JavaScript after all
jsOp (Cast ty ty2) [x] = "__jsPrim_idris_crash('invalid cast: ' + " ++ jsString (show ty) ++ " + ' -> ' + " ++ jsString (show ty2) ++ ")"
jsOp BelieveMe [_,_,x] = x
jsOp (Crash) [_, msg] = "__jsPrim_idris_crash(" ++ jsString msg ++ ")"

mutual
  jsExp : Int -> NamedCExp -> Core String
  jsExp i (NmLocal fc n) = pure $ jsName n
  jsExp i (NmRef fc n) = pure $ jsName n
  jsExp i (NmLam fc n e) = pure $ "((" ++ jsName n ++ ") => {return " ++ !(jsExp i e) ++ "})"
  jsExp i (NmApp fc x args) = pure $ !(jsExp i x) ++ "(" ++ (showSep ", " !(traverse (jsExp i) args)) ++ ")"
  jsExp i (NmPrimVal fc c) = pure $ jsConstant c
  jsExp i (NmOp fc op args) = pure $ jsOp op !(jsArgs i args)
  jsExp i (NmConCase fc sc alts def)
    = do tcode <- jsExp i sc
         defc <- maybe (pure Nothing) (\v => pure (Just !(jsExp i v))) def
         let n = "sc_" ++ (show i)
         calts <- showSep "\nelse " <$> traverse (jsConAlt (i+1) n) alts
         pure $ "(() => {const " ++ n ++ " = " ++ tcode ++ ";\n" ++
                       "" ++ calts ++ "\nreturn " ++ (fromMaybe "__jsPrim_idris_crash('unhandled con case')" defc) ++ ";})()"

  jsExp i (NmConstCase fc sc alts def)
    = do defc <- maybe (pure Nothing) (\v => pure (Just !(jsExp i v))) def
         tcode <- jsExp i sc
         let n = "scc_" ++ (show i)
         calts <- showSep "\nelse " <$> traverse (jsConstAlt (i+1) n) alts
         pure $ "(() => {const " ++ n ++ " = " ++ tcode ++ ";\n" ++
                       "" ++ calts ++ "\nreturn " ++ (fromMaybe "__jsPrim_idris_crash('unhandled const case')" defc) ++ ";})()"
  jsExp i (NmExtPrim fc p args) = jsPrim i p args
  jsExp i (NmCon fc x tag args) = pure $ jsConstructor x tag !(traverse (jsExp i) args)
  jsExp i (NmDelay fc t) = pure $ "(/*delayed*/() => (" ++ !(jsExp i t) ++ "))"
  jsExp i (NmForce fc t) = pure $ "(/*force*/(" ++ !(jsExp i t) ++ ")())"
  jsExp i (NmLet fc x val sc)
    = do val' <- jsExp i val
         sc' <- jsExp i sc
         {-pure $ "const " ++ jsName x ++ " = " ++ val' ++ ";\n" ++ sc' ++ ""-}
         pure $ "((" ++ jsName x ++ ")=>(" ++ sc' ++ "))(" ++ val' ++ ")"
  jsExp i (NmErased fc) = pure "/* erased */ undefined"
  jsExp i (NmCrash fc msg) = pure $ "__jsPrim_idris_crash(" ++ jsString msg ++ ")"

  jsConstructor : Name -> Maybe Int -> List String -> String
  jsConstructor _ (Just i) args = "[" ++ (show i) ++ ", " ++ (showSep ", " args) ++ "]"
  jsConstructor n Nothing args = "[" ++ (jsName n) ++ ", " ++ (showSep ", " args) ++ "]"

  jsConstAlt : Int -> String -> NamedConstAlt -> Core String
  jsConstAlt i target (MkNConstAlt (I c) exp) =
    pure $ "if (" ++ target ++ " === " ++ (show c) ++ "){ return " ++ !(jsExp i exp) ++ ";}"
  jsConstAlt i target (MkNConstAlt c exp) =
    pure $ "if (" ++ target ++ " === " ++ (jsConstant c) ++ "){ return " ++ !(jsExp i exp) ++ ";}"

  jsConAlt : Int -> String -> NamedConAlt -> Core String
  jsConAlt i target (MkNConAlt n tag args exp) =
    pure $ "if (" ++ target ++ "[0] === " ++ (show tag) ++ "){ return " ++ bindArgs args !(jsExp i exp) ++ ";}"
      where
        allIndices : List a -> List Nat
        allIndices xs = findIndices (\_ => True) xs
        bindArgs : List Name -> String -> String
        bindArgs [] body = body
        bindArgs args body = "( ((" ++ (showSep ", " (map jsName args)) ++ ") " ++
                             "=> {return " ++ body ++ ";})" ++
                             "(" ++ showSep ", " (map (\i => target ++ "[" ++ (show (i+1)) ++ "]") (allIndices args)) ++ ") )"

  jsArgs : Int -> Vect n NamedCExp -> Core (Vect n String)
  jsArgs _i [] = pure []
  jsArgs i (arg :: args) = pure $ !(jsExp i arg) :: !(jsArgs i args)

  jsConstant : Constant -> String
  jsConstant (I i) = toBigInt $ show i
  jsConstant (BI i) = toBigInt $ show i
  jsConstant (Str s) = jsString s
  jsConstant (Ch c) = jsString $ singleton c
  jsConstant (Db f) = show f
  jsConstant WorldVal = "__jsPrim_IdrisWorld"
  -- otherwise: IntType, StringType, WorldType...
  jsConstant ty = "__jsPrim_idris_crash('code tried to evaluate a type constant: ' + " ++ show ty ++ ")"

  jsPrim : Int -> Name -> List NamedCExp -> Core String
  jsPrim i (NS _ (UN "prim__putStr")) args = pure $ "__jsPrim_putStr(" ++ (showSep ", " !(traverse (jsExp i) args)) ++ ")"
  jsPrim i (NS _ (UN "prim__codegen")) [] = pure $ jsString "es2020"
  jsPrim i (NS _ (UN "prim__os")) args = pure $ jsString os
  jsPrim i (NS _ (UN "prim__open")) args = pure $ "__jsPrim_open(" ++ (showSep ", " !(traverse (jsExp i) args)) ++ ")"
  jsPrim i (NS _ (UN "prim__close")) args = pure $ "__jsPrim_close(" ++ (showSep ", " !(traverse (jsExp i) args)) ++ ")"
  jsPrim i x args = throw (InternalError $ "prim not implemented: " ++ (show x))

jsDef : Name -> NamedDef -> Core String
jsDef n (MkNmFun args exp) =
  do e <- jsExp 100 exp
     pure ("const " ++ (jsName n) ++ " = ((" ++ (showSep ", " (map jsName args)) ++ ") => {\nreturn "
        ++ e ++ ";\n});\n\n")
jsDef n (MkNmError exp) = pure ""
jsDef n (MkNmForeign _ _ _) = pure ""
jsDef n (MkNmCon _ _ _) = pure ""

getJS : Defs -> (Name, FC, NamedDef) -> Core String
getJS defs (n, _fc, nd) =
  jsDef n nd
  {-
    = debugTrace (show (n, _fc, nd)) $ case !(lookupCtxtExact n (gamma defs)) of
           Nothing => throw (InternalError ("Compiling undefined name " ++ show n))
           Just d => case namedcompexpr d of
                          Nothing =>
                             throw (InternalError ("No compiled definition for " ++ show n))
                          Just d => jsDef n d
                          -}

findNode : IO String
findNode =
  do env <- getEnv "NODE"
     pure $ fromMaybe "/usr/bin/env node" env

compileJs : Ref Ctxt Defs -> ClosedTerm -> Core String
compileJs c tm
    = do cdata <- getCompileData Cases tm
         let ns = namedDefs cdata
         let ctm = forget (mainExpr cdata)
         defs <- get Ctxt
         compdefs <- traverse (getJS defs) ns

         main <- jsExp 0 ctm
         support <- readDataFile "es2020/support.js"
         let js = "/* generated by idris es2020 backend */\n\n" ++
                  support ++
                  (fastAppend compdefs) ++
                  "\n\n" ++ main ++ "\n"
         pure js


compileExpr : Ref Ctxt Defs -> (exexDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c execDir tm outfile
    = do let outJs = (outfile ++ ".js")
         js <- compileJs c tm
         Right () <- coreLift $ writeFile outJs js
            | Left err => throw (FileErr outJs err)
         pure (Just (outJs))

executeExpr : Ref Ctxt Defs -> (exexDir : String) -> ClosedTerm -> Core ()
executeExpr c execDir tm
    = do tmp <- coreLift $ tmpName
         let outn = tmp ++ ".js"
         js <- compileJs c tm
         Right () <- coreLift $ writeFile outn js
            | Left err => throw (FileErr outn err)
         node <- coreLift findNode
         coreLift $ system (node ++ " " ++ outn)
         pure ()

export
codegenES2020 : Codegen
codegenES2020 = MkCG compileExpr executeExpr