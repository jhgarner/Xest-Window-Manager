{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module TH where

import           Language.Haskell.TH
import           Standard

-- |This awful piece of template haskell creates a series of
-- patterns based on a given data type.
--
-- Why? I decided typing Fix and unfix everywhere was annoying
-- and I wanted to have smart constructors/deconstructors to
-- handle everything for me.
--
-- Originally I wanted to use Coercible to transform between the
-- Fixed and unfixed versions but that didn't work. It seemed like
-- Coercible didn't like the Wrap constructor for reasons I don't
-- fully understand.
--
-- Instead, I created a new type class which had to be passed to
-- this function. I have no idea if this template haskell is useful
-- for any other datatypes or type classes. Each of those Name
-- parameters correspond to either the datatype of one of the
-- class details. For example, we need the class name, the associated
-- type family, and the to/from functions inside the class.
--
-- I was originally using quasiquoting, but things got weird in
-- the type signatures and pattern names so I just made the AST
-- manually. Interestingly enough, the docs around the AST are
-- pretty great.
makeSimpleBase :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
makeSimpleBase name className tfName toName fromName = do
  -- Match on the datatype to get its constructors (cons') and
  -- type variables (aType).
  TyConI (DataD _ _ [KindedTV aType _] _ cons' _) <- reify name
  join <$> traverse (newDec aType) cons'

 where
  newDec
    :: Name -- ^The type variable name for the content of the container
    -> Con -- ^A constructor
    -> Q [Dec] -- ^The patterns
  -- cName = the contsructor name
  -- types = the list of parameter types for the contsructor
  newDec aType (NormalC cName types) = do
      -- Given the list of types this constructor
      -- takes as a parameter, turn them into unique names.
    binds <- typeListToBinds $ fmap snd types
    -- get the old and new names to use in the signatures
    let oldName = nameBase cName
    let newerName =
          mkName $ fromMaybe (error "nameTooShort") $ initMay oldName
    -- A random other type we use in the pattern
    let bType     = mkName "b"
    -- The type corresponding to the class name we were passed in
    let classType = ConT className
    -- The type signature for the pattern
    -- See the docs for TH to get more info.
    -- Corresponds to:
    -- forall a b. (ClassType b, a ~ TfName b) => a -> a1 -> ... -> b
    let patSigRHS =
          ForallT
              [ PlainTV aType, PlainTV bType ]
              [ AppT classType $ VarT bType
              , AppT (AppT EqualityT $ VarT aType)
                $ AppT (ConT tfName) (VarT bType)
              ]
            $ foldl' mkArrowType (VarT bType)
            $ fmap snd types
    -- Equivalent to:
    -- pattern NewerName :: <patSigRHS>
    let patSig = PatSynSigD newerName patSigRHS

    -- The right hand side of the function. Equivalent to:
    -- (toName -> CName a a1 a2 a3 ... an)
    patRHS <-
      [p| ($(varE toName) -> $(conP cName $ fmap (return . VarP) binds)) |]
    -- The function given by the fromName
    let fromNameFunc = VarE fromName
    -- The inmplementation of the bidirectional pattern. Equivalent to:
    -- pattern NewerName a a1 ... an <- (toName -> CName a a1 ... an) where
    --    NewerName a a1 ... an = fromName $ CName a a1 ... an
    let pat = PatSynD
          newerName
          (PrefixPatSyn binds)
          (ExplBidir
            [ Clause
                (fmap VarP binds)
                (NormalB $ AppE fromNameFunc $ applyAll (ConE cName) binds)
                []
            ]
          )
          patRHS


    return [patSig, pat]

  newDec _ _ = error "Unable to build for this type"
  mkArrowType :: Type -> Type -> Type
  mkArrowType t1 t2 = AppT (AppT ArrowT t2) t1
  applyAll :: Exp -> [Name] -> Exp
  applyAll = foldl' (\en n -> AppE en $ VarE n)
  typeListToBinds :: [a] -> Q [Name]
  typeListToBinds = traverse (\_ -> newName "c")

