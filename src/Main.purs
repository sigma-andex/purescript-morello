module Main where

import Prelude
import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Bitraversable (bisequence)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Functor (mapFlipped)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Lens (Forget(..), Prism', over, preview, prism', set, traverseOf, traversed, view)
import Data.Lens.Getter (view)
import Data.Lens.Record (prop)
import Data.Lens.Setter (Setter)
import Data.Lens.Traversal (sequenceOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong ((&&&))
import Data.String (null)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup (V(..), andThen)
import Data.Validation.Semigroup as V
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, class HFoldl, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Nub, class Union, class Lacks, class Cons)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Record (insert, merge, union)
import Type.Prelude (Proxy(..), RLProxy(..))

type Person
  = { person ::
        { addresses ::
            Array
              { zip :: String
              }
        }
    , profession ::
        { title :: String
        }
    }

-- x :: Person
x =
  { person:
      { addresses:
          [ { zip: "123"
            }
          , { zip: "456"
            }
          ]
      }
  , profession:
      { title: "Software Engineer"
      }
  }

personL = prop (SProxy :: SProxy "person")

addressesL = prop (SProxy :: SProxy "addresses")

zipL = prop (SProxy :: SProxy "zip")

professionL = prop (SProxy :: SProxy "profession")

titleL = prop (SProxy :: SProxy "title")

addresses = personL <<< addressesL <<< traversed

data ValidationError
  = ZipIsEmpty
  | ZipInvalid
  | TitleMissing
  | TitleInvalid

derive instance genericValidationError :: Generic ValidationError _

instance showValidationError :: Show ValidationError where
  show = genericShow

validateNonEmpty :: String -> V (NonEmptyArray ValidationError) String
validateNonEmpty input
  | null input = V.invalid $ NonEmpty.singleton ZipIsEmpty
  | otherwise = pure input

validateZip :: String -> V (NonEmptyArray ValidationError) Int
validateZip input = case fromString input of
  Just i -> pure i
  Nothing -> V.invalid (NonEmpty.singleton ZipInvalid)

validateForm ::
  String ->
  V (NonEmptyArray ValidationError) Int
validateForm zip = validateNonEmpty zip *> validateZip zip

maybeToV :: forall a. ValidationError -> Maybe a -> V (NonEmptyArray ValidationError) a
maybeToV _ (Just v) = pure v

maybeToV err Nothing = V.invalid (NonEmpty.singleton err)

type Validated r
  = V (NonEmptyArray ValidationError) r

type PersonX
  = { person ::
        { addresses ::
            Array
              { zip :: Int
              }
        }
    , profession ::
        { title :: String
        }
    }

type TitleX
  = { title :: String }

type Validator a b
  = a -> Validated b

-- asValidated :: forall a b. Monoid a => Validator a b -> Prism' a (Validated a)
-- asValidated validate = prism' backward forward 
--   where 
--     backward = mempty
--     forward = pure <<< validate 
valid = pure

asValid :: forall a. Prism' (Validated a) a
asValid =
  prism' valid case _ of
    V (Left err) -> Nothing
    V (Right v) -> Just v

validate :: forall t29 t30 t31 t32 t33. (Star t33 t30 t29 -> Star t33 t32 t31) -> (t30 -> t33 t29) -> t32 -> t33 t31
validate = traverseOf

validTitle :: Validator String String
validTitle s@"Software Engineer" = pure s

validTitle e = V.invalid $ NonEmpty.singleton TitleInvalid

branch :: forall input. input -> Tuple input (Validated {})
branch = identity &&& const (valid {})

infixr 8 branch as 🌱

cherry :: forall input from to' to. 
  Union from to' to ⇒ 
  (input -> Validated { | to' } ) -> 
  Tuple input (Validated { | from }) -> 
  Tuple input (Validated { | to })
cherry f = fst &&& transform f
  where
  
    transform :: (input -> Validated { | to' }) -> Tuple input (Validated { | from }) -> (Validated { | to })
    transform f' tuple = Tuple <$> snd tuple <*> (f' (fst tuple)) <#> uncurry union

infixr 8 cherry as 🍒

blossom :: forall input output. Tuple input (Validated output) -> Validated output
blossom = snd

infixr 8 blossom as 🌸

data ShowProps = ShowProps

-- instance showProps ::
--   (Show a, 
--   IsSymbol sym,
--   Lacks sym (),
--   Cons sym String () whatever,
--   Union whatever input output
--   ) =>
--   FoldingWithIndex ShowProps (SProxy sym) { | input } a { | output } where
--   foldingWithIndex ShowProps prop pre a =
--     Tuple (insert prop ("fck-yeah-" <> show a) {}) pre # uncurry union

-- pick :: forall t77 t78. HFoldlWithIndex ShowProps (Record ()) t77 t78 => t77 -> t78
-- pick r =
--   hfoldlWithIndex ShowProps {} r

instance showProps ::
  (--Show a, 
  IsSymbol sym,
  Lacks sym (),
  Cons sym { | a } () whatever,
  RowToList a aRL,
  FoldlRecord ShowProps {} aRL a { | a },
  Union whatever input output
  ) =>
  FoldingWithIndex ShowProps (SProxy sym) { | input } { | a }  { | output } where
  foldingWithIndex ShowProps prop pre a =
    Tuple (insert prop (spy "a" $ hfoldlWithIndex ShowProps {} a) {}) (spy "pre" pre) # uncurry union

instance showPropsS ::
  (--Show a, 
  IsSymbol sym,
  Lacks sym (),
  Cons sym String () whatever,
  Union whatever input output
  ) =>
  FoldingWithIndex ShowProps (SProxy sym) { | input } String  { | output } where
  foldingWithIndex ShowProps prop pre a =
    Tuple (insert prop ("fck-yeah-" <> (spy "a" $ a)) {}) (spy "pre" pre) # uncurry union

pick :: forall input output. HFoldlWithIndex ShowProps {} { | input } { | output } => { | input }  -> { | output }
pick r =
  hfoldlWithIndex ShowProps {} r


invalid = NonEmpty.singleton >>> V.invalid

convert = branch >>>
  cherry (\_ -> valid { firstName : "Hans" }) >>>
  cherry (\_ -> invalid ZipInvalid :: Validated { zip:: String}) >>> 
  cherry (validate (professionL <<< titleL) validTitle) >>>
  cherry (\_ -> invalid TitleInvalid :: Validated { title:: String}) >>> 
  blossom

-- purescript-morello

convert2 = 
  (🌱) >>>
  (🍒) (\_ -> valid { firstName : "Hans" }) >>>
  (🍒) (\_ -> invalid ZipInvalid :: Validated { zip :: String}) >>> 
  (🍒) (validate (professionL <<< titleL) validTitle) >>>
  (🍒) (\_ -> invalid TitleInvalid :: Validated { title :: String}) >>> 
  (🌸)

main :: Effect Unit
main = do
  logShow $ (convert2 x)
  let
    _ = spy "record" $ pick { a: { b : "foo" } , b: "42" , c: "false", d : { e : "hans", f: "23"} }
  logShow $ ""
