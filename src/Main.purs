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
import Morello.Morello.Core (blossom, branch, cherry)
import Morello.Validated (Validated, ValidationError, Validator(..), valid)
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

x :: Person
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

titleValidator :: Validator Person String
titleValidator = Validator (view (professionL <<< titleL) >>> valid)

type PersonB = { title :: String }


--convert :: Person -> Validated PersonB
convert :: Person -> Validated PersonB
convert = branch >>>
      cherry {
            title : titleValidator
          } >>>
      blossom

      
-- purescript-morello

-- convert2 = 
--   (🌱) >>>
--   (🍒) (\_ -> valid { firstName : "Hans" }) >>>
--   (🍒) (\_ -> invalid ZipInvalid :: Validated { zip :: String}) >>> 
--   (🍒) (validate (professionL <<< titleL) validTitle) >>>
--   (🍒) (\_ -> invalid TitleInvalid :: Validated { title :: String}) >>> 
--   (🌸)

main :: Effect Unit
main = do
  let 
    _ = spy "bla" (convert x)
  log ""
