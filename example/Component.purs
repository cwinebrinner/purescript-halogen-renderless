module Example.Component where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.ChildQuery as CQ
import Renderless.State (modifyStore_)

data Query pq cs m a
  = Raise (pq Unit) a
  | Send (CQ.ChildQueryBox cs a)
  | Receive (Input pq cs m) a

type StateStore pq cs m =
  Store State (H.ComponentHTML (Query pq cs m) cs m)

type State = Unit

type Input pq cs m =
  { render :: State -> H.ComponentHTML (Query pq cs m) cs m }

data Message pq
  = Emit (pq Unit)

type Slot pq cs m = H.Slot (Query pq cs m) (Message pq)

component :: ∀ pq cs m. H.Component HH.HTML (Query pq cs m) (Input pq cs m) (Message pq) m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: Input pq cs m -> StateStore pq cs m
  initialState { render } = store render unit

  eval
    :: Query pq cs m
    ~> H.HalogenM (StateStore pq cs m) (Query pq cs m) cs (Message pq) m
  eval = case _ of
    Raise query a -> do
      H.raise (Emit query)
      pure a

    Send box -> do
      H.HalogenM $ liftF $ H.ChildQuery box

    Receive { render } a -> do
      modifyStore_ render (\s -> s)
      pure a

