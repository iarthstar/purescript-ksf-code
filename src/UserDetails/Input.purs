module UserDetails.Input where

import Axios
import Debug.Trace
import Effect.Console
import Prelude
import UserDetails.Types

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic (JSX, capture_, element, StateUpdate(..), capture, monitor, Self)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, targetChecked, preventDefault, targetValue)
import React.Basic.Events as Events
import UserDetails.Utils (classy)

foreign import updateDom :: Effect Unit

type Props = 
  { onChange :: Maybe String -> Effect Unit
  , id :: String
  , placeHolder :: String
  , defaultValue :: String
  , type :: String
  }

type State = { inputValue :: String }

initialState :: State
initialState = { inputValue : "" }

data Action
  = Noop
  | LoadState State

component :: React.Component Props
component = React.createComponent "Input"

input :: Props -> JSX
input = React.make component
    { initialState
    , render
    , didMount
    , update 
    } 
  where
    didMount self@{ props } =  React.send self (LoadState { inputValue: props.defaultValue })
    
    update self@{ state } action = case action of

      -- LoadState loadedState -> Update loadedState
      LoadState loadedState -> UpdateAndSideEffects loadedState (const $ updateDom)
      

      _ -> NoUpdate

render :: Self Props State Action -> JSX
render self@{ props } = classy DOM.div "mdl-textfield mdl-js-textfield mdl-textfield--floating-label"
    [ DOM.input
      { className: "mdl-textfield__input"
      , type: props.type
      , id: props.id
      , name: props.id
      , autoComplete: "off"
      , value: props.defaultValue
      , onChange
      }
    , DOM.label
      { className: "mdl-textfield__label"
      , htmlFor: props.id
      , children: [ DOM.text props.placeHolder ]
      }
    ]
  where 
    onChange = Events.handler (preventDefault >>> Events.merge { targetValue })
      \{ targetValue } -> props.onChange targetValue