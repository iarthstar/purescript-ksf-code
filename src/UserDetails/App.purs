module UserDetails.App where

import Axios (Config(..), Header(..), Method(..), axios)
import Axios (class Axios, genericAxios)
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
import React.Basic.DOM.Events (key, targetChecked, targetValue)
import React.Basic.Events as Events
import UserDetails.Input as Input
import UserDetails.Utils (classy)

type Props = {}

type State =
  { username      :: Maybe String
  , password      :: Maybe String
  , token         :: Maybe String 
  , uuid          :: Maybe String 
  , firstName     :: Maybe String 
  , lastName      :: Maybe String 
  , streetAddress :: Maybe String 
  , streetName    :: Maybe String 
  , zipCode       :: Maybe String 
  , countryCode   :: Maybe String 
  , isLoading     :: Boolean
  , loggedIn      :: Boolean
  }

initialState :: State
initialState =
  { username      : Just "iarthstar@gmail.com"
  , password      : Just "12345678"
  , token         : Nothing
  , uuid          : Nothing
  , firstName     : Nothing
  , lastName      : Nothing
  , streetAddress : Nothing
  , streetName    : Nothing
  , zipCode       : Nothing
  , countryCode   : Nothing
  , isLoading     : false
  , loggedIn      : false
  }

data Action
  = Noop
  | LoadState State
  | UpdateAndDom State
  | OnFirstName (Maybe String)
  | OnLastName (Maybe String)
  | OnStreetAddress (Maybe String)
  | OnStreetName (Maybe String)
  | OnZipCode (Maybe String)
  | OnCountryCode (Maybe String)
  | OnUsername (Maybe String)
  | OnPassword (Maybe String)
  | OnLogin
  | OnLogout
  | OnSubmit

component :: React.Component Props
component = React.createComponent "App"

type AppProps = {}

app :: AppProps -> JSX
app = React.make component
    { initialState
    , render
    , update
    }

-- | Pure render function
render :: Self Props State Action -> JSX
render self = 
    classy DOM.div "parent"
      ([ 
      -- | Loading View
        DOM.div
          { className: "lParent"
          , id: "loading"
          , style: DOM.css { display: if self.state.isLoading then "block" else "none" }
          , children: [ classy DOM.div "mdl-spinner mdl-spinner--single-color mdl-js-spinner is-active lChild" [] ]
          }
      ] <> if self.state.loggedIn 
        then  
          -- | Details View
          [ classy DOM.div "mdl-grid mdl-grid--no-spacing child"
              [ classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
              , classy DOM.div "mdl-cell mdl-cell--4-col-tablet  mdl-cell--4-col-desktop mdl-cell--4-col-phone"
                  [ classy DOM.div "mdl-card mdl-shadow--4dp"
                      [ classy DOM.div "mdl-card__title"
                          [ classy DOM.h2 "mdl-card__title-text" [ DOM.text "User Details" ] ]
                      , classy DOM.div "mdl-card__supporting-text"
                          [ DOM.form 
                            { action: "javascript:void(0);"
                            , children: 
                                [ Input.input 
                                  { onChange: React.send self <<< OnFirstName
                                  , id: "firstName"
                                  , placeHolder: "First Name"
                                  , type: "text"
                                  , defaultValue: fromMaybe "" self.state.firstName
                                  }
                              , Input.input 
                                  { onChange: React.send self <<< OnLastName
                                  , id: "lastName"
                                  , placeHolder: "Last Name"
                                  , type: "text"
                                  , defaultValue: fromMaybe "" self.state.lastName
                                  }
                              , Input.input 
                                  { onChange: React.send self <<< OnStreetAddress
                                  , id: "streetAddress"
                                  , placeHolder: "Street Address"
                                  , type: "text"
                                  , defaultValue: fromMaybe "" self.state.streetAddress
                                  }
                              , Input.input 
                                  { onChange: React.send self <<< OnStreetName
                                  , id: "streetName"
                                  , placeHolder: "Street Name"
                                  , type: "text"
                                  , defaultValue: fromMaybe "" self.state.streetName
                                  }
                              , Input.input 
                                  { onChange: React.send self <<< OnZipCode
                                  , id: "zipCode"
                                  , placeHolder: "Zip Code"
                                  , type: "text"
                                  , defaultValue: fromMaybe "" self.state.zipCode
                                  }
                              , Input.input 
                                  { onChange: React.send self <<< OnCountryCode
                                  , id: "countryCode"
                                  , placeHolder: "Country Code"
                                  , type: "text"
                                  , defaultValue: fromMaybe "" self.state.countryCode
                                  }
                              , DOM.br {}
                              , DOM.br {}
                              , DOM.button
                                  { className: "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored"
                                  , onClick: React.capture_ self OnSubmit
                                  , children: [ DOM.text "SUBMIT" ]
                                  }
                              , DOM.button
                                  { className: "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored"
                                  , style: DOM.css { float: "right" }
                                  , onClick: React.capture_ self OnLogout
                                  , children: [ DOM.text "LOGOUT" ]
                                  }
                              ]
                          }
                        ]
                      ]
                  ]
              , classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
              ]
          ]
        else
          -- | Login View
          [ classy DOM.div "mdl-grid mdl-grid--no-spacing child"
              [ classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
              , classy DOM.div "mdl-cell mdl-cell--4-col-tablet  mdl-cell--4-col-desktop mdl-cell--4-col-phone"
                  [ classy DOM.div "mdl-card mdl-shadow--4dp"
                      [ classy DOM.div "mdl-card__supporting-text"
                          [ DOM.form 
                              { action: "javascript:void(0);"
                              , children: 
                                  [ Input.input 
                                      { onChange: React.send self <<< OnUsername
                                      , id: "username"
                                      , placeHolder: "Username"
                                      , defaultValue: fromMaybe "" self.state.username
                                      , type: "text"
                                      } 
                                  , DOM.br {}
                                  , Input.input
                                      { onChange: React.send self <<< OnPassword
                                      , id: "password"
                                      , placeHolder: "Password"
                                      , defaultValue: fromMaybe "" self.state.password
                                      , type: "password"
                                      }
                                  , DOM.br {}
                                  , DOM.br {}
                                  , DOM.button
                                      { className: "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored"
                                      , onClick: React.capture_ self OnLogin
                                      , children: [ DOM.text "LOGIN" ]
                                      }
                                  ]
                              }
                          ]
                      ]
                  ]
              , classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
              ]
          ])


update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self@{ state } action = case action of

    OnUsername value -> do
      let _ = spy "Username" value
      Update self.state { username = value }
    
    OnPassword value -> Update self.state { password = value }        
    
    OnFirstName value -> do
      let _ = spy "FirstName" value
      Update self.state { firstName = value }
    
    OnLastName value -> do
      let _ = spy "LastName" value
      Update self.state { lastName = value }
    
    OnStreetAddress value -> do
      let _ = spy "streetAddress" value
      Update self.state { streetAddress = value }
    
    OnStreetName value -> do
      let _ = spy "streetName" value
      Update self.state { streetName = value }

    OnZipCode value -> do
      let _ = spy "zipCode" value
      Update self.state { zipCode = value }
    
    OnCountryCode value -> do
      let _ = spy "countryCode" value
      Update self.state { countryCode = value }


    OnLogin -> case state.username, state.password of
        Just ""      , _             -> NoUpdate
        _            , Just ""       -> NoUpdate
        Just username, Just password -> do
          let updatedState = self.state { isLoading = true }
          let loginConfig = Config
                { url: "https://persona.api.ksfmedia.fi/v1/login"
                , method: POST
                , headers:  [ Header "Cache-Control" "no-cache"
                            , Header "Content-Type" "application/json"
                            , Header "accept" "application/json;charset=utf-8"
                            , Header "Access-Control-Allow-Origin" "https://purescript-ksf-code.netlify.com"
                            , Header "Access-Control-Allow-Methods" "POST, GET, PATCH"
                          ]
                , data: LoginReq { username, password }
                }
          UpdateAndSideEffects updatedState (const $ loginUserDetails self loginConfig)
        _            ,             _ -> NoUpdate
      
    OnLogout -> Update initialState  

    OnSubmit -> case self.state.uuid, self.state.token of
      Just uuid, Just token -> do
        let patchDetailsReq = PatchDetailsReq
              { firstName : fromMaybe "" self.state.firstName
              , lastName : fromMaybe "" self.state.lastName
              , address :
                  { streetAddress : fromMaybe "" self.state.streetAddress
                  , streetName : fromMaybe "" self.state.streetName
                  , zipCode : fromMaybe "" self.state.zipCode
                  , countryCode : fromMaybe "" self.state.countryCode
                  }
              }
        let userConfig = Config
              { url: "https://persona.api.ksfmedia.fi/v1/users/" <> uuid
              , method: PATCH
              , headers:  [ Header "accept" "application/json;charset=utf-8"
                          , Header "Content-Type" "application/json"
                          , Header "Authorization" ("OAuth " <> token) 
                          , Header "Access-Control-Allow-Origin" "https://purescript-ksf-code.netlify.com"
                          , Header "Access-Control-Allow-Methods" "POST, GET, PATCH"
                          ]
              , data: patchDetailsReq
              }
        UpdateAndSideEffects self.state { isLoading = true } (const $ submitUserDetails self userConfig)
      _        ,          _ -> NoUpdate

    LoadState loadedState -> Update loadedState

    _ -> NoUpdate


loginUserDetails :: Axios LoginReq LoginRes => Self Props State Action -> Config LoginReq -> Effect Unit
loginUserDetails self loginConfig = Aff.launchAff_ $ axios loginConfig >>= liftEffect <<< case _ of
    Left _ -> pure unit
    Right (LoginRes a) -> do
        let detailsConfig = Config
              { url: "https://persona.api.ksfmedia.fi/v1/users/" <> a.uuid
              , method: GET
              , headers:  [ Header "Cache-Control" "no-cache"
                          , Header "accept" "application/json;charset=utf-8"
                          , Header "Authorization" ("OAuth " <> a.token)
                          , Header "Access-Control-Allow-Origin" "https://purescript-ksf-code.netlify.com"
                          , Header "Access-Control-Allow-Methods" "POST, GET, PATCH"
                          ]
              , data: DetailsReq {}
              }
        Aff.launchAff_ $ axios detailsConfig >>= liftEffect <<< case _ of
          Left _ -> pure unit
          Right (DetailsRes b) -> do
              let updatedState = self.state { isLoading = false
                                            , loggedIn = true
                                            , token = Just a.token
                                            , uuid = Just a.uuid
                                            , firstName = Just b.firstName
                                            , lastName = Just b.lastName
                                            , streetAddress = Just b.address.streetAddress
                                            , streetName = Just b.address.streetName
                                            , zipCode = Just b.address.zipCode
                                            , countryCode = Just b.address.countryCode
                                            }
              React.send self (LoadState updatedState)


submitUserDetails :: Axios PatchDetailsReq PatchDetailsRes => Self Props State Action -> Config PatchDetailsReq -> Effect Unit
submitUserDetails self userConfig = Aff.launchAff_ $ axios userConfig >>= liftEffect <<< case _ of
    Left _ -> pure unit
    Right (PatchDetailsRes a) -> React.send self (LoadState self.state { isLoading = false })