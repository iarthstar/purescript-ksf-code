module UserDetails.App where

import Axios (Config(..), Header(..), Method(..), axios)
import Axios (class Axios, genericAxios)
import Debug.Trace
import Effect.Console
import Prelude
import UserDetails.Types

import Data.Array
import Data.Either (Either(..))
import Data.Foldable
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.String as String
import Data.Tuple
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
  , editable      :: Boolean
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
  , editable      : false
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
  | OnEdit
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
render self@{ state } = 
    classy DOM.div "parent"
      ([ 
      -- | Loading View
        DOM.div
          { className: "lParent"
          , id: "loading"
          , style: DOM.css { display: if state.isLoading then "block" else "none" }
          , children: [ classy DOM.div "mdl-spinner mdl-spinner--single-color mdl-js-spinner is-active lChild" [] ]
          }
      ] <> if state.loggedIn 
        then  
          -- | Details View
          [ detailsView self
          ]
        else
          -- | Login View
            [ loginView self ]
      )

links :: Array (Tuple String String)
links = [ Tuple "Frågor och svar" "https://www.hbl.fi/fragor-och-svar/"
        , Tuple "Ingen tidning" "https://www.hbl.fi/ingen-tidning/"
        , Tuple "Läs e-tidning" "https://www.hbl.fi/epaper/" 
        ]

anchor :: String -> String -> JSX
anchor text href = DOM.a { className: "href-link", href, children: [ DOM.text text ]}

makeLinksWithBullets :: Int -> Tuple String String -> Array JSX
makeLinksWithBullets index (Tuple text link) = case mod index 2 of 
    0 ->  [ anchor text link ]
    _ ->  [ DOM.text " • "
          , anchor text link ]

loginView :: Self Props State Action -> JSX
loginView self@{ state } = classy DOM.div "mdl-grid mdl-grid--no-spacing child"
    [ classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
    , classy DOM.div "mdl-cell mdl-cell--4-col-tablet  mdl-cell--4-col-desktop mdl-cell--4-col-phone"
        [ classy DOM.div ""
            [ classy DOM.div "mdl-card__title full-width font-duplex-bold text-center"
                [ classy DOM.p "mdl-card__title-text full-width inline-block" 
                    [ DOM.text "Välkommen till KSF Media’s Mitt Konto"] 
                ]
            , classy DOM.div "mdl-card__supporting-text text-center" 
                [ classy DOM.p "full-width inline-block" (concat $ mapWithIndex makeLinksWithBullets links)
                , classy DOM.p "full-width inline-block"
                    [ DOM.text "Här kan du göra tillfällig eller permanent adressändring eller göra uppehåll i tidningsutdelningen. Dessutom kan du få allmän information som är relevant för dig som prenumerant." ]
                , DOM.br {}
                , DOM.br {}
                , classy DOM.p "full-width inline-block"
                    [ DOM.text "Allt du behöver göra för att komma igång är att logga in! " ]
                , classy DOM.p "full-width inline-block"
                    [ DOM.text "Behöver du hjälp? "
                    , anchor "Kundservice" "https://www.hbl.fi/kundservice/"    
                    ]
                , classy DOM.div "mdl-grid mdl-grid--no-spacing"
                    [ classy DOM.div "mdl-cell mdl-cell--1-col-tablet  mdl-cell--2-col-desktop mdl-cell--0-col-phone" []
                    , classy DOM.div "mdl-cell mdl-cell--6-col-tablet  mdl-cell--8-col-desktop mdl-cell--4-col-phone" 
                        [ DOM.form
                            { action: "javascript:void(0);"
                            , children: 
                                [ Input.input
                                    { onChange: React.send self <<< OnUsername
                                    , id: "username"
                                    , placeHolder: "Username"
                                    , type: "text"
                                    , defaultValue: fromMaybe "" state.username
                                    , className: Just "email"
                                    , disabled: false
                                    }
                                , Input.input
                                    { onChange: React.send self <<< OnPassword
                                    , id: "password"
                                    , placeHolder: "Password"
                                    , type: "password"
                                    , defaultValue: fromMaybe "" state.password
                                    , className: Just "pass"
                                    , disabled: false
                                    }
                                , DOM.button
                                    { className: "button"
                                    , style: DOM.css { width: "100%" }
                                    , onClick: React.capture_ self OnLogin
                                    , children: [ DOM.text "LOGGA IN" ]
                                    }   
                                ]
                            }
                        ]
                    , classy DOM.div "mdl-cell mdl-cell--1-col-tablet  mdl-cell--2-col-desktop mdl-cell--0-col-phone" []
                    ]
                ]
            ]
        ]
    , classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
    ]


detailsView :: Self Props State Action -> JSX
detailsView self@{ state } = classy DOM.div "mdl-grid mdl-grid--no-spacing child"
    [ classy DOM.div "mdl-cell mdl-cell--2-col-tablet  mdl-cell--4-col-desktop mdl-cell--0-col-phone" []
    , classy DOM.div "mdl-cell mdl-cell--4-col-tablet  mdl-cell--4-col-desktop mdl-cell--4-col-phone"
        [ classy DOM.div "mdl-card"
            [ classy DOM.div "mdl-card__title"
                [ classy DOM.h2 "mdl-card__title-text full-width inline-block" 
                  [ DOM.text "User Details" 
                  , DOM.button
                      { className: "button"
                      , style: DOM.css { width: "60px", float: "right", fontSize: "0.8em", marginRight: "4.5%" }
                      , onClick: React.capture_ self OnEdit
                      , children: [ classy DOM.i "material-icons" [ DOM.text if state.editable then "done" else "edit" ] ]
                      }
                  ] ]
            , classy DOM.div "mdl-card__supporting-text"
                [ DOM.form 
                  { action: "javascript:void(0);"
                  , children: 
                      [ Input.input 
                        { onChange: React.send self <<< OnFirstName
                        , id: "firstName"
                        , placeHolder: "First Name"
                        , type: "text"
                        , defaultValue: fromMaybe "" state.firstName
                        , className: Nothing
                        , disabled: not state.editable
                        }
                    , Input.input 
                        { onChange: React.send self <<< OnLastName
                        , id: "lastName"
                        , placeHolder: "Last Name"
                        , type: "text"
                        , defaultValue: fromMaybe "" state.lastName
                        , className: Nothing
                        , disabled: not state.editable
                        }
                    , Input.input 
                        { onChange: React.send self <<< OnStreetAddress
                        , id: "streetAddress"
                        , placeHolder: "Street Address"
                        , type: "text"
                        , defaultValue: fromMaybe "" state.streetAddress
                        , className: Nothing
                        , disabled: not state.editable
                        }
                    , Input.input 
                        { onChange: React.send self <<< OnStreetName
                        , id: "streetName"
                        , placeHolder: "Street Name"
                        , type: "text"
                        , defaultValue: fromMaybe "" state.streetName
                        , className: Nothing
                        , disabled: not state.editable
                        }
                    , Input.input 
                        { onChange: React.send self <<< OnZipCode
                        , id: "zipCode"
                        , placeHolder: "Zip Code"
                        , type: "text"
                        , defaultValue: fromMaybe "" state.zipCode
                        , className: Nothing
                        , disabled: not state.editable
                        }
                    , Input.input 
                        { onChange: React.send self <<< OnCountryCode
                        , id: "countryCode"
                        , placeHolder: "Country Code"
                        , type: "text"
                        , defaultValue: fromMaybe "" state.countryCode
                        , className: Nothing
                        , disabled: not state.editable
                        }
                    , DOM.br {}
                    , DOM.br {}
                    , DOM.button
                        { className: "button"
                        , style: DOM.css { width: "120px" }
                        , onClick: React.capture_ self OnSubmit
                        , children: [ DOM.text "SUBMIT" ]
                        }
                      , DOM.button
                        { className: "button"
                        , style: DOM.css { float: "right", width: "120px" }
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

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self@{ state } action = case action of

    OnUsername value -> Update self.state { username = value }
    
    OnPassword value -> Update self.state { password = value }        
    
    OnFirstName value -> Update self.state { firstName = value }
    
    OnLastName value -> Update self.state { lastName = value }
    
    OnStreetAddress value -> Update self.state { streetAddress = value }
    
    OnStreetName value -> Update self.state { streetName = value }

    OnZipCode value -> Update self.state { zipCode = value }
    
    OnCountryCode value -> Update self.state { countryCode = value }

    OnEdit -> case self.state.editable of
        true -> Update self.state { editable = false }
        false -> Update self.state { editable = true }


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
                          ]
              , data: patchDetailsReq
              }
        UpdateAndSideEffects self.state { isLoading = true } (const $ submitUserDetails self userConfig)
      _        ,          _ -> NoUpdate

    LoadState loadedState -> Update loadedState

    _ -> NoUpdate


loginUserDetails :: Axios LoginReq LoginRes => Self Props State Action -> Config LoginReq -> Effect Unit
loginUserDetails self loginConfig = Aff.launchAff_ $ axios loginConfig >>= liftEffect <<< case _ of
    Left _ -> React.send self (LoadState self.state { isLoading = false })
    Right (LoginRes a) -> do
        let detailsConfig = Config
              { url: "https://persona.api.ksfmedia.fi/v1/users/" <> a.uuid
              , method: GET
              , headers:  [ Header "Cache-Control" "no-cache"
                          , Header "accept" "application/json;charset=utf-8"
                          , Header "Authorization" ("OAuth " <> a.token)
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
    Left _ -> React.send self (LoadState self.state { isLoading = false })
    Right (PatchDetailsRes a) -> React.send self (LoadState self.state { isLoading = false })