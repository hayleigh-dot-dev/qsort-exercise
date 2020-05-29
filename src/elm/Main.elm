port module Main exposing
  ( main
  )

{- Imports ------------------------------------------------------------------ -}
import Browser
import Browser.Dom
import Browser.Navigation
import Html
import Json.Decode
import Json.Encode
import Set
import Task
import Tuple.Extra
import Url exposing (Url)

import Data.UserConsent exposing (UserConsent)
import Data.QSort exposing (QSort)

import Pages.Info
import Pages.Consent
import Pages.QSort


{- Ports -------------------------------------------------------------------- -}
port toWebSocket : Json.Encode.Value -> Cmd msg
port fromWebSocket : (Json.Decode.Value -> msg) -> Sub msg


{- Main --------------------------------------------------------------------- -}
main : Program Json.Decode.Value App Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = onUrlChange
    , onUrlRequest = onUrlRequest
    }

onUrlChange : Url -> Msg
onUrlChange url =
  UrlChanged url

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
  case urlRequest of
    Browser.Internal url ->
      InternalUrlRequested url

    Browser.External url ->
      ExternalUrlRequested url

{- Model -------------------------------------------------------------------- -}
type alias App =
  (Page, Browser.Navigation.Key, Model)

type alias Model =
  { errorMessage : Maybe String
  , userConsent : UserConsent
  , userName : String
  , userDate : String
  , userEmail : String
  , qsort : QSort
  }

type Page
  = Info
  | Consent
  | QSort
  | Error

type alias Flags =
  { qsort : QSort
  }

flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
  Json.Decode.map Flags
    (Json.Decode.field "qsort" Data.QSort.decoder)

init : Json.Decode.Value -> Url -> Browser.Navigation.Key -> (App, Cmd Msg)
init flags _ key =
  case Json.Decode.decodeValue flagsDecoder flags of
    Ok { qsort } ->
      ( ( Info
        , key
        , { errorMessage = Nothing
          , userConsent = Data.UserConsent.init
          , userName = ""
          , userDate = ""
          , userEmail = ""
          , qsort = qsort
          }
        )
      , Cmd.none
      )

    Err e ->
      Tuple.Extra.pairWith Cmd.none <|
        ( Error
        , key
        , { errorMessage = Just (Json.Decode.errorToString e)
          , userConsent = Data.UserConsent.init
          , userName = ""
          , userDate = ""
          , userEmail = ""
          , qsort = Data.QSort.init "" "" Set.empty
          }
        )

{- Update ------------------------------------------------------------------- -}
type Msg
  = None
  | UrlChanged Url
  | InternalUrlRequested Url
  | ExternalUrlRequested String
  -- Consent Form
  | UserConsentChanged Data.UserConsent.Field
  | UserNameChanged String
  | UserDateChanged String
  -- QSort Exercise
  | ItemSelected Data.QSort.Statement
  | ItemRated Data.QSort.Rating
  | ItemSorted Int
  | StepForward
  | StepBackward
  | ExternalUpdate QSort

update : Msg -> App -> (App, Cmd Msg)
update msg (page, key, model) =
  case msg of
    None ->
      Tuple.pair (page, key, model) Cmd.none

    UrlChanged ({ fragment } as url) ->
      case fragment of
        Just "3" ->
          Tuple.pair (updatePage url, key, model)
            <| Task.perform (\_ -> None) (Browser.Dom.setViewport 0 0)

        Just "success" ->
          Tuple.pair (updatePage url, key, model)
            <| Task.perform (\_ -> None) (Browser.Dom.setViewport 0 0)

        _ ->
          Tuple.Extra.pairWith (Task.perform (\_ -> None) (Browser.Dom.setViewport 0 0)) <|
            ( updatePage url
            , key
            , model
            )

    InternalUrlRequested url ->
      Tuple.Extra.pairWith (Browser.Navigation.pushUrl key (Url.toString url)) <|
        ( page
        , key
        , model
        )

    ExternalUrlRequested url ->
      Tuple.Extra.pairWith (Browser.Navigation.load url) <|
        ( page
        , key
        , model
        )

    UserConsentChanged field ->
      let 
        m = { model | userConsent = Data.UserConsent.update field model.userConsent }
      in
      Tuple.pair (page, key, m) Cmd.none

    UserNameChanged name ->
      let
        m = { model | userName = name }
      in 
      Tuple.pair (page, key, m) Cmd.none

    UserDateChanged date ->
      let
        m = { model | userDate = date }
      in
      Tuple.pair (page, key, m) Cmd.none

    ItemSelected statement ->
      let
        m = { model | qsort = Data.QSort.select statement model.qsort }
      in
      Tuple.pair (page, key, m)
        <| toWebSocket (Data.QSort.encode m.qsort)

    ItemRated rating ->
      let
        m = { model | qsort = Data.QSort.rate rating model.qsort }
      in
      Tuple.pair (page, key, m)
        <| toWebSocket (Data.QSort.encode m.qsort)

    ItemSorted position ->
      let
        m = { model | qsort = Data.QSort.sort position model.qsort }
      in
      Tuple.pair (page, key, m)
        <| toWebSocket (Data.QSort.encode m.qsort)

    StepForward ->
      let
        m = { model | qsort = Data.QSort.stepForward model.qsort }
      in
      Tuple.pair (page, key, m) <| Cmd.batch
        [ Task.perform (\_ -> None) <| Browser.Dom.setViewport 0 0
        , toWebSocket (Data.QSort.encode m.qsort)
        ]

    StepBackward ->
      let
        m = { model | qsort = Data.QSort.stepBackward model.qsort }
      in
      Tuple.pair (page, key, m) <| Cmd.batch
        [ Task.perform (\_ -> None) <| Browser.Dom.setViewport 0 0
        , toWebSocket (Data.QSort.encode m.qsort)
        ]

    ExternalUpdate qsort ->
      let
        m = { model | qsort = qsort }
      in
      Tuple.pair (page, key, m) Cmd.none

updatePage : Url -> Page
updatePage { fragment } =
  case fragment of
    Nothing         -> Info
    Just "info"     -> Info
    Just "consent"  -> Consent
    Just "qsort"    -> QSort
    _               -> Info


{- View --------------------------------------------------------------------- -}
title : String -> String
title prefix =
  prefix ++ " – Understanding Programming Practice in Audio Software Programming"

view : App -> Browser.Document Msg
view (page, _, model) =
  case page of
    Info ->
      { title = title "Info"
      , body =
          Pages.Info.view
      }

    Consent ->
      { title = title "Consent"
      , body =
          Pages.Consent.view model
            { userConsentChanged = UserConsentChanged
            , userNameChanged = UserNameChanged
            , userDateChanged = UserDateChanged
            }
      }

    QSort ->
      { title = title "QSort Exercise"
      , body =
          Pages.QSort.view model
            { itemSelected = ItemSelected
            , itemRated = ItemRated
            , itemSorted = ItemSorted
            , stepForward = StepForward
            , stepBackward = StepBackward
            }
      }

    Error ->
      { title = title "Error"
      , body =
          [ Html.text <| Maybe.withDefault "Decode error" model.errorMessage ]
      }

{- Subscriptions ------------------------------------------------------------ -}
subscriptions : App -> Sub Msg
subscriptions _ =
  Sub.batch
    [ fromWebSocket (
        Json.Decode.decodeValue Data.QSort.decoder
          >> Result.map ExternalUpdate
          >> Result.withDefault None
      )
    ]
