module Pages.Consent exposing
  ( view
  )

{- Imports ------------------------------------------------------------------ -}
import Html exposing (Html)
import Html.Attributes
import Html.Events

import Data.UserConsent exposing (UserConsent)

{- Types -------------------------------------------------------------------- -}
type alias Data model =
  { model
  | userConsent : UserConsent
  , userName : String
  , userDate : String
  }

type alias Events msg =
  { userConsentChanged : Data.UserConsent.Field -> msg
  , userNameChanged : String -> msg
  , userDateChanged : String -> msg
  }

{- View --------------------------------------------------------------------- -}
view : Data model -> Events msg -> List (Html msg)
view model { userConsentChanged, userNameChanged, userDateChanged } =
  [ Html.main_
    [ Html.Attributes.class "py-2 container md:mx-auto px-4 pt-8" ]
    [ Html.h1 
      [ Html.Attributes.class "text-2xl mb-2" ]
      [ Html.text "Consent Form" ]
    , Html.p []
      [ Html.span 
        [ Html.Attributes.class "font-bold" ]
        [ Html.text "Title of Study: " ]
      , Html.span []
        [ Html.text "Understanding Programming Practice in Audio Software Programming" ]
      ]
    , Html.p 
      [ Html.Attributes.class "mb-2" ]
      [ Html.span
        [ Html.Attributes.class "font-bold" ]
        [ Html.text "Queen Mary Ethics of Research Committee Ref: " ]
      , Html.span []
        [ Html.text "2308" ]
      ]
    , Html.p 
      [ Html.Attributes.class "mb-2" ]
      [ Html.text 
          <| "Thank you for considering taking part in this research. "
          ++ "The person organizing the research must explain the project to "
          ++ "you before you agree to take part." 
      ]
    , Html.p 
      [ Html.Attributes.class "mb-2" ]
      [ Html.text
          <| "If you have any questions arising from the Information Sheet or "
          ++ "explanation already given to you, please ask the researcher before "
          ++ "you decide whether to join in. You should save a copy of this "
          ++ "Consent Form to keep and refer to at any time. If you are willing "
          ++ "to participate in this study, please check the appropriate "
          ++ "responses and sign and date the declaration underneath."
      ]
    , Html.p []
      [ Html.text "You can contact the researcher at the following email address: "
      , Html.a
        [ Html.Attributes.href "mailto:andrew.thompson@qmul.ac.uk" ]
        [ Html.text "andrew.thompson@qmul.ac.uk" ]
      ]
    , Data.UserConsent.toHtml userConsentChanged model.userConsent
    , Html.p
      [ Html.Attributes.class "mb-2" ]
      [ Html.span 
        [ Html.Attributes.class "font-bold" ]
        [ Html.text "Participant's name: " ]
      , Html.input
        [ Html.Attributes.class "ml-2 border-b-2"
        , Html.Attributes.value model.userName
        , Html.Events.onInput userNameChanged
        ] []
      ]
    , Html.p
      [ Html.Attributes.class "mb-2" ]
      [ Html.span []
        [ Html.text "Date: " ]
      , Html.input
        [ Html.Attributes.class "ml-2 border-b-2"
        , Html.Attributes.value model.userDate
        , Html.Events.onInput userDateChanged
        ] []
      ]
    , Html.h2 
      [ Html.Attributes.class "text-xl mt-4 mb-2" ]
      [ Html.text "Investigator's Statement:" ]
    , Html.p 
      [ Html.Attributes.class "mb-4" ]
      [ Html.text
          <| "I Andrew Thompson confirm that I have carefully explained the "
          ++ "nature, demands and any foreseeable risks (where applicable) of "
          ++ "the proposed research to the volunteer and provided a copy of "
          ++ "this form."
      ]
    ]
  , Html.footer
    [ Html.Attributes.class "flex mt-4 py-2 container md:mx-auto px-4 pb-8" ]
    [ Html.a
      [ Html.Attributes.class 
          <| "flex-1 mr-10 bg-transparent hover:bg-blue-500 text-blue-700 "
          ++ "font-semibold hover:text-white py-2 px-4 border border-blue-500 "
          ++ "hover:border-transparent rounded"
      , Html.Attributes.href "#info" 
      ]
     [ Html.text "back" ]
    , if Data.UserConsent.hasUserConsent model.userConsent then
      Html.a
        [ Html.Attributes.class 
            <| "flex-1 ml-10 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 "
            ++ "px-4 rounded"
        , Html.Attributes.href "#qsort" 
        ]
        [ Html.text "next" ]
      else
      Html.button
        [ Html.Attributes.class 
            <| "flex-1 ml-10 bg-blue-500 text-white font-bold py-2 px-4 rounded "
            ++ "opacity-50 cursor-not-allowed"
        ]
        [ Html.text "next" ]
    ]
  ] 
