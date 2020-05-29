module Pages.QSort exposing
  ( view
  )

{- Imports ------------------------------------------------------------------ -}
import Html exposing (Html)
import Html.Attributes

import Data.QSort exposing (QSort)

{- Types -------------------------------------------------------------------- -}
type alias Data model =
  { model
  | qsort : QSort
  }

type alias Events msg =
  { itemSelected : Data.QSort.Statement -> msg
  , itemRated : Data.QSort.Rating -> msg
  , itemSorted : Int -> msg
  , stepForward : msg
  , stepBackward : msg
  }

{- View --------------------------------------------------------------------- -}
view : Data model -> Events msg -> List (Html msg)
view model { itemSelected, itemRated, itemSorted, stepForward, stepBackward } =
  [ Html.main_
    [ Html.Attributes.class "container md:mx-auto px-4 pt-8" ]
    [ model.qsort |> Data.QSort.toHtml
        { selectMsg = itemSelected
        , rateMsg = itemRated
        , sortMsg = itemSorted
        , stepForward = stepForward
        , stepBackward = stepBackward
        }
    ]
  , Html.footer
    [ Html.Attributes.class "flex mt-4 py-2 container md:mx-auto px-4 pb-8" ]
    [ Html.a
      [ Html.Attributes.class 
          <| "flex-1 mr-10 bg-transparent hover:bg-blue-500 text-blue-700 "
          ++ "font-semibold hover:text-white py-2 px-4 border border-blue-500 "
          ++ "hover:border-transparent rounded"
      , Html.Attributes.href "#2" 
      ]
      [ Html.text "back" ]
    , if Data.QSort.isComplete model.qsort then
        Html.a
          [ Html.Attributes.class 
              <| "flex-1 ml-10 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 "
              ++ "px-4 rounded"
          , Html.Attributes.href "#success"
          ]
          [ Html.text "submit" ]
      else
        Html.button
          [ Html.Attributes.class 
              <| "flex-1 ml-10 bg-blue-500 text-white font-bold py-2 px-4 rounded "
              ++ "opacity-50 cursor-not-allowed"
          ]
          [ Html.text "submit" ]
    ]
  ]
