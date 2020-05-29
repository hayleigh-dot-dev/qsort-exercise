module Data.QSort exposing
  ( QSort, init
  , Statement, Rating
  , select, rate, sort
  , stepForward, stepBackward
  , isComplete
  , toHtml
  , encode, decoder
  )

-- Imports ---------------------------------------------------------------------
import Html as Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Set exposing (Set)
import Ui.Colour
import Ui.Button
import Ui.Section

-- Types -----------------------------------------------------------------------
type QSort
  = Basic BasicData
  | Normal NormalData

type alias BasicData =
  { title : String
  , description : String
  , statements : List Statement
  , unsorted : List Statement
  , selected : Maybe Statement
  }

type alias NormalData =
  { title : String
  , description : String
  , statements : List (Maybe Statement)
  , unsorted : List Statement
  , selected : Maybe Statement
  , length : Int
  , shape : List Int
  }

type alias Statement =
  { title : String
  , key : String
  , description : List String
  , image : Maybe String
  , rating : Rating
  }

type Rating
  = Negative
  | Neutral
  | Positive

type alias Events msg =
  { selectMsg : Statement -> msg
  , rateMsg : Rating -> msg
  , sortMsg : Int -> msg
  , stepForward : msg
  , stepBackward : msg
  }

-- Functions -------------------------------------------------------------------
--
init : String -> String -> Set String -> QSort
init title description items =
  Basic
    { title = title
    , description = description
    , statements = []
    , unsorted = Set.toList items |> List.map (\s -> Statement s "" [] Nothing Neutral)
    , selected = Nothing
    }

--
select : Statement -> QSort -> QSort
select statement qsort =
  case qsort of
    Basic ({ statements, unsorted, selected } as data) ->
      if Just statement == selected then
        Basic { data | selected = Nothing }
      else if List.member statement statements || List.member statement unsorted then
        Basic { data | selected = Just statement }
      else
        Basic { data | selected = Nothing }

    Normal ({ statements, unsorted, selected } as data) ->
      if Just statement == selected then
        Normal { data | selected = Nothing }
      else if List.member (Just statement) statements || List.member statement unsorted then
        Normal { data | selected = Just statement }
      else
        Normal { data | selected = Nothing }

--
rate : Rating -> QSort -> QSort
rate rating qsort =
  case qsort of
    Basic ({ selected, statements, unsorted } as data) ->
      Basic
        { data
        | unsorted = 
            unsorted |> List.filterMap (Just >> (\s -> 
              if s == selected then 
                Nothing 
              else 
                s
            ))
        , statements =
            statements
              |> List.filterMap (Just >> (\s -> if s == selected then Nothing else s))
              |> Just
              |> Maybe.map2 (::) (Maybe.map (\s -> { s | rating = rating }) selected)
              |> Maybe.withDefault statements
        , selected =
            Nothing
        }

    Normal ({ selected, statements, unsorted } as data) ->
      Normal
        { data
        | unsorted =
            if List.member selected statements then
              List.map Just unsorted
                |> (::) (Maybe.map (\s -> { s | rating = rating }) selected)
                |> List.filterMap identity
            else
              unsorted |> List.map (\s ->
                if Just s == selected then 
                  { s | rating = rating}
                else
                  s
              )
        , statements =
            statements |> List.map (\s ->
              if s /= selected then
                s
              else
                Nothing
            )
        }

--
sort : Int -> QSort -> QSort
sort position qsort =
  case qsort of
    Basic data ->
      Basic data

    Normal ({ selected, statements, unsorted } as data) ->
      Normal
        { data
        | statements =
            statements |> List.indexedMap (\i s ->
              if i == position && Maybe.isJust selected then
                selected
              else if i /= position && s == selected then
                Nothing
              else
                s
            )
        , unsorted =
            unsorted |> List.filter (Just >> (/=) selected)
        , selected = Nothing
        }

--
stepForward : QSort -> QSort
stepForward qsort =
  let
    nearestSquare : Int -> Int
    nearestSquare n =
      Basics.toFloat n
        |> Basics.sqrt
        |> Basics.round
        |> \x -> x ^ 2

    squareRoot : Int -> Int
    squareRoot n =
      Basics.toFloat n
       |> Basics.sqrt
       |> Basics.round
  in
  case qsort of
    Basic { title, description, statements } ->
      Normal
        { title = title
        , description = description
        , statements = List.repeat (List.length statements |> nearestSquare) Nothing
        , unsorted = statements
        , selected = Nothing
        , length = List.length statements |> nearestSquare
        , shape = 
            List.length statements 
              |> nearestSquare 
              |> squareRoot
              |> (\root -> List.range 1 root ++ (List.reverse <| List.range 1 <| root - 1))
        }

    Normal data ->
      Normal data

--
stepBackward : QSort -> QSort
stepBackward qsort =
  case qsort of
    Basic data ->
      Basic data

    Normal { title, description, statements, unsorted } ->
      Basic
        { title = title
        , description = description
        , statements = []
        , unsorted = unsorted ++ List.filterMap identity statements
        , selected = Nothing
        }

--
isComplete : QSort -> Bool
isComplete qsort =
  case qsort of
    Basic _ ->
      False

    Normal { unsorted } ->
      List.length unsorted == 0


-- View ------------------------------------------------------------------------
--
toHtml : Events msg -> QSort -> Html msg
toHtml events qsort =
  case qsort of
    Basic data ->
      viewBasicSort events data

    Normal data ->
      viewNormalSort events data

--
viewBasicSort : Events msg -> BasicData -> Html msg
viewBasicSort events { title, description, statements, unsorted, selected } =
  let
    instructions =
      Html.div
        []
        [ Html.h3 
          [ A.class "font-bold text-xl" ]
          [ Html.text "Instructions" ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "For this part of the exercise you must rate features on whether "
              ++ "they might have a positive or negative effect on your programming "
              ++ "practice, or none at all."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "Select an item from the list on the left. A description and "
              ++ "sometimes an image will appear on the right. This can be helpful "
              ++ "if you're not familiar with the feature being described."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "The language features you'll be presented with come from a variety "
              ++ "of different languages and paradigms, and examples have been chosen "
              ++ "to exemplify how and where they're most typically used. Try not "
              ++ "to focus too much on the syntax of a particular example (unless "
              ++ "the syntax is the main focus of that example) and instead try "
              ++ "to imagine that feature's impact regardless of whether your preferred "
              ++ "language does or can support it."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "We estimate this task will take around 30 minutes, dependent on "
              ++ "your familiarity with each of the features presented. Don't forget "
              ++ "you are free to leave this questionnaire and return at a later "
              ++ "time, your progress is saved."
          ]
        , Html.hr [ A.class "border border-black mb-4" ] []
        ]

    unsortedList =
      Html.div [ A.class "flex my-2 h-96" ]
        [ Html.div [ A.class "flex-1 mr-4" ]
          [ viewStatementList selected events.selectMsg unsorted ]
        , Html.div [ A.class "flex-1 overflow-y-scroll overflow-x-hidden" ]
          [ selected |> Maybe.map viewStatementInfo
              |> Maybe.withDefault (Html.text "")
          ]
        ]

    controls =
      Html.div [ A.class "flex my-2" ]
        [ Ui.Button.builder
            |> Ui.Button.withText "Negative"
            |> Ui.Button.withColour (Ui.Colour.lighten Ui.Colour.red)
            |> Ui.Button.withHandler (events.rateMsg Negative)
            |> Ui.Button.withClass "flex-1 p-2 mr-4" 
            |> Ui.Button.toHtml
        , Ui.Button.builder
            |> Ui.Button.withText "Neutral"
            |> Ui.Button.withColour (Ui.Colour.lighten Ui.Colour.grey)
            |> Ui.Button.withHandler (events.rateMsg Neutral)
            |> Ui.Button.withClass "flex-1 p-2 mx-2" 
            |> Ui.Button.toHtml
        , Ui.Button.builder
            |> Ui.Button.withText "Positive"
            |> Ui.Button.withColour (Ui.Colour.lighten Ui.Colour.green)
            |> Ui.Button.withHandler (events.rateMsg Positive)
            |> Ui.Button.withClass "flex-1 p-2 ml-4" 
            |> Ui.Button.toHtml
        ]

    sortedList =
      Html.div [ A.class "flex mt-4 h-64" ]
        [ Html.div [ A.class "flex-1 mr-4" ]
          [ List.filter (.rating >> (==) Negative) statements
              |> viewStatementList selected events.selectMsg
          ]
        , Html.div [ A.class "flex-1 mx-2" ]
          [ List.filter (.rating >> (==) Neutral) statements
              |> viewStatementList selected events.selectMsg
          ]
        , Html.div [ A.class "flex-1 ml-4" ]
          [ List.filter (.rating >> (==) Positive) statements
              |> viewStatementList selected events.selectMsg
          ]
        ]

    nextButton =
      Html.div [ A.class "flex mb-4" ]
        [ Ui.Button.builder
          |> Ui.Button.withText "Next Step"
          |> Ui.Button.withColour (
            if List.isEmpty unsorted then
              Ui.Colour.blue
            else
              Ui.Colour.grey
          )
          |> Ui.Button.withHandler events.stepForward
          |> Ui.Button.withClass "flex-1 p-2"
          |> Ui.Button.withClass (
            if List.isEmpty unsorted then
              ""
            else
              "cursor-not-allowed"
          )
          |> Ui.Button.withAttr (A.disabled (not <| List.isEmpty unsorted))
          |> Ui.Button.toHtml
        ]
  in
  Ui.Section.empty
    |> Ui.Section.withTitle title
    |> Ui.Section.withDescription description
    |> Ui.Section.addClass "container mx-auto"
    |> Ui.Section.addAttr (A.attribute "data-q-sort" "basic")
    |> Ui.Section.addChild instructions
    |> Ui.Section.addChild unsortedList
    |> Ui.Section.addChild controls
    |> Ui.Section.addChild sortedList
    |> Ui.Section.addChild nextButton
    |> Ui.Section.toHtml

--
viewNormalSort : Events msg -> NormalData -> Html msg
viewNormalSort events { title, description, statements, unsorted, selected, shape, length } =
  let
    instructions =
      Html.div
        []
        [ Html.h3 
          [ A.class "font-bold text-xl" ]
          [ Html.text "Instructions" ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "In this part of the exercise you'll need to arrange the features "
              ++ "you just rated from most negatively impactful to your practice "
              ++ "to most positively impactful."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "A rating of -5 corresponds to 'Most Negatively Impactful' and "
              ++ "a rating of 5 corresponds to 'Most Positively Impactful'. Ratings "
              ++ "closer to 0 indicate that the feature has little to no impact "
              ++ "on your practice. For ratings that hold more than on feature, "
              ++ "there is no signficance to ordering. This means that two items "
              ++ "rated at 3 have roughly the same impact on your practice."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "Start by picking out the most impactful features and working your "
              ++ "way inwoards. Don't worry if you have more items in one category "
              ++ "than another, the scale is relative. This means a feature rated "
              ++ "at -5 isn't necessarily as impactful as a feature rated at 5."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "The language features you'll be presented with come from a variety "
              ++ "of different languages and paradigms, and examples have been chosen "
              ++ "to exemplify how and where they're most typically used. Try not "
              ++ "to focus too much on the syntax of a particular example (unless "
              ++ "the syntax is the main focus of that example) and instead try "
              ++ "to imagine that feature's impact regardless of whether your preferred "
              ++ "language does or can support it."
          ]
        , Html.p
          [ A.class "mb-2" ]
          [ Html.text
              <| "We estimate this task will take around 30 minutes, dependent on "
              ++ "your familiarity with each of the features presented. Don't forget "
              ++ "you are free to leave this questionnaire and return at a later "
              ++ "time, your progress is saved."
          ]
        , Html.hr [ A.class "border border-black mb-4" ] []
        ]

    statementInfo =
      Html.div [ A.class "flex h-96 overflow-y-scroll mb-4" ]
        ( selected |> Maybe.map viewSplitStatementInfo
            |> Maybe.withDefault [ Html.text "" ]
        )

    normalDistribution =
      Html.div [ A.class "flex justify-between my-2" ]
        ( List.indexedMap Tuple.pair statements
            |> viewNormalDistribution events.selectMsg events.sortMsg selected shape
        )

    sortButtons =
      Html.div [ A.class "flex my-2" ]
        [ Ui.Button.builder
            |> Ui.Button.withText "Negative"
            |> Ui.Button.withColour (Ui.Colour.lighten Ui.Colour.red)
            |> Ui.Button.withHandler (events.rateMsg Negative)
            |> Ui.Button.withClass "flex-1 p-2 mr-4" 
            |> Ui.Button.toHtml
        , Ui.Button.builder
            |> Ui.Button.withText "Neutral"
            |> Ui.Button.withColour (Ui.Colour.lighten Ui.Colour.grey)
            |> Ui.Button.withHandler (events.rateMsg Neutral)
            |> Ui.Button.withClass "flex-1 p-2 mx-2" 
            |> Ui.Button.toHtml
        , Ui.Button.builder
            |> Ui.Button.withText "Positive"
            |> Ui.Button.withColour (Ui.Colour.lighten Ui.Colour.green)
            |> Ui.Button.withHandler (events.rateMsg Positive)
            |> Ui.Button.withClass "flex-1 p-2 ml-4" 
            |> Ui.Button.toHtml
        ]

    unsortedList =
      Html.div [ A.class "flex my-2 h-64" ]
        [ Html.div [ A.class "flex-1 mr-4" ]
          [ List.filter (.rating >> (==) Negative) unsorted
              |> viewStatementList selected events.selectMsg
          ]
        , Html.div [ A.class "flex-1 mx-2" ]
          [ List.filter (.rating >> (==) Neutral) unsorted
              |> viewStatementList selected events.selectMsg
          ]
        , Html.div [ A.class "flex-1 ml-4" ]
          [ List.filter (.rating >> (==) Positive) unsorted
              |> viewStatementList selected events.selectMsg
          ]
        ]
  in
  Ui.Section.empty
    |> Ui.Section.withTitle title
    |> Ui.Section.withDescription description
    |> Ui.Section.addClass "container mx-auto"
    |> Ui.Section.addAttr (A.attribute "data-q-sort" "normal")
    |> Ui.Section.addChild instructions
    |> Ui.Section.addChildren
      [ statementInfo
      , Html.hr [ A.class "border border-black mb-4" ] []
      , Html.div [ A.class "flex justify-between my-2" ]
        ( length 
            |> Basics.toFloat 
            |> Basics.sqrt 
            |> (*) 2 
            |> Basics.floor
            |> (\n -> n - 2)
            |> List.range 0
            |> List.map (\n -> n + 1 - (length |> Basics.toFloat |> Basics.sqrt |> Basics.floor))
            |> List.map (\n -> 
              Html.span 
                [ A.class "flex-1 mx-2 text-center font-bold" ] 
                [ Html.text (String.fromInt n) ]
            )
        )
      , normalDistribution
      , Html.hr [ A.class "border border-black mb-4" ] []
      , sortButtons
      , unsortedList
      ]
    |> Ui.Section.toHtml
    
--
ratingToString : Rating -> String
ratingToString rating =
  case rating of
    Negative  -> "Negative"
    Neutral   -> "Neutral"
    Positive  -> "Positive"

--
viewStatementList : Maybe Statement -> (Statement -> msg) -> List Statement -> Html msg
viewStatementList selected handler statements =
  Html.ul [ A.class "h-full overflow-y-scroll overflow-x-hidden" ]
    ( statements |> List.map (\statement ->
        viewStatementItem (Just statement == selected) (handler statement) statement
      )
    ) 

--
viewStatementItem : Bool -> msg -> Statement ->  Html msg
viewStatementItem active handler { title, key } =
  Html.li 
    [ E.onClick handler
    , A.class "cursor-pointer my-2 p-2"
    , if active then
        A.class "bg-blue-300 hover:bg-blue-500"
      else
        A.class "bg-gray-300 hover:bg-gray-400"
    ] 
    [ Html.span [ A.class "font-bold pr-2" ] [ Html.text <| "[" ++ key ++ "]" ]
    , Html.span [ A.class "text-justify" ] [ Html.text title ]
    ]

viewStatementInfo : Statement -> Html msg
viewStatementInfo { title, description, image } =
  Html.div
    []
    [ Html.h3 [ A.class "text-xl font-bold mb-4" ] 
      [ Html.text title ]
    , image |> Maybe.map (\src ->
        Html.img [ A.src src, A.class "w-full" ] []
      ) |> Maybe.withDefault (Html.text "")
    , Html.div [] ( description |> List.map (\text ->
        Html.p [ A.class "text-justify py-2 pr-4" ] 
          [ Html.text text ]
      ))
    ]

viewSplitStatementInfo : Statement -> List (Html msg)
viewSplitStatementInfo { title, description, image } =
  [ Html.div [ A.class "flex-1 pr-4" ]
    [ Html.h3 [ A.class "text-xl font-bold" ] 
      [ Html.text title ]
    , Html.div [] ( description |> List.map (\text ->
        Html.p [ A.class "py-2 text-justify" ] 
          [ Html.text text ]
      ))
    ]
  , Html.div [ A.class "flex-1 pl-4" ]
    [ image |> Maybe.map (\src ->
        Html.img [ A.src src, A.class "w-full" ] []
      ) |> Maybe.withDefault (Html.text "")
    ]
  ]

--
viewNormalDistribution : (Statement -> msg) -> (Int -> msg) -> Maybe Statement -> List Int -> List (Int, Maybe Statement) -> List (Html msg)
viewNormalDistribution selectMsg sortMsg selectedStatement items statements =
  List.foldl (\n  (ns, ss) -> List.take n ss |> (\ss_ -> (ss_ :: ns, List.drop n ss))) ([], statements) items
    |> Tuple.first
    |> List.map (\ss ->
      Html.ul [ A.class "flex-1 mx-2 flex-col" ]
        ( ss |> List.map (viewQSortItem selectMsg sortMsg selectedStatement)
        )
    )  

viewQSortItem : (Statement -> msg) -> (Int -> msg) -> Maybe Statement -> (Int, Maybe Statement) -> Html msg
viewQSortItem selectMsg sortMsg selectedStatement (n, s) =
  case s of
    Just ({ key } as statement) ->
      Html.li
        [ A.class "w-full mb-2 h-10 hover:bg-blue-500 rounded flex content-center items-center align-center cursor-pointer" 
        , if selectedStatement == s then
            A.class "bg-blue-300"
          else
            A.class "bg-gray-400"
        , E.onClick (selectMsg statement) ]
        [ Html.div
          [ A.class "font-bold p-2 mx-auto" ]
          [ Html.text <| "[" ++ key ++ "]" ]
        ]

    Nothing ->
      Html.li
        [ A.class "w-full mb-2 h-10 bg-gray-400 hover:bg-gray-500 rounded flex content-center align-center cursor-pointer" 
        , E.onClick (sortMsg n)
        ] []

-- JSON  -----------------------------------------------------------------------
encode : QSort -> Encode.Value
encode qsort =
  let
    encodeStatement { title, key, description, image, rating } =
      Encode.object
        [ ("title", Encode.string title)
        , ("key", Encode.string key)
        , ("description", Encode.list Encode.string description)
        , ("image", Maybe.withDefault Encode.null <| Maybe.map Encode.string image)
        , ("rating", Encode.string <| ratingToString rating)
        ]

    nullable encoder a =
      Maybe.map encoder a |> Maybe.withDefault Encode.null
      
  in
  case qsort of
    Basic { title, description, statements, unsorted } ->
      Encode.object
        [ ("type", Encode.string "Basic")
        , ("title", Encode.string title)
        , ("description", Encode.string description)
        , ("statements", Encode.list encodeStatement statements)
        , ("unsorted", Encode.list encodeStatement unsorted)
        ]

    Normal { title, description, statements, unsorted, length } ->
      Encode.object
        [ ("type", Encode.string "Normal")
        , ("title", Encode.string title)
        , ("description", Encode.string description)
        , ("statements", Encode.list identity <| List.map (nullable encodeStatement) statements)
        , ("unsorted", Encode.list encodeStatement unsorted)
        , ("length", Encode.int length)
        ]

decoder : Decoder QSort
decoder =
  let
    basicData title description statements unsorted =
      { title = title
      , description = description
      , statements = statements
      , unsorted = unsorted
      , selected = Nothing
      }

    nearestSquare n =
      Basics.toFloat n
        |> Basics.sqrt
        |> Basics.round
        |> \x -> x ^ 2

    squareRoot n =
      Basics.toFloat n
       |> Basics.sqrt
       |> Basics.round

    normalData title description statements unsorted length =
      { title = title
      , description = description
      , statements = statements
      , unsorted = unsorted
      , selected = Nothing
      , length = length
      , shape = 
          List.length statements 
            |> nearestSquare 
            |> squareRoot
            |> (\root -> List.range 1 root ++ (List.reverse <| List.range 1 <| root - 1))
      }

    statementDecoder =
      Decode.map5 Statement
        (Decode.field "title" Decode.string)
        (Decode.field "key" Decode.string)
        (Decode.field "description" <| Decode.list Decode.string)
        (Decode.field "image" <| Decode.nullable Decode.string)
        (Decode.field "rating" ratingDecoder)

    ratingDecoder =
      Decode.string |> Decode.andThen (\s ->
        case s of
          "Negative"  -> Decode.succeed Negative
          "Neutral"   -> Decode.succeed Neutral
          "Positive"  -> Decode.succeed Positive
          _           -> Decode.fail s
      )
  in
  Decode.field "type" Decode.string |> Decode.andThen (\t ->
    case t of
      "Basic" ->
        Decode.map Basic <| Decode.map4 basicData
          (Decode.field "title" Decode.string)
          (Decode.field "description" Decode.string)
          (Decode.field "statements" <| Decode.list statementDecoder)
          (Decode.field "unsorted" <| Decode.list statementDecoder)

      "Normal" ->
        Decode.map Normal <| Decode.map5 normalData
          (Decode.field "title" Decode.string)
          (Decode.field "description" Decode.string)
          (Decode.field "statements" <| Decode.list (Decode.nullable statementDecoder))
          (Decode.field "unsorted" <| Decode.list statementDecoder)
          (Decode.field "length" Decode.int)
          -- (Decode.field "shape" <| Decode.list Decode.int)

      _ ->
        Decode.fail t
  )
