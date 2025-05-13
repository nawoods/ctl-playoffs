module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map, map2, map5, field, int, string, index, list)
import Debug exposing (toString)



-- MAIN



main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL



type Model
  = Failure Http.Error
  | LoadingStandings
  | LoadingMatches (List DivisionStandings)
  | Success State

type alias State =
  { standings : List DivisionStandings
  , matches : List Match
  }

type alias DivisionStandings = 
  { name : String
  , players : List PlayerStandings
  }

type alias PlayerStandings =
  { player : String
  , points : Int 
  }

type alias Match =
  { division : String
  , winner : String
  , winnerGames : Int
  , loser : String
  , loserGames : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  (LoadingStandings, getStandings)



-- UPDATE



type Msg
  = GotStandings (Result Http.Error (List DivisionStandings))
  | GotMatches (Result Http.Error (List Match))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotStandings result ->
      case result of
        Ok standings ->
          (LoadingMatches standings, getMatches)
        Err error ->
          (Failure error, Cmd.none)
    GotMatches result ->
      case result of
        Ok matches ->
          case model of 
            Failure error ->
              (Failure error, Cmd.none)
            LoadingStandings ->
              (LoadingStandings, Cmd.none)
            LoadingMatches standings ->
              (Success { standings = standings, matches = matches }, Cmd.none)
            Success state ->
              (Success state, Cmd.none)
        Err error ->
          (Failure error, Cmd.none)
          



-- SUBSCRIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW



view : Model -> Html Msg
view model =
  case model of
    Failure error ->
      div [] [text (toString error)]
    LoadingStandings ->
      div [] [text "hold ya horses"]
    LoadingMatches _ ->
      div [] [text "just a bit more"]
    Success state ->
      div [] 
        [ viewStandings state.standings
        , viewMatches state.matches
        ]

viewStandings : List DivisionStandings -> Html Msg
viewStandings standings =
  div [] (List.map viewDivisionStandings standings)

viewDivisionStandings : DivisionStandings -> Html Msg
viewDivisionStandings divisionStandings =
  div []
    [ 
      div [] [text ("Division " ++ divisionStandings.name)]
    , table []
        (  viewDivisionHeaderRow 
        :: (List.map viewPlayerStandings divisionStandings.players)
        )
    ]
  
viewDivisionHeaderRow : Html Msg
viewDivisionHeaderRow =
  tr [] [
    td [] [text "Player"]
  , td [] [text "Pts"]
  ]

viewPlayerStandings : PlayerStandings -> Html Msg
viewPlayerStandings playerStandings =
  tr []
    [ td [] [text playerStandings.player]
    , td [] [text (toString playerStandings.points)]
    ]



viewMatches : List Match -> Html Msg
viewMatches matches =
  div []
    [ 
      div [] [text "Matches"]
    , table []
        (  viewMatchesHeaderRow 
        :: (List.map viewMatch matches)
        )
    ]

viewMatchesHeaderRow : Html Msg
viewMatchesHeaderRow =
  tr [] [
    td [] [text "Winner"]
  , td [] [text "Loser"]
  , td [] [text "Score"]
  ]

viewMatch : Match -> Html Msg
viewMatch match =
  tr [] 
    [ td [] [text match.winner]
    , td [] [text match.loser]
    , td [] [text (
        (toString match.winnerGames) 
        ++ " - "
        ++ (toString match.loserGames)
      )]
    ]




-- HTTP



getStandings : Cmd Msg
getStandings =
  Http.get
    { url = "https://ctlscoreboard.herokuapp.com/api/standings"
    , expect = Http.expectJson GotStandings standingsDecoder
    }

standingsDecoder : Decoder (List DivisionStandings)
standingsDecoder = list divisionStandingsDecoder

divisionStandingsDecoder : Decoder DivisionStandings
divisionStandingsDecoder = 
  map2 DivisionStandings
    (field "divisionName" string)
    ((field "standings") (list playerStandingsDecoder))

playerStandingsDecoder : Decoder PlayerStandings
playerStandingsDecoder =
  map2 PlayerStandings
    (field "name" string)
    (field "points" int)



getMatches : Cmd Msg
getMatches =
  Http.get
    { url = "https://ctlscoreboard.herokuapp.com/api/match-data"
    , expect = Http.expectJson GotMatches matchesDecoder
    }

matchesDecoder : Decoder (List Match)
matchesDecoder = list matchDecoder

matchDecoder : Decoder Match
matchDecoder = 
  map5 Match
    (field "division" string)
    (field "winner" string)
    (field "winner_games" int)
    (field "loser" string)
    (field "loser_games" int)