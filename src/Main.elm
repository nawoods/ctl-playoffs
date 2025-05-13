module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, map2, map5, field, int, string, index, list)
import Debug exposing (toString)
import String exposing (left)



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
  , infoView : InfoView
  }

type alias DivisionStandings = 
  { name : String
  , players : List PlayerStandings
  }

type alias PlayerStandings =
  { player : String
  , wins : Int 
  , losses : Int 
  , points : Int 
  }

type alias Match =
  { division : String
  , winner : String
  , winnerGames : Int
  , loser : String
  , loserGames : Int
  }

type InfoView 
  = Standings
  | Matches


init : () -> (Model, Cmd Msg)
init _ =
  (LoadingStandings, getStandings)



-- UPDATE



type Msg
  = GotStandings (Result Http.Error (List DivisionStandings))
  | GotMatches (Result Http.Error (List Match))
  | SetInfoView InfoView

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
            LoadingMatches standings ->
              (Success
                { standings = standings
                , matches = matches 
                , infoView = Standings
                }, Cmd.none)
            _ ->
              (model, Cmd.none)
        Err error ->
          (Failure error, Cmd.none)
    SetInfoView infoView ->
      case model of
        Success state ->
          (Success { state | infoView = infoView }, Cmd.none)
        _ ->
          (model, Cmd.none)
      
          



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
        [ viewInfoButtons
        , viewInfoContent state
        ]

viewInfoButtons : Html Msg
viewInfoButtons =
  div [] 
    [ button [onClick (SetInfoView Standings)] [text "Standings"]
    , button [onClick (SetInfoView Matches)] [text "Matches"]
    ]

viewInfoContent : State -> Html Msg
viewInfoContent state =
      case state.infoView of
        Standings ->
          viewStandings (List.filter isDiv1 state.standings)
        Matches ->
          viewMatches state.matches

viewStandings : List DivisionStandings -> Html Msg
viewStandings standings =
  div [] (List.map viewDivisionStandings standings)

viewDivisionStandings : DivisionStandings -> Html Msg
viewDivisionStandings divisionStandings =
  div []
    [ 
      div [] [text ("Division " ++ divisionStandings.name)]
    , table []
        ( viewDivisionHeaderRow :: viewPlayerRows divisionStandings )
    ]
  
viewDivisionHeaderRow : Html Msg
viewDivisionHeaderRow =
  tr [] [
    td [] [text "Player"]
  , td [] [text "W-L"]
  , td [] [text "Pts"]
  , td [] [text "Pts Back"]
  ]

viewPlayerRows : DivisionStandings -> List (Html Msg)
viewPlayerRows divisionStandings =
  List.map (viewPlayerRow (winnerRelPts divisionStandings)) divisionStandings.players

viewPlayerRow : Float -> PlayerStandings -> Html Msg
viewPlayerRow leaderRelPts playerStandings =
  tr []
    [ td [] [text playerStandings.player]
    , td [] [text 
        ((toString playerStandings.wins)
        ++ "-"
        ++ (toString playerStandings.losses)
        )]
    , td [] [text (toString playerStandings.points)]
    , td [] [text (
      if leaderRelPts == relPts playerStandings
      then "--"
      else toString (leaderRelPts - relPts playerStandings)
    )]
    ]



viewMatches : List Match -> Html Msg
viewMatches matches =
  div []
    [ 
      div [] [text "Matches"]
    , table []
        (  viewMatchesHeaderRow 
        :: (List.map viewMatch (List.filter isDiv1Match matches)))
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

isDiv1 : DivisionStandings -> Bool
isDiv1 division =
  left 1 division.name == "1"

isDiv1Match : Match -> Bool
isDiv1Match match =
  left 1 match.division == "1"


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
  map4 PlayerStandings
    (field "name" string)
    (field "wins" int)
    (field "losses" int)
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



-- CALC
relPts : PlayerStandings -> Float
relPts player =
  toFloat player.points - (3.5 * toFloat (player.wins + player.losses))

winnerRelPts : DivisionStandings -> Float
winnerRelPts division =
  List.foldl max 0 (List.map relPts division.players)
  
  