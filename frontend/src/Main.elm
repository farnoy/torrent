import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http exposing (getString)
import Http
import Json.Decode exposing(decodeString, (:=), string, float, Decoder)
import Json.Decode as Json
import Task as Task
import Time

main =
  Html.program { init = init
               , view = view
               , update = update
               , subscriptions = \_ -> Sub.batch [
                   Time.every (5 * Time.second) (always ReloadActive)
                 ]
               }


type alias Model = { activeTorrents : List ActiveTorrent, connectionLive : Bool }


init : (Model, Cmd Msg)
init = ({ activeTorrents = [], connectionLive = False },
        Task.succeed ReloadActive |> Task.perform (\_ -> Noop) (\a -> a))


view model =
  div []
    [ button [ onClick ReloadActive ] [ text "Refresh" ]
    , table [] [
        tbody [] (List.map viewTorrent model.activeTorrents)
      ]
    , if model.connectionLive then text "Connection live" else text "Connection lost"
    ]

viewTorrent torrent =
  tr [] [
    td [] [text torrent.name]
  , td [] [text <| toString torrent.progress]
  , td [] [text torrent.infoHash]
  ]


type Msg =
    ReloadActive
  | SetActiveTorrents (List ActiveTorrent)
  | Noop
  | LoseConnection


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ReloadActive -> (model, getString "http://localhost:8036/active"
                     |> Task.toResult
                     |> Task.perform (\_ -> Noop) parseResponse)
    Noop -> (model, Cmd.none)
    SetActiveTorrents list -> ({ model | activeTorrents = list, connectionLive = True }, Cmd.none)
    LoseConnection -> ({ model | activeTorrents = [], connectionLive = False }, Cmd.none)

type alias ActiveTorrent = { infoHash : String, name : String, progress: Float }

activeTorrentDecoder : Decoder ActiveTorrent
activeTorrentDecoder =
  Json.object3 ActiveTorrent
    ("infoHash" := string)
    ("name" := string)
    ("progress" := float)

parseResponse : Result Http.Error String -> Msg
parseResponse res =
  case res of
    Ok str -> case decodeString (Json.list activeTorrentDecoder) str of
      Ok val -> SetActiveTorrents val
      Err err -> LoseConnection
    Err err -> LoseConnection
