import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (getString, post, multipart, stringData)
import Http
import Json.Decode exposing(..)
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
        thead [] [
          tr [] [
            th [] [text "name"]
          , th [] [text "progress"]
          , th [] [text "infohash"]
          , th [] [text "status"]
          , th [] [text "connected peers"]
          , th [] [text "start"]
          , th [] [text "stop"]
          ]
        ]
      , tbody [] (List.map viewTorrent model.activeTorrents)
      ]
    , if model.connectionLive then text "Connection live" else text "Connection lost"
    ]

viewTorrent torrent =
  tr [] [
    td [] [text torrent.name]
  , td [] [text <| toString torrent.progress]
  , td [] [text torrent.infoHash]
  , td [] [text torrent.status]
  , td [] [text <| toString torrent.peerCount]
  , td [] [button [ onClick (StartTorrent torrent.infoHash), disabled (torrent.status == "active") ] [ text "start" ] ]
  , td [] [button [ onClick (StopTorrent torrent.infoHash), disabled (torrent.status == "stopped") ] [ text "stop" ] ]
  ]


type Msg =
    ReloadActive
  | SetActiveTorrents (List ActiveTorrent)
  | Noop
  | LoseConnection
  | StartTorrent String
  | StopTorrent String


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ReloadActive -> (model, getString "http://localhost:8036/active"
                     |> Task.toResult
                     |> Task.perform (\_ -> Noop) parseResponse)
    Noop -> (model, Cmd.none)
    SetActiveTorrents list -> ({ model | activeTorrents = list, connectionLive = True }, Cmd.none)
    LoseConnection -> ({ model | activeTorrents = [], connectionLive = False }, Cmd.none)
    StartTorrent infoHash -> (model, post (keyValuePairs string) "http://localhost:8036/start" (multipart [stringData "infoHash" infoHash]) |> Task.toResult |> Task.perform (\_ -> Noop) (\_ -> Noop))
    StopTorrent infoHash -> (model, post (keyValuePairs string) "http://localhost:8036/stop" (multipart [stringData "infoHash" infoHash]) |> Task.toResult |> Task.perform (\_ -> Noop) (\_ -> Noop))

type alias ActiveTorrent =
  {
    infoHash : String
  , name : String
  , progress : Float
  , status : String
  , peerCount : Int
  }

activeTorrentDecoder : Decoder ActiveTorrent
activeTorrentDecoder =
  Json.object5 ActiveTorrent
    ("infoHash" := string)
    ("name" := string)
    ("progress" := float)
    ("status" := string)
    ("peers" := int)

parseResponse : Result Http.Error String -> Msg
parseResponse res =
  case res of
    Ok str -> case decodeString (Json.list activeTorrentDecoder) str of
      Ok val -> SetActiveTorrents val
      Err err -> LoseConnection
    Err err -> LoseConnection
