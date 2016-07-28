import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (getString, post, multipart, stringData)
import Http
import Json.Decode exposing(..)
import Json.Decode as Json
import String
import Svg
import Svg.Attributes as Svg
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
          , th [] [text "download speed"]
          , th [] [text "connected peers"]
          , th [] [text "start"]
          , th [] [text "stop"]
          , th [] [text "bitfield"]
          , th [] [text "download speed history"]
          ]
        ]
      , tbody [] (List.map viewTorrent model.activeTorrents)
      ]
    , if model.connectionLive then text "Connection live" else text "Connection lost"
    ]

viewTorrent torrent =
  tr [] [
    td [] [text torrent.name]
  , td [] [text <| toString (toFloat (round (torrent.progress * 10000)) / 100) ++ "%"]
  , td [] [text torrent.infoHash]
  , td [] [text torrent.status]
  , td [] [torrent.downloadSpeed |> List.filter (\a -> a /= 0) |> List.reverse |> List.take 5 |> List.map toFloat |> List.sum |> \a -> a / 1024 / 5 |> toString |> text]
  , td [] [text <| toString torrent.peerCount]
  , td [] [button [ onClick (StartTorrent torrent.infoHash), disabled (torrent.status == "active") ] [ text "start" ] ]
  , td [] [button [ onClick (StopTorrent torrent.infoHash), disabled (torrent.status == "stopped") ] [ text "stop" ] ]
  , td [] [
      Svg.svg [Svg.width "300", Svg.height "80", Svg.viewBox ("0 0 " ++ toString (List.length torrent.bitField) ++ " 80"), Svg.preserveAspectRatio "none"]
              (List.indexedMap viewBitField torrent.bitField)
    ]
  , td [] [
      Svg.svg [Svg.width "300", Svg.height "80", Svg.viewBox ("0 0 60 1000"), Svg.preserveAspectRatio "none"]
              [(viewDownloadHistory torrent.downloadSpeed)]
    ]
  ]

viewBitField : Int -> Int -> Svg.Svg a
viewBitField index bar =
  let
      xStr = toString index
      y2 = 80 - toFloat bar / 255 * 80
      y2Str = toString y2
  in Svg.line [Svg.x1 xStr, Svg.y1 "80", Svg.x2 xStr, Svg.y2 y2Str, Svg.stroke "red"] []

viewDownloadHistory : List Int -> Svg.Svg a
viewDownloadHistory hist =
  let
      points =  List.reverse hist
             |> List.take 60
             |> List.reverse
             |> List.indexedMap (\ix hist -> [toString ix, toString <| 1000 - (toFloat hist) / 1000])
             |> List.map (String.join ",")
             |> String.join " "
  in Svg.polyline [Svg.points points, Svg.stroke "blue", Svg.fill "none"] []


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
  , bitField : List Int
  , downloadSpeed : List Int
  }

activeTorrentDecoder : Decoder ActiveTorrent
activeTorrentDecoder =
  Json.object7 ActiveTorrent
    ("infoHash" := string)
    ("name" := string)
    ("progress" := float)
    ("status" := string)
    ("peers" := int)
    ("bitField" := Json.Decode.list int)
    ("downloadSpeed" := Json.Decode.list int)

parseResponse : Result Http.Error String -> Msg
parseResponse res =
  case res of
    Ok str -> case decodeString (Json.list activeTorrentDecoder) str of
      Ok val -> SetActiveTorrents val
      Err err -> LoseConnection
    Err err -> LoseConnection
