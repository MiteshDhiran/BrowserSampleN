module LoadingStatus exposing (Status(..))


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed
