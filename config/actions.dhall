let Tilers = ./tilers.dhall
let Mode = { modeName : Text, hasButtons : Bool, hasBorders : Bool }
let Direction = <Back | Front>
let Actions = < Insert : Tilers | RunCommand : Text | ChangeModeTo : Mode | ShowWindow : Text | HideWindow : Text | ZoomInInput | ZoomInMonitor | ZoomOutInput | ZoomOutMonitor | PopTiler | PushTiler | MakeSpecial | ChangeNamed : Text | Move : Direction | KillActive | ExitNow | ToggleLogging | ZoomMonitorToInput | ZoomInInputSkip | ZoomOutInputSkip | ZoomInputToMonitor>
in {Actions = Actions, Mode = Mode, Direction = Direction}
