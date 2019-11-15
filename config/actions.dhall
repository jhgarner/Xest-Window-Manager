let Tilers = ./tilers.dhall
in < Insert : Tilers | RunCommand : Text | ChangeModeTo : Text | ShowWindow : Text | HideWindow : Text | ZoomInInput | ZoomInMonitor | ZoomOutInput | ZoomOutMonitor | PopTiler | PushTiler | MakeSpecial | ChangeNamed : Text | Move : Bool | KillActive | Exit | ToggleLogging | ZoomMonitorToInput | ZoomInInputSkip >
