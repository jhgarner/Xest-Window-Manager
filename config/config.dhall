let Action = https://raw.githubusercontent.com/jhgarner/Xest-Window-Manager/master/config/actions
let Tiler = https://raw.githubusercontent.com/jhgarner/Xest-Window-Manager/master/config/tilers
in {
  keyBindings = [
    {key = "t", mode = "Normal", actions = [Action.RunCommand "termite"]},
    {key = "d", mode = "Normal", actions = [Action.RunCommand "rofi -show drun"]},
    {key = "v", mode = "Normal", actions = [Action.Insert Tiler.Rotate]},
    {key = "f", mode = "Normal", actions = [Action.Insert Tiler.FullScreen]},
    {key = "h", mode = "Normal", actions = [Action.Insert Tiler.Hovering]},
    {key = "n", mode = "Normal", actions = [Action.Insert Tiler.Horizontal]},
    -- {key = "h", mode = "Normal", actions = [Action.ChangeLayoutTo {_1 = Tiler.Horizontal {=}}]},
    -- {key = "w", mode = "Normal", actions = [Action.ChangeLayoutTo {_1 = Tiler.Workspace {=}}]},
    {key = "i", mode = "Normal", actions = [Action.ZoomInInput]},
    {key = "Super_L", mode = "Insert", actions = [Action.ChangeModeTo "Normal"]},
    {key = "Escape", mode = "Normal", actions = [Action.ChangeModeTo "Insert"]},
    {key = "u", mode = "Resize", actions = [Action.ChangeModeTo "Normal"]},
    {key = "o", mode = "Normal", actions = [Action.ZoomOutInput]},
    {key = "Left", mode = "Normal", actions = [Action.Move True]},
    {key = "Right", mode = "Normal", actions = [Action.Move False]},
    {key = "1", mode = "Normal", actions = [Action.ChangeNamed "1"]},
    {key = "2", mode = "Normal", actions = [Action.ChangeNamed "2"]},
    {key = "3", mode = "Normal", actions = [Action.ChangeNamed "3"]},
    {key = "4", mode = "Normal", actions = [Action.ChangeNamed "4"]},
    {key = "5", mode = "Normal", actions = [Action.ChangeNamed "5"]},
    {key = "6", mode = "Normal", actions = [Action.ChangeNamed "6"]},
    {key = "7", mode = "Normal", actions = [Action.ChangeNamed "7"]},
    {key = "8", mode = "Normal", actions = [Action.ChangeNamed "8"]},
    {key = "9", mode = "Normal", actions = [Action.ChangeNamed "9"]},
    {key = "y", mode = "Normal", actions = [Action.PopTiler]},
    -- {key = "u", mode = "Normal", actions = [Action.ChangeLayoutTo Tiler.Floating]},
    {key = "p", mode = "Normal", actions = [Action.PushTiler]},
    {key = "r", mode = "Normal", actions = [Action.ChangeModeTo "Resize"]},
    {key = "s", mode = "Normal", actions = [Action.MakeSpecial]},
    {key = "m", mode = "Normal", actions = [Action.KillActive]},
    {key = "z", mode = "Normal", actions = [Action.ZoomInMonitor]},
    {key = "b", mode = "Normal", actions = [Action.Exit]},
    {key = "w", mode = "Normal", actions = [Action.ToggleLogging]},
    {key = "q", mode = "Normal", actions = [Action.ZoomOutMonitor]}
  ],
  definedModes = [
    { modeName = "Insert", introActions = [] : List Action, exitActions = [] : List Action, hasButtons = False, hasBorders = False },
    { modeName = "Resize", introActions = [] : List Action, exitActions = [] : List Action, hasButtons = True, hasBorders = True },
    { modeName = "Normal", introActions = [Action.ShowWindow "Polybar"], exitActions = [Action.HideWindow "Polybar"], hasButtons = False, hasBorders = True}
  ]
}
