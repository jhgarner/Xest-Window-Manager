let Actions = ./actions.dhall
let Action = Actions.Actions
let Back = Actions.Direction.Back
let Front = Actions.Direction.Front
let Tiler = ./tilers.dhall
let Normal = { modeName = "Normal", hasButtons = False, hasBorders = True }
let NormalS = { modeName = "NormalS", hasButtons = False, hasBorders = True }
let Resize = { modeName = "Resize", hasButtons = True, hasBorders = True }
let Insert = { modeName = "Insert", hasButtons = False, hasBorders = False }
-- let bind = \(k: Text) -> \(m: Mode) -> \(as: [Action]) -> \(ea: [Action]) -> {key = k, mode = m, actions = as, exitActions = ea}
in {
  startupScript = "~/.config/xest/startup.sh",
  initialMode = Insert,
  fontLocation = "/nix/store/1sldx0ry67bvr6nk7pblqdyl2yrr6lh1-fira-code-2/share/fonts/opentype/FiraCode-Retina.otf",
  keyBindings = [
    {key = "t", mode = Normal, actions = [Action.RunCommand "xterm"], exitActions = [] : List Action},
    {key = "d", mode = Normal, actions = [Action.RunCommand "rofi -show drun"], exitActions = [] : List Action},
    -- {key = "v", mode = Normal, actions = [Action.Insert Tiler.Rotate], exitActions = [] : List Action},
    {key = "f", mode = Normal, actions = [Action.MoveToFront], exitActions = [] : List Action},
    -- {key = "h", mode = Normal, actions = [Action.Insert Tiler.Hovering], exitActions = [] : List Action},
    {key = "n", mode = Normal, actions = [Action.Insert], exitActions = [] : List Action},
    {key = "i", mode = NormalS, actions = [Action.ZoomInInput], exitActions = [] : List Action},
    {key = "i", mode = Normal, actions = [Action.ZoomInInput], exitActions = [] : List Action},
    {key = "Alt_L", mode = Insert, actions = [Action.ChangeModeTo Normal, Action.ShowWindow "Polybar"], exitActions = [Action.HideWindow "Polybar"] : List Action},
    {key = "Shift_L", mode = Normal, actions = [Action.ChangeModeTo NormalS], exitActions = [] : List Action},
    {key = "Escape", mode = NormalS, actions = [Action.ChangeModeTo Normal], exitActions = [] : List Action},
    {key = "Escape", mode = Normal, actions = [Action.ChangeModeTo Insert, Action.HideWindow "Polybar", Action.ZoomInputToMonitor], exitActions = [] : List Action},
    {key = "Escape", mode = Resize, actions = [Action.ChangeModeTo Normal], exitActions = [] : List Action},
    {key = "o", mode = Normal, actions = [Action.ZoomOutInput], exitActions = [] : List Action},
    {key = "o", mode = NormalS, actions = [Action.ZoomOutInput], exitActions = [] : List Action},
    {key = "Left", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.Move Front, Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "Right", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.Move Back, Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "Left", mode = NormalS, actions = [Action.Move Front], exitActions = [] : List Action},
    {key = "Right", mode = NormalS, actions = [Action.Move Back], exitActions = [] : List Action},
    {key = "1", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "1", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "2", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "2", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "3", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "3", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "4", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "4", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "5", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "5", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "6", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "6", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "7", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "7", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "8", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "8", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "9", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.ChangeNamed "9", Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "1", mode = NormalS, actions = [Action.ChangeNamed "1"], exitActions = [] : List Action},
    {key = "2", mode = NormalS, actions = [Action.ChangeNamed "2"], exitActions = [] : List Action},
    {key = "3", mode = NormalS, actions = [Action.ChangeNamed "3"], exitActions = [] : List Action},
    {key = "4", mode = NormalS, actions = [Action.ChangeNamed "4"], exitActions = [] : List Action},
    {key = "5", mode = NormalS, actions = [Action.ChangeNamed "5"], exitActions = [] : List Action},
    {key = "6", mode = NormalS, actions = [Action.ChangeNamed "6"], exitActions = [] : List Action},
    {key = "7", mode = NormalS, actions = [Action.ChangeNamed "7"], exitActions = [] : List Action},
    {key = "8", mode = NormalS, actions = [Action.ChangeNamed "8"], exitActions = [] : List Action},
    {key = "9", mode = NormalS, actions = [Action.ChangeNamed "9"], exitActions = [] : List Action},
    {key = "y", mode = Normal, actions = [Action.PopTiler], exitActions = [] : List Action},
    {key = "p", mode = Normal, actions = [Action.PushTiler], exitActions = [] : List Action},
    {key = "r", mode = Normal, actions = [Action.ChangeModeTo Resize], exitActions = [] : List Action},
    {key = "s", mode = Normal, actions = [Action.ZoomOutMonitor, Action.ZoomOutInput, Action.MakeEmpty, Action.ZoomInInput, Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "s", mode = NormalS, actions = [Action.MakeEmpty], exitActions = [] : List Action},
    {key = "m", mode = Normal, actions = [Action.KillActive], exitActions = [] : List Action},
    {key = "z", mode = Normal, actions = [Action.ZoomMonitorToInput], exitActions = [] : List Action},
    {key = "z", mode = NormalS, actions = [Action.ZoomInMonitor], exitActions = [] : List Action},
    {key = "b", mode = NormalS, actions = [Action.ExitNow], exitActions = [] : List Action},
    {key = "w", mode = Normal, actions = [Action.ToggleLogging], exitActions = [] : List Action},
    {key = "q", mode = Normal, actions = [Action.ZoomOutMonitor], exitActions = [] : List Action}
  ]
}
