----------Useful Environment Settings----------
let terminal = "/etc/xest/sensible-terminal.sh"
--^ The command used to launch your terminal
let fontLoc = "/usr/share/fonts/TTF/DejaVuSans.ttf"
--^ The font Xest will use in its UI
let launcher = "/etc/xest/sensible-launcher.sh"
--^ The command you use to launch other apps


----------Action File Helpers----------
let Actions = /etc/xest/actions.dhall
let Action = Actions.Actions
let Back = Actions.Direction.Back
let Front = Actions.Direction.Front
let SimpleKB = {key: Text, actions: List Action}
let KB = {key: Text, actions: List Action, exitActions: List Action, mode: Actions.Mode}


----------Some Scary Looking Helper Functions----------
let newKey = \(mode: Actions.Mode) -> \(l: List SimpleKB) ->
  List/fold SimpleKB l (List KB) (\(a: SimpleKB) -> \(ls: List KB) -> [a // {mode = mode, exitActions = []: List Action}] # ls) ([]: List KB)
let mkDesktops = \(n: Natural) ->
  let result = Natural/fold n {a: Natural, b: List SimpleKB}
    (\(r: {a: Natural, b: List SimpleKB}) -> {a = r.a + 1, b = [{key = Natural/show r.a, actions = [Action.ChangeNamed (Natural/show r.a)]}] # r.b})
    ({a = 1, b = []: List SimpleKB})
  in result.b


----------Keybinding Modes----------
let Normal = { modeName = "Normal", hasButtons = True, hasBorders = True }
let NormalS = { modeName = "NormalS", hasButtons = True, hasBorders = True }
let Insert = { modeName = "Insert", hasButtons = False, hasBorders = False }


in {
  startupScript = "/etc/xest/startup.sh",
  --^ Replace with your startup script which launches things like compton
  -- Or modify the one in /etc/

  initialMode = Insert,
  fontLocation = fontLoc,


  keyBindings =
    newKey Insert
      [ {key = "Super_L", actions = [Action.ChangeModeTo Normal]}
      ]
    #
    newKey Normal (
      [ {key = "Escape", actions = [Action.ChangeModeTo Insert]}
      , {key = "Shift_L", actions = [Action.ChangeModeTo NormalS]}
      , {key = "Return", actions = [Action.RunCommand terminal]}
      , {key = "space", actions = [Action.RunCommand launcher]}
      , {key = "f", actions = [Action.ToggleDocks]}
      , {key = "m", actions = [Action.SetFull]}
      , {key = "c", actions = [Action.SetNoMod]}
      , {key = "n", actions = [Action.Insert]}
      , {key = "v", actions = [Action.SetRotate]}
      , {key = "i", actions = [Action.ZoomInInput]}
      , {key = "o", actions = [Action.ZoomOutInput]}
      , {key = "q", actions = [Action.ZoomOutMonitor]}
      , {key = "z", actions = [Action.ZoomMonitorToInput]}
      , {key = "s", actions = [Action.MakeEmpty]}
      , {key = "w", actions = [Action.ToggleLogging]}
      , {key = "Left", actions = [Action.Move Front]}
      , {key = "Right", actions = [Action.Move Back]}
      ] # mkDesktops 9)
    #
    newKey NormalS
      [ {key = "Escape", actions = [Action.ChangeModeTo Normal]}
      , {key = "t", actions = [Action.ChangeToTwoCols]}
      , {key = "f", actions = [Action.ChangeToFloating]}
      , {key = "n", actions = [Action.ChangeToHorizontal]}
      , {key = "d", actions = [Action.PopTiler]}
      , {key = "p", actions = [Action.PushTiler]}
      , {key = "x", actions = [Action.KillActive]}
      , {key = "b", actions = [Action.ExitNow]}
      , {key = "z", actions = [Action.ZoomInMonitor]}
      , {key = "1", actions = [Action.MoveToLoc 1]}
      , {key = "2", actions = [Action.MoveToLoc 2]}
      , {key = "3", actions = [Action.MoveToLoc 3]}
      , {key = "4", actions = [Action.MoveToLoc 4]}
      , {key = "5", actions = [Action.MoveToLoc 5]}
      , {key = "6", actions = [Action.MoveToLoc 6]}
      , {key = "7", actions = [Action.MoveToLoc 7]}
      , {key = "8", actions = [Action.MoveToLoc 8]}
      , {key = "9", actions = [Action.MoveToLoc 9]}
      ]
}
