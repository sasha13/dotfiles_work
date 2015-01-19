-- Import stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO
import Graphics.X11.ExtraTypes.XF86
import SideSpacing

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.MouseResize

-- utils
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt    as P
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger

-- Data.Ratio for IM layout
import Data.Ratio ((%))


-- Main --
main = do
        xmproc <- spawnPipe "xmobar"  -- start xmobar
        xmonad  $ withUrgencyHook NoUrgencyHook $ defaultConfig
                { manageHook = myManageHook
                , layoutHook = myLayoutHook
                , borderWidth = myBorderWidth
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys = myKeys
                , logHook = myLogHook xmproc
                , modMask = myModMask
                , startupHook = myStartupHook
                , terminal = myTerminal
                , workspaces = myWorkspaces
                , focusFollowsMouse = False
                }



-- hooks
-- automaticly switching app to workspace
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.07 0.09 0.86 0.85) <+> ( composeAll . concat $
                [[isDialog --> doFloat
                  , isFullscreen --> doFloat
                  , className =? "Empathy" --> doShift "1:c"
                  , className =? "Skype" --> doShift "1:c"
                  , className =? "Firefox" --> doShift "2:w"
                  , className =? "Chromium-browser" --> doShift "2:w"
                  , className =? "Google-chrome" --> doShift "2:w"
                ]]
              )  <+> manageDocks



--StartupHook
myStartupHook :: X ()
myStartupHook = do
                setWMName "LG3D"
                spawn "xmodmap ~/.Xmodmap"
                spawn "xsetroot -cursor_name left_ptr"
                spawn "xset r rate 180 90"
                spawn "xset b 50 440 50"
                spawn "xrdb -load ~/.Xresources"
                spawn "xsetroot -solid '#151515'"
                spawn "feh --bg-scale ~/Downloads/wallpaper.jpg"
                spawn "xbacklight -set 30"
                --spawnOn "1:c" "skype"
                --spawnOn "2:w" "firefox"
                --spawnOn "2:w" "google-chrome"
                --spawnOn "3:c" "urxvt"

                --spawn "xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1"
                --spawn "xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2"
                --spawn "xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200"
                --spawn "xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5"


--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }



---- Looks --
---- bar
customPP :: PP
customPP = defaultPP {
                        ppHidden = xmobarColor "#B8D68C" ""
                      , ppCurrent = xmobarColor "#F39D21" "" . wrap "[" "]"
                      , ppUrgent = xmobarColor "#E84F4F" "" . wrap "*" "*"
                      , ppLayout = \x -> ""
                      , ppTitle = xmobarColor "#B8D68C" "" . shorten 120
                      , ppSep = "<fc=#A0CF5D> || </fc>"
                     }

-- some nice colors for the prompt windows
myXPConfig = defaultXPConfig
    { font = "xft:Liberation Mono:size=10:antialias=true:hinting=true"
    , bgColor = "#151515"
    , fgColor = "#D7D0C7"
    , fgHLight = "#D7D0C7"
    , bgHLight = "#151515"
    , borderColor = "#151515"
    , promptBorderWidth = 1
    , position = Bottom
    , height = 14
    , historySize = 50
    }

--- MyTheme For Tabbed layout
myTheme = defaultTheme { decoHeight = 14
    , activeColor = "#151515"
    , activeBorderColor = "#151515"
    , activeTextColor = "#D7D0C7"
    , inactiveColor = "#151515"
    , inactiveBorderColor = "#151515"
    , inactiveTextColor = "#dddddd"
    , urgentColor = "#E84F4F"
    , urgentTextColor = "#D7D0C7"
}

--LayoutHook
myLayoutHook  = spacing 2 $ onWorkspace "1:c" imLayout $ onWorkspace "2:w" webL $ standardLayouts
   where

        standardLayouts = avoidStruts $ (tiled ||| tabLayout ||| reflectTiled ||| Mirror tiled |||  Grid ||| Full)

        --Layouts
        tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbedBottom shrinkText myTheme)
        full    = noBorders Full
        centered = sideSpacing 175 $ Full

        --Im Layout
        imLayout = avoidStruts $ smartBorders $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout  = Grid
                skypeRatio  = (1%6)
                skypeRoster = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

        --Gimp Layout
--        gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") tabLayout

        --Full screen Web layout
        webL = avoidStruts $ mouseResize $ windowArrange $ full

        --Centered Web layout
        --webL = avoidStruts $ mouseResize $ windowArrange $ centered


-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask

-- borders
myBorderWidth :: Dimension
myBorderWidth = 1
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#3E6169"
--myFocusedBorderColor = "#400000"
--

--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:c", "2:w", "3:c", "4:w", "5:v" ,"6:m", "7:w", "8:g", "9:v"]
--

-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_F1), spawn $ XMonad.terminal conf)
    , ((modMask, xK_c ), kill)

    -- opening program launcher / search engine
--    ,((modMask , xK_F2), runOrRaisePrompt myXPConfig)
    ,((modMask , xK_F2), shellPrompt myXPConfig)

    -- GridSelect
    , ((modMask, xK_g), goToSelected defaultGSConfig)

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)

    -- refresh'
    , ((modMask, xK_n ), refresh)

    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )

    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)

    -- scratchpad
    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal})

    -- volume control
    , ((0, 0x1008ff13), spawn "~/pa-vol.sh plus") -- raise volume
    , ((0, 0x1008ff11), spawn "~/pa-vol.sh minus") -- lower volume
    , ((0, 0x1008ff12), spawn "~/pa-vol.sh mute") -- mute/unmute

    -- brightness control
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10") -- brightness up
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10") -- brightness down

    -- suspend
    , ((modMask .|. shiftMask, xK_x), spawn "sudo pm-suspend")

    -- take screenshot
    , ((0, xK_Print), spawn "import -window root ~/Pictures/screenshots/$(date '+%Y%m%d-%H%M%S').png")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
