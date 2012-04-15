-- xmonad.hs

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) christopher Hall 2012
-- License     :  BSD3-style (see LICENSE.text)
--
-- Maintainer  :  hsw@ms2.hinet.net
-- Stability   :
-- Portability :
--
-- This module specifies my xmonad defaults.
--
------------------------------------------------------------------------

import qualified Data.Map as M

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Theme
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doFloatDep)


-- entry point
main :: IO ()
main = xmonad =<< myConfig


-- centre a floating window and make it use portion of the full screen
doScaledCenterFloat :: Rational -> ManageHook
doScaledCenterFloat scale = doFloatDep move
  where
    move (W.RationalRect _ _ w h) = W.RationalRect cx cy sw sh
      where cx = (1 - sw) / 2
            cy = (1 - sh) / 2
            sw = scale
            sh = scale


-- window management hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
               [ firstItems
               , [className =? c --> doCenterFloat | c <- myClassFloats]
               , [title     =? t --> doCenterFloat | t <- myTitleFloats]
               , [resource  =? r --> doCenterFloat | r <- myResourceFloats]
               ]
                 where
                   firstItems =
                       [ stringProperty "WM_NAME" =? "pinentry-gtk-2" --> doCenterFloat 
                       , stringProperty "WM_NAME" =? "Sending message" --> doScaledCenterFloat 0.5
--                       , stringProperty "WM_WINDOW_ROLE" =? "compose" --> doScaledCenterFloat 0.8  -- claws-mail compose message
                       , stringProperty "WM_WINDOW_ROLE" =? "Preferences for new account" --> doScaledCenterFloat 0.8  -- claws-mail
                       , isDialog --> doCenterFloat
                       ]
                   myClassFloats = ["Gimp"]
                   myTitleFloats = ["Downloads", "Add-ons", "Firefox Preferences", "Edit accounts", "Preferences for new account"]
                   myResourceFloats = ["xmessage"]

myNamedWorkspaces :: [String]
myNamedWorkspaces = ["base"]

myNumberedWorkspaces :: [String]
myNumberedWorkspaces = map show [ (length myNamedWorkspaces + 1) .. 9 :: Int]

myWorkspaces :: [String]
myWorkspaces = myNamedWorkspaces ++ myNumberedWorkspaces


-- configuration
myConfig = do
  return $ defaultConfig
             { workspaces         = myWorkspaces
             , manageHook         = myManageHook
             , layoutHook         = avoidStruts $ noBorders mytabs
             , terminal           = "urxvt"
             , normalBorderColor = "blue"
             , focusedBorderColor = "red"
             , keys               = myKeyBindings
             , handleEventHook    = serverModeEventHook
             , focusFollowsMouse  = False
             }
               where
                 mytabs    = tabbed shrinkText (theme kavonLakeTheme)

-- keyboard changes
myKeyBindings x = foldr (uncurry M.insert) (removeKeyBindings x) (addKeyBindings x)
  where
    removeKeyBindings x = foldr M.delete (defaultKeyBindings x) (delKeyBindings x)
    defaultKeyBindings = keys defaultConfig

-- remove some keys
delKeyBindings x =
  [ (modMask x, xK_j)
  , (modMask x, xK_k)
  , (modMask x, xK_p)
  , (modMask x .|. shiftMask, xK_p)
  , (modMask x .|. shiftMask, xK_q)
  , (modMask x, xK_q)
  ]

-- add some keys
addKeyBindings x   =
  [ ((modMask x, xK_F2), spawn "urxvt" )
  , ((modMask x, xK_F3), spawn "xterm" )
  , ((modMask x, xK_F4), shellPrompt defaultXPConfig)
  , ((modMask x, xK_F5), sshPrompt defaultXPConfig)

  , ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
  , ((modMask x, xK_c), kill) -- close a window
  , ((modMask x .|. shiftMask, xK_comma ), sendMessage (IncMasterN 1))
  , ((modMask x .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
  , ((modMask x, xK_comma ), prevWS)  -- think <
  , ((modMask x, xK_period), nextWS)  -- think >
  , ((modMask x, xK_Right ), windows W.focusDown)
  , ((modMask x, xK_Left  ), windows W.focusUp)
  ]
