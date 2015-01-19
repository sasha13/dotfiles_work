atom_WM_TAKE_FOCUS ::
  X Atom
atom_WM_TAKE_FOCUS =
  getAtom "WM_TAKE_FOCUS"

takeFocusX ::
  Window
  -> X ()
takeFocusX w =
  withWindowSet . const $ do
    dpy       <- asks display
    wmtakef   <- atom_WM_TAKE_FOCUS
    wmprot    <- atom_WM_PROTOCOLS
    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $
      io . allocaXEvent $ \ev -> do
          setEventType ev clientMessage
          setClientMessageEvent ev w wmprot 32 wmtakef currentTime
          sendEvent dpy w False noEventMask ev

takeTopFocus ::
  X ()
takeTopFocus =
  withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek
