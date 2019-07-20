module Game.Horca.Ui.BPMEditor where

import qualified Brick.Widgets.Edit as E

type BPMEditor = E.Editor AppState Name


defaultBPMEditor :: AppState ->  BPMEditor
defaultBPMEditor =  E.editor BPMView (Just 1)

editorApp
  :: App AppState () Name
editorApp =
  let
    _attrMap = attrMap
  in
    App
    { appDraw = drawEditor
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEditorEvent
    , appStartEvent = return
    , appAttrMap  = \_ -> (attrMap VA.defAttr [])
    }

drawEditor :: AppState -> [Widget Name]
drawEditor state =
  let
  hasFocus = True;
  editorDrawContent :: [T.Text] -> Widget Name-- = str . unlines
  editorDrawContent t = txt $ T.unlines t -- . T.pack $ state & bpm

  inputW = E.renderEditor editorDrawContent hasFocus (state ^. editor)
  labelW = str "Path: "
  dds = labelW <+> inputW
  in [ dds ]

handleEditorEvent :: AppState -> BrickEvent Name HorcaEvent -> EventM Name (Next AppState)
handleEditorEvent state (VtyEvent (VTY.EvKey (VTY.KEnter) [])) =
  let content = E.getEditContents $ state ^. editor
      newBPM = TR.decimal $ T.unlines content
      x = fmap ((BPM).fst) newBPM
  in
  case x of
     Right bpm' -> continue $ state & bpm .~ bpm'
     Left errMsg -> continue state

handleEditorEvent state (VtyEvent ev) =
  let w =  state ^. form
  in do
    editor' <- E.handleEditorEvent ev (state ^. editor)
    continue $ state & editor .~ editor'
