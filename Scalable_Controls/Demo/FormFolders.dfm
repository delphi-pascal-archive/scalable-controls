object FormFolder: TFormFolder
  Left = 248
  Top = 107
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'FormFolder'
  ClientHeight = 245
  ClientWidth = 190
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clMaroon
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 190
    Height = 245
    Align = alClient
    DockSite = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnGetSiteInfo = PageControlGetSiteInfo
  end
  object PopupMenu1: TPopupMenu
    Left = 19
    Top = 18
    object MultiLineTabs1: TMenuItem
      Caption = 'Multi Line Tabs'
      RadioItem = True
      OnClick = MultiLineTabs1Click
    end
  end
end
