object MainForm: TMainForm
  Left = 324
  Top = 233
  Width = 823
  Height = 112
  Caption = 'Main Form'
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    815
    60)
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 246
    Top = 7
    Width = 558
    Height = 51
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 0
    object Label2: TLabel
      Left = 50
      Top = 4
      Width = 297
      Height = 44
      Caption = 'Scaleable Controls'
      Font.Charset = ANSI_CHARSET
      Font.Color = clTeal
      Font.Height = -38
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 363
      Top = 4
      Width = 183
      Height = 44
      Caption = 'Application'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -38
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 5
    Top = 15
    Width = 277
    Height = 37
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    Color = clWhite
    TabOrder = 1
    DesignSize = (
      277
      37)
    object Label3: TLabel
      Left = 6
      Top = 7
      Width = 264
      Height = 24
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object MainMenu1: TMainMenu
    Left = 190
    Top = 65529
    object Actions: TMenuItem
      Caption = 'Actions'
      object NewForm2: TMenuItem
        Caption = 'New Form'
        OnClick = NewForm1Click
      end
      object ShowFormStack1: TMenuItem
        Caption = 'Show Form Stack'
        OnClick = ShowFormStack1Click
      end
      object ShowFormFolder1: TMenuItem
        Caption = 'Show Form Folder'
        OnClick = ShowFormFolder1Click
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 218
    Top = 65533
    object Stack1: TMenuItem
      Caption = 'Stack'
      OnClick = Stack1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ShowScroller1: TMenuItem
      Caption = 'Show Scroller'
      OnClick = ShowScroller1Click
    end
    object MaxTextSize1: TMenuItem
      Caption = 'Memo1:Text -- No Max'
      OnClick = MaxTextSize1Click
    end
    object Memo1Scaling1: TMenuItem
      Caption = 'Memo1: No Scaling'
      OnClick = Memo1Scaling1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MemoVisible1: TMenuItem
      Caption = 'Hide Memo 1'
      OnClick = MemoVisible1Click
    end
    object EnableMemo21: TMenuItem
      Caption = 'Enable Memo 2'
      OnClick = EnableMemo21Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object DesignSize1: TMenuItem
      Caption = 'Design Size'
      OnClick = DesignSize1Click
    end
    object DefaultSize1: TMenuItem
      Caption = 'Default Size'
      OnClick = DefaultSize1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Close1: TMenuItem
      Caption = 'Close'
      OnClick = Close1Click
    end
  end
end
