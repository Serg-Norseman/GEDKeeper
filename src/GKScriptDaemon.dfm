object fmScriptDaemon: TfmScriptDaemon
  Left = 447
  Top = 230
  Width = 728
  Height = 472
  Caption = 'ScriptDaemon'
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 294
    Width = 720
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 720
    Height = 30
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 27
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    Images = fmGEDKeeper.ImageList_Buttons
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object btnNewScript: TToolButton
      Left = 0
      Top = 0
      Hint = #1053#1086#1074#1099#1081' '#1089#1082#1088#1080#1087#1090
      Caption = 'btnNewScript'
      ImageIndex = 0
      OnClick = btnNewScriptClick
    end
    object btnLoadScript: TToolButton
      Left = 27
      Top = 0
      Hint = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1089#1082#1088#1080#1087#1090
      Caption = 'LoadScript'
      ImageIndex = 1
      OnClick = btnLoadScriptClick
    end
    object btnSaveScript: TToolButton
      Left = 54
      Top = 0
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1089#1082#1088#1080#1087#1090
      Caption = 'SaveScript'
      ImageIndex = 2
      OnClick = btnSaveScriptClick
    end
    object ToolButton2: TToolButton
      Left = 81
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object btnRun: TToolButton
      Left = 89
      Top = 0
      Hint = #1042#1099#1087#1086#1083#1085#1080#1090#1100
      Caption = 'Run'
      ImageIndex = 33
      OnClick = btnRunClick
    end
  end
  object mmDebugOutput: TMemo
    Left = 0
    Top = 297
    Width = 720
    Height = 148
    Align = alBottom
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object mmScriptText: TMemo
    Left = 0
    Top = 30
    Width = 720
    Height = 264
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    OnChange = mmScriptTextChange
  end
  object OpenDialog1: TOpenDialog
    Filter = #1057#1082#1088#1080#1087#1090#1099'|*.lua'
    Left = 376
    Top = 176
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'lua'
    Filter = #1057#1082#1088#1080#1087#1090#1099'|*.lua'
    Left = 376
    Top = 224
  end
end
