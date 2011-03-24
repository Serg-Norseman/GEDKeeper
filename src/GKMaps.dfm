object fmMaps: TfmMaps
  Left = 271
  Top = 105
  Width = 861
  Height = 564
  Caption = #1050#1072#1088#1090#1099
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Width = 4
    Height = 518
    Beveled = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 518
    Width = 853
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 518
    ActivePage = tsPlaces
    Align = alLeft
    TabOrder = 1
    object tsPlaces: TTabSheet
      Caption = #1052#1077#1089#1090#1072
      object TreePlaces: TTreeView
        Left = 0
        Top = 173
        Width = 281
        Height = 317
        Align = alClient
        Indent = 19
        SortType = stText
        TabOrder = 0
        OnDblClick = TreePlacesDblClick
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 173
        Align = alTop
        Caption = #1042#1099#1073#1086#1088#1082#1072
        TabOrder = 1
        object ComboPersons: TComboBox
          Left = 8
          Top = 104
          Width = 265
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 5
        end
        object chkResidence: TCheckBox
          Left = 19
          Top = 64
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1072' '#1087#1088#1086#1078#1080#1074#1072#1085#1080#1103
          TabOrder = 3
        end
        object chkDeath: TCheckBox
          Left = 19
          Top = 48
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1072' '#1089#1084#1077#1088#1090#1080
          TabOrder = 2
        end
        object chkBirth: TCheckBox
          Left = 19
          Top = 32
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
          TabOrder = 1
        end
        object btnSelectPlaces: TButton
          Left = 198
          Top = 136
          Width = 75
          Height = 25
          Caption = #1055#1086#1082#1072#1079#1072#1090#1100
          Enabled = False
          TabOrder = 6
          OnClick = btnSelectPlacesClick
        end
        object btnSaveImage: TButton
          Left = 8
          Top = 136
          Width = 121
          Height = 25
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1089#1085#1080#1084#1086#1082'..'
          TabOrder = 7
          OnClick = btnSaveImageClick
        end
        object radTotal: TRadioButton
          Left = 8
          Top = 16
          Width = 198
          Height = 17
          Caption = #1055#1086' '#1074#1089#1077#1084' '#1083#1102#1076#1103#1084
          TabOrder = 0
          OnClick = radTotalClick
        end
        object radSelected: TRadioButton
          Left = 8
          Top = 87
          Width = 198
          Height = 17
          Caption = #1058#1086#1083#1100#1082#1086' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091
          TabOrder = 4
          OnClick = radTotalClick
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 293
    Top = 0
    Width = 560
    Height = 518
    Align = alClient
    BorderStyle = bsSingle
    TabOrder = 2
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'jpg'
    Filter = 'Image files|*.jpg'
    Left = 121
    Top = 367
  end
end
