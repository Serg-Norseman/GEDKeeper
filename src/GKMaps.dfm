object fmMaps: TfmMaps
  Left = 169
  Top = 104
  Caption = #1050#1072#1088#1090#1099
  ClientHeight = 603
  ClientWidth = 901
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
    Height = 584
    Beveled = True
    ExplicitHeight = 595
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 584
    Width = 901
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 584
    ActivePage = tsPlaces
    Align = alLeft
    TabOrder = 1
    object tsPlaces: TTabSheet
      Caption = #1052#1077#1089#1090#1072
      object TreePlaces: TTreeView
        Left = 0
        Top = 238
        Width = 281
        Height = 318
        Align = alClient
        Indent = 19
        SortType = stText
        TabOrder = 0
        OnDblClick = TreePlacesDblClick
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 65
        Width = 281
        Height = 173
        Align = alTop
        Caption = #1042#1099#1073#1086#1088#1082#1072
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 88
          Width = 140
          Height = 13
          Caption = #1042#1089#1105' '#1087#1086' '#1079#1072#1076#1072#1085#1085#1086#1084#1091' '#1095#1077#1083#1086#1074#1077#1082#1091
        end
        object ComboPersons: TComboBox
          Left = 8
          Top = 104
          Width = 265
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object CheckResidence: TCheckBox
          Left = 8
          Top = 64
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1086' '#1078#1080#1090#1077#1083#1100#1089#1090#1074#1072
          TabOrder = 1
        end
        object CheckDeath: TCheckBox
          Left = 8
          Top = 40
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1086' '#1089#1084#1077#1088#1090#1080
          TabOrder = 2
        end
        object CheckBirth: TCheckBox
          Left = 8
          Top = 16
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1086' '#1088#1086#1078#1076#1077#1085#1080#1103
          TabOrder = 3
        end
        object btnSelectPlaces: TButton
          Left = 198
          Top = 136
          Width = 75
          Height = 25
          Caption = #1055#1086#1082#1072#1079#1072#1090#1100
          Enabled = False
          TabOrder = 4
          OnClick = btnSelectPlacesClick
        end
        object btnSaveImage: TButton
          Left = 8
          Top = 136
          Width = 121
          Height = 25
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1089#1085#1080#1084#1086#1082'..'
          TabOrder = 5
          OnClick = btnSaveImageClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 65
        Align = alTop
        Caption = #1055#1086#1080#1089#1082
        TabOrder = 2
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 48
          Height = 13
          Caption = #1053#1072#1079#1074#1072#1085#1080#1077
        end
        object btnSearch: TButton
          Left = 168
          Top = 28
          Width = 105
          Height = 25
          Caption = #1055#1086#1080#1089#1082
          TabOrder = 0
          OnClick = btnSearchClick
        end
        object edSearch: TEdit
          Left = 8
          Top = 32
          Width = 153
          Height = 21
          TabOrder = 1
          Text = #1052#1086#1089#1082#1074#1072
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 293
    Top = 0
    Width = 608
    Height = 584
    Align = alClient
    BorderStyle = bsSingle
    TabOrder = 2
    object WebBrowser: TWebBrowser
      Left = 1
      Top = 1
      Width = 602
      Height = 578
      Align = alClient
      TabOrder = 0
      OnStatusTextChange = WebBrowserStatusTextChange
      ExplicitWidth = 610
      ExplicitHeight = 589
      ControlData = {
        4C000000383E0000BD3B00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126200000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'jpg'
    Filter = 'Image files|*.jpg'
    Left = 121
    Top = 367
  end
end
