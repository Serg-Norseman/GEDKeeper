object fmDBImport: TfmDBImport
  Left = 397
  Top = 191
  Width = 766
  Height = 531
  Caption = #1048#1084#1087#1086#1088#1090' '#1073#1072#1079' '#1076#1072#1085#1085#1099#1093
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 758
    Height = 29
    ButtonHeight = 21
    ButtonWidth = 103
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    ShowCaptions = True
    TabOrder = 0
    object btnDBLoad: TToolButton
      Left = 0
      Top = 2
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1041#1044'...'
      ImageIndex = 0
      OnClick = btnDBLoadClick
    end
    object ToolButton1: TToolButton
      Left = 103
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object btnQueryExec: TToolButton
      Left = 111
      Top = 2
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100' '#1079#1072#1087#1088#1086#1089
      Enabled = False
      ImageIndex = 1
      OnClick = btnQueryExecClick
    end
    object ToolButton2: TToolButton
      Left = 214
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnDataTransfer: TToolButton
      Left = 222
      Top = 2
      Caption = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1076#1072#1085#1085#1099#1077
      Enabled = False
      ImageIndex = 2
      OnClick = btnDataTransferClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 29
    Width = 758
    Height = 475
    ActivePage = SheetDBProperties
    Align = alClient
    TabOrder = 1
    object SheetDBProperties: TTabSheet
      Caption = #1057#1074#1086#1081#1089#1090#1074#1072' '#1041#1044
      object mProps: TMemo
        Left = 0
        Top = 0
        Width = 750
        Height = 447
        Align = alClient
        Color = clBtnFace
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object SheetDBQueries: TTabSheet
      Caption = #1047#1072#1087#1088#1086#1089#1099
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 0
        Top = 137
        Width = 750
        Height = 4
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object mQuery: TMemo
        Left = 0
        Top = 0
        Width = 750
        Height = 137
        Align = alTop
        Lines.Strings = (
          
            'select (Family + '#39' '#39' + Name + '#39' '#39' + Patronymic) as PersName, Bir' +
            'thDate, BirthPlace, (City + '#39', '#39' + District) as Place, (Street +' +
            ' '#39', '#39' + House + '#39'-'#39' + Apartment) as Address'
          'from XBase'
          'where (Family = '#39#1048#1074#1072#1085#1086#1074#39')'
          'order by Street')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 141
        Width = 750
        Height = 306
        Align = alClient
        DataSource = DataSource1
        TabOrder = 1
        TitleFont.Charset = RUSSIAN_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object SheetDataTransfer: TTabSheet
      Caption = #1055#1077#1088#1077#1085#1086#1089' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 2
      object BoxFields: TScrollBox
        Left = 0
        Top = 0
        Width = 766
        Height = 469
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object ADOConnection1: TADOConnection
    LoginPrompt = False
    Provider = 'MSDASQL.1'
    Left = 232
    Top = 152
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MS Access|*.mdb|CSV-'#1090#1072#1073#1083#1080#1094#1072'|*.csv'
    Left = 232
    Top = 248
  end
  object DataSource1: TDataSource
    Left = 232
    Top = 200
  end
end
