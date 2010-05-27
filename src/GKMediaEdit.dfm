object fmMediaEdit: TfmMediaEdit
  Left = 363
  Top = 210
  BorderStyle = bsDialog
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077' '#1084#1091#1083#1100#1090#1080#1084#1077#1076#1080#1072' '#1086#1073#1098#1077#1082#1090#1072
  ClientHeight = 298
  ClientWidth = 522
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PagesData: TPageControl
    Left = 0
    Top = 0
    Width = 522
    Height = 249
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1054#1073#1097#1080#1077' '#1076#1072#1085#1085#1099#1077
      ImageIndex = 2
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 48
        Height = 13
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      object Label2: TLabel
        Left = 8
        Top = 56
        Width = 18
        Height = 13
        Caption = #1058#1080#1087
      end
      object Label4: TLabel
        Left = 192
        Top = 56
        Width = 87
        Height = 13
        Caption = #1057#1087#1086#1089#1086#1073' '#1093#1088#1072#1085#1077#1085#1080#1103
      end
      object Label3: TLabel
        Left = 8
        Top = 104
        Width = 26
        Height = 13
        Caption = #1060#1072#1081#1083
      end
      object edName: TEdit
        Left = 8
        Top = 24
        Width = 497
        Height = 21
        TabOrder = 0
        OnChange = edNameChange
      end
      object cbMediaType: TComboBox
        Left = 9
        Top = 72
        Width = 169
        Height = 21
        Style = csDropDownList
        DropDownCount = 15
        ItemHeight = 13
        TabOrder = 1
      end
      object cbStoreType: TComboBox
        Left = 192
        Top = 72
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
      end
      object edFile: TEdit
        Left = 8
        Top = 120
        Width = 449
        Height = 21
        TabOrder = 3
      end
      object btnFileSelect: TButton
        Left = 464
        Top = 120
        Width = 43
        Height = 21
        Caption = '...'
        TabOrder = 4
        OnClick = btnFileSelectClick
      end
    end
    object SheetNotes: TTabSheet
      Caption = #1047#1072#1084#1077#1090#1082#1080
      ImageIndex = 1
    end
    object SheetSources: TTabSheet
      Caption = #1048#1089#1090#1086#1095#1085#1080#1082#1080
      ImageIndex = 3
    end
  end
  object btnAccept: TBitBtn
    Left = 344
    Top = 264
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 1
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 432
    Top = 264
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 2
    Kind = bkCancel
  end
  object btnView: TBitBtn
    Left = 8
    Top = 264
    Width = 81
    Height = 25
    Caption = #1055#1088#1086#1089#1084#1086#1090#1088'...'
    TabOrder = 3
    OnClick = btnViewClick
    NumGlyphs = 2
  end
  object OpenDialog1: TOpenDialog
    Filter = #1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Left = 320
    Top = 176
  end
end
