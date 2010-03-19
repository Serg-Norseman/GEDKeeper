object fmEventEdit: TfmEventEdit
  Left = 384
  Top = 164
  ActiveControl = EditEventType
  BorderStyle = bsDialog
  Caption = #1057#1086#1073#1099#1090#1080#1077
  ClientHeight = 369
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnAccept: TBitBtn
    Left = 240
    Top = 336
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 328
    Top = 336
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
  object PageEventData: TPageControl
    Left = 0
    Top = 0
    Width = 418
    Height = 323
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1054#1073#1097#1077#1077
      ImageIndex = 3
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 45
        Height = 13
        Caption = #1057#1086#1073#1099#1090#1080#1077
      end
      object Label2: TLabel
        Left = 8
        Top = 80
        Width = 31
        Height = 13
        Caption = #1052#1077#1089#1090#1086
      end
      object Label3: TLabel
        Left = 8
        Top = 112
        Width = 26
        Height = 13
        Caption = #1044#1072#1090#1072
      end
      object Label4: TLabel
        Left = 8
        Top = 168
        Width = 43
        Height = 13
        Caption = #1055#1088#1080#1095#1080#1085#1072
      end
      object Label5: TLabel
        Left = 8
        Top = 200
        Width = 159
        Height = 13
        Caption = #1047#1072#1089#1074#1080#1076#1077#1090#1077#1083#1100#1089#1090#1074#1086#1074#1072#1074#1096#1080#1081' '#1086#1088#1075#1072#1085
      end
      object Label6: TLabel
        Left = 8
        Top = 48
        Width = 43
        Height = 13
        Caption = #1040#1090#1088#1080#1073#1091#1090
      end
      object EditEventType: TComboBox
        Left = 56
        Top = 8
        Width = 183
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = EditEventTypeChange
      end
      object EditEventName: TEdit
        Left = 248
        Top = 8
        Width = 153
        Height = 21
        TabOrder = 1
      end
      object EditEventPlace: TEdit
        Left = 56
        Top = 72
        Width = 345
        Height = 21
        TabOrder = 2
      end
      object EditEventDateType: TComboBox
        Left = 56
        Top = 104
        Width = 183
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = EditEventDateTypeChange
      end
      object EditEventDate1: TMaskEdit
        Left = 248
        Top = 104
        Width = 153
        Height = 21
        EditMask = '!99/99/9999;1;_'
        MaxLength = 10
        TabOrder = 4
        Text = '  .  .    '
      end
      object EditEventDate2: TMaskEdit
        Left = 248
        Top = 128
        Width = 153
        Height = 21
        EditMask = '!99/99/9999;1;_'
        MaxLength = 10
        TabOrder = 5
        Text = '  .  .    '
      end
      object EditEventCause: TEdit
        Left = 56
        Top = 160
        Width = 345
        Height = 21
        TabOrder = 6
      end
      object EditEventOrg: TEdit
        Left = 184
        Top = 192
        Width = 217
        Height = 21
        TabOrder = 7
      end
      object EditAttribute: TEdit
        Left = 56
        Top = 40
        Width = 345
        Height = 21
        TabOrder = 8
      end
    end
    object SheetNotes: TTabSheet
      Caption = #1047#1072#1084#1077#1090#1082#1080
      ImageIndex = 1
    end
    object SheetMultimedia: TTabSheet
      Caption = #1052#1091#1083#1100#1090#1080#1084#1077#1076#1080#1072
      ImageIndex = 2
    end
    object SheetSources: TTabSheet
      Caption = #1048#1089#1090#1086#1095#1085#1080#1082#1080
      ImageIndex = 3
    end
  end
  object btnAddress: TBitBtn
    Left = 8
    Top = 336
    Width = 81
    Height = 25
    Caption = #1040#1076#1088#1077#1089'...'
    TabOrder = 1
    OnClick = btnAddressClick
    NumGlyphs = 2
  end
end
