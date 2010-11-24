object fmPersonScan: TfmPersonScan
  Left = 418
  Top = 155
  BorderStyle = bsDialog
  Caption = #1044#1086#1073#1072#1074#1083#1077#1085#1080#1077' '#1087#1077#1088#1089#1086#1085' '#1080#1079' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
  ClientHeight = 417
  ClientWidth = 481
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 133
    Height = 13
    Caption = #1055#1086#1083#1085#1086#1077' '#1080#1084#1103' ('#1092#1086#1088#1084#1072#1090' '#1060#1048#1054')'
  end
  object Label2: TLabel
    Left = 8
    Top = 232
    Width = 42
    Height = 13
    Caption = #1047#1072#1084#1077#1090#1082#1072
  end
  object btnMale: TSpeedButton
    Left = 424
    Top = 24
    Width = 23
    Height = 21
    GroupIndex = 1
    Down = True
    Caption = #1052
  end
  object btnFemale: TSpeedButton
    Left = 448
    Top = 24
    Width = 23
    Height = 21
    GroupIndex = 1
    Caption = #1046
  end
  object EditName: TEdit
    Left = 8
    Top = 24
    Width = 409
    Height = 21
    TabOrder = 0
  end
  object MemoNote: TMemo
    Left = 8
    Top = 248
    Width = 465
    Height = 121
    TabOrder = 1
  end
  object btnCreate: TBitBtn
    Left = 296
    Top = 384
    Width = 81
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 2
    OnClick = btnCreateClick
  end
  object btnCancel: TBitBtn
    Left = 392
    Top = 384
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
  object Panel1: TPanel
    Left = 8
    Top = 56
    Width = 465
    Height = 81
    BorderStyle = bsSingle
    TabOrder = 4
    object Label3: TLabel
      Left = 8
      Top = 32
      Width = 80
      Height = 13
      Caption = #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
    end
    object Label5: TLabel
      Left = 112
      Top = 32
      Width = 85
      Height = 13
      Caption = #1052#1077#1089#1090#1086' '#1088#1086#1078#1076#1077#1085#1080#1103
    end
    object EditBirthDate: TMaskEdit
      Left = 8
      Top = 48
      Width = 97
      Height = 21
      EditMask = '!99/99/9999;1;_'
      MaxLength = 10
      TabOrder = 0
      Text = '  .  .    '
      OnChange = EditBirthDateChange
    end
    object EditBirthPlace: TEdit
      Left = 112
      Top = 48
      Width = 337
      Height = 21
      TabOrder = 1
      OnChange = EditBirthDateChange
    end
    object CheckBirth: TCheckBox
      Left = 8
      Top = 8
      Width = 96
      Height = 17
      Caption = #1056#1086#1076#1080#1083#1089#1103
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 144
    Width = 465
    Height = 81
    BorderStyle = bsSingle
    TabOrder = 5
    object Label6: TLabel
      Left = 8
      Top = 32
      Width = 64
      Height = 13
      Caption = #1044#1072#1090#1072' '#1089#1084#1077#1088#1090#1080
    end
    object Label7: TLabel
      Left = 112
      Top = 32
      Width = 69
      Height = 13
      Caption = #1052#1077#1089#1090#1086' '#1089#1084#1077#1088#1090#1080
    end
    object CheckDeath: TCheckBox
      Left = 8
      Top = 8
      Width = 95
      Height = 17
      Caption = #1059#1084#1077#1088
      TabOrder = 0
    end
    object EditDeathDate: TMaskEdit
      Left = 8
      Top = 48
      Width = 97
      Height = 21
      EditMask = '!99/99/9999;1;_'
      MaxLength = 10
      TabOrder = 1
      Text = '  .  .    '
      OnChange = EditDeathDateChange
    end
    object EditDeathPlace: TEdit
      Left = 112
      Top = 48
      Width = 337
      Height = 21
      TabOrder = 2
      OnChange = EditDeathDateChange
    end
  end
end
