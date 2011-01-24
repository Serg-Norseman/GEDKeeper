object fmSexCheck: TfmSexCheck
  Left = 468
  Top = 288
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1087#1086#1083#1072
  ClientHeight = 130
  ClientWidth = 361
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object edName: TEdit
    Left = 8
    Top = 8
    Width = 345
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 35
    Width = 345
    Height = 49
    Caption = #1055#1086#1083
    TabOrder = 1
    object sbNone: TSpeedButton
      Left = 8
      Top = 16
      Width = 105
      Height = 22
      GroupIndex = 1
      Down = True
      Caption = '?'
    end
    object sbMale: TSpeedButton
      Left = 119
      Top = 16
      Width = 105
      Height = 22
      GroupIndex = 1
      Caption = #1052#1091#1078#1089#1082#1086#1081
    end
    object sbFemale: TSpeedButton
      Left = 231
      Top = 16
      Width = 105
      Height = 22
      GroupIndex = 1
      Caption = #1046#1077#1085#1089#1082#1080#1081
    end
  end
  object btnAccept: TBitBtn
    Left = 184
    Top = 96
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 272
    Top = 96
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
end
