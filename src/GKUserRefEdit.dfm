object fmUserRefEdit: TfmUserRefEdit
  Left = 347
  Top = 271
  BorderStyle = bsDialog
  Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100#1089#1082#1072#1103' '#1089#1085#1086#1089#1082#1072
  ClientHeight = 145
  ClientWidth = 353
  Color = clBtnFace
  DefaultMonitor = dmMainForm
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 192
    Height = 13
    Caption = #1057#1085#1086#1089#1082#1072'/'#1089#1089#1099#1083#1082#1072'/'#1087#1086#1084#1077#1090#1082#1072'/'#1082#1086#1084#1084#1077#1085#1090#1072#1088#1080#1081
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 18
    Height = 13
    Caption = #1058#1080#1087
  end
  object btnAccept: TBitBtn
    Left = 176
    Top = 112
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 264
    Top = 112
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
  object EditRef: TComboBox
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object EditType: TComboBox
    Left = 8
    Top = 72
    Width = 337
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
end
