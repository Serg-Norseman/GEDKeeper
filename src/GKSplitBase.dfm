object fmSplitBase: TfmSplitBase
  Left = 315
  Top = 166
  BorderStyle = bsDialog
  Caption = #1056#1072#1079#1076#1077#1083#1080#1090#1100' '#1073#1072#1079#1091
  ClientHeight = 370
  ClientWidth = 713
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnSelectAll: TBitBtn
    Left = 8
    Top = 304
    Width = 105
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1074#1089#1077' '#1089#1074#1103#1079#1080
    TabOrder = 0
    OnClick = btnSelectAllClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 345
    Height = 281
    ItemHeight = 13
    TabOrder = 1
  end
  object ListBox2: TListBox
    Left = 360
    Top = 8
    Width = 345
    Height = 281
    ItemHeight = 13
    TabOrder = 2
  end
  object btnClose: TBitBtn
    Left = 624
    Top = 336
    Width = 81
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
  object btnSelectFamily: TBitBtn
    Left = 120
    Top = 304
    Width = 105
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1089#1077#1084#1100#1102
    Enabled = False
    TabOrder = 4
  end
  object btnSelectAncestors: TBitBtn
    Left = 232
    Top = 304
    Width = 105
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1087#1088#1077#1076#1082#1086#1074
    Enabled = False
    TabOrder = 5
  end
  object btnSelectDescendants: TBitBtn
    Left = 344
    Top = 304
    Width = 105
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1087#1086#1090#1086#1084#1082#1086#1074
    Enabled = False
    TabOrder = 6
  end
  object btnDelete: TBitBtn
    Left = 8
    Top = 336
    Width = 105
    Height = 25
    Caption = #1059#1076#1072#1083#1080#1090#1100
    TabOrder = 7
    OnClick = btnDeleteClick
  end
end
