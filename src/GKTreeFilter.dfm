object fmTreeFilter: TfmTreeFilter
  Left = 474
  Top = 313
  BorderStyle = bsDialog
  Caption = #1060#1080#1083#1100#1090#1088
  ClientHeight = 361
  ClientWidth = 393
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 280
    Width = 54
    Height = 13
    Caption = #1048#1089#1090#1086#1095#1085#1080#1082#1080
  end
  object btnAccept: TBitBtn
    Left = 216
    Top = 328
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 1
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 304
    Top = 328
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 2
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object cbSource: TComboBox
    Left = 8
    Top = 296
    Width = 377
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object rgBranchCut: TGroupBox
    Left = 8
    Top = 8
    Width = 377
    Height = 265
    Caption = #1054#1090#1089#1077#1095#1077#1085#1080#1077' '#1074#1077#1090#1074#1077#1081
    TabOrder = 3
    object Label1: TLabel
      Left = 32
      Top = 80
      Width = 19
      Height = 13
      Caption = #1043#1086#1076
    end
    object rbCutNone: TRadioButton
      Left = 16
      Top = 24
      Width = 249
      Height = 17
      Caption = #1085#1077#1090
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbCutNoneClick
    end
    object rbCutYears: TRadioButton
      Left = 16
      Top = 48
      Width = 249
      Height = 17
      Caption = #1087#1086' '#1075#1088#1072#1085#1080#1094#1077' '#1083#1077#1090
      TabOrder = 1
      OnClick = rbCutNoneClick
    end
    object rbCutPersons: TRadioButton
      Left = 16
      Top = 104
      Width = 249
      Height = 17
      Caption = #1087#1086' '#1079#1072#1076#1072#1085#1085#1099#1084' '#1083#1080#1094#1072#1084
      TabOrder = 2
      OnClick = rbCutNoneClick
    end
    object edYear: TEdit
      Left = 64
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object UpDown1: TUpDown
      Left = 185
      Top = 72
      Width = 17
      Height = 21
      Associate = edYear
      Max = 3000
      Increment = 10
      TabOrder = 4
      Thousands = False
    end
    object Panel1: TPanel
      Left = 16
      Top = 128
      Width = 345
      Height = 121
      Caption = 'Panel1'
      TabOrder = 5
    end
  end
end
