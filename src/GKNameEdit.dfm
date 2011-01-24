object fmNameEdit: TfmNameEdit
  Left = 713
  Top = 177
  BorderStyle = bsDialog
  Caption = #1048#1084#1103
  ClientHeight = 201
  ClientWidth = 273
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 19
    Height = 13
    Caption = #1048#1084#1103
  end
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 19
    Height = 13
    Caption = #1055#1086#1083
  end
  object edName: TEdit
    Left = 72
    Top = 8
    Width = 193
    Height = 21
    TabOrder = 0
    OnKeyPress = edFPatrKeyPress
  end
  object edSex: TComboBox
    Left = 72
    Top = 40
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object btnAccept: TBitBtn
    Left = 96
    Top = 168
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 3
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 184
    Top = 168
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 4
    Kind = bkCancel
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 72
    Width = 257
    Height = 78
    Caption = #1054#1090#1095#1077#1089#1090#1074#1072
    TabOrder = 2
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 45
      Height = 13
      Caption = #1046#1077#1085#1089#1082#1086#1077
    end
    object Label1: TLabel
      Left = 8
      Top = 56
      Width = 45
      Height = 13
      Caption = #1052#1091#1078#1089#1082#1086#1077
    end
    object edFPatr: TEdit
      Left = 64
      Top = 16
      Width = 185
      Height = 21
      TabOrder = 0
      OnKeyPress = edFPatrKeyPress
    end
    object edMPatr: TEdit
      Left = 64
      Top = 48
      Width = 185
      Height = 21
      TabOrder = 1
      OnKeyPress = edFPatrKeyPress
    end
  end
end
