object fmSourceParse: TfmSourceParse
  Left = 425
  Top = 166
  Caption = #1056#1072#1079#1073#1086#1088' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
  ClientHeight = 412
  ClientWidth = 625
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
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 48
    Height = 13
    Caption = #1048#1089#1090#1086#1095#1085#1080#1082
  end
  object Label2: TLabel
    Left = 304
    Top = 16
    Width = 75
    Height = 13
    Caption = #1051#1080#1089#1090'/'#1089#1090#1088#1072#1085#1080#1094#1072
  end
  object Label3: TLabel
    Left = 528
    Top = 16
    Width = 19
    Height = 13
    Caption = #1043#1086#1076
  end
  object Label4: TLabel
    Left = 8
    Top = 40
    Width = 95
    Height = 13
    Caption = #1053#1072#1089#1077#1083#1077#1085#1085#1099#1081' '#1087#1091#1085#1082#1090
  end
  object cbSource: TComboBox
    Left = 64
    Top = 8
    Width = 225
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'cbSource'
  end
  object edPage: TEdit
    Left = 392
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'edPage'
  end
  object edSourceYear: TMaskEdit
    Left = 560
    Top = 8
    Width = 57
    Height = 21
    EditMask = '!0000;1;_'
    MaxLength = 4
    TabOrder = 2
    Text = '    '
  end
  object edPlace: TEdit
    Left = 112
    Top = 32
    Width = 505
    Height = 21
    TabOrder = 3
    Text = 'edPlace'
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 80
    Width = 609
    Height = 113
    ColCount = 6
    DefaultColWidth = 80
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 100
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing]
    TabOrder = 4
    OnKeyDown = StringGrid1KeyDown
    OnSelectCell = StringGrid1SelectCell
  end
  object cbPersonLink: TComboBox
    Left = 416
    Top = 208
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnExit = cbPersonLinkExit
    OnKeyDown = cbPersonLinkKeyDown
  end
  object btnAdd: TButton
    Left = 264
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 6
    OnClick = btnAddClick
  end
end
