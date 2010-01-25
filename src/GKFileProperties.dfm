object fmFileProperties: TfmFileProperties
  Left = 396
  Top = 197
  BorderStyle = bsDialog
  Caption = #1057#1074#1086#1081#1089#1090#1074#1072' '#1092#1072#1081#1083#1072
  ClientHeight = 329
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnAccept: TBitBtn
    Left = 272
    Top = 296
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 0
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 360
    Top = 296
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 1
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 433
    Height = 273
    ActivePage = SheetAuthor
    TabOrder = 2
    object SheetAuthor: TTabSheet
      Caption = #1040#1074#1090#1086#1088
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 19
        Height = 13
        Caption = #1048#1084#1103
      end
      object Label2: TLabel
        Left = 8
        Top = 32
        Width = 31
        Height = 13
        Caption = #1040#1076#1088#1077#1089
      end
      object Label3: TLabel
        Left = 8
        Top = 152
        Width = 44
        Height = 13
        Caption = #1058#1077#1083#1077#1092#1086#1085
      end
      object EditName: TEdit
        Left = 64
        Top = 8
        Width = 353
        Height = 21
        TabOrder = 0
      end
      object EditTel: TEdit
        Left = 64
        Top = 152
        Width = 353
        Height = 21
        TabOrder = 1
      end
      object MemoAddress: TMemo
        Left = 64
        Top = 32
        Width = 353
        Height = 113
        TabOrder = 2
      end
    end
    object SheetDiagnostics: TTabSheet
      Caption = #1044#1080#1072#1075#1085#1086#1089#1090#1080#1082#1072
      ImageIndex = 1
      object ListDiags: TBSListView
        Left = 0
        Top = 0
        Width = 425
        Height = 245
        Align = alClient
        Columns = <
          item
            Caption = #1047#1072#1087#1080#1089#1100
            Width = 75
          end
          item
            Caption = #1044#1080#1072#1075#1085#1086#1089#1090#1080#1082#1072
            Width = 320
          end>
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        SortColumn = 0
        SortDirection = sdAscending
      end
    end
  end
end
