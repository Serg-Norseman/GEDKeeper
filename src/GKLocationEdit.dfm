object fmLocationEdit: TfmLocationEdit
  Left = 363
  Top = 124
  ActiveControl = EditName
  BorderStyle = bsDialog
  Caption = #1052#1077#1089#1090#1086#1087#1086#1083#1086#1078#1077#1085#1080#1077
  ClientHeight = 329
  ClientWidth = 561
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
  object btnAccept: TBitBtn
    Left = 384
    Top = 296
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 472
    Top = 296
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 1
    Kind = bkCancel
  end
  object PagesData: TPageControl
    Left = 0
    Top = 0
    Width = 561
    Height = 281
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1054#1089#1085#1086#1074#1085#1086#1077
      ImageIndex = 4
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 48
        Height = 13
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      object Label2: TLabel
        Left = 8
        Top = 40
        Width = 40
        Height = 13
        Caption = #1064#1080#1088#1086#1090#1072
      end
      object Label3: TLabel
        Left = 8
        Top = 64
        Width = 43
        Height = 13
        Caption = #1044#1086#1083#1075#1086#1090#1072
      end
      object EditName: TEdit
        Left = 80
        Top = 8
        Width = 441
        Height = 21
        TabOrder = 0
        OnChange = EditNameChange
        OnKeyDown = EditNameKeyDown
      end
      object EditLatitude: TEdit
        Left = 80
        Top = 32
        Width = 145
        Height = 21
        TabOrder = 1
      end
      object EditLongitude: TEdit
        Left = 80
        Top = 56
        Width = 145
        Height = 21
        TabOrder = 2
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 96
        Width = 537
        Height = 153
        Caption = #1055#1086#1080#1089#1082' '#1082#1086#1086#1088#1076#1080#1085#1072#1090' (Google Maps)'
        TabOrder = 3
        object ListGeoCoords: TListView
          Left = 8
          Top = 16
          Width = 393
          Height = 129
          Columns = <
            item
              Caption = #1053#1072#1079#1074#1072#1085#1080#1077
              Width = 200
            end
            item
              Caption = #1064#1080#1088#1086#1090#1072
              Width = 80
            end
            item
              Caption = #1044#1086#1083#1075#1086#1090#1072
              Width = 80
            end>
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
        object btnSearch: TBitBtn
          Left = 416
          Top = 16
          Width = 105
          Height = 25
          Caption = #1055#1086#1080#1089#1082
          TabOrder = 1
          OnClick = btnSearchClick
          NumGlyphs = 2
        end
        object btnSelect: TBitBtn
          Left = 416
          Top = 48
          Width = 105
          Height = 25
          Caption = #1042#1099#1073#1088#1072#1090#1100' '#1082#1086#1086#1088#1076'.'
          TabOrder = 2
          OnClick = btnSelectClick
          NumGlyphs = 2
        end
        object btnSelectName: TBitBtn
          Left = 416
          Top = 80
          Width = 105
          Height = 25
          Caption = #1042#1099#1073#1088#1072#1090#1100' '#1085#1072#1079#1074#1072#1085#1080#1077
          TabOrder = 3
          OnClick = btnSelectNameClick
          NumGlyphs = 2
        end
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
  end
end
