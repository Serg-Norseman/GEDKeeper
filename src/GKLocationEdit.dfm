object fmLocationEdit: TfmLocationEdit
  Left = 364
  Top = 125
  ActiveControl = EditName
  BorderStyle = bsDialog
  Caption = #1052#1077#1089#1090#1086#1087#1086#1083#1086#1078#1077#1085#1080#1077
  ClientHeight = 473
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnAccept: TBitBtn
    Left = 384
    Top = 440
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 472
    Top = 440
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
    Height = 425
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1054#1089#1085#1086#1074#1085#1086#1077
      ImageIndex = 4
      object Label1: TLabel
        Left = 16
        Top = 8
        Width = 48
        Height = 13
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      object Label2: TLabel
        Left = 287
        Top = 8
        Width = 40
        Height = 13
        Caption = #1064#1080#1088#1086#1090#1072
      end
      object Label3: TLabel
        Left = 375
        Top = 8
        Width = 43
        Height = 13
        Caption = #1044#1086#1083#1075#1086#1090#1072
      end
      object EditName: TEdit
        Left = 16
        Top = 24
        Width = 265
        Height = 21
        TabOrder = 0
        OnChange = EditNameChange
        OnKeyDown = EditNameKeyDown
      end
      object EditLatitude: TEdit
        Left = 287
        Top = 24
        Width = 81
        Height = 21
        TabOrder = 1
      end
      object EditLongitude: TEdit
        Left = 375
        Top = 24
        Width = 81
        Height = 21
        TabOrder = 2
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 51
        Width = 553
        Height = 346
        Align = alBottom
        Caption = #1055#1086#1080#1089#1082' '#1082#1086#1086#1088#1076#1080#1085#1072#1090' (Google Maps)'
        TabOrder = 3
        object ListGeoCoords: TListView
          Left = 16
          Top = 16
          Width = 402
          Height = 89
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
          OnClick = ListGeoCoordsClick
        end
        object btnSearch: TBitBtn
          Left = 429
          Top = 16
          Width = 105
          Height = 25
          Caption = #1055#1086#1080#1089#1082
          TabOrder = 1
          OnClick = btnSearchClick
          NumGlyphs = 2
        end
        object btnSelect: TBitBtn
          Left = 429
          Top = 48
          Width = 105
          Height = 25
          Caption = #1042#1099#1073#1088#1072#1090#1100' '#1082#1086#1086#1088#1076'.'
          TabOrder = 2
          OnClick = btnSelectClick
          NumGlyphs = 2
        end
        object btnSelectName: TBitBtn
          Left = 429
          Top = 80
          Width = 105
          Height = 25
          Caption = #1042#1099#1073#1088#1072#1090#1100' '#1085#1072#1079#1074#1072#1085#1080#1077
          TabOrder = 3
          OnClick = btnSelectNameClick
          NumGlyphs = 2
        end
        object panMap: TPanel
          Left = 2
          Top = 111
          Width = 549
          Height = 233
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 4
        end
      end
      object btnShowOnMap: TButton
        Left = 464
        Top = 24
        Width = 70
        Height = 21
        Hint = #1055#1086#1082#1072#1079#1072#1090#1100' '#1085#1072' '#1082#1072#1088#1090#1077
        Caption = #1055#1086#1082#1072#1079#1072#1090#1100
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnShowOnMapClick
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
