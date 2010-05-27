object fmSourceEdit: TfmSourceEdit
  Left = 344
  Top = 103
  ActiveControl = EditShortTitle
  BorderStyle = bsDialog
  Caption = #1048#1089#1090#1086#1095#1085#1080#1082
  ClientHeight = 449
  ClientWidth = 537
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
    Left = 360
    Top = 416
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 1
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 448
    Top = 416
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 2
    Kind = bkCancel
  end
  object PagesData: TPageControl
    Left = 0
    Top = 0
    Width = 537
    Height = 401
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1054#1089#1085#1086#1074#1085#1086#1077
      ImageIndex = 4
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 93
        Height = 13
        Caption = #1050#1088#1072#1090#1082#1086#1077' '#1085#1072#1079#1074#1072#1085#1080#1077
      end
      object Label3: TLabel
        Left = 8
        Top = 32
        Width = 31
        Height = 13
        Caption = #1040#1074#1090#1086#1088
      end
      object Label2: TLabel
        Left = 8
        Top = 144
        Width = 48
        Height = 13
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      object Label4: TLabel
        Left = 8
        Top = 256
        Width = 74
        Height = 13
        Caption = #1054#1087#1091#1073#1083#1080#1082#1086#1074#1072#1085#1086
      end
      object EditShortTitle: TEdit
        Left = 112
        Top = 8
        Width = 233
        Height = 21
        TabOrder = 0
        OnChange = EditShortTitleChange
      end
      object EditAuthor: TMemo
        Left = 112
        Top = 32
        Width = 409
        Height = 105
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object EditTitle: TMemo
        Left = 112
        Top = 144
        Width = 409
        Height = 105
        ScrollBars = ssVertical
        TabOrder = 2
      end
      object EditPublication: TMemo
        Left = 112
        Top = 256
        Width = 409
        Height = 105
        ScrollBars = ssVertical
        TabOrder = 3
      end
    end
    object SheetText: TTabSheet
      Caption = #1058#1077#1082#1089#1090
      ImageIndex = 3
      object EditText: TMemo
        Left = 0
        Top = 0
        Width = 529
        Height = 373
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object SheetRepositories: TTabSheet
      Caption = #1040#1088#1093#1080#1074#1099
      ImageIndex = 2
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
