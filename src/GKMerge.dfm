object fmMerge: TfmMerge
  Left = 307
  Top = 172
  BorderStyle = bsDialog
  Caption = #1054#1073#1098#1077#1076#1080#1085#1080#1090#1100' '#1076#1091#1073#1083#1080#1082#1072#1090#1099'...'
  ClientHeight = 458
  ClientWidth = 705
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
  object btnClose: TBitBtn
    Left = 616
    Top = 424
    Width = 81
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 0
    OnClick = btnCloseClick
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 689
    Height = 409
    ActivePage = SheetMerge
    TabOrder = 1
    object SheetMerge: TTabSheet
      Caption = #1054#1073#1098#1077#1076#1080#1085#1077#1085#1080#1077
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 24
        Height = 13
        Caption = 'XXX1'
      end
      object Label2: TLabel
        Left = 344
        Top = 24
        Width = 24
        Height = 13
        Caption = 'XXX2'
      end
      object btnSearch: TBitBtn
        Left = 8
        Top = 328
        Width = 75
        Height = 25
        Caption = #1040#1074#1090#1086#1087#1086#1080#1089#1082
        TabOrder = 0
        OnClick = btnSearchClick
      end
      object Edit1: TEdit
        Left = 8
        Top = 40
        Width = 329
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object Edit2: TEdit
        Left = 344
        Top = 40
        Width = 329
        Height = 21
        ReadOnly = True
        TabOrder = 2
      end
      object btnRec1Select: TBitBtn
        Left = 256
        Top = 8
        Width = 81
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100'...'
        TabOrder = 3
        OnClick = btnRec1SelectClick
      end
      object btnRec2Select: TBitBtn
        Left = 592
        Top = 8
        Width = 81
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100'...'
        TabOrder = 4
        OnClick = btnRec2SelectClick
      end
      object Memo1: TMemo
        Left = 8
        Top = 72
        Width = 329
        Height = 241
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
      object Memo2: TMemo
        Left = 344
        Top = 72
        Width = 329
        Height = 241
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 6
      end
      object btnMergeToLeft: TBitBtn
        Left = 256
        Top = 328
        Width = 81
        Height = 25
        Caption = '<<<'
        TabOrder = 7
        OnClick = btnMergeToLeftClick
      end
      object btnMergeToRight: TBitBtn
        Left = 344
        Top = 328
        Width = 81
        Height = 25
        Caption = '>>>'
        TabOrder = 8
        OnClick = btnMergeToRightClick
      end
      object btnSkip: TBitBtn
        Left = 88
        Top = 328
        Width = 75
        Height = 25
        Caption = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100
        TabOrder = 9
        OnClick = btnSkipClick
      end
      object ProgressBar1: TProgressBar
        Left = 8
        Top = 360
        Width = 665
        Height = 16
        Step = 1
        TabOrder = 10
      end
    end
    object SheetOptions: TTabSheet
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ImageIndex = 1
      object rgMode: TRadioGroup
        Left = 8
        Top = 8
        Width = 225
        Height = 73
        Caption = #1056#1077#1078#1080#1084
        Items.Strings = (
          #1055#1077#1088#1089#1086#1085#1099
          #1047#1072#1084#1077#1090#1082#1080
          #1057#1077#1084#1100#1080)
        TabOrder = 0
        OnClick = rgModeClick
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 88
        Width = 297
        Height = 161
        Caption = #1055#1086#1080#1089#1082' '#1087#1077#1088#1089#1086#1085
        TabOrder = 1
        object Label3: TLabel
          Left = 8
          Top = 96
          Width = 98
          Height = 13
          Caption = #1058#1086#1095#1085#1086#1089#1090#1100' '#1080#1084#1077#1085#1080', %'
        end
        object Label4: TLabel
          Left = 120
          Top = 96
          Width = 88
          Height = 13
          Caption = #1055#1086#1075#1088#1077#1096#1085#1086#1089#1090#1100' '#1083#1077#1090
        end
        object rbDirectMatching: TRadioButton
          Left = 8
          Top = 16
          Width = 153
          Height = 17
          Caption = #1055#1088#1103#1084#1086#1077' '#1089#1088#1072#1074#1085#1077#1085#1080#1077
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbIndistinctMatching: TRadioButton
          Left = 8
          Top = 32
          Width = 153
          Height = 17
          Caption = #1053#1077#1095#1077#1090#1082#1086#1077' '#1089#1088#1072#1074#1085#1077#1085#1080#1077
          TabOrder = 1
        end
        object edNameAccuracy: TEdit
          Left = 8
          Top = 112
          Width = 89
          Height = 21
          TabOrder = 2
          Text = '0'
        end
        object udNameAccuracy: TUpDown
          Left = 97
          Top = 112
          Width = 15
          Height = 21
          Associate = edNameAccuracy
          Min = 40
          Position = 40
          TabOrder = 3
        end
        object edYearInaccuracy: TEdit
          Left = 120
          Top = 112
          Width = 89
          Height = 21
          TabOrder = 4
          Text = '3'
        end
        object udYearInaccuracy: TUpDown
          Left = 209
          Top = 112
          Width = 15
          Height = 21
          Associate = edYearInaccuracy
          Max = 10
          Position = 3
          TabOrder = 5
        end
        object chkBirthYear: TCheckBox
          Left = 8
          Top = 56
          Width = 201
          Height = 17
          Caption = #1059#1095#1080#1090#1099#1074#1072#1090#1100' '#1075#1086#1076' '#1088#1086#1078#1076#1077#1085#1080#1103
          TabOrder = 6
        end
      end
    end
  end
end
