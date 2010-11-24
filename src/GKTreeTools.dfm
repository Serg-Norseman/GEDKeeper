object fmTreeTools: TfmTreeTools
  Left = 319
  Top = 144
  BorderStyle = bsDialog
  Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099
  ClientHeight = 482
  ClientWidth = 737
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 721
    Height = 425
    ActivePage = SheetBaseCheck
    TabOrder = 0
    object SheetChoice: TTabSheet
      Caption = 'SheetChoice'
      TabVisible = False
      object rgOperation: TRadioGroup
        Left = 16
        Top = 16
        Width = 273
        Height = 305
        Caption = #1054#1087#1077#1088#1072#1094#1080#1103
        ItemIndex = 0
        Items.Strings = (
          #1057#1088#1072#1074#1085#1080#1090#1100' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
          #1054#1073#1098#1077#1076#1080#1085#1080#1090#1100' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
          #1056#1072#1079#1076#1077#1083#1080#1090#1100' '#1073#1072#1079#1091' '#1076#1072#1085#1085#1099#1093
          #1054#1073#1098#1077#1076#1080#1085#1080#1090#1100' '#1076#1091#1073#1083#1080#1082#1072#1090#1099' '#1079#1072#1087#1080#1089#1077#1081
          #1048#1084#1087#1086#1088#1090' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1092#1086#1088#1084#1072#1090#1072
          #1055#1088#1086#1074#1077#1088#1082#1072' '#1089#1074#1103#1079#1085#1086#1089#1090#1080' '#1089#1077#1084#1077#1081
          #1055#1088#1086#1074#1077#1088#1082#1072' '#1091#1084#1077#1088#1096#1080#1093' '#1083#1102#1076#1077#1081
          #1057#1080#1085#1093#1088#1086#1085#1080#1079#1072#1094#1080#1103' '#1073#1072#1079' '#1076#1072#1085#1085#1099#1093
          #1055#1086#1080#1089#1082' '#1087#1072#1090#1088#1080#1072#1088#1093#1086#1074
          #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1084#1077#1089#1090#1072#1084#1080
          #1055#1088#1086#1074#1077#1088#1082#1072' '#1073#1072#1079#1072' '#1076#1072#1085#1085#1099#1093)
        TabOrder = 0
      end
    end
    object SheetTreeCompare: TTabSheet
      Caption = 'SheetTreeCompare'
      ImageIndex = 1
      TabVisible = False
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 26
        Height = 13
        Caption = #1060#1072#1081#1083
      end
      object ListCompare: TListView
        Left = 8
        Top = 40
        Width = 697
        Height = 369
        Align = alCustom
        Columns = <
          item
            Caption = #1047#1072#1087#1080#1089#1100
            Width = 100
          end
          item
            Caption = #1044#1080#1072#1075#1085#1086#1089#1090#1080#1082#1072
            Width = 500
          end>
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
      end
      object edCompareFile: TEdit
        Left = 40
        Top = 8
        Width = 577
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object btnFileChoose: TBitBtn
        Left = 624
        Top = 6
        Width = 81
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100'...'
        TabOrder = 2
        OnClick = btnFileChooseClick
        NumGlyphs = 2
      end
    end
    object SheetTreeMerge: TTabSheet
      Caption = 'SheetTreeMerge'
      ImageIndex = 2
      TabVisible = False
      object Label2: TLabel
        Left = 8
        Top = 16
        Width = 26
        Height = 13
        Caption = #1060#1072#1081#1083
      end
      object edMergeFile: TEdit
        Left = 40
        Top = 8
        Width = 577
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object btnMergeFileChoose: TBitBtn
        Left = 624
        Top = 6
        Width = 81
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100'...'
        TabOrder = 1
        OnClick = btnMergeFileChooseClick
        NumGlyphs = 2
      end
      object mRes: TMemo
        Left = 8
        Top = 40
        Width = 697
        Height = 369
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
      end
    end
    object SheetTreeSplit: TTabSheet
      Caption = 'SheetTreeSplit'
      ImageIndex = 3
      TabVisible = False
      object btnSelectAll: TBitBtn
        Left = 8
        Top = 352
        Width = 105
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100' '#1074#1089#1077' '#1089#1074#1103#1079#1080
        TabOrder = 0
        OnClick = btnSelectAllClick
      end
      object ListSelected: TListBox
        Left = 8
        Top = 8
        Width = 345
        Height = 329
        ItemHeight = 13
        TabOrder = 1
      end
      object ListSkipped: TListBox
        Left = 360
        Top = 8
        Width = 345
        Height = 329
        ItemHeight = 13
        TabOrder = 2
      end
      object btnSelectFamily: TBitBtn
        Left = 120
        Top = 352
        Width = 105
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100' '#1089#1077#1084#1100#1102
        TabOrder = 3
        OnClick = btnSelectFamilyClick
      end
      object btnSelectAncestors: TBitBtn
        Left = 8
        Top = 384
        Width = 105
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100' '#1087#1088#1077#1076#1082#1086#1074
        TabOrder = 4
        OnClick = btnSelectAncestorsClick
      end
      object btnSelectDescendants: TBitBtn
        Left = 120
        Top = 384
        Width = 105
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100' '#1087#1086#1090#1086#1084#1082#1086#1074
        TabOrder = 5
        OnClick = btnSelectDescendantsClick
      end
      object btnDelete: TBitBtn
        Left = 600
        Top = 352
        Width = 105
        Height = 25
        Caption = #1059#1076#1072#1083#1080#1090#1100
        TabOrder = 6
        OnClick = btnDeleteClick
      end
      object btnSave: TBitBtn
        Left = 600
        Top = 384
        Width = 105
        Height = 25
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100'...'
        TabOrder = 7
        OnClick = btnSaveClick
      end
    end
    object SheetRecMerge: TTabSheet
      Caption = 'SheetRecMerge'
      ImageIndex = 4
      TabVisible = False
      object PageControl1: TPageControl
        Left = 8
        Top = 8
        Width = 689
        Height = 393
        ActivePage = SheetMerge
        TabOrder = 0
        object SheetMerge: TTabSheet
          Caption = #1054#1073#1098#1077#1076#1080#1085#1077#1085#1080#1077
          object Lab1: TLabel
            Left = 8
            Top = 8
            Width = 24
            Height = 13
            Caption = 'XXX1'
          end
          object Lab2: TLabel
            Left = 344
            Top = 8
            Width = 24
            Height = 13
            Caption = 'XXX2'
          end
          object btnSearch: TBitBtn
            Left = 8
            Top = 312
            Width = 75
            Height = 25
            Caption = #1040#1074#1090#1086#1087#1086#1080#1089#1082
            TabOrder = 0
            OnClick = btnSearchClick
          end
          object Edit1: TEdit
            Left = 8
            Top = 24
            Width = 241
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
          object Edit2: TEdit
            Left = 344
            Top = 24
            Width = 241
            Height = 21
            ReadOnly = True
            TabOrder = 2
          end
          object btnRec1Select: TBitBtn
            Left = 256
            Top = 22
            Width = 81
            Height = 25
            Caption = #1042#1099#1073#1088#1072#1090#1100'...'
            TabOrder = 3
            OnClick = btnRec1SelectClick
          end
          object btnRec2Select: TBitBtn
            Left = 592
            Top = 22
            Width = 81
            Height = 25
            Caption = #1042#1099#1073#1088#1072#1090#1100'...'
            TabOrder = 4
            OnClick = btnRec2SelectClick
          end
          object Memo1: TMemo
            Left = 8
            Top = 56
            Width = 329
            Height = 241
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 5
          end
          object Memo2: TMemo
            Left = 344
            Top = 56
            Width = 329
            Height = 241
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 6
          end
          object btnMergeToLeft: TBitBtn
            Left = 256
            Top = 312
            Width = 81
            Height = 25
            Caption = '<<<'
            TabOrder = 7
            OnClick = btnMergeToLeftClick
          end
          object btnMergeToRight: TBitBtn
            Left = 344
            Top = 312
            Width = 81
            Height = 25
            Caption = '>>>'
            TabOrder = 8
            OnClick = btnMergeToRightClick
          end
          object btnSkip: TBitBtn
            Left = 88
            Top = 312
            Width = 75
            Height = 25
            Caption = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100
            TabOrder = 9
            OnClick = btnSkipClick
          end
          object ProgressBar1: TProgressBar
            Left = 8
            Top = 344
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
            Height = 97
            Caption = #1056#1077#1078#1080#1084
            Items.Strings = (
              #1055#1077#1088#1089#1086#1085#1099
              #1047#1072#1084#1077#1090#1082#1080
              #1057#1077#1084#1100#1080
              #1048#1089#1090#1086#1095#1085#1080#1082#1080)
            TabOrder = 0
            OnClick = rgModeClick
          end
          object GroupBox1: TGroupBox
            Left = 8
            Top = 112
            Width = 297
            Height = 161
            Caption = #1055#1086#1080#1089#1082' '#1087#1077#1088#1089#1086#1085
            TabOrder = 1
            object Label5: TLabel
              Left = 8
              Top = 96
              Width = 98
              Height = 13
              Caption = #1058#1086#1095#1085#1086#1089#1090#1100' '#1080#1084#1077#1085#1080', %'
            end
            object Label6: TLabel
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
              Text = '40'
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
    object SheetImport: TTabSheet
      Caption = 'SheetImport'
      ImageIndex = 5
      TabVisible = False
      object Label3: TLabel
        Left = 8
        Top = 16
        Width = 26
        Height = 13
        Caption = #1060#1072#1081#1083
      end
      object edImportFile: TEdit
        Left = 40
        Top = 8
        Width = 577
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object btnImportFileChoose: TBitBtn
        Left = 624
        Top = 6
        Width = 81
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100'...'
        TabOrder = 1
        OnClick = btnImportFileChooseClick
        NumGlyphs = 2
      end
      object ListBox1: TListBox
        Left = 8
        Top = 40
        Width = 697
        Height = 369
        ItemHeight = 13
        Items.Strings = (
          #1042' '#1053' '#1048' '#1052' '#1040' '#1053' '#1048' '#1045' !!!'
          ''
          
            '* '#1044#1072#1085#1085#1099#1081' '#1073#1083#1086#1082' '#1080#1084#1087#1086#1088#1090#1072' '#1086#1073#1088#1072#1073#1072#1090#1099#1074#1072#1077#1090' '#1090#1086#1083#1100#1082#1086' '#1090#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' '#1088#1086#1089#1087#1080#1089 +
            #1077#1081'.'
          ''
          '* '#1060#1086#1088#1084#1072#1090' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1094#1080#1080' '#1087#1077#1088#1089#1086#1085' '#1090#1086#1083#1100#1082#1086' '#1087#1086' '#1050#1086#1085#1086#1074#1072#1083#1086#1074#1091'.'
          ''
          '* '#1048#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088' '#1087#1077#1088#1089#1086#1085#1099' '#1076#1086#1083#1078#1077#1085' '#1073#1099#1090#1100' '#1086#1090#1076#1077#1083#1077#1085' '#1086#1090' '#1080#1084#1077#1085#1080' '#1090#1086#1095#1082#1086#1081
          '  ('#1085#1072#1087#1088#1080#1084#1077#1088', "2-1. '#1048#1074#1072#1085' '#1048#1074#1072#1085#1086#1074#1080#1095' '#1048#1074#1072#1085#1086#1074'").'
          ''
          '* '#1048#1084#1103' '#1088#1072#1089#1087#1086#1079#1085#1072#1077#1090#1089#1103' '#1074' '#1092#1086#1088#1084#1072#1090#1077' "'#1048#1084#1103' '#1054#1090#1095#1077#1089#1090#1074#1086' '#1060#1072#1084#1080#1083#1080#1103'".'
          ''
          
            '* '#1055#1086#1083' '#1086#1087#1088#1077#1076#1077#1083#1103#1077#1090#1089#1103' '#1087#1086' '#1090#1080#1087#1080#1095#1085#1099#1084' '#1087#1088#1080#1079#1085#1072#1082#1072#1084' '#1078#1077#1085#1089#1082#1086#1075#1086' '#1080#1084#1077#1085#1080' '#1074' '#1088#1091#1089#1089#1082#1086 +
            #1084' '#1103#1079#1099#1082#1077
          
            '  ('#1087#1086#1089#1083#1077#1076#1085#1080#1077' '#1073#1091#1082#1074#1099' "'#1072'", "'#1103'" '#1074' '#1080#1084#1077#1085#1080' '#1080' '#1086#1090#1095#1077#1089#1090#1074#1077'), '#1074' '#1080#1085#1086#1084' '#1089#1083#1091#1095#1072#1077' -' +
            ' '#1087#1086' '#1079#1072#1087#1088#1086#1089#1091'.'
          ''
          #1056' '#1045' '#1050' '#1054' '#1052' '#1045' '#1053' '#1044' '#1040' '#1062' '#1048' '#1048
          ''
          '* '#1048#1084#1087#1086#1088#1090#1080#1088#1086#1074#1072#1085#1080#1077' '#1074#1099#1087#1086#1083#1085#1103#1090#1100' '#1074' '#1085#1077#1089#1082#1086#1083#1100#1082#1086' '#1087#1086#1087#1099#1090#1086#1082', '#1087#1086#1089#1083#1077' '#1082#1072#1078#1076#1086#1081
          
            '  '#1086#1090#1089#1084#1072#1090#1088#1080#1074#1072#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090' '#1080' '#1077#1089#1083#1080' '#1095#1090#1086' '#1085#1077' '#1090#1072#1082' - '#1074#1085#1086#1089#1080#1090#1100' '#1087#1088#1072#1074#1082#1080' '#1074' '#1090#1077 +
            #1082#1089#1090#1086#1074#1099#1081
          
            '  '#1086#1088#1080#1075#1080#1085#1072#1083'. '#1050' '#1087#1088#1080#1084#1077#1088#1091' '#1079#1072#1095#1072#1089#1090#1091#1102' '#1074' '#1089#1090#1088#1086#1082#1077' '#1080#1084#1077#1085#1080' '#1076#1072#1090#1099' '#1088#1086#1078#1076#1077#1085#1080#1103' '#1085#1077' '#1086 +
            #1090#1084#1077#1095#1077#1085#1099
          '  '#1089#1080#1084#1074#1086#1083#1086#1084' "*". '#1056#1072#1079#1091#1084#1085#1099#1081' '#1072#1083#1075#1086#1088#1080#1090#1084' '#1086#1087#1086#1079#1085#1072#1085#1080#1103' '#1089#1076#1077#1083#1072#1090#1100' '#1085#1077#1083#1100#1079#1103', '#1090'.'#1082'.'
          
            '  '#1091#1074#1077#1083#1080#1095#1080#1090#1089#1103' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1083#1086#1078#1085#1099#1093' '#1089#1088#1072#1073#1072#1090#1099#1074#1072#1085#1080#1081'. '#1050#1088#1086#1084#1077' '#1090#1086#1075#1086', '#1074' '#1089#1090#1088#1086#1082 +
            #1077
          '  '#1080#1084#1077#1085#1080' '#1090#1072#1082#1078#1077' '#1074#1089#1090#1088#1077#1095#1072#1102#1090#1089#1103' '#1074#1072#1088#1080#1072#1085#1090#1099' '#1080#1084#1077#1085' '#1080#1083#1080' '#1086#1090#1095#1077#1089#1090#1074' '#1074' '#1089#1082#1086#1073#1082#1072#1093', '
          
            '  '#1073#1077#1079' '#1089#1082#1086#1073#1086#1082' '#1080#1083#1080' '#1089#1086' '#1079#1085#1072#1082#1086#1084' "?". '#1042#1089#1105' '#1101#1090#1086' '#1090#1072#1082#1078#1077' '#1085#1077#1088#1072#1079#1091#1084#1085#1086' '#1087#1099#1090#1072#1090#1100#1089#1103 +
            ' '
          '  '#1088#1072#1089#1087#1086#1079#1085#1072#1074#1072#1090#1100' '#1072#1083#1075#1086#1088#1080#1090#1084#1080#1095#1077#1089#1082#1080'. '#1055#1086#1076#1086#1073#1085#1099#1077' '#1076#1072#1085#1085#1099#1077' '#1083#1091#1095#1096#1077' '#1074#1099#1085#1086#1089#1080#1090#1100' '#1074
          '  '#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1073#1083#1086#1082' '#1087#1086#1089#1083#1077' '#1089#1090#1088#1086#1082#1080' '#1080#1084#1077#1085#1080'. '#1042#1072#1088#1080#1072#1085#1090#1099' '#1076#1072#1090' - '#1090#1086#1095#1085#1086' '#1090#1072#1082#1078#1077'.')
        TabOrder = 2
      end
    end
    object SheetFamilyGroups: TTabSheet
      Caption = 'SheetFamilyGroups'
      ImageIndex = 6
      TabVisible = False
      object TreeView1: TTreeView
        Left = 8
        Top = 8
        Width = 697
        Height = 401
        Indent = 19
        TabOrder = 0
        OnDblClick = TreeView1DblClick
      end
    end
    object SheetDeads: TTabSheet
      Caption = 'SheetDeads'
      ImageIndex = 7
      TabVisible = False
      object btnPrepare: TBitBtn
        Left = 560
        Top = 382
        Width = 145
        Height = 25
        Caption = #1057#1076#1077#1083#1072#1090#1100' '#1086#1090#1084#1077#1090#1082#1091' '#1086' '#1089#1084#1077#1088#1090#1080
        TabOrder = 0
        OnClick = btnPrepareClick
        NumGlyphs = 2
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 713
        Height = 369
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 8
        TabOrder = 1
      end
    end
    object SheetSync: TTabSheet
      Caption = 'SheetSync'
      ImageIndex = 8
      TabVisible = False
      object Label4: TLabel
        Left = 8
        Top = 8
        Width = 64
        Height = 13
        Caption = #1052#1072#1089#1090#1077#1088'-'#1073#1072#1079#1072
      end
      object Label7: TLabel
        Left = 8
        Top = 56
        Width = 90
        Height = 13
        Caption = #1054#1073#1085#1086#1074#1083#1077#1085#1080#1077' '#1073#1072#1079#1099
      end
      object edMasterBase: TEdit
        Left = 8
        Top = 24
        Width = 609
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Text = '['#1090#1077#1082#1091#1097#1072#1103' '#1073#1072#1079#1072' '#1076#1072#1085#1085#1099#1093']'
      end
      object edUpdateBase: TEdit
        Left = 8
        Top = 72
        Width = 609
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object btnUpdateSelect: TBitBtn
        Left = 624
        Top = 70
        Width = 81
        Height = 25
        Caption = #1042#1099#1073#1088#1072#1090#1100'...'
        TabOrder = 2
        OnClick = btnUpdateSelectClick
        NumGlyphs = 2
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 104
        Width = 321
        Height = 81
        Caption = #1057#1080#1085#1093#1088#1086#1085#1080#1079#1072#1094#1080#1103
        TabOrder = 3
        object RadioButton1: TRadioButton
          Left = 16
          Top = 24
          Width = 289
          Height = 17
          Caption = #1044#1086#1074#1077#1088#1077#1085#1085#1099#1081' '#1080#1089#1090#1086#1095#1085#1080#1082' ('#1073#1077#1079#1091#1089#1083#1086#1074#1085#1072#1103' '#1089#1080#1085#1093#1088#1086#1085#1080#1079#1072#1094#1080#1103')'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButton2: TRadioButton
          Left = 16
          Top = 48
          Width = 289
          Height = 17
          Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1074#1089#1077#1093' '#1101#1083#1077#1084#1077#1085#1090#1086#1074' '#1073#1072#1079' '#1076#1072#1085#1085#1099#1093
          Enabled = False
          TabOrder = 1
        end
      end
      object Panel2: TPanel
        Left = 8
        Top = 384
        Width = 697
        Height = 25
        BorderStyle = bsSingle
        Caption = 
          #1053#1077#1079#1072#1074#1077#1088#1096#1077#1085#1085#1072#1103' '#1088#1077#1072#1083#1080#1079#1072#1094#1080#1103', '#1087#1088#1080#1084#1077#1085#1103#1090#1100' '#1085#1072' '#1088#1077#1072#1083#1100#1085#1099#1093' '#1076#1072#1085#1085#1099#1093' '#1085#1077' '#1088#1077#1082#1086#1084#1077 +
          #1085#1076#1091#1077#1090#1089#1103
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
      end
      object mSyncRes: TMemo
        Left = 8
        Top = 192
        Width = 697
        Height = 185
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
    end
    object SheetPatSearch: TTabSheet
      Caption = 'SheetPatSearch'
      ImageIndex = 9
      TabVisible = False
      object Label8: TLabel
        Left = 8
        Top = 392
        Width = 154
        Height = 13
        Caption = #1055#1086#1082#1086#1083#1077#1085#1080#1081' '#1087#1086#1090#1086#1084#1082#1086#1074' '#1085#1077' '#1084#1077#1085#1077#1077
      end
      object btnPatSearch: TBitBtn
        Left = 632
        Top = 384
        Width = 75
        Height = 25
        Caption = #1055#1086#1080#1089#1082
        TabOrder = 0
        OnClick = btnPatSearchClick
        NumGlyphs = 2
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 713
        Height = 369
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 8
        TabOrder = 1
      end
      object edMinGens: TEdit
        Left = 168
        Top = 384
        Width = 57
        Height = 21
        TabOrder = 2
        Text = '2'
      end
      object udMinGens: TUpDown
        Left = 225
        Top = 384
        Width = 16
        Height = 21
        Associate = edMinGens
        Min = 1
        Position = 2
        TabOrder = 3
      end
    end
    object SheetPlaceManage: TTabSheet
      Caption = 'SheetPlaceManage'
      ImageIndex = 10
      TabVisible = False
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 713
        Height = 369
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 8
        TabOrder = 0
      end
      object btnPlacesUpdate: TBitBtn
        Left = 632
        Top = 384
        Width = 75
        Height = 25
        Caption = #1054#1073#1085#1086#1074#1080#1090#1100
        TabOrder = 1
        OnClick = btnPlacesUpdateClick
        NumGlyphs = 2
      end
    end
    object SheetBaseCheck: TTabSheet
      Caption = 'SheetBaseCheck'
      ImageIndex = 11
      TabVisible = False
      object btnBaseCheck: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100'...'
        TabOrder = 0
        OnClick = btnBaseCheckClick
      end
      object mCheckText: TMemo
        Left = 16
        Top = 47
        Width = 681
        Height = 338
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object ProgressBar2: TProgressBar
        Left = 16
        Top = 391
        Width = 681
        Height = 16
        Step = 1
        TabOrder = 2
      end
    end
  end
  object btnClose: TBitBtn
    Left = 648
    Top = 448
    Width = 81
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 1
    Kind = bkCancel
  end
  object btnBack: TBitBtn
    Left = 8
    Top = 448
    Width = 81
    Height = 25
    Caption = '< '#1053#1072#1079#1072#1076
    Enabled = False
    TabOrder = 2
    OnClick = btnBackClick
    NumGlyphs = 2
  end
  object btnNext: TBitBtn
    Left = 96
    Top = 448
    Width = 81
    Height = 25
    Caption = #1042#1087#1077#1088#1077#1076' >'
    TabOrder = 3
    OnClick = btnNextClick
    NumGlyphs = 2
  end
  object OpenDialog1: TOpenDialog
    Filter = 'GEDCOM|*.ged|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Left = 440
    Top = 120
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ged'
    Filter = 'GEDCOM|*.ged'
    Left = 440
    Top = 168
  end
  object OpenDialog2: TOpenDialog
    Filter = 
      #1042#1089#1077' '#1087#1086#1076#1076#1077#1088#1078#1080#1074#1072#1077#1084#1099#1077' '#1092#1086#1088#1084#1072#1090#1099' (*.txt, *.csv, *.doc, *.xls)|*.txt;*.' +
      'csv;*.doc;*.xls|'#1056#1086#1089#1087#1080#1089#1100' '#1074' txt-'#1092#1086#1088#1084#1072#1090#1077' (*.txt)|*.txt|'#1056#1086#1089#1087#1080#1089#1100' '#1074' cs' +
      'v-'#1092#1086#1088#1084#1072#1090#1077' (*.csv)|*.csv|'#1056#1086#1089#1087#1080#1089#1100' '#1074' '#1092#1086#1088#1084#1072#1090#1077' Word (*.doc)|*.doc|'#1056#1086#1089 +
      #1087#1080#1089#1100' '#1074' '#1092#1086#1088#1084#1072#1090#1077' Excel (*.xls)|*.xls'
    Left = 440
    Top = 216
  end
end
