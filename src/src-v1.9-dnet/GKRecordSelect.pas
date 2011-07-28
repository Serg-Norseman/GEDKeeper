unit GKRecordSelect; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKEngine, GKBase, GKLists, GKLangs;

type
  TfmRecordSelect = class(System.Windows.Forms.Form)
  strict private
    btnSelect: System.Windows.Forms.Button;
    btnCreate: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    panList: System.Windows.Forms.Panel;
    panFilter: System.Windows.Forms.Panel;

    FBase: TfmBase;
    FMode: TGEDCOMRecord.TGEDCOMRecordType;
    FFilter: string;
    FTargetMode: TGenEngine.TTargetMode;
    FLocalFilter: TPersonsFilter;

    procedure SetMode(const Value: TGEDCOMRecord.TGEDCOMRecordType);
    procedure DataRefresh();
    procedure SetFilter(const Value: string);
    procedure SetTargetMode(const Value: TGenEngine.TTargetMode);

    procedure InitializeComponent;
    procedure btnSelect_Click(sender: System.Object; e: System.EventArgs);
    procedure btnCreate_Click(sender: System.Object; e: System.EventArgs);
    procedure edFastFilter_TextChanged(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    edFastFilter: System.Windows.Forms.TextBox;

    FTarget: TGEDCOMIndividualRecord;
    FNeedSex: TGEDCOMObject.TGEDCOMSex;
    ResultRecord: TGEDCOMRecord;

    ListRecords: TRecordsView;

    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Filter: string read FFilter write SetFilter;
    property Mode: TGEDCOMRecord.TGEDCOMRecordType read FMode write SetMode;
    property TargetMode: TGenEngine.TTargetMode read FTargetMode write SetTargetMode;
  end;

implementation

procedure TfmRecordSelect.InitializeComponent;
begin
  Self.btnSelect := System.Windows.Forms.Button.Create;
  Self.btnCreate := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.panList := System.Windows.Forms.Panel.Create;
  Self.panFilter := System.Windows.Forms.Panel.Create;
  Self.edFastFilter := System.Windows.Forms.TextBox.Create;
  Self.panFilter.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnSelect
  // 
  Self.btnSelect.Location := System.Drawing.Point.Create(200, 384);
  Self.btnSelect.Name := 'btnSelect';
  Self.btnSelect.Size := System.Drawing.Size.Create(81, 25);
  Self.btnSelect.TabIndex := 3;
  Self.btnSelect.Text := 'Выбрать';
  Include(Self.btnSelect.Click, Self.btnSelect_Click);
  // 
  // btnCreate
  // 
  Self.btnCreate.Location := System.Drawing.Point.Create(104, 384);
  Self.btnCreate.Name := 'btnCreate';
  Self.btnCreate.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCreate.TabIndex := 2;
  Self.btnCreate.Text := 'Добавить';
  Include(Self.btnCreate.Click, Self.btnCreate_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Location := System.Drawing.Point.Create(296, 384);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 4;
  Self.btnCancel.Text := 'Отменить';
  // 
  // panList
  // 
  Self.panList.Location := System.Drawing.Point.Create(0, 41);
  Self.panList.Name := 'panList';
  Self.panList.Size := System.Drawing.Size.Create(385, 329);
  Self.panList.TabIndex := 1;
  // 
  // panFilter
  // 
  Self.panFilter.Controls.Add(Self.edFastFilter);
  Self.panFilter.Location := System.Drawing.Point.Create(0, 0);
  Self.panFilter.Name := 'panFilter';
  Self.panFilter.Size := System.Drawing.Size.Create(385, 41);
  Self.panFilter.TabIndex := 0;
  // 
  // edFastFilter
  // 
  Self.edFastFilter.Location := System.Drawing.Point.Create(8, 8);
  Self.edFastFilter.Name := 'edFastFilter';
  Self.edFastFilter.Size := System.Drawing.Size.Create(361, 21);
  Self.edFastFilter.TabIndex := 0;
  Self.edFastFilter.Text := '';
  Include(Self.edFastFilter.TextChanged, Self.edFastFilter_TextChanged);
  //
  // TfmRecordSelect
  //
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(385, 417);
  Self.Controls.Add(Self.panFilter);
  Self.Controls.Add(Self.panList);
  Self.Controls.Add(Self.btnSelect);
  Self.Controls.Add(Self.btnCreate);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular,
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.KeyPreview := True;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmRecordSelect';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Выбор записи';
  Self.panFilter.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmRecordSelect.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;
  FLocalFilter := TPersonsFilter.Create;
  FLocalFilter.List := flSelector;
  FFilter := '*';

  /// SetLang
  Text := LSList[LSID_WinRecordSelect];
  btnCreate.Text := LSList[LSID_DlgAppend];
  btnSelect.Text := LSList[LSID_DlgSelect];
  btnCancel.Text := LSList[LSID_DlgCancel];
end;

procedure TfmRecordSelect.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FLocalFilter.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmRecordSelect.DataRefresh();
begin
  FLocalFilter.Clear();
  FLocalFilter.Name := FFilter;
  FLocalFilter.Sex := FNeedSex;

  if Assigned(ListRecords) then begin
    ListRecords.Dispose;
    ListRecords := nil;
  end;

  Base.CreateRecordsView(panList, FMode, ListRecords);
  ListRecords.UpdateContents(Base.ShieldState, True, FLocalFilter, 1);
end;

procedure TfmRecordSelect.btnCreate_Click(sender: System.Object; e: System.EventArgs);
var
  iRec: TGEDCOMIndividualRecord;
  famRec: TGEDCOMFamilyRecord;
  noteRec: TGEDCOMNoteRecord;
  sourceRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
  repRec: TGEDCOMRepositoryRecord;
  mmRec: TGEDCOMMultimediaRecord;
  taskRec: TGEDCOMTaskRecord;
  corrRec: TGEDCOMCommunicationRecord;
  locRec: TGEDCOMLocationRecord;
  fam_target: TGenEngine.TFamilyTarget;
begin
  //Hide;

  case FMode of
    rtIndividual: begin
      iRec := Base.CreatePersonDialog(FTarget, FTargetMode, FNeedSex);
      if (iRec <> nil) then begin
        ResultRecord := iRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtFamily: begin
      famRec := nil;

      if (FTargetMode = tmChildToFamily)
      then fam_target := ftChild
      else fam_target := ftNone;

      if Base.ModifyFamily(famRec, fam_target, FTarget) then begin
        ResultRecord := famRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtNote: begin
      noteRec := nil;
      if Base.ModifyNote(noteRec) then begin
        ResultRecord := noteRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtMultimedia: begin
      mmRec := nil;
      if Base.ModifyMedia(mmRec) then begin
        ResultRecord := mmRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtSource: begin
      sourceRec := nil;
      if Base.ModifySource(sourceRec) then begin
        ResultRecord := sourceRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtRepository: begin
      repRec := nil;
      if Base.ModifyRepository(repRec) then begin
        ResultRecord := repRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtGroup: begin
      groupRec := nil;
      if Base.ModifyGroup(groupRec) then begin
        ResultRecord := groupRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtTask: begin
      taskRec := nil;
      if Base.ModifyTask(taskRec) then begin
        ResultRecord := taskRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtCommunication: begin
      corrRec := nil;
      if Base.ModifyCommunication(corrRec) then begin
        ResultRecord := corrRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;

    rtLocation: begin
      locRec := nil;
      if Base.ModifyLocation(locRec) then begin
        ResultRecord := locRec;
        DialogResult := System.Windows.Forms.DialogResult.OK;
      end;
    end;
  end;
end;

procedure TfmRecordSelect.btnSelect_Click(sender: System.Object; e: System.EventArgs);
begin
  ResultRecord := ListRecords.GetSelectedRecord();
  DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmRecordSelect.edFastFilter_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  SetFilter(edFastFilter.Text);
end;

procedure TfmRecordSelect.SetMode(const Value: TGEDCOMRecord.TGEDCOMRecordType);
begin
  FMode := Value;
  DataRefresh();
end;

procedure TfmRecordSelect.SetFilter(const Value: string);
begin
  FFilter := Value;

  if (FFilter = '')
  then FFilter := '*'
  else
    if (FFilter <> '*')
    then FFilter := '*' + FFilter + '*';

  DataRefresh();
end;

procedure TfmRecordSelect.SetTargetMode(const Value: TGenEngine.TTargetMode);
begin
  FTargetMode := Value;
  FLocalFilter.ChildSelector := (FTargetMode = tmAncestor);
end;

end.
