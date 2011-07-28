unit GKTimeLine; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  GedCom551, GKBase, GKLists, GKLangs;

type
  TfmTimeLine = class(System.Windows.Forms.Form)
  strict private
    StatusBarPanel1: System.Windows.Forms.StatusBarPanel;
    StatusBarPanel2: System.Windows.Forms.StatusBarPanel;
    tbTimeLine: System.Windows.Forms.TrackBar;
    StatusBar1: System.Windows.Forms.StatusBar;

    FBase: TfmBase;

    procedure StatusUpdate();
    function GetIListMan(): TIndividualListMan;
    procedure InitializeComponent;
    procedure TfmTimeLine_Closed(sender: System.Object; e: System.EventArgs);
    procedure tbTimeLine_ValueChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create;

    procedure CheckTimeWin(aBase: TfmBase);
  end;

implementation

uses GKMain;

procedure TfmTimeLine.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_StatusBarPanel = array of System.Windows.Forms.StatusBarPanel;
begin
  Self.tbTimeLine := System.Windows.Forms.TrackBar.Create;
  Self.StatusBar1 := System.Windows.Forms.StatusBar.Create;
  Self.StatusBarPanel1 := System.Windows.Forms.StatusBarPanel.Create;
  Self.StatusBarPanel2 := System.Windows.Forms.StatusBarPanel.Create;
  (System.ComponentModel.ISupportInitialize(Self.tbTimeLine)).BeginInit;
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel1)).BeginInit;
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel2)).BeginInit;
  Self.SuspendLayout;
  // 
  // tbTimeLine
  // 
  Self.tbTimeLine.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.tbTimeLine.LargeChange := 1;
  Self.tbTimeLine.Location := System.Drawing.Point.Create(0, 0);
  Self.tbTimeLine.Minimum := 5;
  Self.tbTimeLine.Name := 'tbTimeLine';
  Self.tbTimeLine.Size := System.Drawing.Size.Create(524, 42);
  Self.tbTimeLine.TabIndex := 0;
  Self.tbTimeLine.Value := 5;
  Include(Self.tbTimeLine.ValueChanged, Self.tbTimeLine_ValueChanged);
  // 
  // StatusBar1
  // 
  Self.StatusBar1.Location := System.Drawing.Point.Create(0, 42);
  Self.StatusBar1.Name := 'StatusBar1';
  Self.StatusBar1.Panels.AddRange(TArrayOfSystem_Windows_Forms_StatusBarPanel.Create(Self.StatusBarPanel1, 
          Self.StatusBarPanel2));
  Self.StatusBar1.ShowPanels := True;
  Self.StatusBar1.Size := System.Drawing.Size.Create(524, 19);
  Self.StatusBar1.TabIndex := 1;
  // 
  // StatusBarPanel1
  // 
  Self.StatusBarPanel1.Text := ' ';
  Self.StatusBarPanel1.Width := 250;
  //
  // StatusBarPanel2
  //
  Self.StatusBarPanel2.Text := ' ';
  Self.StatusBarPanel2.Width := 250;
  // 
  // TfmTimeLine
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(524, 61);
  Self.Controls.Add(Self.tbTimeLine);
  Self.Controls.Add(Self.StatusBar1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedToolWindow;
  Self.Name := 'TfmTimeLine';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'Линия времени';
  Self.TopMost := True;
  Include(Self.Closed, Self.TfmTimeLine_Closed);
  (System.ComponentModel.ISupportInitialize(Self.tbTimeLine)).EndInit;
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel1)).EndInit;
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel2)).EndInit;
  Self.ResumeLayout(False);
end;

constructor TfmTimeLine.Create;
begin
  inherited Create;
  InitializeComponent;

  CheckTimeWin(fmGEDKeeper.GetCurrentFile());

  /// SetLang
  Text := LSList[LSID_MITimeLine];
end;

procedure TfmTimeLine.CheckTimeWin(aBase: TfmBase);
var
  max, min, cur, x: Integer;
begin
  FBase := aBase;
  if (FBase = nil) then Exit;

  try
    max := GetIListMan().YearMax + 1;
    min := GetIListMan().YearMin - 1;
    cur := FBase.TimeLine_GetYear();
    if (min > max) then begin
      x := min;
      min := max;
      max := x;
    end;
    if (cur < min) then cur := min;
    if (cur > max) then cur := max;

    Exclude(Self.tbTimeLine.ValueChanged, Self.tbTimeLine_ValueChanged);
    tbTimeLine.Maximum := max;
    tbTimeLine.Minimum := min;
    tbTimeLine.Value := cur;
    Include(Self.tbTimeLine.ValueChanged, Self.tbTimeLine_ValueChanged);

    StatusUpdate();

    FBase.TimeLine_Init();
  finally
  end;
end;

procedure TfmTimeLine.TfmTimeLine_Closed(sender: System.Object; e: System.EventArgs);
var
  i: Integer;
begin
  for i := 0 to Length(fmGEDKeeper.MdiChildren) - 1 do
    if (fmGEDKeeper.MdiChildren[i] is TfmBase)
    then TfmBase(fmGEDKeeper.MdiChildren[i]).TimeLine_Done();

  fmGEDKeeper.miTimeLine.Checked := False;
  fmGEDKeeper.fmTimeLine := nil;
end;

function TfmTimeLine.GetIListMan(): TIndividualListMan;
begin
  Result := TIndividualListMan(FBase.ListPersons.ListMan);
end;

procedure TfmTimeLine.StatusUpdate();
begin
  if Assigned(FBase) then begin
    StatusBarPanel1.Text := LSList[LSID_TimeScale] + ': ' + GetIListMan().YearMin.ToString() + ' - ' + GetIListMan().YearMax.ToString();
    StatusBarPanel2.Text := LSList[LSID_CurrentYear] + ': ' + FBase.TimeLine_GetYear().ToString();
  end;
end;

procedure TfmTimeLine.tbTimeLine_ValueChanged(sender: System.Object; e: System.EventArgs);
begin
  if Assigned(FBase) then FBase.TimeLine_SetYear(tbTimeLine.Value);
  StatusUpdate();
end;

end.
