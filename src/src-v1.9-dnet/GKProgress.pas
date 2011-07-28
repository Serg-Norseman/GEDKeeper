unit GKProgress; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GKLangs;

type
  TfmProgress = class(System.Windows.Forms.Form)
  strict private
    class var
      frm: TfmProgress;
  strict private
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label7: System.Windows.Forms.Label;
    Label8: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    Label9: System.Windows.Forms.Label;
    ProgressBar1: System.Windows.Forms.ProgressBar;
    Label1: System.Windows.Forms.Label;
    StartTime: DateTime;

    procedure Step();
    procedure InitializeComponent;
  public
    constructor Create;

    class procedure ProgressInit(aMax: Integer; aTitle: string); static;
    class procedure ProgressDone(); static;
    class procedure ProgressStep(); static;
  end;

implementation

{ TfmProgress }

procedure TfmProgress.InitializeComponent;
begin
  Self.ProgressBar1 := System.Windows.Forms.ProgressBar.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label7 := System.Windows.Forms.Label.Create;
  Self.Label8 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label9 := System.Windows.Forms.Label.Create;
  Self.SuspendLayout;
  // 
  // ProgressBar1
  // 
  Self.ProgressBar1.Location := System.Drawing.Point.Create(8, 24);
  Self.ProgressBar1.Name := 'ProgressBar1';
  Self.ProgressBar1.Size := System.Drawing.Size.Create(401, 16);
  Self.ProgressBar1.TabIndex := 0;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(31, 13);
  Self.Label1.TabIndex := 1;
  Self.Label1.Text := 'Label1';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 48);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(96, 13);
  Self.Label2.TabIndex := 2;
  Self.Label2.Text := 'Времени прошло';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 64);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(104, 13);
  Self.Label3.TabIndex := 3;
  Self.Label3.Text := 'Времени осталось';
  // 
  // Label7
  // 
  Self.Label7.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Bold, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.Label7.Location := System.Drawing.Point.Create(184, 48);
  Self.Label7.Name := 'Label7';
  Self.Label7.Size := System.Drawing.Size.Create(225, 16);
  Self.Label7.TabIndex := 4;
  Self.Label7.TextAlign := System.Drawing.ContentAlignment.TopRight;
  // 
  // Label8
  // 
  Self.Label8.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Bold, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.Label8.Location := System.Drawing.Point.Create(184, 64);
  Self.Label8.Name := 'Label8';
  Self.Label8.Size := System.Drawing.Size.Create(225, 16);
  Self.Label8.TabIndex := 5;
  Self.Label8.TextAlign := System.Drawing.ContentAlignment.TopRight;
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 80);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(73, 13);
  Self.Label4.TabIndex := 6;
  Self.Label4.Text := 'Времени всего';
  // 
  // Label9
  // 
  Self.Label9.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Bold, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.Label9.Location := System.Drawing.Point.Create(184, 80);
  Self.Label9.Name := 'Label9';
  Self.Label9.Size := System.Drawing.Size.Create(225, 16);
  Self.Label9.TabIndex := 7;
  Self.Label9.Text := '?';
  Self.Label9.TextAlign := System.Drawing.ContentAlignment.TopRight;
  // 
  // TfmProgress
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(417, 105);
  Self.Controls.Add(Self.ProgressBar1);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.Label3);
  Self.Controls.Add(Self.Label7);
  Self.Controls.Add(Self.Label8);
  Self.Controls.Add(Self.Label4);
  Self.Controls.Add(Self.Label9);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedSingle;
  Self.Name := 'TfmProgress';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.TopMost := True;
  Self.Text := 'Прогресс';
  Self.ResumeLayout(False);
end;

constructor TfmProgress.Create;
begin
  inherited Create;
  InitializeComponent;

  /// SetLang
  Text := LSList[LSID_Progress];
  Label2.Text := LSList[LSID_TimePassed];
  Label3.Text := LSList[LSID_TimeRemain];
  Label4.Text := LSList[LSID_TimeTotal];
end;

procedure TfmProgress.Step();
var
  PassTime, RestTime, tmp: TimeSpan;
  count, pos: Integer;
begin
  PassTime := DateTime.Now - StartTime;
  Label7.Text := PassTime.ToString({'HH:mm:ss', nil});

  count := ProgressBar1.Maximum;
  pos := ProgressBar1.Value;
  if (pos = 0) then pos := 1;

  RestTime := TimeSpan.Create(Trunc((PassTime.Ticks / pos) * (count - pos)));
  Label8.Text := RestTime.ToString({'HH:mm:ss', nil});

  tmp := PassTime + RestTime;
  Label9.Text := tmp.ToString({'HH:mm:ss', nil});

  ProgressBar1.Increment(1);
  Update();
end;

class procedure TfmProgress.ProgressInit(aMax: Integer; aTitle: string);
begin
  ProgressDone();

  frm := TfmProgress.Create();
  frm.ProgressBar1.Minimum := 0;
  frm.ProgressBar1.Maximum := aMax;
  frm.Label1.Text := aTitle;
  frm.Show;
  frm.StartTime := DateTime.Now;

  Application.DoEvents;
end;

class procedure TfmProgress.ProgressDone();
begin
  if (frm <> nil) then begin
    frm.Dispose;
    frm := nil;
  end;
end;

class procedure TfmProgress.ProgressStep();
begin
  if (frm <> nil) then frm.Step();
end;

end.
