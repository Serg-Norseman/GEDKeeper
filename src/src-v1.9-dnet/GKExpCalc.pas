unit GKExpCalc; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, ExpCalc;

type
  TfmCalcWidget = class(System.Windows.Forms.Form)
  strict private
    lbOutput: System.Windows.Forms.ListBox;
    edExpression: System.Windows.Forms.TextBox;
    chkPutToClipboard: System.Windows.Forms.CheckBox;
    edCalcResult: System.Windows.Forms.TextBox;

    calc: TCalculator;

    procedure OnModalBegin(Sender: TObject);
    procedure InitializeComponent;
    procedure edExpression_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure edCalcResult_MouseMove(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
    procedure TfmCalcWidget_Closed(sender: System.Object; e: System.EventArgs);
    procedure edCalcResult_DragOver(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create;
  end;

implementation

uses GKMain, GKLangs;

procedure TfmCalcWidget.InitializeComponent;
begin
  Self.lbOutput := System.Windows.Forms.ListBox.Create;
  Self.edExpression := System.Windows.Forms.TextBox.Create;
  Self.chkPutToClipboard := System.Windows.Forms.CheckBox.Create;
  Self.edCalcResult := System.Windows.Forms.TextBox.Create;
  Self.SuspendLayout;
  // 
  // lbOutput
  // 
  Self.lbOutput.Location := System.Drawing.Point.Create(8, 8);
  Self.lbOutput.Name := 'lbOutput';
  Self.lbOutput.Size := System.Drawing.Size.Create(257, 95);
  Self.lbOutput.TabIndex := 0;
  // 
  // edExpression
  // 
  Self.edExpression.Location := System.Drawing.Point.Create(8, 120);
  Self.edExpression.Name := 'edExpression';
  Self.edExpression.Size := System.Drawing.Size.Create(257, 21);
  Self.edExpression.TabIndex := 1;
  Self.edExpression.Text := '';
  Include(Self.edExpression.KeyDown, Self.edExpression_KeyDown);
  // 
  // chkPutToClipboard
  // 
  Self.chkPutToClipboard.Location := System.Drawing.Point.Create(8, 176);
  Self.chkPutToClipboard.Name := 'chkPutToClipboard';
  Self.chkPutToClipboard.Size := System.Drawing.Size.Create(257, 17);
  Self.chkPutToClipboard.TabIndex := 3;
  Self.chkPutToClipboard.Text := 'PutToClipboard';
  // 
  // edCalcResult
  // 
  Self.edCalcResult.Location := System.Drawing.Point.Create(8, 144);
  Self.edCalcResult.Name := 'edCalcResult';
  Self.edCalcResult.ReadOnly := True;
  Self.edCalcResult.Size := System.Drawing.Size.Create(257, 21);
  Self.edCalcResult.TabIndex := 2;
  Self.edCalcResult.TabStop := False;
  Self.edCalcResult.Text := '1979';
  Include(Self.edCalcResult.DragOver, Self.edCalcResult_DragOver);
  Include(Self.edCalcResult.MouseMove, Self.edCalcResult_MouseMove);
  // 
  // TfmCalcWidget
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(274, 201);
  Self.Controls.Add(Self.lbOutput);
  Self.Controls.Add(Self.edExpression);
  Self.Controls.Add(Self.chkPutToClipboard);
  Self.Controls.Add(Self.edCalcResult);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedToolWindow;
  Self.Name := 'TfmCalcWidget';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'ExpCalc';
  Self.TopMost := True;
  Include(Self.Closed, Self.TfmCalcWidget_Closed);
  Self.ResumeLayout(False);
end;

constructor TfmCalcWidget.Create;
begin
  inherited Create;
  InitializeComponent;

  calc := TCalculator.Create;

  //  edCalcResult.DragMode = dmAutomatic
  //  edCalcResult.OnDragOver = edCalcResultDragOver
  //  edCalcResult.OnKeyDown = edExpressionKeyDown

  Self.lbOutput.Items.Clear;

  // SetLang
  Text := LSList[LSID_MICalc];
  chkPutToClipboard.Text := LSList[LSID_CopyResultToClipboard];

  (*
      OnModalBegin only occurs for the first modal form. That is,
      if the modal form displays another modal form, there is no
      second OnModalBegin event when the second modal form is launched.
  *)
  //Application.OnModalBegin := OnModalBegin;alert
end;

procedure TfmCalcWidget.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    //Application.OnModalBegin := nil;alert
    calc.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmCalcWidget.edExpression_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
var
  res: string;
begin
  if (e.KeyCode = Keys.Return) then begin
    try
      res := calc.Calc(edExpression.Text).ToString();

      if (chkPutToClipboard.Checked)
      then Clipboard.SetDataObject(res);
    except
      on E: Exception do res := '[ошибка]: ' + E.Message;
    end;

    lbOutput.Items.Add('> ' + edExpression.Text);
    lbOutput.Items.Add('= ' + res);
    lbOutput.SelectedIndex := lbOutput.Items.Count - 1;
    edCalcResult.Text := res;
  end;
end;

procedure TfmCalcWidget.TfmCalcWidget_Closed(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.miCalc.Checked := False;
  fmGEDKeeper.fmCalcWidget := nil;
end;

procedure TfmCalcWidget.edCalcResult_MouseMove(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
begin
  if (e.Button = System.Windows.Forms.MouseButtons.Left)
  then edCalcResult.DoDragDrop(edCalcResult.Text, DragDropEffects.Move);
end;

procedure TfmCalcWidget.OnModalBegin(Sender: TObject);
begin
  PostMessage(fmGEDKeeper.Handle.ToInt32, fmGEDKeeper.WM_KEEPMODELESS, 0, 0);
end;

procedure TfmCalcWidget.edCalcResult_DragOver(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
begin
  e.Effect := DragDropEffects.None;
end;

end.
