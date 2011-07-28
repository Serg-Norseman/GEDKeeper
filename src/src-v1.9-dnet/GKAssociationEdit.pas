unit GKAssociationEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  GedCom551, GKBase, GKEngine, GKMain, GKLangs;

type
  TfmAssociationEdit = class(System.Windows.Forms.Form)
  strict private
    Components: System.ComponentModel.Container;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label1: System.Windows.Forms.Label;
    EditRelation: System.Windows.Forms.ComboBox;
    Label2: System.Windows.Forms.Label;
    EditPerson: System.Windows.Forms.TextBox;
    btnPersonAdd: System.Windows.Forms.Button;

    FBase: TfmBase;
    FAssociation: TGEDCOMAssociation;
    FTempInd: TGEDCOMIndividualRecord;

    procedure SetAssociation(const Value: TGEDCOMAssociation);

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPersonAdd_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Association: TGEDCOMAssociation read FAssociation write SetAssociation;
    property Base: TfmBase read FBase;
  end;

implementation

procedure TfmAssociationEdit.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmAssociationEdit));
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.btnPersonAdd := System.Windows.Forms.Button.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.EditRelation := System.Windows.Forms.ComboBox.Create;
  Self.EditPerson := System.Windows.Forms.TextBox.Create;
  Self.SuspendLayout;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(65, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Отношение';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 56);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(50, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Персона';
  // 
  // btnPersonAdd
  // 
  Self.btnPersonAdd.AccessibleDescription := 'Выбрать персональную запись';
  Self.btnPersonAdd.AccessibleName := '';
  Self.btnPersonAdd.AccessibleRole := System.Windows.Forms.AccessibleRole.ToolTip;
  Self.btnPersonAdd.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.btnPersonAdd.Location := System.Drawing.Point.Create(320, 69);
  Self.btnPersonAdd.Name := 'btnPersonAdd';
  Self.btnPersonAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPersonAdd.TabIndex := 2;
  Include(Self.btnPersonAdd.Click, Self.btnPersonAdd_Click);
  // 
  // btnAccept
  // 
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(176, 112);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 3;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Image := (System.Drawing.Image(resources.GetObject('btnCanc' +
    'el.Image')));
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(264, 112);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 4;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // EditRelation
  // 
  Self.EditRelation.Location := System.Drawing.Point.Create(8, 24);
  Self.EditRelation.Name := 'EditRelation';
  Self.EditRelation.Size := System.Drawing.Size.Create(337, 21);
  Self.EditRelation.Sorted := True;
  Self.EditRelation.TabIndex := 5;
  // 
  // EditPerson
  // 
  Self.EditPerson.BackColor := System.Drawing.SystemColors.Control;
  Self.EditPerson.Location := System.Drawing.Point.Create(8, 72);
  Self.EditPerson.Name := 'EditPerson';
  Self.EditPerson.ReadOnly := True;
  Self.EditPerson.Size := System.Drawing.Size.Create(306, 21);
  Self.EditPerson.TabIndex := 6;
  Self.EditPerson.Text := '';
  // 
  // TfmAssociationEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(353, 145);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.btnPersonAdd);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.EditRelation);
  Self.Controls.Add(Self.EditPerson);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmAssociationEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Ассоциация';
  Self.ResumeLayout(False);
end;

constructor TfmAssociationEdit.Create(aBase: TfmBase);
var
  i: Integer;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  for i := 0 to fmGEDKeeper.Options.Relations.Count - 1 do
    EditRelation.Items.Add(fmGEDKeeper.Options.Relations[i]);

  /// SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_Association];
  Label1.Text := LSList[LSID_Relation];
  Label2.Text := LSList[LSID_Person];
end;

procedure TfmAssociationEdit.SetAssociation(const Value: TGEDCOMAssociation);
begin
  FAssociation := Value;

  EditRelation.Text := FAssociation.Relation;
  EditPerson.Text := TGenEngine.GetNameStr(FAssociation.Individual);
end;

procedure TfmAssociationEdit.btnPersonAdd_Click(sender: System.Object; e: System.EventArgs);
begin
  FTempInd := Base.SelectPerson(nil, tmNone, svNone);
  EditPerson.Text := TGenEngine.GetNameStr(FTempInd);
end;

procedure TfmAssociationEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  rel: string;
begin
  rel := EditRelation.Text.Trim();
  if (rel <> '') then begin
    if (fmGEDKeeper.Options.Relations.IndexOf(rel) < 0)
    then fmGEDKeeper.Options.Relations.Add(rel);
  end;

  FAssociation.Relation := EditRelation.Text;
  FAssociation.Individual := FTempInd;

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

end.
