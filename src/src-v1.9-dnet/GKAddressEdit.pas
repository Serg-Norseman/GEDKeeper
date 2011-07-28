unit GKAddressEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  GKUI.Common.InputBox, GedCom551, GKEngine, GKLists, GKLangs;

type
  TfmAddressEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    PageAddrData: System.Windows.Forms.TabControl;
    SheetPhones: System.Windows.Forms.TabPage;
    SheetEmails: System.Windows.Forms.TabPage;
    SheetCommon: System.Windows.Forms.TabPage;
    SheetWebPages: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    Label5: System.Windows.Forms.Label;
    edCountry: System.Windows.Forms.TextBox;
    edState: System.Windows.Forms.TextBox;
    edCity: System.Windows.Forms.TextBox;
    edPostalCode: System.Windows.Forms.TextBox;
    edAddress: System.Windows.Forms.TextBox;

    FAddress: TGEDCOMAddress;

    FPhonesList: TSheetList;
    FMailsList: TSheetList;
    FWebsList: TSheetList;

    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure SetAddress(const Value: TGEDCOMAddress);
    procedure UpdateLists();

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create;

    property Address: TGEDCOMAddress read FAddress write SetAddress;
  end;

implementation

procedure TfmAddressEdit.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmAddressEdit));
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.PageAddrData := System.Windows.Forms.TabControl.Create;
  Self.SheetCommon := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.edCountry := System.Windows.Forms.TextBox.Create;
  Self.edState := System.Windows.Forms.TextBox.Create;
  Self.edCity := System.Windows.Forms.TextBox.Create;
  Self.edPostalCode := System.Windows.Forms.TextBox.Create;
  Self.edAddress := System.Windows.Forms.TextBox.Create;
  Self.SheetPhones := System.Windows.Forms.TabPage.Create;
  Self.SheetEmails := System.Windows.Forms.TabPage.Create;
  Self.SheetWebPages := System.Windows.Forms.TabPage.Create;
  Self.PageAddrData.SuspendLayout;
  Self.SheetCommon.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(232, 280);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 1;
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
  Self.btnCancel.Location := System.Drawing.Point.Create(320, 280);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // PageAddrData
  // 
  Self.PageAddrData.Controls.Add(Self.SheetCommon);
  Self.PageAddrData.Controls.Add(Self.SheetPhones);
  Self.PageAddrData.Controls.Add(Self.SheetEmails);
  Self.PageAddrData.Controls.Add(Self.SheetWebPages);
  Self.PageAddrData.Dock := System.Windows.Forms.DockStyle.Top;
  Self.PageAddrData.Location := System.Drawing.Point.Create(0, 0);
  Self.PageAddrData.Name := 'PageAddrData';
  Self.PageAddrData.SelectedIndex := 0;
  Self.PageAddrData.Size := System.Drawing.Size.Create(409, 264);
  Self.PageAddrData.TabIndex := 0;
  // 
  // SheetCommon
  // 
  Self.SheetCommon.Controls.Add(Self.Label1);
  Self.SheetCommon.Controls.Add(Self.Label2);
  Self.SheetCommon.Controls.Add(Self.Label3);
  Self.SheetCommon.Controls.Add(Self.Label4);
  Self.SheetCommon.Controls.Add(Self.Label5);
  Self.SheetCommon.Controls.Add(Self.edCountry);
  Self.SheetCommon.Controls.Add(Self.edState);
  Self.SheetCommon.Controls.Add(Self.edCity);
  Self.SheetCommon.Controls.Add(Self.edPostalCode);
  Self.SheetCommon.Controls.Add(Self.edAddress);
  Self.SheetCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommon.Name := 'SheetCommon';
  Self.SheetCommon.Size := System.Drawing.Size.Create(401, 238);
  Self.SheetCommon.TabIndex := 0;
  Self.SheetCommon.Text := 'Адрес';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(45, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Страна';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(216, 8);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(80, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Штат/Область';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 56);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(35, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Город';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(216, 56);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(80, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Почтовый код';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 104);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(35, 13);
  Self.Label5.TabIndex := 4;
  Self.Label5.Text := 'Адрес';
  // 
  // edCountry
  // 
  Self.edCountry.Location := System.Drawing.Point.Create(8, 24);
  Self.edCountry.Name := 'edCountry';
  Self.edCountry.Size := System.Drawing.Size.Create(201, 21);
  Self.edCountry.TabIndex := 0;
  Self.edCountry.Text := '';
  // 
  // edState
  // 
  Self.edState.Location := System.Drawing.Point.Create(216, 24);
  Self.edState.Name := 'edState';
  Self.edState.Size := System.Drawing.Size.Create(177, 21);
  Self.edState.TabIndex := 1;
  Self.edState.Text := '';
  // 
  // edCity
  // 
  Self.edCity.Location := System.Drawing.Point.Create(8, 72);
  Self.edCity.Name := 'edCity';
  Self.edCity.Size := System.Drawing.Size.Create(201, 21);
  Self.edCity.TabIndex := 2;
  Self.edCity.Text := '';
  // 
  // edPostalCode
  // 
  Self.edPostalCode.Location := System.Drawing.Point.Create(216, 72);
  Self.edPostalCode.Name := 'edPostalCode';
  Self.edPostalCode.Size := System.Drawing.Size.Create(177, 21);
  Self.edPostalCode.TabIndex := 3;
  Self.edPostalCode.Text := '';
  // 
  // edAddress
  // 
  Self.edAddress.Location := System.Drawing.Point.Create(8, 120);
  Self.edAddress.Name := 'edAddress';
  Self.edAddress.Size := System.Drawing.Size.Create(385, 21);
  Self.edAddress.TabIndex := 4;
  Self.edAddress.Text := '';
  // 
  // SheetPhones
  // 
  Self.SheetPhones.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetPhones.Name := 'SheetPhones';
  Self.SheetPhones.Size := System.Drawing.Size.Create(401, 238);
  Self.SheetPhones.TabIndex := 1;
  Self.SheetPhones.Text := 'Телефоны';
  // 
  // SheetEmails
  // 
  Self.SheetEmails.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetEmails.Name := 'SheetEmails';
  Self.SheetEmails.Size := System.Drawing.Size.Create(401, 238);
  Self.SheetEmails.TabIndex := 2;
  Self.SheetEmails.Text := 'Эл. почта';
  // 
  // SheetWebPages
  // 
  Self.SheetWebPages.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetWebPages.Name := 'SheetWebPages';
  Self.SheetWebPages.Size := System.Drawing.Size.Create(401, 238);
  Self.SheetWebPages.TabIndex := 3;
  Self.SheetWebPages.Text := 'Веб-страницы';
  // 
  // TfmAddressEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(409, 313);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.PageAddrData);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmAddressEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Адрес';
  Self.PageAddrData.ResumeLayout(False);
  Self.SheetCommon.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmAddressEdit.Create;
begin
  inherited Create;
  InitializeComponent;

  ///
  FPhonesList := TSheetList.Create(SheetPhones);
  FPhonesList.OnModify := ListModify;
  FPhonesList.List.AddListColumn(LSList[LSID_Telephone], 350, False);

  FMailsList := TSheetList.Create(SheetEmails);
  FMailsList.OnModify := ListModify;
  FMailsList.List.AddListColumn(LSList[LSID_Mail], 350, False);

  FWebsList := TSheetList.Create(SheetWebPages);
  FWebsList.OnModify := ListModify;
  FWebsList.List.AddListColumn(LSList[LSID_WebSite], 350, False);

  /// SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_Address];
  SheetCommon.Text := LSList[LSID_Address];

  Label1.Text := LSList[LSID_AdCountry];
  Label2.Text := LSList[LSID_AdState];
  Label3.Text := LSList[LSID_AdCity];
  Label4.Text := LSList[LSID_AdPostalCode];
  Label5.Text := LSList[LSID_Address];

  SheetPhones.Text := LSList[LSID_Telephones];
  SheetEmails.Text := LSList[LSID_EMails];
  SheetWebPages.Text := LSList[LSID_WebSites];
end;

procedure TfmAddressEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  FAddress.AddressCountry := edCountry.Text;
  FAddress.AddressState := edState.Text;
  FAddress.AddressCity := edCity.Text;
  FAddress.AddressPostalCode := edPostalCode.Text;
  TGenEngine.SetAddressValue(FAddress, edAddress.Text);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmAddressEdit.UpdateLists();
var
  i: Integer;
begin
  // (+1) - hack, because 0 transforms to nil,
  // and algorhitm of ListModify exits

  FPhonesList.List.Items.Clear;
  for i := 0 to FAddress.PhoneNumbersCount - 1 do
    FPhonesList.List.AddItem(FAddress.PhoneNumbers[i], System.Object(i+1));

  FMailsList.List.Items.Clear;
  for i := 0 to FAddress.EmailAddressesCount - 1 do
    FMailsList.List.AddItem(FAddress.EmailAddresses[i], System.Object(i+1));

  FWebsList.List.Items.Clear;
  for i := 0 to FAddress.WebPagesCount - 1 do
    FWebsList.List.AddItem(FAddress.WebPages[i], System.Object(i+1));
end;

procedure TfmAddressEdit.SetAddress(const Value: TGEDCOMAddress);
begin
  FAddress := Value;

  edCountry.Text := FAddress.AddressCountry;
  edState.Text := FAddress.AddressState;
  edCity.Text := FAddress.AddressCity;
  edPostalCode.Text := FAddress.AddressPostalCode;
  edAddress.Text := FAddress.Address.Text.Trim();

  UpdateLists();
end;

procedure TfmAddressEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);

  function GetInput(aTitle: string; var aValue: string): Boolean;
  begin
    Result := InputBox.Query(aTitle, LSList[LSID_Value], aValue) and (aValue.Trim() <> '');
  end;

var
  val: string;
  Index: Integer;
  ptr: IntPtr;
begin
  if (Action in [raEdit, raDelete]) then begin
    ptr := IntPtr(ItemData);
    Index := Integer(ptr) - 1;
  end;

  if (Sender = FPhonesList) then begin
    case Action of
      raAdd: begin
        val := '';
        if GetInput(LSList[LSID_Telephone], val)
        then FAddress.PhoneNumbers[FAddress.PhoneNumbersCount] := val;
      end;
      raEdit: begin
        if (Index < 0) then Exit;
        val := FAddress.PhoneNumbers[Index];
        if GetInput(LSList[LSID_Telephone], val)
        then FAddress.PhoneNumbers[Index] := val;
      end;
      raDelete: begin
        if (Index < 0) then Exit;
        FAddress.DeletePhoneNumber(Index);
      end;
    end;
  end
  else
  if (Sender = FMailsList) then begin
    case Action of
      raAdd: begin
        val := '';
        if GetInput(LSList[LSID_Mail], val)
        then FAddress.EmailAddresses[FAddress.EmailAddressesCount] := val;
      end;
      raEdit: begin
        if (Index < 0) then Exit;
        val := FAddress.EmailAddresses[Index];
        if GetInput(LSList[LSID_Mail], val)
        then FAddress.EmailAddresses[Index] := val;
      end;
      raDelete: begin
        if (Index < 0) then Exit;
        FAddress.DeleteEmail(Index);
      end;
    end;
  end
  else
  if (Sender = FWebsList) then begin
    case Action of
      raAdd: begin
        val := '';
        if GetInput(LSList[LSID_WebSite], val)
        then FAddress.WebPages[FAddress.WebPagesCount] := val;
      end;
      raEdit: begin
        if (Index < 0) then Exit;
        val := FAddress.WebPages[Index];
        if GetInput(LSList[LSID_WebSite], val)
        then FAddress.WebPages[Index] := val;
      end;
      raDelete: begin
        if (Index < 0) then Exit;
        FAddress.DeleteWebPage(Index);
      end;
    end;
  end;

  UpdateLists();
end;

end.
