unit GKAddressEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKEngine, GKLists, GKLangs;

type
  TfmAddressEdit = class(TForm, ILocalization)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    PageAddrData: TPageControl;
    SheetPhones: TTabSheet;
    SheetEmails: TTabSheet;
    SheetCommon: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edCountry: TEdit;
    edState: TEdit;
    edCity: TEdit;
    edPostalCode: TEdit;
    edAddress: TEdit;
    SheetWebPages: TTabSheet;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAddress: TGEDCOMAddress;

    FPhonesList: TSheetList;
    FMailsList: TSheetList;
    FWebsList: TSheetList;

    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure SetAddress(const Value: TGEDCOMAddress);
    procedure UpdateLists();
  public
    property Address: TGEDCOMAddress read FAddress write SetAddress;

    procedure SetLang();
  end;

var
  fmAddressEdit: TfmAddressEdit;

implementation

{$R *.dfm}

procedure TfmAddressEdit.FormCreate(Sender: TObject);
begin
  FPhonesList := TSheetList.Create(SheetPhones);
  FPhonesList.OnModify := ListModify;
  AddListColumn(FPhonesList.List, LSList[LSID_Telephone], 350, False);

  FMailsList := TSheetList.Create(SheetEmails);
  FMailsList.OnModify := ListModify;
  AddListColumn(FMailsList.List, LSList[LSID_Mail], 350, False);

  FWebsList := TSheetList.Create(SheetWebPages);
  FWebsList.OnModify := ListModify;
  AddListColumn(FWebsList.List, LSList[LSID_WebSite], 350, False);

  SetLang();
end;

procedure TfmAddressEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_Address];
  SheetCommon.Caption := LSList[LSID_Address];

  Label1.Caption := LSList[LSID_AdCountry];
  Label2.Caption := LSList[LSID_AdState];
  Label3.Caption := LSList[LSID_AdCity];
  Label4.Caption := LSList[LSID_AdPostalCode];
  Label5.Caption := LSList[LSID_Address];

  SheetPhones.Caption := LSList[LSID_Telephones];
  SheetEmails.Caption := LSList[LSID_EMails];
  SheetWebPages.Caption := LSList[LSID_WebSites];
end;

procedure TfmAddressEdit.UpdateLists();
var
  i: Integer;
begin
  // (+1) - hack, because 0 transforms to nil,
  // and algorhitm of ListModify exits

  FPhonesList.List.Clear;
  for i := 0 to FAddress.PhoneNumbersCount - 1 do
    FPhonesList.List.AddItem(FAddress.PhoneNumbers[i], TObject(i+1));

  FMailsList.List.Clear;
  for i := 0 to FAddress.EmailAddressesCount - 1 do
    FMailsList.List.AddItem(FAddress.EmailAddresses[i], TObject(i+1));

  FWebsList.List.Clear;
  for i := 0 to FAddress.WebPagesCount - 1 do
    FWebsList.List.AddItem(FAddress.WebPages[i], TObject(i+1));
end;

procedure TfmAddressEdit.SetAddress(const Value: TGEDCOMAddress);
begin
  FAddress := Value;

  edCountry.Text := FAddress.AddressCountry;
  edState.Text := FAddress.AddressState;
  edCity.Text := FAddress.AddressCity;
  edPostalCode.Text := FAddress.AddressPostalCode;
  edAddress.Text := Trim(FAddress.Address.Text);

  UpdateLists();
end;

procedure TfmAddressEdit.btnAcceptClick(Sender: TObject);
begin
  FAddress.AddressCountry := edCountry.Text;
  FAddress.AddressState := edState.Text;
  FAddress.AddressCity := edCity.Text;
  FAddress.AddressPostalCode := edPostalCode.Text;
  SetAddressValue(FAddress, edAddress.Text);
end;

procedure TfmAddressEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);

  function GetInput(aTitle: string; var aValue: string): Boolean;
  begin
    Result := InputQuery(aTitle, LSList[LSID_Value], aValue) and (Trim(aValue) <> '');
  end;

var
  val: string;
  Index: Integer;
begin
  Index := Integer(ItemData) - 1;

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
