unit GKAddressEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GedCom551, StdCtrls, Buttons, ComCtrls, ExtCtrls, GKBase, GKCommon,
  GKSheetList;

type
  TfmAddressEdit = class(TForm)
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
  end;

var
  fmAddressEdit: TfmAddressEdit;

implementation

{$R *.dfm}

type
  TAddressTab = (atPhones, atMails, atWebs);

const
  Titles: array [TAddressTab] of string = ('Телефон', 'Эл. почта', 'Веб-страница');

{ TfmAddressEdit }

procedure TfmAddressEdit.FormCreate(Sender: TObject);
begin
  FPhonesList := TSheetList.Create(SheetPhones);
  FPhonesList.OnModify := ListModify;
  AddListColumn(FPhonesList.List, Titles[atPhones], 350, False);

  FMailsList := TSheetList.Create(SheetEmails);
  FMailsList.OnModify := ListModify;
  AddListColumn(FMailsList.List, Titles[atMails], 350, False);

  FWebsList := TSheetList.Create(SheetWebPages);
  FWebsList.OnModify := ListModify;
  AddListColumn(FWebsList.List, Titles[atWebs], 350, False);
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
var
  sl: TStringList;
begin
  FAddress.AddressCountry := edCountry.Text;
  FAddress.AddressState := edState.Text;
  FAddress.AddressCity := edCity.Text;
  FAddress.AddressPostalCode := edPostalCode.Text;

  sl := TStringList.Create;
  try
    sl.Text := edAddress.Text;
    FAddress.Address := sl;
  finally
    sl.Free;
  end;
end;

procedure TfmAddressEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
var
  val: string;
  Index: Integer;
begin
  Index := Integer(ItemData) - 1;

  if (Sender = FPhonesList) then begin
    case Action of
      raAdd: begin
        val := '';
        if InputQuery(Titles[atPhones], 'Значение', val)
        then FAddress.PhoneNumbers[FAddress.PhoneNumbersCount] := val;
      end;
      raEdit: begin
        if (Index < 0) then Exit;
        val := FAddress.PhoneNumbers[Index];
        if InputQuery(Titles[atPhones], 'Значение', val)
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
        if InputQuery(Titles[atMails], 'Значение', val)
        then FAddress.EmailAddresses[FAddress.EmailAddressesCount] := val;
      end;
      raEdit: begin
        val := FAddress.EmailAddresses[Index];
        if (Index < 0) then Exit;
        if InputQuery(Titles[atMails], 'Значение', val)
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
        if InputQuery(Titles[atWebs], 'Значение', val)
        then FAddress.WebPages[FAddress.WebPagesCount] := val;
      end;
      raEdit: begin
        val := FAddress.WebPages[Index];
        if (Index < 0) then Exit;
        if InputQuery(Titles[atWebs], 'Значение', val)
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
