unit GKCommands; {trans:fin}

interface

uses
  GedCom551, GKCommon;

type
  {
  TCmd = class(TCustomCommand)
  private
  public
    constructor Create(aManager: TUndoManager; ); reintroduce;

    function Redo(): Boolean; override;
    procedure Undo(); override;
  end;
  }

  TCmdPersonChangeSex = class(TCustomCommand)
  private
    FPersonXRef: string;
    FOldSex, FNewSex: TGEDCOMObject.TGEDCOMSex;
  public
    constructor Create(aManager: TUndoManager; aPerson: TGEDCOMIndividualRecord; NewSex: TGEDCOMObject.TGEDCOMSex); reintroduce;

    function Redo(): Boolean; override;
    procedure Undo(); override;
  end;

  TCmdPersonChangePatriarch = class(TCustomCommand)
  private
    FPersonXRef: string;
    FOldValue, FNewValue: Boolean;
  public
    constructor Create(aManager: TUndoManager; aPerson: TGEDCOMIndividualRecord; NewValue: Boolean); reintroduce;

    function Redo(): Boolean; override;
    procedure Undo(); override;
  end;

implementation

{ TCmdPersonChangeSex }

constructor TCmdPersonChangeSex.Create(aManager: TUndoManager;
  aPerson: TGEDCOMIndividualRecord; NewSex: TGEDCOMObject.TGEDCOMSex);
begin
  inherited Create(aManager);
  FPersonXRef := aPerson.XRef;
  FOldSex := aPerson.Sex;
  FNewSex := NewSex;
end;

function TCmdPersonChangeSex.Redo(): Boolean;
var
  i_rec: TGEDCOMIndividualRecord;
begin
  Result := True;

  i_rec := FManager.Tree.XRefIndex_Find(FPersonXRef) as TGEDCOMIndividualRecord;
  if (i_rec = nil)
  then Result := False
  else i_rec.Sex := FNewSex;
end;

procedure TCmdPersonChangeSex.Undo();
var
  i_rec: TGEDCOMIndividualRecord;
begin
  i_rec := FManager.Tree.XRefIndex_Find(FPersonXRef) as TGEDCOMIndividualRecord;
  if (i_rec <> nil) then i_rec.Sex := FOldSex;
end;

{ TCmdPersonChangePatriarch }

constructor TCmdPersonChangePatriarch.Create(aManager: TUndoManager;
  aPerson: TGEDCOMIndividualRecord; NewValue: Boolean);
begin
  inherited Create(aManager);
  FPersonXRef := aPerson.XRef;
  FOldValue := aPerson.Patriarch;
  FNewValue := NewValue;
end;

function TCmdPersonChangePatriarch.Redo(): Boolean;
var
  i_rec: TGEDCOMIndividualRecord;
begin
  Result := True;

  i_rec := FManager.Tree.XRefIndex_Find(FPersonXRef) as TGEDCOMIndividualRecord;
  if (i_rec = nil)
  then Result := False
  else i_rec.Patriarch := FNewValue;
end;

procedure TCmdPersonChangePatriarch.Undo();
var
  i_rec: TGEDCOMIndividualRecord;
begin
  i_rec := FManager.Tree.XRefIndex_Find(FPersonXRef) as TGEDCOMIndividualRecord;
  if (i_rec <> nil) then i_rec.Patriarch := FOldValue;
end;

end.
