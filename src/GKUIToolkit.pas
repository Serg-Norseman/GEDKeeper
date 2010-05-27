unit GKUIToolkit;

interface

uses
  Classes, Contnrs, GedCom551;

type
  TGKStack = class(TStack)
  protected
    procedure Clear; 
  end;

  TBackManager = class(TObject)
  private
    FNavBusy: Boolean;
    FStackBackward, FStackForward: TGKStack;
    FCurrent: TObject;
    procedure SetCurrent(const Value: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Back(): TObject;
    function Next(): TObject;
    procedure Clear();

    procedure BeginNav();
    procedure EndNav();

    function CanBackward(): Boolean;
    function CanForward(): Boolean;

    property Busy: Boolean read FNavBusy;
    property Current: TObject read FCurrent write SetCurrent;
  end;

type
  TUndoManager = class;

  TCustomCommand = class
  private
  protected
    FManager: TUndoManager;
  public
    constructor Create(aManager: TUndoManager); virtual;

    function Redo(): Boolean; virtual; abstract;
    procedure Undo(); virtual; abstract;
  end;

  TTransactionEventArg = (taCommit, taCommitUndo, taCommitRedo, taRollback);

  TUndoManType = (autoCommit, manualCommit);

  TTransactionEvent = procedure(Sender: TObject; Arg: TTransactionEventArg) of object;

  TUndoManager = class(TObject)
  private
    FDepth: Integer;
    FOnTransaction: TTransactionEvent;
    FStackUndo: TGKStack;
    FStackRedo: TGKStack;
    FTree: TGEDCOMTree;
    FType: TUndoManType;
  protected
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure Transaction(Arg: TTransactionEventArg);
  public
    constructor Create(aTree: TGEDCOMTree; aType: TUndoManType);
    destructor Destroy; override;

    function CmdDo(cmd: TCustomCommand): Boolean;
    procedure CmdUndo();
    procedure CmdRedo();

    function CanUndo(): Boolean;
    function CanRedo(): Boolean;

    procedure Commit();
    procedure Rollback();

    procedure Clear();

    property Depth: Integer read FDepth write FDepth;
    property OnTransaction: TTransactionEvent read FOnTransaction write FOnTransaction;
    property Tree: TGEDCOMTree read FTree;
  end;

implementation

uses
  Forms;

{ TGKStack }

procedure TGKStack.Clear;
begin
  List.Clear;
end;

{ TBackManager }

constructor TBackManager.Create;
begin
  FStackBackward := TGKStack.Create;
  FStackForward := TGKStack.Create;
  FCurrent := nil;
end;

destructor TBackManager.Destroy;
begin
  FStackBackward.Free;
  FStackForward.Free;

  inherited Destroy;
end;

function TBackManager.Back(): TObject;
begin
  if Assigned(FCurrent) then FStackForward.Push(FCurrent);
  FCurrent := FStackBackward.Pop();
  Result := FCurrent;
end;

function TBackManager.Next(): TObject;
begin
  if Assigned(FCurrent) then FStackBackward.Push(FCurrent);
  FCurrent := FStackForward.Pop();
  Result := FCurrent;
end;

procedure TBackManager.Clear();
begin
  FStackBackward.Clear();
  FStackForward.Clear();
  FCurrent := nil;
end;

function TBackManager.CanBackward(): Boolean;
begin
  Result := (FStackBackward.Count > 0);
end;

function TBackManager.CanForward(): Boolean;
begin
  Result := (FStackForward.Count > 0);
end;

procedure TBackManager.SetCurrent(const Value: TObject);
begin
  if Assigned(FCurrent) then FStackBackward.Push(FCurrent);
  FCurrent := Value;
  FStackForward.Clear();
end;

procedure TBackManager.BeginNav();
begin
  FNavBusy := True;
end;

procedure TBackManager.EndNav();
begin
  FNavBusy := False;
end;

{ TCustomCommand }

constructor TCustomCommand.Create(aManager: TUndoManager);
begin
  FManager := aManager;
end;

{ TUndoManager }

constructor TUndoManager.Create(aTree: TGEDCOMTree; aType: TUndoManType);
begin
  FDepth := 1000;
  FTree := aTree;

  FType := aType;
  if (FType = autoCommit) then Application.OnIdle := OnIdle;

  FStackUndo := TGKStack.Create;
  FStackRedo := TGKStack.Create;
end;

destructor TUndoManager.Destroy;
begin
  FStackUndo.Destroy;
  FStackRedo.Destroy;

  inherited Destroy;
end;

procedure TUndoManager.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Commit();
end;

function TUndoManager.CmdDo(cmd: TCustomCommand): Boolean;
begin
  if not(cmd.Redo()) then begin
    Rollback();
    Result := False;
    Exit;
  end;
  FStackUndo.Push(cmd);
  FStackRedo.Clear();
  Result := True;
end;

procedure TUndoManager.CmdRedo();
var
  cmd: TCustomCommand;
begin
  if (FStackRedo.Count = 0) then Exit;

  //Если транзакция не завершена, завершаем её
  if (FStackUndo.Peek() <> nil) then FStackUndo.Push(nil);

  while (FStackRedo.Peek() <> nil) do begin
    cmd := TCustomCommand(FStackRedo.Pop());
    FStackUndo.Push(cmd);
    if not(cmd.Redo()) then begin
      Rollback();
      Exit;
    end;
  end;
  FStackRedo.Pop();
  FStackUndo.Push(nil);
  Transaction(taCommitRedo);
end;

procedure TUndoManager.CmdUndo();
var
  cmd: TCustomCommand;
begin
  if (FStackUndo.Count < 2) then Exit;

  // Если транзакция завершена, снимаем контр. точку
  if (FStackUndo.Peek() = nil) then FStackUndo.Pop();

  FStackRedo.Push(nil);
  while (FStackUndo.Peek() <> nil) do begin
    cmd := TCustomCommand(FStackUndo.Pop());
    FStackRedo.Push(cmd);
    cmd.Undo();
  end;
  Transaction(taCommitUndo);
end;

function TUndoManager.CanRedo(): Boolean;
begin
  Result := (FStackRedo.Count - 1 > 0);
end;

function TUndoManager.CanUndo(): Boolean;
begin
  Result := (FStackUndo.Count - 1 > 0);
end;

procedure TUndoManager.Commit();
var
  cmd: TCustomCommand;
begin
  cmd := TCustomCommand(FStackUndo.Peek());
  if (cmd <> nil) then begin
    FStackUndo.Push(nil);
    Transaction(taCommit);
  end;
end;

procedure TUndoManager.Rollback();
var
  cmd: TCustomCommand;
begin
  while (FStackUndo.Peek() <> nil) do begin
    cmd := TCustomCommand(FStackUndo.Pop());
    cmd.Undo();
  end;
  Transaction(taRollback);
end;

procedure TUndoManager.Transaction(Arg: TTransactionEventArg);
begin
  if Assigned(FOnTransaction) then FOnTransaction(Self, Arg);
end;

procedure TUndoManager.Clear();
begin
  FStackUndo.Clear();
  FStackUndo.Push(nil); // Initial Check Point
  FStackRedo.Clear();
end;

end.
