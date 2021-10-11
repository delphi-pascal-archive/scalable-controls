unit Listing_3;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TKeyValues = array of TVarRec;
  TKey = class
  private
    fKeyValues: TKeyValues;
  public
    constructor Create(AKey: array of const); virtual;
    property KeyValues: TKeyValues read fKeyValues;
  end;

  TPersistentObject = class
  protected
    function GetKey: TKey; virtual; abstract;
  public
    property Key: TKey read GetKey;
  end;

  TPersistenceSubsystem = class
  public
    procedure SaveObject(const Obj: TPersistentObject); virtual; abstract;
    function RetrieveObject(Key: TKey): TPersistentObject; virtual; abstract;
  end;

  IPersistenceSubsystem = interface
  ['{EDC51BC0-C0F5-11DB-AFCD-525405E47322}']
    function RefCount: Integer;
    procedure SaveObject(const Obj: TPersistentObject);
    function RetrieveObject(Key: TKey): TPersistentObject;
  end;

  {declare support for IInterface for consistency with TInterfacedObject}
  TInterfacedPersistenceSubsystem = class(TPersistenceSubsystem,  {ClassA}
    IInterface, IPersistenceSubsystem)
  private
    fRefCount: Integer;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function RefCount: Integer;
    procedure SaveObject(const Obj: TPersistentObject); override;
    function RetrieveObject(Key: TKey): TPersistentObject; override;
  end;

  TClassB = class
  private
    fPersistenceSubsystem: IPersistenceSubsystem;
  public
    {const parameters avoid reference count thrashing}
    constructor Create(const APersistenceSubsystem: IPersistenceSubsystem);
  end;

  TClassC = class
  private
    fPersistenceSubsystem: IPersistenceSubsystem;
  public
    constructor Create(const APersistenceSubsystem: IPersistenceSubsystem);
  end;

  TClassO = class
  private
    fSubsystemB: TClassB;
    fSubsystemC: TClassC;
  public
    constructor Create(ASubsystem: TInterfacedPersistenceSubsystem);
    destructor Destroy; override;
  end;


implementation

{ TInterfacedPersistenceSubsystem }

{ implementing IUnknown - just paste in TInterfaced object methods if prefered
  its overriden methods are entirely optional I think }

function TInterfacedPersistenceSubsystem._AddRef: Integer;
asm
  mov  eax, Self
  lock add  [eax].fRefCount, 1
  mov  eax, [eax].fRefCount
end;

function TInterfacedPersistenceSubsystem._Release: Integer;
asm
  mov  eax, Self
  lock sub  [eax].fRefCount, 1
  jnz @ends
  mov  dl, 1
  mov  edx, [eax]
  call DWORD PTR [edx + VMTOFFSET TObject.Destroy]
  xor  eax, eax
  jmp  @exit
@ends: 
  mov  eax, Self
  mov  eax, [eax].fRefCount
@exit:
end;

function TInterfacedPersistenceSubsystem.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedPersistenceSubsystem.RetrieveObject(
  Key: TKey): TPersistentObject;
begin

end;

procedure TInterfacedPersistenceSubsystem.SaveObject(
  const Obj: TPersistentObject);
begin

end;

function TInterfacedPersistenceSubsystem.RefCount: Integer;
begin
  Result := fRefCount;
end;

{ TKey }

constructor TKey.Create(AKey: array of const);
var
  Ix: Integer;
begin
  SetLength(fKeyValues, Succ(High(AKey)));
  for Ix := 0 to High(AKey) do
    fKeyValues[Ix] := AKey[Ix];
end;

{ TClassB }

constructor TClassB.Create(const APersistenceSubsystem: IPersistenceSubsystem);
begin
  fPersistenceSubsystem := APersistenceSubsystem;
end;

{ TClassC }

constructor TClassC.Create(const APersistenceSubsystem: IPersistenceSubsystem);
begin
  fPersistenceSubsystem := APersistenceSubsystem;
end;

{ TClassO }

constructor TClassO.Create(ASubsystem: TInterfacedPersistenceSubsystem);
var
  SharedSub: IPersistenceSubsystem;
begin
  if ASubsystem.QueryInterface(IPersistenceSubsystem, SharedSub)= S_OK then begin
    fSubsystemB := TClassB.Create(SharedSub);
    fSubsystemC := TClassC.Create(SharedSub);
  end else
    raise Exception.Create('Only genuine Interfaces allowed here');
end;

destructor TClassO.Destroy;
begin
  fSubsystemB.Free;
  fSubsystemC.Free;
end;

end.
