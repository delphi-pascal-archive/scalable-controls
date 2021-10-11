unit Listing_3Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

const
  IOK = S_OK;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  Listing_3;

{$R *.dfm}

var
  A, B: IPersistenceSubsystem;
  R: Integer;
  P: Pointer;

procedure TForm1.Button1Click(Sender: TObject);
var
  Obj: TInterfacedPersistenceSubsystem;
begin
  Obj := TInterfacedPersistenceSubsystem.Create;
  if Obj.QueryInterface(IInterface, A)= S_OK then
    R := Obj.RefCount;
  A := Obj;
  R := Obj.RefCount;
  B := Obj;
  R := Obj.RefCount;
  A := nil;
  R := Obj.RefCount;
  B := nil;
end;

var
  AComplexClass: TClassO;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AComplexClass.Free;
  AComplexClass := TClassO.Create(TInterfacedPersistenceSubsystem.Create);
  AComplexClass.Free;
  AComplexClass := nil;
end;

end.
