unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls,
  ParentScaledControls;

type

  TScalingForm = class(TForm)
  protected
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop, NewWidth,
      NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
      override;
  end;

  TForm1 = class(TScalingForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    SButton1: TParentScaledButton;
    SMemo1:  TParentScaledMemo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TScalingForm }

type
  TStaticScaleControl = procedure(Control: TObject; out NewLeft, NewTop,
    NewWidth, NewHeight, NewParentClientWidth, NewParentClientHeight: Integer);
var
//  S: string;
  T: Integer;

procedure TScalingForm.CustomAlignPosition(Control: TControl; var NewLeft,
  NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
var
  NewParentClientWidth, NewParentClientHeight: Integer;
  P: Pointer;
begin
//  S := Control.ClassName;
  P := Pointer(Control.Tag);
  if Control.Tag >= $401000 then
    TStaticScaleControl(P)(Control, NewLeft, NewTop, NewWidth, NewHeight,
      NewParentClientWidth, NewParentClientHeight);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SButton1 := TParentScaledButton.Create(Self);
  with SButton1 do begin
    Parent := Self;
    SetDesignBounds(83,138,75,25);
    Caption := 'SButton1';
    Align := alCustom;
  end;
  SMemo1 := TParentScaledMemo.Create(Self);
  with SMemo1 do begin
    Parent := Self;
    SetDesignBounds(32,32,185,90);
    Lines.Add('SMemo1');
    MaxFontSize := 255;
    Align := alCustom;
  end;
end;

end.
 