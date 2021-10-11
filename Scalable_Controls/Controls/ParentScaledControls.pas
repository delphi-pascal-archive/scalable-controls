unit ParentScaledControls;

interface
uses
  Classes, ScaleableButton, ScaleableMemo;

type
  TParentScaledButton = class(TModTextScalingButton)
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
  end;
  
  TParentScaledMemo = class(_ScaleableMemo)
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
  end;


implementation
uses
  StdCtrls;

{ TParentScaledButton }

type
  {ScaleControl is protected so have to use friend class rule}
  friendButton = class(TModTextScalingButton);
  friendButtonClass = class of friendButton;

constructor TParentScaledButton.Create(AOwner: TComponent);
begin
  inherited;
  Tag := Cardinal(@friendButtonClass.ScaleControl);
end;

type
  TStaticSetBounds = procedure(Control: TObject;
                       ALeft, ATop, AWidth, AHeight: Integer);
{SetBounds is public}
procedure TParentScaledButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  TStaticSetBounds(@TButton.SetBounds)(Self, ALeft, ATop, AWidth, AHeight);
end;

{to call a virtual constructor pass 0 to signal no new object expected}

type
  TStaticConstructor = procedure(Self: TObject; Signal: Cardinal;
     AnyOtherParametersEg: TComponent);

procedure CallTComponentConstructor(AComponent, AnOwner: TComponent);
begin
  TStaticConstructor(@TComponent.Create)(AComponent, 0, AnOwner);
end;

{ TParentScaledMemo }

type
  friendMemo = class(_ScaleableMemo);
  friendMemoClass = class of friendMemo;

constructor TParentScaledMemo.Create(AOwner: TComponent);
begin
  inherited;
  Tag := Cardinal(@friendMemoClass.ScaleControl);
end;

procedure TParentScaledMemo.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  TStaticSetBounds(@TCustomMemo.SetBounds)(Self, ALeft, ATop, AWidth, AHeight);
end;

end.
 