unit Listing_1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TScalingForm = class(TForm)
  private
    fDesignWidth, fDesignHeight: Integer;
    fDesignSizes: array of TRect;
    fInitialized: Boolean;
    fOnResize: TNotifyEvent;
  protected
    procedure FormResize(Sender: TObject);
  public
    property Initialized: Boolean read fInitialized;
    constructor Create(AOwner: TComponent); override;
  published
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
  end;

  TForm2 = class(TScalingForm)
    Memo1: TMemo;
    Button1: TButton;
    Panel1: TPanel;
    Memo2: TMemo;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

{ TScalingForm }

constructor TScalingForm.Create(AOwner: TComponent);
var
  ChildIx: Integer;
begin
  inherited;
  fDesignWidth := Width;
  fDesignHeight := Height;
  SetLength(fDesignSizes, ControlCount);
  for ChildIx := 0 to Pred(ControlCount) do
    fDesignSizes[ChildIx] := Controls[ChildIx].BoundsRect;
  inherited OnResize := FormResize;
  fInitialized := True;
end;

procedure ScaleRect(const ARect: TRect; const XScale, YScale: Double;
  out ScaledRect: TRect);
begin
  with ScaledRect do begin
    Left := Round(ARect.Left*XScale);
    Top := Round(ARect.Top*YScale);
    Right := Round(ARect.Right*XScale);
    Bottom := Round(ARect.Bottom*YScale);
  end;
end;

procedure TScalingForm.FormResize(Sender: TObject);
var
  ChildIx: Integer;
  XScale, YScale: Double;
  ScaledRect: TRect;
begin
  if Initialized then begin
    XScale := Width / fDesignWidth;
    YScale := Height / fDesignHeight;
    for ChildIx := 0 to Pred(ControlCount) do
      with Controls[ChildIx] do
        if Align = alNone then begin
          ScaleRect(fDesignSizes[ChildIx], XScale, YScale, ScaledRect);
          BoundsRect := ScaledRect;
        end;
  end;
  if Assigned(fOnResize) then fOnResize(Self);
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  Memo1.Lines.Add(Format('[%d : %d]',[Width, Height]));
end;

end.
 