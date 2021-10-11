unit ScaleableUpDown;

////////////////////////////////////////////////////////////////////////////////
// Scalable  Controls - Demo                                                  //
// for Delphi Magazine                                                        //
//                                                                            //
// Copyright (c) Martin Humby 2007  Freeware                                  //
////////////////////////////////////////////////////////////////////////////////

interface
uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, Forms,
  ScaleableControl, ComCtrls;

type
  _ScaleableUpDown = class(TUpDown)
   private
    fNoUpScaling: Boolean;    {do not scale to a size larger than Designed - use anchors}
    fDoScaling: Boolean;
    fDesignParentClientWidth: Integer;
    fDesignParentClientHeight: Integer;
    fParentClientHeight: Integer;
    fParentClientWidth: Integer;
    fDesignWidth: Integer;
    fDesignHeight: Integer;
    fDesignXCentre: Integer;
    fDesignYCentre: Integer;
    fDesignLeft: Integer;
    fDesignTop: Integer;
    fStaticX,                  {left, right borders etc. not scaled}
    fStaticY: Integer;
    fMaxFontSize: Integer;
    fMinFontSize: Integer;
    fScalings: TScalings;
    fDesignAnchors: TAnchors; {anchor control in relation to Design Size - not size when set as TControl}
    fParentCanScroll: Boolean;
    fXScroll,
    fYScroll: Integer;
    fDesignFont: TActiveFont;
    fOnScaleChange: TScaleEvent;
    fOnSizeChange: TNotifyEvent;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetDoScaling(const Value: Boolean);
    procedure SetNoUpScaling(const Value: Boolean);
    procedure UpdateDesignCLs;
    procedure UpdateDesignDims(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetScalings(const Value: TScalings);
    procedure SetDesignHeight(const Value: Integer);
    procedure SetDesignLeft(const Value: Integer);
    procedure SetDesignTop(const Value: Integer);
    procedure SetDesignWidth(const Value: Integer);
    function GetAlign: TAlign;
    function GetAnchors: TAnchors;
    procedure SetAlign(const Value: TAlign);
    procedure SetAnchors(const Value: TAnchors);
    procedure DesignFontChange(Sender: TObject);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetMaxFontSize(const Value: Integer);
    procedure SetMinFontSize(const Value: Integer);
    function GetDesignFontSize: Integer;
    function GetScaledFontSize: Integer;
  protected  {class implementation}
    procedure SetParent(AParent: TWinControl); override;
    procedure Rescale; virtual;
    procedure SetStaticDims(out StaticX, StaticY: Integer); virtual;
    property StaticX: Integer read fStaticX;
    property StaticY: Integer read fStaticY;
  protected  { abstract model: scaling }
    procedure ScaleControl(out NewLeft, NewTop, NewWidth, NewHeight,
      NewParentClientWidth, NewParentClientHeight: Integer); virtual;
    procedure ScaleText(var FontSize, NewWidth, NewHeight: Integer); virtual;
    procedure ScaleX(ModParentClientWidth: Integer;
      out NewLeft, NewWidth: Integer); virtual;
    procedure ScaleY(ModParentClientHeight: Integer;
      out NewTop, NewHeight: Integer); virtual;
    procedure ProcessXAnchors(ModParentClientWidth: Integer;
      out NewLeft, NewWidth: Integer); virtual;
    procedure ProcessYAnchors(ModParentClientHeight: Integer;
      out NewTop, NewHeight: Integer); virtual;
    function CanScaleX: Boolean; virtual;
    function CanScaleY: Boolean; virtual;
  public     { emergent properties }
    property DesignParentClientWidth: Integer read fDesignParentClientWidth;
    property DesignParentClientHeight: Integer read fDesignParentClientHeight;
    property ParentClientWidth: Integer read fParentClientWidth;
    property ParentClientHeight: Integer read fParentClientHeight;
    property ParentCanScroll: Boolean read fParentCanScroll;
    property DesignXCL: Integer read fDesignXCentre;
    property DesignYCL: Integer read fDesignYCentre;
    property DesignFontSize: Integer read GetDesignFontSize;
    property ScaledFontSize: Integer read GetScaledFontSize;
    property XScroll: Integer read fXScroll;
    property YScroll: Integer read fYScroll;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetScaledBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetDesignBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property DesignLeft: Integer read FDesignLeft write SetDesignLeft;
    property DesignTop: Integer read FDesignTop write SetDesignTop;
    property DesignWidth: Integer read FDesignWidth write SetDesignWidth;
    property DesignHeight: Integer read FDesignHeight write SetDesignHeight;
  published
   property Scalings: TScalings read fScalings write SetScalings
      default [scXPos..scHeight, scText];
    property NoUpScaling: Boolean read fNoUpScaling write SetNoUpScaling;
    property DoScaling: Boolean read fDoScaling write SetDoScaling
      default True; {anchors used if False}
    property MaxFontSize: Integer read fMaxFontSize write SetMaxFontSize
      default 10; {Font scaling is indepedant of DoScaling, NoUpScaling - set Min=Max to turn off}
    property MinFontSize: Integer read fMinFontSize write SetMinFontSize
      default 2; {smallest DBGrids.TColumn.SetWidth can cope with}

    property Align: TAlign read GetAlign write SetAlign;
    property Anchors: TAnchors read GetAnchors write SetAnchors;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Font: TFont read GetFont write SetFont;
    property OnScaleChange: TScaleEvent read fOnScaleChange
      write fOnScaleChange;
    property OnSizeChange: TNotifyEvent read fOnSizeChange write fOnSizeChange;
  end;

  TScaleableUpDown = class(_ScaleableUpDown)
  private
    fScaledWidth: Integer;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging);
      message WM_WINDOWPOSCHANGING;
    function GetOrientation: TUDOrientation;
    procedure SetOrientation(const Value: TUDOrientation);
  protected
    procedure ScaleControl(out NewLeft, NewTop, NewWidth, NewHeight,
      NewParentClientWidth, NewParentClientHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); reintroduce; virtual;
  published
    property Orientation: TUDOrientation read GetOrientation write SetOrientation;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Scaleable Controls', [TScaleableUpDown]);
end;

{ _ScaleableUpDown }

function _ScaleableUpDown.CanScaleX: Boolean;
begin
  Result := DoScaling and (not NoUpScaling or
               (inherited Parent.ClientWidth<=DesignParentClientWidth))
             and (Scalings*[scXPos,scWidth,scXCentred]<>[]);
end;

function _ScaleableUpDown.CanScaleY: Boolean;
begin
  Result := DoScaling and (not NoUpScaling or
               (inherited Parent.ClientHeight<=DesignParentClientHeight))
             and (Scalings*[scYPos,scHeight,scYCentred]<>[]);
end;

constructor _ScaleableUpDown.Create(AOwner: TComponent);
begin
  inherited;
  fDoScaling := True;
  fScalings := [scXPos..scHeight, scText];
  fDesignAnchors := [akLeft,akTop];
  fMaxFontSize := 10;
  fMinFontSize := 2;
  fDesignLeft := inherited Left; fDesignTop := inherited Top;
  fDesignWidth := inherited Width; fDesignHeight := inherited Height;
  UpdateDesignCLs;
  inherited Font.Name := 'Arial';
  fDesignFont := TActiveFont.Create(DesignFontChange);
  if AOwner is TWinControl then SetParent(TWinControl(AOwner));
end;

procedure _ScaleableUpDown.DesignFontChange(Sender: TObject);
begin
  inherited Font.Assign(fDesignFont);
end;

function _ScaleableUpDown.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

function _ScaleableUpDown.GetAnchors: TAnchors;
begin
  if csDesigning in ComponentState then
    Result := inherited Anchors
  else
    Result := fDesignAnchors;
end;

function _ScaleableUpDown.GetDesignFontSize: Integer;
begin
  Result := Font.Size;
end;

function _ScaleableUpDown.GetFont: TFont;
begin
  if csDesigning in ComponentState then
    Result := inherited Font
  else
    Result := fDesignFont;
end;

function _ScaleableUpDown.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function _ScaleableUpDown.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function _ScaleableUpDown.GetScaledFontSize: Integer;
begin
  Result := inherited Font.Size;
end;

function _ScaleableUpDown.GetTop: Integer;
begin
  Result := inherited Top;
end;

function _ScaleableUpDown.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure _ScaleableUpDown.ProcessXAnchors(ModParentClientWidth: Integer;
  out NewLeft, NewWidth: Integer);
var
  XAnchors: TAnchors;
begin
  XAnchors := Anchors*[akLeft,akRight];
  if (XAnchors<>[]) then begin
    if not(akRight in XAnchors) then begin
      NewLeft := DesignLeft;
      NewWidth := DesignWidth;
    end else if (akLeft in XAnchors) then begin
      NewLeft := DesignLeft;
      NewWidth := DesignWidth + ModParentClientWidth - DesignParentClientWidth;
    end else begin
      NewLeft := DesignLeft + ModParentClientWidth - DesignParentClientWidth;
      NewWidth := DesignWidth;
    end;
  end else begin
    NewLeft :=
      //MulDiv(DesignXCL, ModParentClientWidth, DesignParentClientWidth)-
        Round(DesignXCL*ModParentClientWidth/DesignParentClientWidth)-
          DesignWidth shr 1;
    NewWidth := DesignWidth;
  end;
end;

procedure _ScaleableUpDown.ProcessYAnchors(ModParentClientHeight: Integer;
  out NewTop, NewHeight: Integer);
var
  YAnchors: TAnchors;
begin
  YAnchors := Anchors*[akTop,akBottom];
  if (YAnchors<>[]) then begin
    if not(akBottom in YAnchors) then begin
      NewTop := DesignTop;
      NewHeight := DesignHeight;
    end else if (akTop in YAnchors) then begin
      NewTop := DesignTop;
      NewHeight := DesignHeight + ModParentClientHeight - DesignParentClientHeight;
    end else begin
      NewTop := DesignTop + ModParentClientHeight - DesignParentClientHeight;
      NewHeight := DesignHeight;
    end;
  end else begin
    NewTop :=
      //MulDiv(DesignYCL, ModParentClientHeight, DesignParentClientHeight)-
        Round(DesignYCL*ModParentClientHeight/DesignParentClientHeight)-
        DesignHeight shr 1;
    NewHeight := DesignHeight;
  end;
end;

procedure _ScaleableUpDown.Rescale;
var
  ALeft, ATop, AWidth, AHeight, Pcw, Pch: Integer;
begin
  if (Parent<>nil) then begin
    ScaleControl(ALeft, ATop, AWidth, AHeight, Pcw, Pch);
    if ParentCanScroll then
      with TScrollingWinControl(Parent) do begin
        Dec(ALeft, HorzScrollBar.Position);
        Dec(ATop, VertScrollBar.Position);
      end;
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    if Assigned(OnSizeChange) then
      OnSizeChange(Self);
  end;
end;

procedure _ScaleableUpDown.ScaleControl(out NewLeft, NewTop, NewWidth, NewHeight,
  NewParentClientWidth, NewParentClientHeight: Integer);
var
  ModParentClientWidth, ModParentClientHeight,
  FontSize, ModFontSize: Integer;
begin
  ModParentClientWidth := Parent.ClientWidth;
  ModParentClientHeight := Parent.ClientHeight;
  if CanScaleX then
    ScaleX(ModParentClientWidth, NewLeft, NewWidth)
  else
    ProcessXAnchors(ModParentClientWidth, NewLeft, NewWidth);
  if CanScaleY then
    ScaleY(ModParentClientHeight, NewTop, NewHeight)
  else
    ProcessYAnchors(ModParentClientHeight, NewTop, NewHeight);
  FontSize := Font.Size;
  if (scText in Scalings)then
    ScaleText(FontSize, NewWidth, NewHeight);
  try
    if Assigned(OnScaleChange) then begin
      ModFontSize := 0;
      OnScaleChange(Self, FontSize, NewLeft, NewTop, NewWidth,
        NewHeight, ModFontSize);
      if ModFontSize<>0 then
        FontSize := ModFontSize;
    end;
  finally
    NewParentClientWidth :=  ModParentClientWidth;
    NewParentClientHeight := ModParentClientHeight;
  end;
  if inherited Font.Size <> FontSize then
    inherited Font.Size := FontSize;
end;

procedure _ScaleableUpDown.ScaleText(var FontSize, NewWidth, NewHeight: Integer);
var
  NewFontSize: Integer;
  NegSize: Boolean;
begin
  Negsize := FontSize < 0;
  NewFontSize := MulDiv(Abs(DesignFontSize),
                    NewWidth-StaticX, DesignWidth-StaticX);
  if NewFontSize<Abs(MinFontSize) then
    NewFontSize := Abs(MinFontSize)
  else if NewFontSize>Abs(MaxFontSize) then
    NewFontSize := Abs(MaxFontSize);
  if NegSize then
    FontSize := -NewFontSize
  else
    FontSize := NewFontSize;
end;

procedure _ScaleableUpDown.ScaleX(ModParentClientWidth: Integer; out NewLeft,
  NewWidth: Integer);
var
  XCentred: Boolean;
  XPos, XDim: Integer;
begin
  XCentred := scXCentred in Scalings;
  if scWidth in Scalings then
    XDim := //MulDiv(DesignWidth,ModParentClientWidth,DesignParentClientWidth)
            Round(DesignWidth*ModParentClientWidth/DesignParentClientWidth)
  else
    XDim := DesignWidth;
  if scXPos in Scalings then
    if XCentred then
      XPos := //MulDiv(DesignXCL,ModParentClientWidth,DesignParentClientWidth)
              Round(DesignXCL*ModParentClientWidth/DesignParentClientWidth)
    else
      XPos := //MulDiv(DesignLeft,ModParentClientWidth,DesignParentClientWidth)
              Round(DesignLeft*ModParentClientWidth/DesignParentClientWidth)
  else if XCentred then
    XPos := DesignLeft + DesignWidth shr 1
  else begin
    NewLeft := DesignLeft;
    NewWidth := XDim;
    Exit;
  end;
  if XCentred then
    NewLeft := XPos - XDim div 2
  else
    NewLeft := XPos;
  NewWidth := XDim;
end;

procedure _ScaleableUpDown.ScaleY(ModParentClientHeight: Integer; out NewTop,
  NewHeight: Integer);
var
  YCentred: Boolean;
  YPos, YDim: Integer;
begin
  YCentred := scYCentred in Scalings;
  if scHeight in Scalings then
    YDim := //MulDiv(DesignHeight,ModParentClientHeight,DesignParentClientHeight)
            Round(DesignHeight*ModParentClientHeight/DesignParentClientHeight)
  else
    YDim := DesignHeight;
  if scYPos in Scalings then
    if YCentred then
      YPos := //MulDiv(DesignYCL,ModParentClientHeight,DesignParentClientHeight)
              Round(DesignYCL*ModParentClientHeight/DesignParentClientHeight)
    else
      YPos := //MulDiv(DesignTop,ModParentClientHeight,DesignParentClientHeight)
              Round(DesignTop*ModParentClientHeight/DesignParentClientHeight)
  else if YCentred then
    YPos := DesignTop + DesignHeight shr 1
  else begin
    NewTop := DesignTop;
    NewHeight := YDim;
    Exit;
  end;
  if YCentred then
    NewTop := YPos - YDim div 2
  else
    NewTop := YPos;
  NewHeight := YDim;
end;

procedure _ScaleableUpDown.SetAlign(const Value: TAlign);
begin
  if not(Value in [DEF_ALIGN, alCustom]) then
    SetDoScaling(False);
  inherited Align := Value;
end;

procedure _ScaleableUpDown.SetAnchors(const Value: TAnchors);
begin
  if csDesigning in ComponentState then
    inherited Anchors := Value
  else begin
    inherited Anchors := [];
    if (Value<>fDesignAnchors)then begin
      fDesignAnchors := Value;
      Rescale;
    end;
  end;
end;

procedure _ScaleableUpDown.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Scrolled, ParentChange: Boolean;
begin
  if csDesigning in ComponentState then begin
    inherited SetBounds(ALeft, ATop, AWidth,AHeight);
    Exit;
  end;
  if not Assigned(Parent) then begin
    fDesignLeft := ALeft; fDesignTop := ATop;
    fDesignWidth := AWidth; fDesignHeight := AHeight;
    UpdateDesignCLs;
    inherited;
    Exit;
  end;
  if ParentCanScroll then begin
    with TScrollingWinControl(Parent) do begin
      Scrolled := (fXScroll<>HorzScrollBar.Position) or
                       (fYScroll<>VertScrollBar.Position);
      fXScroll:=HorzScrollBar.Position;
      fYScroll:=VertScrollBar.Position;
    end;
  end else
    Scrolled := False;
  ParentChange := (ParentClientWidth<>inherited Parent.ClientWidth) or
                  (ParentClientHeight<>inherited Parent.ClientHeight);
  if ParentChange or Scrolled then begin
    ScaleControl(ALeft, ATop, AWidth, AHeight, fParentClientWidth,
                 fParentClientHeight);
    Dec(ALeft, fXScroll);
    Dec(ATop, fYScroll);
    inherited;
    if Assigned(OnSizeChange) then
      OnSizeChange(Self);
  end;
end;

procedure _ScaleableUpDown.SetDesignBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (ALeft=fDesignLeft)and(ATop=fDesignTop)and
     (AWidth=fDesignWidth)and(AHeight=fDesignHeight) then
    Exit;
  fDesignLeft := ALeft; fDesignTop := ATop;
  fDesignWidth := AWidth; fDesignHeight := AHeight;
  UpdateDesignCLs;
  if [csDesigning, csLoading{, csReading}]* ComponentState <> [] then begin
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    Exit;
  end;
  if Assigned(Parent) then
    Rescale
  else begin
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  end;
end;

procedure _ScaleableUpDown.SetDesignHeight(const Value: Integer);
var
  NewTop: Integer;
begin
  if Value = fDesignHeight then Exit;
  if scYCentred in Scalings then begin
    NewTop := fDesignTop - (Value-fDesignHeight)div 2;
    SetDesignBounds(fDesignLeft, NewTop, fDesignWidth, Value);
  end else begin
    SetDesignBounds(fDesignLeft, fDesignTop, fDesignWidth, Value);
  end;
end;

procedure _ScaleableUpDown.SetDesignLeft(const Value: Integer);
begin
  if Value = fDesignLeft then Exit;
  SetDesignBounds(Value, fDesignTop, fDesignWidth, fDesignHeight);
end;

procedure _ScaleableUpDown.SetDesignTop(const Value: Integer);
begin
  if Value = fDesignTop then Exit;
  SetDesignBounds(fDesignLeft, Value, fDesignWidth, fDesignHeight);
end;

procedure _ScaleableUpDown.SetDesignWidth(const Value: Integer);
var
  NewLeft: Integer;
begin
  if Value = fDesignWidth then Exit;
  if scXCentred in Scalings then begin
    NewLeft := fDesignLeft - (Value-fDesignWidth)div 2;
    SetDesignBounds(NewLeft, fDesignTop, Value, fDesignHeight);
  end else begin
    SetDesignBounds(fDesignLeft, fDesignTop, Value, fDesignHeight);
  end;
end;

procedure _ScaleableUpDown.SetDoScaling(const Value: Boolean);
begin
  if fDoScaling = Value then Exit;
  fDoScaling := Value;
  if csDesigning in ComponentState then Exit;
  if Value then begin
    if not(inherited Align in [alCustom]+[DEF_ALIGN]) then
      inherited Align := DEF_ALIGN;
  end;
  Rescale;
end;

procedure _ScaleableUpDown.SetFont(const Value: TFont);
begin
  fDesignFont.Assign(Value);
  if not (csDesigning in ComponentState) then
    Rescale;
end;

procedure _ScaleableUpDown.SetHeight(const Value: Integer);
begin
  SetScaledBounds(inherited Left, inherited Top, inherited Width,  Value);
end;

procedure _ScaleableUpDown.SetLeft(const Value: Integer);
begin
  SetScaledBounds(Value, inherited Top, inherited Width,  inherited Height);
end;

procedure _ScaleableUpDown.SetMaxFontSize(const Value: Integer);
begin
  if fMaxFontSize = Value then Exit;
  fMaxFontSize := Value;
  if not (csDesigning in ComponentState) then
    Rescale;
end;

procedure _ScaleableUpDown.SetMinFontSize(const Value: Integer);
begin
  if fMinFontSize = Value then Exit;
  fMinFontSize := Value;
  if not (csDesigning in ComponentState) then
    Rescale;
end;

procedure _ScaleableUpDown.SetNoUpScaling(const Value: Boolean);
begin
  if fNoUpscaling = Value then Exit;
  fNoUpScaling := Value;
  if (csDesigning in ComponentState) then Exit;
  Rescale;
end;

procedure _ScaleableUpDown.SetParent(AParent: TWinControl);
begin
  if (AParent<>nil) then begin
    fParentClientWidth := AParent.ClientWidth;
    fDesignParentClientWidth := fParentClientWidth;
    fParentClientHeight := AParent.ClientHeight;
    fDesignParentClientHeight := fParentClientHeight;
    fParentCanScroll := (AParent is TScrollingWinControl);
    if not(csDesigning in ComponentState) then
      inherited Anchors := [];
  end else begin
    fParentClientWidth := 0;
    fDesignParentClientWidth := 0;
    fParentClientHeight := 0;
    fDesignParentClientHeight := 0;
  end;
  inherited;
  if csDesigning in ComponentState then Exit;
  if Assigned(Parent) then SetStaticDims(fStaticX, fStaticY);
  Rescale;
end;

procedure _ScaleableUpDown.SetScaledBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (ALeft=inherited Left) and (ATop=inherited Top) and
       (AWidth=inherited Width) and (AHeight=inherited Height) then
      Exit;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if csDesigning in ComponentState then Exit;
  UpdateDesignDims(ALeft, ATop, AWidth, AHeight);
  if Assigned(OnSizeChange) then
    OnSizeChange(Self);
end;

procedure _ScaleableUpDown.SetScalings(const Value: TScalings);
begin
  if Value = fScalings then Exit;
  fScalings := Value;
  if csDesigning in ComponentState then Exit;
  Rescale;
end;

procedure _ScaleableUpDown.SetStaticDims(out StaticX, StaticY: Integer);
begin
  StaticX := 0;
  StaticY := 0;
  {borders etc. that cannot be scaled: override in descendants}
end;

procedure _ScaleableUpDown.SetTop(const Value: Integer);
begin
  SetScaledBounds(inherited Left, Value, inherited Width,  inherited Height);
end;

procedure _ScaleableUpDown.SetWidth(const Value: Integer);
begin
  SetScaledBounds(inherited Left, inherited Top, Value,  inherited Height);
end;

procedure _ScaleableUpDown.UpdateDesignCLs;
begin
  fDesignXCentre := fDesignLeft + fDesignWidth div 2;
  fDesignYCentre := fDesignTop + fDesignHeight div 2;
end;

procedure _ScaleableUpDown.UpdateDesignDims(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if CanScaleX then begin
    if (scWidth in Scalings) then
      fDesignWidth := MulDiv(AWidth, DesignParentClientWidth, ParentClientWidth)
    else
      Inc(fDesignWidth, AWidth - inherited Width);
    if ([scXPos,scXCentred]*Scalings<>[]) then begin
      fDesignXCentre := MulDiv(ALeft+AWidth div 2,
                        fDesignParentClientWidth, fParentClientWidth);
      fDesignLeft := fDesignXCentre - fDesignWidth div 2;
    end else
      Inc(fDesignLeft, ALeft - inherited Left);
  end;
  if CanScaleY then begin
    if (scHeight in Scalings) then
      fDesignHeight := MulDiv(AHeight, fDesignParentClientHeight, fParentClientHeight)
    else
      Inc(fDesignHeight, AHeight - inherited Height);
    if ([scYPos,scYCentred]*Scalings<>[])then begin
      fDesignYCentre := MulDiv(ATop+AHeight div 2,
                        fDesignParentClientHeight, fParentClientHeight);
      fDesignTop := fDesignYCentre - fDesignHeight div 2;
    end else
      Inc(fDesignTop, ATop - inherited Top);
  end;
end;

{ TScaleableUpDown }

constructor TScaleableUpDown.Create(AOwner: TComponent);
begin
  inherited;
  fScaledWidth := GetSystemMetrics(SM_CXVSCROLL);
end;

function TScaleableUpDown.GetOrientation: TUDOrientation;
begin
  Result := inherited Orientation;
end;

procedure TScaleableUpDown.ScaleControl(out NewLeft, NewTop, NewWidth,
  NewHeight, NewParentClientWidth, NewParentClientHeight: Integer);
begin
  inherited;
  fScaledWidth := NewWidth;
end;

procedure TScaleableUpDown.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited SetScaledBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TScaleableUpDown.SetOrientation(const Value: TUDOrientation);
begin
  inherited Orientation := Value;
  fScaledWidth := Width;
end;

procedure TScaleableUpDown.WMWindowPosChanging(
  var Message: TWMWindowPosChanging);
begin
  {it seems reluctant to accept a new width for vertical alignment
   when not visible e.g. when created}
  if not (csDesigning in ComponentState) then
    Message.WindowPos^.cx := fScaledWidth;
  inherited;
end;

end.


