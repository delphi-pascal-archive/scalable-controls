unit DockLists;

////////////////////////////////////////////////////////////////////////////////
// Scalable  Controls - Demo                                                  //
// for Delphi Magazine                                                        //
//                                                                            //
// Copyright (c) Martin Humby 2007  Freeware                                  //
////////////////////////////////////////////////////////////////////////////////

{known features: if a scaleable controls TestForm client height is zero
 when docked nothing is displayed in client area when undocked.
 Mouse resize, Dock-Undock again restores components.
 Workaround: prevent client-height reaching zero

 Unsuitable for Window 98 when many forms are docked}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  ExtCtrls, Forms, Dialogs, WinStuff;

const
  DefTitleBarHeight = 15;
  DefBarSpacing = 3;
  DefSplitterHeight = 5;
  ThumbnailHt = 408;
  FormFix = 8; {for Form version of FormStack}
{ FormFix = 0; {component version of FormStack}
type
  TInterval = object               
  private
    fStart, fSuccEnd: Integer;
    function GetEndVal: Integer;
    function GetLength: Integer;
  public
    function Includes(Value: Integer): Boolean; //inline;
    function Excludes(Value: Integer): Boolean; //inline;
    property Start: Integer read fStart;
    property Length: Integer read GetLength;
    property EndVal: Integer read GetEndVal;
    property SuccEnd: Integer read fSuccEnd;
  end;
  function NewInterval(Start, Length: Integer): TInterval; //inline;
  function DefInterval(Start, SuccEnd: Integer): TInterval; //inline;

type
  TNotifyExclusiveEvent = procedure(Sender: TObject; var Exclusive: Boolean) of object;
  TClientDockingEvent = procedure(Sender: TObject; Client: TControl) of object;
  TDockList = class;
  TDockListZone = class;

  TDockListZone = class(TCustomPanel)
  private
    fOnClientUndocked: TClientDockingEvent;
    fOnClientStartDock:  TClientDockingEvent;
    fOnPaint: TNotifyEvent;
    fOnNCPaint: TNotifyExclusiveEvent;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    property WindowHandle;
    property BevelOuter default bvNone;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetBounds(Force: Boolean = True);
    property OnClientUndocked: TClientDockingEvent read fOnClientUndocked
      write fOnClientUndocked;
    property OnClientStartDock: TClientDockingEvent read fOnClientStartDock
      write fOnClientStartDock;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
    property OnNCPaint: TNotifyExclusiveEvent read fOnNCPaint write fOnNCPaint;
    property Canvas;
    property DockSite;
    property DockManager;
    property UseDockManager;
    property Handle;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnEndDock;
    property Color;
  end;

  TStackDockTree = class(TDockTree)
  private
    fTitleBarHeight: Integer;
  protected
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
    procedure PaintDockFrame(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); override;
  public
    constructor Create(DockSite: TWinControl); override;
    property TitleBarHeight: integer read fTitleBarHeight write fTitleBarHeight
      default DefTitleBarHeight;
  end;

  TDockListChange = procedure(Sender: TObject; ListCount: Integer) of object;
  TDockListActiveChange = procedure(Sender: TObject;
     PrevObject, CurObject: TControl) of object;

  TDockList = class(TStringList)
  private
    fOnCountChange: TDockListChange;
    fOnActiveChange: TDockListActiveChange;
    fOnActiveHidden: TNotifyEvent;
    fActiveIndex: Integer;  {index of object currently displayed}
    fActiveObject: TControl;
    fLowIndex: Integer;     {index of first object displayed}
    fHighIndex: Integer;    {index of last object displayed}
    fTitleHeight: Integer;  {height of title-bar + space}
    fBarHeight,
    fBarSpace,
    fSplitterHeight: Integer;
    fDisplayedObjects: Integer; {number displayed by last paint}
    fActiveOffset: Integer; {active object's 0 based index in displayed objects}
    fScrollDockZone,        {DockZone scrolled by Next, Prev..}
    fKeepLowIndex,          {else List scrolled}
    fKeepHighIndex,
    fNewObject: Boolean;    {T when inserted until painted}
    fTopSplitterIv,         {location when last displayed}
    fBottomSplitterIv: TInterval;
    fObjectIntervals: array of TInterval;
    procedure SetBarHeight(const Value: Integer);
    procedure SetBarSpace(const Value: Integer);
    procedure SetActiveObject;
    procedure DoReleaseIxConstraints;
    procedure PositionObjects( ZoneHeight, InHeight: Integer;
      var ObjectsToPaint, PaintAt: Integer);
  protected {abstract model: ListPainting}
    procedure PositionNewObject(const PaintObjects, ActiveIndex,
      HighIndex, ZonePos: Integer; var LowIndex: Integer); virtual;
    procedure PositionObject(const PaintObjects: Integer;
      var ActiveIndex, LowIndex: Integer);
    procedure DoPaintObjects(const Canvas: TCanvas; DockZone: TControl;
      PaintYPos, ZoneHeight: Integer; out NewZoneOffset: Integer;
      out TopSplitter, BottomSplitter: TInterval); virtual;
    procedure DrawSplitters(const Canvas: TCanvas; DockZone: TControl;
      out TopSplitter, BottomSplitter: TInterval); virtual;
  protected  {emergent properties}
    property LowIndex: Integer read fLowIndex;
    property HighIndex: Integer read fHighIndex;
    property DisplayedObjects: Integer read fDisplayedObjects;
    property ActiveOffset: Integer read fActiveOffset;
  public {emergent properties}
    property ActiveObject: TControl read fActiveObject;
    property ActiveIndex: Integer read fActiveIndex;
    property TopSplitterIv: TInterval read fTopSplitterIv;
    property BottomSplitterIv: TInterval read fBottomSplitterIv;
  public
    constructor Create(BarHeight, BarSpace: Integer);
    function Next: Boolean;
    function Prev: Boolean;
    function First: Boolean;
    function Last: Boolean;
    function MakeActive(Index: Integer): Boolean;
    procedure ReleaseIxConstraints;
    procedure Add(Control: TControl); reintroduce;  virtual;
    function DeleteActive: Boolean; virtual;
    procedure PaintList(Canvas: TCanvas; DockZone: TWinControl;
      AtY, InHeight: Integer); virtual;
    procedure PaintLeader(DC: HDC; AtY, Width, Height: Integer;
      HlPen, SdPen: HPEN); virtual;
    procedure PaintLeading(DC: HDC; Width, Height: Integer;
      HlPen, SdPen: HPEN); virtual;
    procedure PaintTrailer(DC: HDC; AtY, Width, Height: Integer;
      HlPen, SdPen: HPEN); virtual;
    procedure PaintTrailing(DC: HDC; Width, Height: Integer;
      HlPen, SdPen: HPEN); virtual;
    procedure Refresh;
    function InTopSplitter(YVal: Integer): Boolean;
    function InBottomSplitter(YVal: Integer): Boolean;
    function ActivateObjectAt(YPos: Integer): Boolean;
    property BarHeight: Integer read fBarHeight write SetBarHeight;
    property BarSpace: Integer read fBarSpace write SetBarSpace;
    property SplitterHeight: Integer read fSplitterHeight
      write fSplitterHeight;
    property ScrollDockZone: Boolean read fScrollDockZone
      write fScrollDockZone default True;
    property TitleHeight: Integer read fTitleHeight;
    property OnCountChange: TDockListChange read fOnCountChange
      write fOnCountChange;
    property OnActiveChange: TDockListActiveChange read fOnActiveChange
      write fOnActiveChange;
    property OnActiveHidden: TNotifyEvent read fOnActiveHidden
      write fOnActiveHidden;
  end;

procedure DrawTitleBar(Canvas: TCanvas; Control: TControl;
  const ARect: TRect; Active: Boolean); overload;
procedure DrawTitleBar(DC: HDC; Control: TControl; const ARect: TRect;
  Active: Boolean); overload;
procedure DrawShadowedRect(Canvas: TCanvas; Left, Top, Width, Height: Integer); overload;
procedure DrawRect(Canvas: TCanvas; ARect: TRect);

implementation
uses
 Types;

{ TDockListZone }

constructor TDockListZone.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  Color := clWindow;
  {if either of the following are set here changing later stops
   dragging-off working (D7)}
  DockManager := TStackDockTree.Create(Self);
  DockSite := True;
  UseDockManager := True;
  DoubleBuffered := False; {improves redraw when size changed}
  if AOwner is TWinControl then
    Parent := TWinControl(AOwner);
end;

procedure TDockListZone.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if Assigned(OnStartDock) then
    fOnClientStartDock(Self, Client);
  inherited; {Client.Parent := Self (D7)}
end;

procedure TDockListZone.DoRemoveDockClient(Client: TControl);
begin
  inherited; {do nothing (D7)}
  if Assigned(OnClientUndocked) then
    OnClientUndocked(Self, Client);
end;

procedure TDockListZone.ResetBounds(Force: Boolean);
begin
  DockManager.ResetBounds(Force);
end;

procedure TDockListZone.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TDockListZone.WMNCPaint(var Message: TMessage);
var
  Exclusive: Boolean;
begin
  if Assigned(fOnNCPaint) then begin
    Exclusive := False;
    fOnNCPaint(Self, Exclusive);
    if Exclusive then begin
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

procedure TDockListZone.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if Assigned(OnPaint) then
    OnPaint(Self);
end;

{ TStackDockTree }

const
  GrabberSize = DefTitleBarHeight;

procedure DrawTitleBar(Canvas: TCanvas; Control: TControl;
  const ARect: TRect; Active: Boolean);
var
  Handle: HWND;
begin
  Assert(Control is TWinControl, 'Must have HWND to get Caption');
  Handle := TWinControl(Control).Handle;
  DrawCaption(Handle, Canvas.Handle, ARect, Integer(Active) // DC_ACTIVE = 1
                {or DC_GRADIENT} or DC_SMALLCAP or DC_TEXT);
end;

procedure DrawTitleBar(DC: HDC; Control: TControl;
  const ARect: TRect; Active: Boolean);
var
  Handle: HWND;
begin
  Assert(Control is TWinControl, 'Must have HWND to get Caption');
  Handle := TWinControl(Control).Handle;
  DrawCaption(Handle, DC, ARect, Integer(Active) // DC_ACTIVE = 1
                {or DC_GRADIENT} or DC_SMALLCAP or DC_TEXT);
end;

procedure DrawMinimizeButton(Canvas: TCanvas; Left, Top, Width, Height: Integer);
begin
  DrawFrameControl(Canvas.Handle, Bounds(Left, Top, Width, Height),
     DFC_CAPTION, DFCS_CAPTIONMIN);
end;

procedure TStackDockTree.PaintDockFrame(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);
begin
  with ARect do begin
    if DockSite.Align in [alTop, alBottom] then
      inherited
    else begin
      DrawTitleBar(Canvas, Control, Rect(Left,Top,Right,Top+TitleBarHeight),
        True);
    end;
    DrawMinimizeButton(Canvas, Right-TitleBarHeight, Top+2, 12, 11);
  end;
end;

procedure TStackDockTree.AdjustDockRect(Control: TControl;
  var ARect: TRect);
begin
  if DockSite.Align in [alTop, alBottom] then
    inherited
  else
    Inc(ARect.Top, TitleBarHeight);
end;



constructor TStackDockTree.Create(DockSite: TWinControl);
begin
  inherited;
  fTitleBarHeight := DefTitleBarHeight;
end;

{ TDockList }

type
  friendControl = class(TControl);

function TDockList.ActivateObjectAt(YPos: Integer): Boolean;
var
  Ix, ObjectIndex: Integer;
begin
  Result := False;
  if (DisplayedObjects=0) then Exit;
  for Ix := 0 to Pred(DisplayedObjects) do
    if fObjectIntervals[Ix].Includes(YPos) then begin
      Result := True;
      ObjectIndex := Ix;
      Break;
    end;
  if Result then begin
    ReleaseIxConstraints;
    Inc(ObjectIndex, LowIndex);
    fActiveIndex := ObjectIndex;
    SetActiveObject;
  end;
end;

procedure TDockList.Add(Control: TControl);
begin
  if Control = fActiveObject then Exit;
  
  fActiveIndex := AddObject(friendControl(Control).Caption, Control);
  fActiveObject := Control;
  fNewObject := True;
  if Assigned(fOnCountChange) then fOnCountChange(Self, Count);
end;

constructor TDockList.Create(BarHeight, BarSpace: Integer);
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupAccept;
  fTitleHeight := BarHeight + BarSpace;
  fBarHeight := BarHeight;
  fBarSpace := BarSpace;
  fSplitterHeight := DefSplitterHeight;
  fScrollDockZone := True;
  SetLength(fObjectIntervals, 5);
end;

function TDockList.DeleteActive: Boolean;
begin
  Result := False;
  if Count=0 then Exit;
  Delete(fActiveIndex);
  if Count>0 then begin
    fActiveIndex := Pred(fActiveIndex);
    if fActiveIndex<0 then fActiveIndex := 0;
    SetActiveObject;
  end else begin
    fActiveIndex := 0;
    fLowIndex := 0;
    fActiveObject := nil;
  end;
  if Assigned(fOnCountChange) then fOnCountChange(Self, Count);
  Result := True;
end;

procedure DrawShadowedRect(Canvas: TCanvas; Left, Top, Width, Height: Integer);
var
  Xx, Yx: Integer;
  HA, HB, HC,
  SA, SB, SC: TPoint;
begin
  FillRect(Canvas.Handle,Bounds(Left,Top,Width,Height),
    GetStockObject(LTGRAY_BRUSH));
  Xx := Left+Width; Yx := Top+Height-1;
  HA := Point(Left, Yx); HB := Point(Left, Top); HC := Point(Xx, Top);
  SA := Point(Left+1, Yx); SB := Point(Xx, Yx);  SC := Point(Xx, Top-1);
  with Canvas do begin
    Pen.Color := clBtnHighlight;
    PolyLine([HA, HB, HC]);
    Pen.Color := clBtnShadow;
    PolyLine([SA, SB, SC]);
  end;
end;

procedure DrawRect(Canvas: TCanvas; ARect: TRect);
var
  TR,BL: TPoint;
begin
  TR := Point(ARect.Right,ARect.Top);
  BL := Point(Arect.Left,ARect.Bottom);
  with Canvas do
    PolyLine([ARect.TopLeft,TR,ARect.BottomRight,BL,ARect.TopLeft]);
end;


procedure TDockList.DoPaintObjects(const Canvas: TCanvas; DockZone: TControl;
      PaintYPos, ZoneHeight: Integer; out NewZoneOffset: Integer;
      out TopSplitter, BottomSplitter: TInterval);
var
  Ix, PaintWidth, DrawWidth, ObjectsPainted, SY: Integer;
begin
  PaintWidth := DockZone.Width;
  DrawWidth := PaintWidth-1;
  ObjectsPainted := 0;
  for Ix := fLowIndex to fHighIndex do begin
    SY := PaintYPos;
    if (Ix=fActiveIndex)and(fActiveObject.Visible) then begin
      DrawShadowedRect(Canvas,0,PaintYPos,DrawWidth,SplitterHeight);
      TopSplitter := NewInterval(PaintYPos,SplitterHeight);
      Inc(PaintYPos, SplitterHeight + BarSpace);
      DockZone.Top := PaintYPos;
      NewZoneOffset := ObjectsPainted;
      Inc(PaintYPos, DockZone.Height + BarSpace);
      DrawShadowedRect(Canvas,0,PaintYPos,DrawWidth,SplitterHeight);
      BottomSplitter := NewInterval(PaintYPos,SplitterHeight);
      Inc(PaintYPos, SplitterHeight + BarSpace);
    end else begin
      if (Ix=fActiveIndex)and Assigned(OnActiveHidden) then
        OnActiveHidden(Self);
      if Objects[Ix] is TWinControl then
        DrawTitleBar(Canvas, TControl(Objects[Ix]),
          Bounds(0, PaintYPos, PaintWidth, BarHeight),(Ix=ActiveIndex));
      Inc(PaintYPos, TitleHeight);
    end;
    fObjectIntervals[ObjectsPainted] := DefInterval(SY,PaintYPos);
    Inc(ObjectsPainted);
  end;
end;

procedure TDockList.DoReleaseIxConstraints;
begin
  fKeepLowIndex := False;
  fKeepHighIndex := False;
end;

procedure TDockList.DrawSplitters(const Canvas: TCanvas;
  DockZone: TControl; out TopSplitter, BottomSplitter: TInterval);
var
  PaintYPos: Integer;
begin
  PaintYPos := DockZone.Top - (SplitterHeight+BarSpace);
  DrawShadowedRect(Canvas,0,PaintYPos,DockZone.Width,SplitterHeight);
  TopSplitter := NewInterval(PaintYPos,SplitterHeight);
  Inc(PaintYPos,SplitterHeight+BarSpace*2+DockZone.Height);
  DrawShadowedRect(Canvas,0,PaintYPos,DockZone.Width,SplitterHeight);
  BottomSplitter := NewInterval(PaintYPos,SplitterHeight);
end;

function TDockList.First: Boolean;
begin
  Result := False;
  if fActiveIndex <= 0 then Exit;
  fActiveIndex := 0;
  SetActiveObject;
  fLowIndex := 0;
  fKeepHighIndex := False;
  fKeepLowIndex := True;
  Result := True;
end;

function TDockList.InBottomSplitter(YVal: Integer): Boolean;
begin
  Result := ((Count=0) or ActiveObject.Visible)and
    BottomSplitterIv.Includes(YVal);
end;

function TDockList.InTopSplitter(YVal: Integer): Boolean;
begin
  Result := ((Count=0) or ActiveObject.Visible)and
    TopSplitterIv.Includes(YVal);
end;

function TDockList.Last: Boolean;
begin
  Result := False;
  if fActiveIndex = Count then Exit;
  fActiveIndex := Pred(Count);
  SetActiveObject;
  fHighIndex := fActiveIndex;
  fKeepHighIndex := False;
  fKeepLowIndex := False;
  Result := True;
end;

function TDockList.MakeActive(Index: Integer): Boolean;
begin
  Result := False;
  if (Index>0)and(Index<Count-1) then begin
    fActiveIndex := Index;
    SetActiveObject;
    Result := True;
  end;
end;

function TDockList.Next: Boolean;
begin
  Result := False;
  if ActiveIndex = Pred(Count) then Exit;
  if ScrollDockZone then begin
    Inc(fActiveIndex);
    SetActiveObject;
    if HighIndex < ActiveIndex then begin
      fHighIndex := ActiveIndex;
      Inc(fLowIndex);
    end;
  end else begin
    if HighIndex = Pred(Count) then Exit;
    Inc(fHighIndex);
    Inc(fLowIndex);
    if ActiveIndex < LowIndex then begin
      fActiveIndex := fLowIndex;
      SetActiveObject;
    end;
  end;
  fKeepHighIndex := True;
  fKeepLowIndex := False;
  Result := True;
end;

procedure TDockList.PaintLeader(DC: HDC; AtY, Width, Height: Integer; HlPen,
  SdPen: HPEN);
var
  BlankSpace, ObjectsToPaint, ListHeight, Ix, PaintAt: Integer;
begin
  BlankSpace := BarSpace*2+SplitterHeight;
  ObjectsToPaint := (Height-BlankSpace)div TitleHeight;
  if ObjectsToPaint > ActiveIndex then
    ObjectsToPaint := ActiveIndex;
  ListHeight := ObjectsToPaint*TitleHeight + BlankSpace;
  PaintAt := AtY + Height-ListHeight+BarSpace;
  if ObjectsToPaint > 0 then begin
    Ix := ActiveIndex-ObjectsToPaint;
    repeat
      DrawTitleBar(DC,TControl(Objects[Ix]),
           Bounds(0,PaintAt,Width,BarHeight),False);
      Inc(Ix);
      Inc(PaintAt,TitleHeight);
    until Ix=ActiveIndex;
  end;
  DrawShadowedRect(DC, 0, PaintAt, Width, SplitterHeight,
                           HlPen, SdPen);
end;

procedure TDockList.PositionObjects(ZoneHeight, InHeight: Integer;
  var ObjectsToPaint, PaintAt: Integer);
var
  ListHeight: Integer;
begin
  ListHeight := Pred(Count)* TitleHeight + ZoneHeight;
  if ListHeight<=InHeight then begin  {centre list}
    PaintAt := PaintAt + (InHeight-ListHeight) div 2;
    fLowIndex := 0;
    ObjectsToPaint := Count;
  end else begin                           {allow last bar to abut ZoneHeight}
    ObjectsToPaint := (InHeight-ZoneHeight+BarSpace-1)div fTitleHeight + 1;
    if ObjectsToPaint>Count then
      ObjectsToPaint := Count;
    if fNewObject then begin
      PositionNewObject(ObjectsToPaint, ActiveIndex, HighIndex,
         ActiveOffset, fLowIndex);
      fNewObject := False;
    end else
      PositionObject(ObjectsToPaint, fActiveIndex, fLowIndex);
    if LowIndex < 0 then fLowIndex := 0;
  end;
end;

procedure TDockList.PaintLeading(DC: HDC; Width, Height: Integer;
      HlPen, SdPen: HPEN);
var
  BlankSpace, ObjectsToPaint, Leaders, ListHeight, Ix, PaintAt: Integer;
begin
            {paint over the top of displayed Form: Title bar is intermittent}
  BlankSpace := BarSpace+SplitterHeight+BarHeight;
  ObjectsToPaint := (Height-BlankSpace)div TitleHeight;
  Leaders := ActiveIndex;
  if ObjectsToPaint > Leaders then
    ObjectsToPaint := Leaders;
  ListHeight := ObjectsToPaint*TitleHeight + BlankSpace;
  PaintAt := Height-ListHeight;
  if ObjectsToPaint > 0 then begin
    Ix := ActiveIndex-ObjectsToPaint;
    repeat
      DrawTitleBar(DC,TControl(Objects[Ix]),
           Bounds(0,PaintAt,Width,BarHeight),False);
      Dec(ObjectsToPaint);
      Inc(PaintAt,TitleHeight);
      Inc(Ix);
    until ObjectsToPaint = 0;
  end;
  DrawShadowedRect(DC, 0, PaintAt, Width, SplitterHeight,
                           HlPen, SdPen);
  Inc(PaintAt, SplitterHeight+BarSpace);
  if Count>0 then begin
    DrawTitleBar(DC,TControl(Objects[ActiveIndex]),
           Bounds(0,PaintAt,Width,BarHeight),True);
                                              {-8? fix comply with DockTree, only for Form version}
    DrawFrameControl(DC, Bounds(Width-BarHeight-FormFix, PaintAt+2, 12, 11),
       DFC_CAPTION, DFCS_CAPTIONMIN);
  end;
end;

procedure TDockList.PaintList(Canvas: TCanvas; DockZone: TWinControl;
  AtY, InHeight: Integer);
var
  ZoneHeight, Ht, ZoneMargins,
  ObjectsToPaint: Integer;
begin
  Inc(AtY, BarSpace);  {minimum top margin}
  if Count=0 then begin
    DrawSplitters(Canvas, DockZone, fTopSplitterIv, fBottomSplitterIv);
    fDisplayedObjects := 0;
    Exit;
  end;
  ZoneMargins := 2*SplitterHeight + 2*BarSpace;
  ZoneHeight := DockZone.Height + ZoneMargins;
    if (InHeight<ZoneHeight)then begin
    Ht := InHeight - ZoneMargins-10;
    {if Ht<TitleHeight then
      Exit; }
    ActiveObject.Hide;
    DockZone.Hide;
    DockZone.Height := Ht;
    DockZone.Top := AtY;
  end;
  if ActiveObject.Visible then begin
    DockZone.Show;
  end else begin
    ZoneHeight := TitleHeight;
    DockZone.Hide;
  end;
  PositionObjects(ZoneHeight, InHeight, ObjectsToPaint, AtY);
  if ObjectsToPaint>Length(fObjectIntervals) then
    SetLength(fObjectIntervals, ObjectsToPaint+5);
  fHighIndex := LowIndex + Pred(ObjectsToPaint);
  if HighIndex > Pred(Count) then
    fHighIndex := Pred(Count);
  DoPaintObjects(Canvas, DockZone, AtY, ZoneHeight, fActiveOffset,
    fTopSplitterIv, fBottomSplitterIv);
  fDisplayedObjects := ObjectsToPaint;
end;

procedure TDockList.PaintTrailer(DC: HDC; AtY, Width, Height: Integer;
      HlPen, SdPen: HPEN);
var
  BlankSpace, ObjectsToPaint, Ix, PaintAt: Integer;
begin
  BlankSpace := BarSpace*2+SplitterHeight;
  ObjectsToPaint := (Height-BlankSpace)div TitleHeight;
  if ObjectsToPaint > Count-ActiveIndex-1 then
    ObjectsToPaint := Count-ActiveIndex-1;
  PaintAt := AtY + BarSpace;
  DrawShadowedRect(DC, 0, PaintAt, Width, SplitterHeight, HlPen, SdPen);
  Inc(PaintAt, SplitterHeight+BarSpace);
  if ObjectsToPaint > 0 then begin
    Ix := Succ(ActiveIndex);
    repeat
      DrawTitleBar(DC,TControl(Objects[Ix]),
           Bounds(0,PaintAt,Width,BarHeight),False);
      Inc(Ix);
      Inc(PaintAt,TitleHeight);
    until Ix=Count;
  end;
end;


procedure TDockList.PaintTrailing(DC: HDC; Width, Height: Integer;
      HlPen, SdPen: HPEN);
var
  BlankSpace, ObjectsToPaint, Trailers, ListHeight, Ix, PaintAt: Integer;
begin
  BlankSpace := BarSpace*2+SplitterHeight;
  ObjectsToPaint := (Height-BlankSpace)div TitleHeight;
  Trailers := Pred(Count)-ActiveIndex;
  if ObjectsToPaint > Trailers then
    ObjectsToPaint := Trailers;
  ListHeight := ObjectsToPaint*TitleHeight + BlankSpace;
  Assert(ListHeight<=Height,'Bitmap too small for: '+IntToStr(ListHeight));
  PaintAt := BarSpace;
  DrawShadowedRect(DC,0,PaintAt,Width,SplitterHeight,HlPen,SdPen);
  Inc(PaintAt, SplitterHeight+BarSpace);
  if ObjectsToPaint > 0 then begin
    Ix := Succ(ActiveIndex);
    repeat
      DrawTitleBar(DC,TControl(Objects[Ix]),
           Bounds(0,PaintAt,Width,BarHeight),False);
      Dec(ObjectsToPaint);
      Inc(PaintAt,TitleHeight);
      Inc(Ix);
    until ObjectsToPaint = 0;
  end;
end;

procedure TDockList.PositionNewObject(const PaintObjects, ActiveIndex,
  HighIndex, ZonePos: Integer; var LowIndex: Integer);
begin
  if (ActiveIndex<LowIndex) then
    {try to keep same list displayed}
    if(ActiveIndex+PaintObjects-1 >= LowIndex) then
      LowIndex := ActiveIndex
    else {keep same position in painted list}
      LowIndex := ActiveIndex - ZonePos
  else if (ActiveIndex>HighIndex) then
    if (ActiveIndex-PaintObjects < LowIndex) then
      LowIndex := ActiveIndex - PaintObjects + 1
    else
      LowIndex := ActiveIndex - ZonePos
  else
    LowIndex := ActiveIndex - ZonePos;
  if LowIndex+PaintObjects > Pred(Count) then
    LowIndex := Count-PaintObjects;
end;

{position Active Object in new displayed list size}
procedure TDockList.PositionObject(const PaintObjects: Integer;
  var ActiveIndex, LowIndex: Integer);
var
  ObjectsOffset: Integer;
begin
  ObjectsOffset := Pred(PaintObjects);
  if fKeepLowIndex then begin
    if LowIndex+ObjectsOffset < ActiveIndex then begin
      ActiveIndex := LowIndex+ObjectsOffset;
      SetActiveObject;
    end;
  end else if fKeepHighIndex then begin
    if HighIndex-ObjectsOffset > ActiveIndex then begin
      ActiveIndex := HighIndex-ObjectsOffset;
      SetActiveObject;
      LowIndex := ActiveIndex;
    end;
  end else begin{ if(ActiveIndex-LowIndex>ObjectsOffset)
                or(ActiveIndex=Pred(Count))then
      LowIndex := ActiveIndex-ObjectsOffset
  else begin
      LowIndex := ActiveIndex - ObjectsOffset div 2;
  end; }
    LowIndex := ActiveIndex - PaintObjects div 2;
    if LowIndex + PaintObjects > Count then
    LowIndex := Count - PaintObjects;
  end;
end;

function TDockList.Prev: Boolean;
begin
  Result := False;
  if ActiveIndex=0 then Exit;
  if ScrollDockZone then begin
    Dec(fActiveIndex);
    SetActiveObject;
    if LowIndex > ActiveIndex then begin
      fLowIndex := ActiveIndex;
      Dec(fHighIndex);
    end;
  end else begin
    Dec(fHighIndex);
    Dec(fLowIndex);
    if HighIndex<ActiveIndex then begin
      fActiveIndex := HighIndex;
      SetActiveObject;
    end;
  end;
  fKeepHighIndex := False;
  fKeepLowIndex := True;
  Result := True;
end;

procedure TDockList.Refresh;
var
  Ix: Integer;
begin
  if Sorted then begin
    Sorted := False;
    for Ix := 0 to Pred(Count) do
      if Objects[Ix] is TControl then
        Strings[Ix] := friendControl(Objects[Ix]).Caption;
    Sorted := True;
    fActiveIndex := IndexOfObject(ActiveObject);
  end;
end;

procedure TDockList.ReleaseIxConstraints;
begin
  DoReleaseIxConstraints;
end;

procedure TDockList.SetActiveObject;
var
  OldActive: TControl;
begin
  Assert(fActiveIndex>=0,'');
  OldActive := ActiveObject;
  fActiveObject := TControl(Objects[fActiveIndex]);
  if Assigned(fOnActiveChange) then
    fOnActiveChange(Self, OldActive, ActiveObject);
end;



procedure TDockList.SetBarHeight(const Value: Integer);
begin
  fBarHeight := Value;
  fTitleHeight := fBarHeight+fBarSpace;
end;

procedure TDockList.SetBarSpace(const Value: Integer);
begin
  fBarSpace := Value;
  fTitleHeight := fBarHeight+fBarSpace;
end;



{ TInterval }

function NewInterval(Start, Length: Integer): TInterval;
begin
  Result.fStart := Start;
  Result.fSuccEnd := Start+Length;
end;

function DefInterval(Start, SuccEnd: Integer): TInterval;
begin
  Result.fStart := Start;
  Result.fSuccEnd := SuccEnd;
end;

function TInterval.Includes(Value: Integer): Boolean;
begin
  Result := (Value>=Start)and(Value<SuccEnd);
end;

function TInterval.Excludes(Value: Integer): Boolean;
begin
  Result := (Value<Start)or(Value>=SuccEnd);
end;

function TInterval.GetEndVal: Integer;
begin
  if SuccEnd>Start then
    Result := Pred(SuccEnd)
  else
    Result := SuccEnd;
end;

function TInterval.GetLength: Integer;
begin
  Result := SuccEnd-Start;
end;

end.



