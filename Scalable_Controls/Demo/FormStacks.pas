unit FormStacks;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DockLists, ComCtrls, ExtCtrls, StdCtrls, Buttons, ScaleableControl;

type
  TStackForm = class(TScaledForm)
    Panel1: TPanel;
    UpDown1: TUpDown;
    btnFirst: TButton;
    btnLast: TButton;
    btnReorder: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure btnReorderClick(Sender: TObject);
  private
//    fOnStartDock:  TClientDockingEvent;
    fDockList: TDockList;
    fDockZone: TDockListZone;
    fListHeight: Integer;
    fAddObject,
    fAddExisting,
    fUndocked: Boolean;
    fInTopSplitter,
    fInBtmSplitter,
    fMoveTopSplitter,
    fMoveBtmSplitter: Boolean;
    fMouseDownY,
    fZoneTop: Integer;
//    fZoneHeight: Integer;
    fDefCursor: TCursor;
    fYOffset,
    fZoneBtm: Integer;
    fHideOnScroll,
    fHideObj: Boolean;
    fOSBitmap: HBITMAP;
    fCDC: HDC;
    fOSYSplit: Integer;
    fControlsStacked: Integer;
    fPassiveDock: Boolean;
    fManualDocking: Boolean;
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockZoneDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure DockZoneDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DockZoneUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure DockZoneClientUndocked(Sender: TObject; Client: TControl);
    procedure DockZoneBeforeClientDocked(Sender: TObject; Client: TControl);
    procedure DockZonePaint(Sender: TObject);
    procedure DockZoneNCPaint(Sender: TObject; var Exclusive: Boolean);
    procedure FormsListActiveChange(Sender: TObject; PrevObj, NewObj: TControl);
    procedure FormsListActiveHidden(Sender: TObject);
    procedure DoAdjustForSizing(const Value: Boolean); override;
    procedure PrepareBitmap;
    procedure DeleteObjects;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    property DockList: TDockList read fDockList;
    property InTopSplitter: Boolean read fInTopSplitter;
    property InBtmSplitter: Boolean read fInBtmSplitter;
    property MoveTopSplitter: Boolean read fMoveTopSplitter;
    property MoveBtmSplitter: Boolean read fMoveBtmSplitter;
    property MouseDownY: Integer read fMouseDownY;
    property DefCursor: TCursor read fDefCursor;
    property YOffset: Integer read fYOffset;
    property ZoneTop: Integer read fZoneTop;
    property ZoneBtm: Integer read fZoneBtm;
    property ListHeight: Integer read fListHeight;
    property OSYSplit: Integer read fOSYSplit;
  public
    procedure RefreshDocked;
    procedure RefreshList;
    procedure ShowItem(Index: Integer);
    property PassiveDock: Boolean read fPassiveDock write fPassiveDock;
    property ManualDocking: Boolean read fManualDocking write fManualDocking;
    property DockZone: TDockListZone read fDockZone;
  end;

var
  StackForm: TStackForm;

implementation
uses
  WinStuff, AppMainForm, TestForms;

{$R *.dfm}

const
  ThumbnailHt = 408;
  TitleBarSpace = 2;

procedure TStackForm.FormCreate(Sender: TObject);
begin
  fDefCursor := Cursor;
  fDockList := TDockList.Create(DefTitleBarHeight, 3);
  fDockList.OnActiveChange := FormsListActiveChange;
  fDockList.OnActiveHidden := FormsListActiveHidden;
  fDockZone := TDockListZone.Create(Self);
  fDockZone.SetBounds(0,(Height-Panel1.Height-ThumbnailHt)div 2,ClientWidth,ThumbnailHt);
  fDockZone.Parent := Self;
  with fDockZone do begin
    Anchors := [akLeft, akRight];
    OnDockOver := DockZoneDockOver;
    OnDockDrop := DockZoneDockDrop;
    OnUnDock := DockZoneUnDock;
    OnClientUndocked := DockZoneClientUndocked;
    OnClientStartDock := DockZoneBeforeClientDocked;
  end;
  fDockZone.OnPaint := DockZonePaint;
  fDockZone.OnNCPaint :=  DockZoneNCPaint;
end;

//type
//  friendWCtrl = class(TWinControl);

procedure TStackForm.DockZoneDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  if fAddExisting then Exit;
  fAddObject := True;
  Inc(fControlsStacked);  
  Label1.Caption := Format('%3d Forms',[fControlsStacked]);
  if Assigned(fDockList.ActiveObject) then
    fDockList.ActiveObject.Hide;
  fDockList.Add(Source.Control);
  fDockZone.ResetBounds;
  with Source do
    if not PassiveDock then Control.Show; {if hidden when docking with StackForm}
//  Caption := friendWCtrl(DockList.ActiveObject).Caption;
  Invalidate;
  fAddObject := False;
end;

procedure TStackForm.DockZoneDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  DockRect: TRect;
begin
  DockRect.TopLeft:= ClientToScreen(fDockZone.BoundsRect.TopLeft);
  DockRect.BottomRight := ClientToScreen(fDockZone.BoundsRect.BottomRight);
  Source.DockRect := DockRect;
end;

procedure TStackForm.FormPaint(Sender: TObject);
var
  TopHt: Integer;
begin
  if MoveBtmSplitter or MoveTopSplitter then begin
    fDockZone.BoundsRect := Rect(0,ZoneTop,Dockzone.BoundsRect.Right,ZoneBtm);
    if MoveBtmSplitter then begin
      BitBlt(Canvas.Handle,0,ZoneBtm,Width,ListHeight-ZoneBtm,fCDC,0,0,SRCCOPY);
      BitBlt(Canvas.Handle,0,0,Width,DockZone.Top,fCDC,0,OSYSplit,SRCCOPY);
    end else begin
      TopHt := ZoneTop+DockList.BarHeight;
      BitBlt(Canvas.Handle,0,0,Width,ZoneTop,fCDC,0,OSYSplit-TopHt,SRCCOPY);
      BitBlt(Canvas.Handle,0,ZoneBtm,Width,ListHeight-ZoneBtm,fCDC,0,OSYSplit,SRCCOPY);
    end;
  end else
    fDockList.PaintList(Canvas, fDockZone, 2, ListHeight-2);
end;

procedure TStackForm.DockZonePaint(Sender: TObject);
begin
  if (fDockList.Count>0)and not fDockList.ActiveObject.Visible then
    Invalidate;
end;

procedure TStackForm.DockZoneNCPaint(Sender: TObject;
  var Exclusive: Boolean);
var
  BarHt:Integer;
begin
  if MoveBtmSplitter or MoveTopSplitter then begin
    fDockZone.BoundsRect := Rect(0,ZoneTop,Dockzone.BoundsRect.Right,ZoneBtm);
    if MoveTopSplitter then begin
      BarHt := DockList.BarHeight;
      BitBlt(DockZone.Canvas.Handle,0,0,Width,BarHt,fCDC,0,OSYSplit-BarHt,SRCCOPY);
      Exclusive := True;
    end;
  end;
end;

procedure TStackForm.DockZoneUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if fAddObject then Exit;
  fUndocked := True;
  with fDockList do begin
    DeleteActive;
  end;
  Dec(fControlsStacked);
  Label1.Caption := Format('%3d Forms',[fControlsStacked]);
  fUndocked := False;
  Invalidate;
end;

procedure TStackForm.FormResize(Sender: TObject);
begin
  fListHeight:= ClientHeight-Panel1.Height;
  fDockList.ReleaseIxConstraints;
  Invalidate;
end;

procedure TStackForm.DockZoneClientUndocked(Sender: TObject;
  Client: TControl);
begin
  fDockZone.ResetBounds;
  Invalidate;
end;

procedure TStackForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  fHideObj := fHideOnScroll;
  case Button of
    btNext: fDockList.Prev;
    btPrev: fDockList.Next;
  else
    fHideObj := False;
    Exit;
  end;
  fHideObj := False;
  Invalidate;
end;

procedure TStackForm.FormsListActiveHidden(Sender: TObject);
begin
  fHideOnScroll := True;
end;

procedure TStackForm.FormsListActiveChange(Sender: TObject; PrevObj,
  NewObj: TControl);
begin
  if Assigned(PrevObj) and not fUndocked then
    PrevObj.Hide;
  try
    if Assigned(NewObj) and not fHideObj then
      NewObj.Show;
  except
    if Assigned(NewObj)then begin
      NewObj.Hide;
//      fHideObj := True;
//      fHideOnScroll := True;
    end;
    raise;
  end;
end;

procedure TStackForm.btnFirstClick(Sender: TObject);
begin
  fDockList.First;
  Invalidate;
end;

procedure TStackForm.btnLastClick(Sender: TObject);
begin
  fDockList.Last;
  Invalidate;
end;

procedure TStackForm.DockZoneBeforeClientDocked(Sender: TObject;
  Client: TControl);
begin
  with fDockList do
    if Assigned(ActiveObject) then begin
      ActiveObject.Hide;
    end;
    Client.Hide;
end;

procedure TStackForm.Button3Click(Sender: TObject);
begin
  fDockList.ScrollDockZone := not fDockList.ScrollDockZone;
  if fDockList.ScrollDockZone then
    Button3.Caption := 'Scroll Lst'
  else
    Button3.Caption := 'Scroll Frm'
end;

procedure TStackForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewHeight: Integer;
begin
  if (Y>ClientHeight)or(Y<0) then begin
    if fMoveTopSplitter or fMoveBtmSplitter then
      FormMouseUp(Sender, mbLeft, Shift, X, Y);
    Exit;
  end;
  if MoveBtmSplitter then begin
    NewHeight := Y+YOffset-fDockZone.Top;
    if NewHeight > 14 then
    fZoneBtm := Y+YOffset;
    Invalidate;
  end else if MoveTopSplitter then begin
    NewHeight := ZoneBtm - Y+YOffset;
    if (NewHeight<=14) then
      Exit;
    fZoneTop := Y+YOffset;
    Invalidate;
  end else if fDockList.InTopSplitter(Y) then begin
    Cursor := crVSplit;
    fInTopSplitter := True;
  end else if fDockList.InBottomSplitter(Y) then begin
    Cursor := crVSplit;
    fInBtmSplitter := True;
  end else begin
    Cursor := fDefCursor;
    fInTopSplitter := False;
    fInBtmSplitter := False;
  end;
end;

procedure TStackForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMouseDownY := Y;
  fZoneTop := DockZone.Top;
  fZoneBtm := DockZone.BoundsRect.Bottom;
  if InTopSplitter then begin
    fYOffset := Dockzone.Top - Y;
    fMoveTopSplitter := True;
  end else if InBtmSplitter then begin
    fYOffset := Dockzone.BoundsRect.Bottom - Y;
    fMoveBtmSplitter := True;
  end;
  if (fMoveTopSplitter or fMoveBtmSplitter) then begin
    if(DockList.ActiveObject is TScaledForm) then
      TScaledForm(Docklist.ActiveObject).AdjustForSizing(True);
    try
      PrepareBitmap;
    except
      DeleteObjects;
      raise;
    end;
  end else if fDockList.ActivateObjectAt(fMouseDownY) then begin
    fHideOnScroll := False;
    Invalidate;
  end;
end;

procedure TStackForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop, NewHeight: Integer;
begin
  if fMoveBtmSplitter and (Y<ClientHeight) then begin
    NewHeight := Y - fDockZone.Top + YOffset ;
    if NewHeight > 14 then
    fDockZone.Height := NewHeight;
    Invalidate;
  end else if fMoveTopSplitter and (Y>0) then begin
    NewHeight := ZoneBtm - Y + YOffset;
    if NewHeight > 14 then
      Dockzone.BoundsRect := Rect(0,Y+YOffset,Dockzone.Width,ZoneBtm);
    Invalidate;
  end;
  if (fMoveTopSplitter or fMoveBtmSplitter) then begin
    if(DockList.ActiveObject is TScaledForm) then
      TScaledForm(Docklist.ActiveObject).AdjustForSizing(False);
    DeleteObjects;
  end;
  fMoveTopSplitter := False;
  fMoveBtmSplitter := False;
  Cursor := fDefCursor;
  Invalidate;
end;

procedure TStackForm.FormClick(Sender: TObject);
begin
{  if fDockList.ActivateObjectAt(fMouseDownY) then begin
    fHideOnScroll := False;
    Invalidate;
  end;}
end;

type
  friendScaledForm = class(TScaledForm);

procedure TStackForm.DoAdjustForSizing(const Value: Boolean);
begin
  inherited;
  if DockList.ActiveObject is TScaledForm then
    friendScaledForm(DockList.ActiveObject).DoAdjustForSizing(Value);
end;

procedure TStackForm.FormDestroy(Sender: TObject);
begin
  DockList.Free;
  DeleteObjects;
end;

procedure TStackForm.PrepareBitmap;
var
  BMInfo: BITMAPINFO;
  Pixels: PPixels;
  HlPen, SdPen: HPEN;
  BgdBrush: HBRUSH;
  BrushColour: Cardinal;
begin
  DeleteObjects;
  if fInBtmSplitter then begin
    fOSYSplit := ListHeight - DockZone.Top;
  end else
    {paint docked Form title bar which disapears intermitently}
    fOSYSplit := ZoneBtm;
  GetBitmap(Canvas.Handle,  // to get compatibillity
            0,
            ClientWidth,
            ListHeight,
            BMInfo,
            fOSBitmap,
            fCDC,           // compatible context
            Pixels
     );
  HlPen := CreatePen(PS_SOLID,1,GetSysColor(COLOR_BTNHIGHLIGHT));
  SdPen := CreatePen(PS_SOLID,1,GetSysColor(COLOR_BTNSHADOW));
  BrushColour := Color;
  if BrushColour>$FFFFFF then
    BrushColour := GetSysColor(BrushColour and $FF);
  BgdBrush := CreateSolidBrush(BrushColour);
  try
    FillRect(fCDC,Rect(0,0,Width,ListHeight),BgdBrush);
    if fInBtmSplitter then begin
      Docklist.PaintTrailing(fCDC,Width,OSYSplit,HlPen,SdPen);
      Docklist.PaintLeader(fCDC,OSYSplit, Width,DockZone.Top,HlPen,SdPen);
    end else begin
      Docklist.PaintLeading(fCDC, Width, ZoneBtm, HlPen,SdPen);
      DockList.PaintTrailer(fCDC,ZoneBtm,Width,ListHeight-ZoneBtm,HlPen,SdPen);
    end;
  finally
    SelectObject(fCDC,GetStockObject(BLACK_PEN));
    DeleteObject(HlPen);
    DeleteObject(SdPen);
    DeleteObject(BgdBrush);
  end;
end;

procedure TStackForm.DeleteObjects;
begin
  if fOSBitmap<>0 then
    DeleteObject(fOSBitmap);
  fOSBitmap := 0;
  if fCDC<>0 then
    DeleteDC(fCDC);
  fCDC := 0;
end;

procedure TStackForm.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if MoveBtmSplitter or MoveTopSplitter then begin
    Message.Result := 1;
    Exit;
  end;
  inherited;
end;

procedure TStackForm.FormDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  with Source do begin
    Control.ManualFloat(Bounds(Screen.Width,Screen.Height,Control.UndockWidth,Control.UndockHeight));
    Control.ManualDock(DockZone);
  end;
    fDockZone.Show;
end;

procedure TStackForm.FormDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ShowRect: TRect;
begin
  with Source do begin
    ShowRect.TopLeft := fDockZone.ClientToScreen(fDockZone.ClientRect.TopLeft);
    ShowRect.BottomRight :=
        fDockZone.ClientToScreen(DockZone.ClientRect.BottomRight);
    DockRect := ShowRect;
  end;
end;

procedure TStackForm.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  Client.Hide; {do not want to see it docked with StackForm}
  inherited;
end;

procedure TStackForm.RefreshDocked;
begin
  DockZone.ResetBounds;
end;

procedure TStackForm.RefreshList;
begin
  DockList.Refresh;
  Invalidate;
end;

procedure TStackForm.ShowItem(Index: Integer);
begin
  fDockList.MakeActive(Index);
//  DockList.Refresh;
  fDockZone.ResetBounds();
end;

procedure TStackForm.btnReorderClick(Sender: TObject);
begin
  RefreshList;
end;

end.




