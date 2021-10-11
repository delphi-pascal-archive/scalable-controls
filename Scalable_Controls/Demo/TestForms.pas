unit TestForms;

////////////////////////////////////////////////////////////////////////////////
// Scalable  Controls - Demo                                                  //
// for Delphi Magazine                                                        //
//                                                                            //
// Copyright (c) Martin Humby 2007  Freeware                                  //
////////////////////////////////////////////////////////////////////////////////

interface
//{$DEFINE USEBDE}
uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  ScaleableControl,
  ScaleableButton, ScaleableMemo, ScaleableCheckBox, ScaleableShape,
  ScaleableUpDown, ScaleableDBGrid, DB,
  {$IFDEF USEBDE} DBTables, {$ELSE} DBClient,{$ENDIF}
  Menus, StdCtrls, ComCtrls, Grids, DBGrids;
const
  FmWidth = 530;
  FmHeight = 635;
  FmCWidth = 522;
  FmCHeight = 608;
  DefFmWidth = 408;
  DefFmHeight = 382;
  DefFmCWidth = 400;
  DefFmCHeight = 355;
  FmLeft = 62;
  DgnTxt = 11;

type
  TTestForm = class(TScaledForm)
    Scroller: TEdit;
    DataSource1: TDataSource;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormStartDock(Sender: TObject;
      var DragObject: TDragDockObject);
  private
    fDBGrid: TScaleableDBGrid;
    fMemo1: TScaleableMemo;
    fMemo2: TScaleableMemo;
    fMemo2Enabled: Boolean;
    fShape: TScaleableShape;
    fButton1: TScaleableButton;
    fButton2: TScaleableButton;
    fButton3: TScaleableButton;
    fCBxGrp1: TScaleableCheckBoxGroup;
    fCBxGrp2: TScaleableCheckBoxGroup;
    fUpDnDgnWd: TScaleableUpDown;
    fUpDnSclWd: TScaleableUpDown;
    fUpDnDgnHt: TScaleableUpDown;
    fUpDnSclHt: TScaleableUpDown;
    fRealHeight: Extended;
    fRealWidth: Extended;
    fStacked: Boolean;
    fDocked: Boolean;
    fDockLocation: TWinControl;
    fDataSet: TDataSet;
    fCurrentKey: Integer;
    procedure CreateShape;
    procedure CreateDataSet;
    procedure CreateDBGrid;
    procedure CreateButtons;
    procedure CreateChkBxs;
    procedure CreateUpDns;
    procedure CreateMemos;
    procedure InitializeChkBxs;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure Cbx1Click(Sender: TObject);
    procedure Cbx2Click(Sender: TObject);
    procedure UpDnsClick(Sender: TObject; Button: TUDBtnType);
    procedure UpdateHints;
    procedure DoMemoUpdate;
    procedure Memo1SizeChange(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); {message WM_ERASEBKGND;}
  protected
    procedure DoAdjustForSizing(const Value: Boolean); override;
  public 
    procedure AdjustForSizing(const Value: Boolean); override;
    function ManualDock(NewDockSite: TWinControl; DropControl: TControl = nil;
      ControlSide: TAlign = alNone): Boolean;
    procedure ToggleMemo2Enabled;    
    procedure SetDims(ClientWd, ClientHt: Integer);
    property Memo1: TScaleableMemo read fMemo1;
    property Memo2: TScaleableMemo read fMemo2;
    property Docked: Boolean read fDocked;
    property Stacked: Boolean read fStacked;
    property DockLocation: TWinControl read fDockLocation;
    property DataSet: TDataSet read fDataSet;
    property CurrentKey: Integer read fCurrentKey write fCurrentKey;
  end;

implementation

uses
  DockLists, FormStacks, FormFolders, AppMainForm;

{$R *.dfm}

const
  CtrlsLeft = FmWidth-220;
  DBGridLeft = 25;
  Mem1Left = DBGridLeft;
  Mem1Width = 248;
  Mem2Left = CtrlsLeft;
  Mem2Width = 198;
  DBGridWidth = CtrlsLeft+Mem2Width-DBGridLeft;
  BtnOS = 25;
  Btn1Left = Mem1Left+BtnOS;
  Btn2Left = Mem1Left+Mem1Width-BtnOS-75;
  UDLeft = CtrlsLeft + 105;
  ShpLeft = Btn1Left+75 div 2 + 1;
  ShpWidth = FmWidth-ShpLeft-4;

  DBGridTop = 18;
  BtnsTop = FmHeight-70;
  UDTTop = BtnsTop-13;
  DBGridHeight = 198;
  Mem1Top = DBGridTop+DBGridHeight+15;
  Mem2Top = Mem1Top;
  Mem1Height = BtnsTop-DBGridTop-DBGridHeight-25;
  CbxTop =  FmHeight-223;
  Mem2Height = CbxTop-Mem2Top-10;

procedure SetScaling(Ctrl: TScaleableMemo; Scaling: TScaling; Incl: Boolean);
begin
  if Incl then
    Ctrl.Scalings := Ctrl.Scalings + [Scaling]
  else
    Ctrl.Scalings := Ctrl.Scalings - [Scaling];
end;

procedure SetAnchors(Ctrl: TScaleableMemo; Anchor: TAnchorKind; Incl: Boolean);
begin
  if Incl then
    Ctrl.Anchors := Ctrl.Anchors + [Anchor]
  else
    Ctrl.Anchors := Ctrl.Anchors - [Anchor];
end;

{ TTestForm }

procedure TTestForm.AdjustForSizing(const Value: Boolean);
begin
  inherited;
  fMemo1.DoubleBuffered := not Value;
  fDBGrid.PaintTwice := Value;
  if not Value then fDBGrid.ResetDims;
end;

procedure TTestForm.btn1Click(Sender: TObject);
begin
  fRealWidth := fRealWidth * 8/7;
  fRealHeight := fRealHeight * 8/7;
  SetBounds(Left,Top,Trunc(fRealWidth),Trunc(fRealHeight));
  UpdateHints;
end;

procedure TTestForm.btn2Click(Sender: TObject);
begin
  fRealWidth := fRealWidth * 7/8;
  fRealHeight := fRealHeight * 7/8;
  SetBounds(Left,Top,Trunc(fRealWidth),Trunc(fRealHeight));
  UpdateHints;
end;

procedure TTestForm.btn3Click(Sender: TObject);
begin
  SetDims(fmCWidth,fmCHeight);
end;

procedure TTestForm.Cbx1Click(Sender: TObject);
begin
  if Sender = fCbxGrp1[0] then
    SetScaling(fMemo1, scXPos, fCbxGrp1[0].Checked)
  else if Sender = fCbxGrp1[1] then
    SetScaling(fMemo1, scYPos, fCbxGrp1[1].Checked)
  else if Sender = fCbxGrp1[2] then
    SetScaling(fMemo1, scWidth, fCbxGrp1[2].Checked)
  else if Sender = fCbxGrp1[3] then
    SetScaling(fMemo1, scHeight, fCbxGrp1[3].Checked)
  else if Sender = fCbxGrp1[4] then
    SetScaling(fMemo1, scXCentred, fCbxGrp1[4].Checked)
  else if Sender = fCbxGrp1[5] then
    SetScaling(fMemo1, scYCentred, fCbxGrp1[5].Checked)
  else if Sender = fCbxGrp1[6] then
    SetScaling(fMemo1, scText, fCbxGrp1[6].Checked);
  UpdateHints;
end;

procedure TTestForm.Cbx2Click(Sender: TObject);
begin
  if Sender = fCbxGrp2[0] then
    SetAnchors(fMemo1, akLeft, fCbxGrp2[0].Checked)
  else if Sender = fCbxGrp2[1] then
    SetAnchors(fMemo1, akTop, fCbxGrp2[1].Checked)
  else if Sender = fCbxGrp2[2] then
    SetAnchors(fMemo1, akRight, fCbxGrp2[2].Checked)
  else if Sender = fCbxGrp2[3] then
    SetAnchors(fMemo1, akBottom, fCbxGrp2[3].Checked)
  else if Sender = fCbxGrp2[4] then
    fMemo1.NoUpScaling := fCbxGrp2[4].Checked;
  UpdateHints;
end;

procedure TTestForm.CreateButtons;
begin
  fButton1 := TScaleableButton.Create(Self);
  fButton1.Parent := Self;
  with fButton1 do begin
    Top := BtnsTop;
    Left := Btn1Left;
    Font.Size := 16;
    Caption := '+';
    Anchors := [akLeft,akBottom];
    Scalings := [scXpos,scYPos,scWidth,scHeight,scText];
    NoUpScaling := True;
    OnClick := btn1Click;
  end;
  fButton2 := TScaleableButton.Create(Self);
  fButton2.Parent := Self;
  with fButton2 do begin
    Top := BtnsTop;
    Left := Btn2Left;
    Font.Name := 'Arial';
    Font.Size := 16;
    Caption := '-';
    Anchors := [akLeft,akBottom];
    Scalings := [scXpos,scYPos,scWidth,scHeight,scText];
    NoUpScaling := True;
    OnClick := btn2Click;
  end;
  fButton3 := TScaleableButton.Create(Self);
  fButton3.Parent := Self;
  with fButton3 do begin
    Top := BtnsTop;
    Left := CtrlsLeft;
    Width := 80;
    Font.Name := 'Arial';
    Font.Size := 10;
    Caption := 'DesignSize';
    Anchors := [akRight,akBottom];
    Scalings := [scXpos,scYPos,scWidth,scHeight,scText];
    NoUpScaling := True;
    OnClick := btn3Click;
  end;
  fMemo1.BringToFront;
end;

procedure TTestForm.CreateChkBxs;
begin
  fCbxGrp1 := TScaleableCheckBoxGroup.Create(Self);
  with fCBxGrp1 do begin
    Anchors := [akRight,akBottom];
    Colour := clBtnFace;
    NoUpScaling := True;
    Left := CtrlsLeft;
    Top := CbxTop;
    MaxBoxes := 7;
    OnClick := Cbx1Click;
    Font.Name := 'Arial';
    Font.Size := 10;
    Add('scXPos');
    Add('scYPos');
    Add('scWidth');
    Add('scHeight');
    Add('scXCentred');
    Add('scYCentred');
    Add('scText');
  end;
  fCbxGrp2 := TScaleableCheckBoxGroup.Create(Self);
  with fCBxGrp2 do begin
    Anchors := [akRight,akBottom];
    Colour := clBtnFace;
    NoUpScaling := True;
    Left := CtrlsLeft + 101;
    Top := CbxTop;
    MaxBoxes := 5;
    OnClick := Cbx2Click;
    Font.Name := 'Arial';
    Font.Size := 10;
    Add('akLeft');
    Add('akTop');
    Add('akRight');
    Add('akBottom');
    Add('NoUpscaling');
  end;
end;

procedure TTestForm.CreateDataSet;
begin
  DataSource1.OnDataChange := DataSource1DataChange;
  fDataSet := MainForm.DataSetCache.AquireRandom;
  DataSource1.DataSet := fDataSet;
end;

procedure TTestForm.CreateDBGrid;
begin
  fDBGrid := TScaleableDBGrid.Create(Self);
//  fDBGrid.BorderStyle := bsNone;
  fDBGrid.Parent := Self;
  with fDBGrid do begin                             {test H scrolling}
    SetDesignBounds(DBGridLeft,DBGridTop,DBGridWidth{-150},DBGridHeight);
    Anchors := [akTop,akLeft,akRight];
    Scalings := [scXpos,scYPos,scWidth,scHeight,scText];
    NoUpScaling := True;
    Font.Name := 'Arial';
    Font.Size := 10;
    TitleFont.Name := 'Arial';
    TitleFont.Size := 10;
    TitleFont.Style := [fsBold];
    MaxFontSize := 10;
    MinFontSize := 2;
    FitColumnsToWidth := True; {rem out for H scrolling}
    DataSource := DataSource1;
    if DataSource.DataSet = nil then Exit;
    Columns[3].Title.Caption := 'Gndr.';
    HideHScrollBar := True;  {rem out for H scrolling}
{    Columns[3].Width := 50;
    Columns[3].Visible := False;
    FitColsWidth;     {test hidden col.}
  end;
//  DoubleBuffered := True;
end;

procedure TTestForm.CreateMemos;
var
  RStream: TResourceStream;
begin
  fMemo1 := TScaleableMemo.Create(Self);
  with fMemo1 do begin
    DoubleBuffered := True;
    Parent := Self;
    SetDesignBounds(Mem1Left,Mem1Top,Mem1Width,Mem1Height);
    Font.Name := 'Arial';
    Font.Size := DgnTxt;
    MaxFontSize := 13;
    Anchors := [akLeft,akTop,akRight,akBottom];
    Scalings := [scXpos,scYPos,scWidth,scHeight,scText];
    ScrollBars := ssVertical;
    NoUpScaling := True;
    ShowHint := True;
    OnSizeChange := Memo1SizeChange;
    RStream := TResourceStream.Create(HInstance,'DEM1','TEXT');
    try
      fMemo1.Lines.LoadFromStream(RStream);
    finally
      RStream.Free;
    end;
    Memo1SizeChange(Self);
  end;
  fMemo2 := TScaleableMemo.Create(Self);
  with fMemo2 do begin
    SetDesignBounds(CtrlsLeft,Mem2Top,Mem2Width,Mem2Height);
    Font.Name := 'Arial';
    Font.Size := 12;
    Anchors := [akRight,akTop,akBottom];
    Scalings := [scXpos,scYPos,scWidth,scHeight,scText];
    ScrollBars := ssVertical;
    NoUpScaling := True;
    Parent := Self;
    Lines.Add('Memo 2');
    Enabled := False;
  end;
end;

procedure TTestForm.CreateShape;
begin
  fShape := TScaleableShape.Create(Self);
  fShape.Parent := Self;
  with fShape do begin
    SetDesignBounds(Memo2.Left,Memo2.BoundsRect.Bottom,
      Memo2.Width, fButton3.BoundsRect.Bottom - Memo2.BoundsRect.Bottom);
    Anchors := [akLeft,akRight,akTop,akBottom];
    Scalings := [scXpos,scYPos,scWidth,scHeight];
    Brush.Color := clBtnFace;
    NoUpScaling := True;
    Parent := Self;
  end;
end;

procedure TTestForm.CreateUpDns;
begin
  fUpDnDgnWd := TScaleableUpDown.Create(Self);
  with fUpDnDgnWd do begin
    Anchors := [akRight,akBottom];
    ShowHint := True;
    Orientation := udHorizontal;
    SetDesignBounds(UDLeft,UDTTop,48,18);
    NoUpScaling := True;
    Parent := Self;
    Max := 2000;
    OnClick := UpDnsClick;
  end;
  fUpDnSclWd := TScaleableUpDown.Create(Self);
  with fUpDnSclWd do begin
    Anchors := [akRight,akBottom];
    ShowHint := True;
    Orientation := udHorizontal;
    SetDesignBounds(UDLeft,UDTTop+20,48,18);
    NoUpScaling := True;
    Parent := Self;
    Max := 2000;
    OnClick := UpDnsClick;
  end;
  fUpDnDgnHt := TScaleableUpDown.Create(Self);
  with fUpDnDgnHt do begin
    Anchors := [akRight,akBottom];
    ShowHint := True;
    Orientation := udVertical;
    SetDesignBounds(UDLeft+50,UDTTop,18,38);
    Parent := Self;
    NoUpScaling := True;
    Max := 2000;
    OnClick := UpDnsClick;
  end;
  fUpDnSclHt := TScaleableUpDown.Create(Self);
  with fUpDnSclHt do begin
    Anchors := [akRight,akBottom];
    ShowHint := True;
    Orientation := udVertical;
    SetDesignBounds(UDLeft+70,UDTTop,18,38);
    Parent := Self;
    NoUpScaling := True;
    Max := 2000;
    OnClick := UpDnsClick;
  end;
end;

procedure TTestForm.DataSource1DataChange(Sender: TObject; Field: TField);
var
  SNum: Integer;
  FName: string;
  SName: string;
begin
  if not Assigned(fDataSet) then Exit;
  with fDataSet do begin
    SNum := FieldByName('Staff No').Value;
    FName := FieldByName('Given Name').Value;
    SName := FieldByName('Surname').Value;
  end;
  fCurrentKey := SNum;
  Caption := Format('%4.4d: %s %s',[SNum, FName, SName]);
  if Stacked then begin
    TStackForm(DockLocation).Refresh;
    TStackForm(DockLocation).Invalidate;
  end;
end;

procedure TTestForm.DoAdjustForSizing(const Value: Boolean);
var
  CVisible: Boolean;
begin
  if ControlsAdjusted=Value then Exit;
  inherited;
  CVisible := not Value;
  fButton1.Visible := CVisible;
  fButton2.Visible := CVisible;
  fButton3.Visible := CVisible;
  fCBxGrp1.Visible := CVisible;
  fCBxGrp2.Visible := CVisible;
  fUpDnDgnWd.Visible := CVisible;
  fUpDnSclWd.Visible := CVisible;
  fUpDnDgnHt.Visible := CVisible;
  fUpDnSclHt.Visible := CVisible;
  if CVisible then Invalidate;
end;

procedure TTestForm.DoMemoUpdate;
begin
  if not fMemo2Enabled then Exit;
  Memo2.Lines.Add(Format('%d, %d, %d, %d',[Memo1.Left,Memo1.Top,Memo1.Width,Memo1.Height]));
  Memo2.Lines.Add(Format('[%d, %d, %d, %d]',[Width,Height,ClientWidth,ClientHeight]));
end;

procedure TTestForm.FormCreate(Sender: TObject);
begin
//  Color := $BAB099;
  ClientWidth := FmCWidth;
  ClientHeight := FmCHeight;
  fRealWidth := Width;
  fRealHeight := Height;
  CreateDataSet;
  CreateDBGrid;
  CreateMemos;
  CreateUpDns;
  CreateButtons;
  CreateChkBxs;
  InitializeChkBxs;
//  CreateShape;
  fUpDnDgnWd.Position := fMemo1.DesignWidth;
  fUpDnSclWd.Position := fMemo1.Width;
  fUpDnDgnHt.Position := fMemo1.DesignHeight;
  fUpDnSclHt.Position := fMemo1.Height;
  UpdateHints;
  FormHide(Self); {free dataset}
//  DoubleBuffered := True; {does not improve flicker on resize}
  PopupMenu := MainForm.PopupMenu1;
end;

procedure TTestForm.FormEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  fStacked := False;
  fDocked := False;
  if Target=nil then Exit;
  Assert((Target is TWinControl), 'Bad Dock Target');
  if TWinControl(Target).Parent is TStackForm then begin
    fStacked := True;
    fDockLocation := TWinControl(Target);
  end;
end;

procedure TTestForm.FormResize(Sender: TObject);
begin
  if ClientHeight < 1 then ClientHeight := 1; {fix undock problem}
  if Assigned(fMemo1) then begin
    DoMemoUpdate;
  end else
    Exit;
  with fUpDnDgnWd do begin
    Position := fMemo1.DesignWidth;
    Hint := 'Design Width: '+IntToStr(fMemo1.DesignWidth);
  end;
  with fUpDnSclWd do begin
    Position := fMemo1.Width;
    Hint := 'Scaled Width: '+IntToStr(fMemo1.Width);
  end;
  with fUpDnDgnHt do begin
    Position := fMemo1.DesignHeight;
    Hint := 'Design Height: '+IntToStr(fMemo1.DesignHeight);
  end;
  with fUpDnSclHt do begin
    Position := fMemo1.Height;
    Hint := 'Scaled Height: '+IntToStr(fMemo1.Height);
  end;
  fRealWidth := Width;
  fRealHeight := Height;
end;

procedure TTestForm.InitializeChkBxs;
begin
  with fMemo1 do begin
    fCbxGrp1[0].Checked := scXPos in Scalings;
    fCbxGrp1[1].Checked := scYPos in Scalings;
    fCbxGrp1[2].Checked := scWidth in Scalings;
    fCbxGrp1[3].Checked := scHeight in Scalings;
    fCbxGrp1[4].Checked := scXCentred in Scalings;
    fCbxGrp1[5].Checked := scYCentred in Scalings;
    fCbxGrp1[6].Checked := scText in Scalings;
    fCbxGrp2[0].Checked := akLeft in Anchors;
    fCbxGrp2[1].Checked := akTop in Anchors;
    fCbxGrp2[2].Checked := akRight in Anchors;
    fCbxGrp2[3].Checked := akBottom in Anchors;
    fCbxGrp2[4].Checked := noUpScaling;
  end;
end;

procedure TTestForm.Memo1SizeChange(Sender: TObject);
begin
  with fMemo1 do
    Hint := Format('%d : %d : %d : %d  txt:%d',
            [Left,Top,Width,Height, ScaledFontSize])+#13+
            Format('%d : %d : %d : %d  txt:%d',
            [DesignLeft,DesignTop,DesignWidth,DesignHeight,
             DesignFontSize]);
end;

procedure TTestForm.SetDims(ClientWd, ClientHt: Integer);
begin
  ClientWidth := ClientWd;
  ClientHeight := ClientHt;
  fRealWidth := Width;
  fRealHeight := Height;
  UpdateHints;
end;

procedure TTestForm.ToggleMemo2Enabled;
begin
  fMemo2.Enabled := not fMemo2Enabled;
  fMemo2Enabled := not fMemo2Enabled;
end;

procedure TTestForm.UpdateHints;
begin
  fUpDnDgnWd.Hint := 'Design Width: '+IntToStr(fMemo1.DesignWidth);
  fUpDnSclWd.Hint := 'Scaled Width: '+IntToStr(fMemo1.Width);
  fUpDnDgnHt.Hint := 'Design Height: '+IntToStr(fMemo1.DesignHeight);
  fUpDnSclHt.Hint := 'Scaled Height: '+IntToStr(fMemo1.Height);
  fUpDnSclWd.Position := fMemo1.Width;
  fUpDnSclHt.Position := fMemo1.Height;
  fUpDnDgnWd.Position := fMemo1.DesignWidth;
  fUpDnDgnHt.Position := fMemo1.DesignHeight;
end;

procedure TTestForm.UpDnsClick(Sender: TObject; Button: TUDBtnType);
begin
  if Sender = fUpDnDgnWd then begin
    fMemo1.DesignWidth := fUpDnDgnWd.Position;
  end else if Sender = fUpDnDgnHt then begin
    fMemo1.DesignHeight := fUpDnDgnHt.Position;
  end else if Sender = fUpDnSclWd then begin
    fMemo1.Width := fUpDnSclWd.Position;
  end else if Sender = fUpDnSclHt then begin
    fMemo1.Height := fUpDnSclHt.Position;
  end;
  UpdateHints;
  DoMemoUpdate;
end;

procedure TTestForm.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if ControlsAdjusted then
    with Canvas do begin
      Brush.Color := Color;
      FillRect(ClientRect);
      Brush.Color := clBtnFace;
      FillRect(fButton1.BoundsRect);
      FillRect(fButton2.BoundsRect);
      Pen.Color := clBtnFace;
//      Pen.Width := 2;
//      DrawRect(Canvas, Rect(Memo2.Left, fCBxGrp1[0].Top,Memo2.BoundsRect.Right,
//                      fButton3.BoundsRect.Bottom));
//      FillRect(Rect(Memo2.Left, fCBxGrp1[1].Top,Memo2.BoundsRect.Right,
//                      fButton3.BoundsRect.Bottom));

FillRect(Rect(Memo2.Left, fButton3.Top,Memo2.BoundsRect.Right,
                      fButton3.BoundsRect.Bottom));
      Message.Result := 1;
    end else
      inherited;
end;

procedure TTestForm.FormShow(Sender: TObject);
begin
  if fDataSet=nil then
  try
    fDataSet := MainForm.DataSetCache.AquireDataSet(CurrentKey);
    DataSource1.DataSet := fDataSet;
    if fDataset<>nil then
      fDBGrid.RescaleGrid;
  except
    on EOSError do;
  end;
end;

procedure TTestForm.FormHide(Sender: TObject);
begin
  DataSource1.DataSet := nil;
  MainForm.DataSetCache.Release(fDataSet);
  fDataSet := nil;
end;

function TTestForm.ManualDock(NewDockSite: TWinControl;
  DropControl: TControl; ControlSide: TAlign): Boolean;
begin
  if (NewDockSite is TDockListZone) then begin
    fDocked := False;
    fStacked := True;
    fDockLocation := TWinControl(NewDockSite);
  end;
  Result := inherited ManualDock(NewDockSite,DropControl,ControlSide);
end;


procedure TTestForm.FormActivate(Sender: TObject);
begin
  Invalidate;
end;

procedure TTestForm.FormStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  //
end;

end.
