unit AppMainForm;

////////////////////////////////////////////////////////////////////////////////
// Scalable Controls - Demo                                                  //
// for Delphi Magazine                                                        //
//                                                                            //
// Copyright (c) Martin Humby 2007  Freeware                                  //
////////////////////////////////////////////////////////////////////////////////


interface
//{$DEFINE USEBDE}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DB,
  {$IFDEF USEBDE} DBTables, {$ELSE} DBClient,{$ENDIF}
  DockLists, FormFolders, FormStacks, TestForms, Menus, StdCtrls;

const
  DefTestFormCWidth = 402;
  DefTestFormCHeight = 408;
  FolderForms = 20;
  StackForms = 30;

type
  EDataSetCache = class(Exception);
  TDataSetElement = record
    DataSet: TDataSet;
    InUse: Boolean;
  end;
  TDSCacheChange = procedure(Sender: Tobject; SetsCreated, SetsInUse: Integer) of object;
  TDataSetCache = class(TComponent)
  private
    fDataSets: array of TDataSetElement;
    fFreeSets: Integer;
    fSetsCreated: Integer;
    fKey: Integer;
    fOnChange: TDSCacheChange;
    function NewDataset: TDataSet;
  public
    constructor Create(AOwner: TComponent); override;
    function AquireDataSet(Key: Integer): TDataSet;
    function AquireRandom: TDataSet;
    procedure Release(DSet: TDataSet);
    property FreeSets: Integer read fFreeSets;
    property SetsCreated: Integer read fSetsCreated;
    property OnChange: TDSCacheChange read fOnChange write fOnChange;
  end;


  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    Actions: TMenuItem;
    NewForm2: TMenuItem;
    PopupMenu1: TPopupMenu;
    Stack1: TMenuItem;
    N3: TMenuItem;
    ShowScroller1: TMenuItem;
    Memo1Scaling1: TMenuItem;
    MaxTextSize1: TMenuItem;
    MemoVisible1: TMenuItem;
    EnableMemo21: TMenuItem;
    N4: TMenuItem;
    DesignSize1: TMenuItem;
    DefaultSize1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    ShowFormStack1: TMenuItem;
    ShowFormFolder1: TMenuItem;
    Panel1: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Panel2: TPanel;
    Label3: TLabel;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure AppPopUpPopup(Sender: TObject);
    procedure Stack1Click(Sender: TObject);
    procedure NewForm1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ShowScroller1Click(Sender: TObject);
    procedure Memo1Scaling1Click(Sender: TObject);
    procedure MaxTextSize1Click(Sender: TObject);
    procedure MemoVisible1Click(Sender: TObject);
    procedure EnableMemo21Click(Sender: TObject);
    procedure DesignSize1Click(Sender: TObject);
    procedure DefaultSize1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure ShowFormStack1Click(Sender: TObject);
    procedure ShowFormFolder1Click(Sender: TObject);
  private
    fDataSetCache: TDataSetCache;
    fStackForm: TStackForm;
    fFormFolder: TFormFolder;
    fTestForm: TTestForm;
    procedure DataSetCacheChange(Sender: Tobject;
      SetsCreated, SetsInUse: Integer);
  public
    property StackForm: TStackForm read fStackForm;
    property FormFolder: TFormFolder read fFormFolder;
    property DataSetCache: TDataSetCache read fDataSetCache;
    property TestForm: TTestForm read fTestForm;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  AppClientHt = 30;


procedure TMainForm.FormCreate(Sender: TObject);
var
  Ix, WorkWd,WorkHt: Integer;
  Fm: TTestForm;
  OFont: TFont;  
begin
  WorkHt := Screen.WorkAreaHeight;
  WorkWd := Screen.WorkAreaWidth;
  BoundsRect := Rect(0,0,WorkWd-410,Height);
  fFormFolder := TFormFolder.Create(Self);
  fFormFolder.SetBounds(10,Height,485,Screen.WorkAreaHeight-Self.Height);
  with fFormFolder do begin
    ClientWidth := 475;
    if ClientHeight>654 then
      ClientHeight := 654;
    Show;
    with PageControl do begin
      OFont := TFont.Create;
      OFont.Assign(Canvas.Font);
      Canvas.Font.Assign(Label1.Font);
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(150,160,'Please Wait');
      Canvas.TextOut(75,210,'while Forms are created');
      Canvas.Font.Assign(OFont);
      OFont.Free;
    end;
  end;
  fStackForm := TStackForm.Create(Self);
  fStackForm.BoundsRect := Rect(WorkWd-410,0,WorkWd,WorkHt);
  fDataSetCache := TDataSetCache.Create(Self);
  fDataSetCache.OnChange := DataSetCacheChange;
  fStackForm.PassiveDock := True;
  for Ix := 1 to 25 do begin
    Fm := TTestForm.Create(nil);
    with Fm do begin
      SetDims(DefTestFormCWidth,DefTestFormCHeight);
      ManualDock(fStackForm.DockZone);
      Show;
    end;
  end;
  fStackForm.PassiveDock := False;
  for Ix := 1 to 15 do begin
    Fm := TTestForm.Create(nil);
    with Fm do begin
      SetDims(DefTestFormCWidth,DefTestFormCHeight);
      ManualDock(fFormFolder.PageControl);
      Show;
    end;
  end;
  fStackForm.Show;
  fStackForm.ShowItem(3);
end;

var
  MD: Boolean;


procedure TMainForm.Shape1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MD := True;

end;

procedure TMainForm.Shape1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin 
  MD := False;

end;

procedure TMainForm.Shape1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MD then
    Height := Height + Y;
end;

{ TDataSetCache }

function TDataSetCache.AquireDataSet(Key: Integer): TDataSet;
var
  Ix: Integer;
begin
  Result := nil;
  //Exit; { does it hold up if fails?}
  if FreeSets>0 then begin
    for Ix := 0 to Pred(fSetsCreated) do
      if not fDataSets[Ix].InUse then begin
        Result := fDataSets[Ix].DataSet;
        fDataSets[Ix].InUse := True;
        Dec(fFreeSets);
        Break;
      end;
  end else begin
    if fSetsCreated = Length(fDataSets) then
      SetLength(fDataSets, fSetsCreated+4);
    fDataSets[fSetsCreated].DataSet := NewDataset;
    fDataSets[fSetsCreated].InUse := True;
    Result := fDataSets[fSetsCreated].DataSet;
    Inc(fSetsCreated);
  end;
  if Key<>0 then
{$IFDEF USEBDE}
    TTable(Result).FindKey([Key]);
{$ELSE}
    TClientDataset(Result).FindKey([Key]);
{$ENDIF}
  if Assigned(fOnChange) then
    fOnChange(Self,SetsCreated,SetsCreated-FreeSets);
end;

function TDataSetCache.AquireRandom: TDataSet;
begin
  Result := AquireDataSet(0);
  if Result = nil then Exit;
  fKey := (fKey+Random(6)+1)mod 186;
{$IFDEF USEBDE}
  while not TTable(Result).FindKey([fKey]) do
{$ELSE}
  while not TClientDataset(Result).FindKey([fKey]) do
{$ENDIF}
    Inc(fKey);
end;

constructor TDataSetCache.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(fDataSets,12);
end;

function TDataSetCache.NewDataset: TDataSet;
begin
{$IFDEF USEBDE}
  Result := TTable.Create(Self);
  with TTable(Result) do begin
    DatabaseName := 'SCALING_DEMO';
    TableName := 'Employee.DB';
    Active := True;
  end;                     
{$ELSE}
  Result := TClientDataSet.Create(Self);
  TClientDataSet(Result).FileName := ('Employees.cds');
  TClientDataSet(Result).IndexName := 'DEFAULT_ORDER';
  TClientDataSet(Result).Active := True;
{$ENDIF}
end;

procedure TDataSetCache.Release(DSet: TDataSet);
var
  Ix: Integer;
begin
  for Ix := 0 to Pred(fSetsCreated) do
    if fDataSets[Ix].DataSet=DSet then begin
      fDataSets[Ix].InUse := False;
      Inc(fFreeSets);
      Break;
    end;
  if Assigned(fOnChange) then
    fOnChange(Self,SetsCreated,SetsCreated-FreeSets);
end;

procedure TMainForm.AppPopUpPopup(Sender: TObject);
begin
  fTestForm := Sender as TTestForm;
  
end;

procedure TMainForm.Stack1Click(Sender: TObject);
begin
  with fTestForm do begin
    if HostDockSite<>nil then begin
      Hide;
      ManualFloat(Bounds(0,0,UndockWidth, UndockHeight));
    end;
    ManualDock(StackForm.DockZone);
  end;
end;

procedure TMainForm.NewForm1Click(Sender: TObject);
var
  Fm: TTestForm;
begin
  Fm := TTestForm.Create(nil);
  Fm.SetDims(DefTestFormCWidth,DefTestFormCHeight);
  Fm.Top := Height;
  Fm.Left := Width div 2;
  Fm.Show;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  fTestForm := PopupMenu1.PopUpComponent as TTestForm;
  with fTestForm do begin
    Stack1.Enabled := not (HostDocksite is TDockListZone);
    if Scroller.Visible then
      ShowScroller1.Caption := 'Hide Scroller'
    else
      ShowScroller1.Caption := 'Show Scroller';
    if Memo1.Visible then
      MemoVisible1.Caption := 'Hide Memo 1'
    else
      MemoVisible1.Caption := 'Show Memo 1';
    if Memo1.DoScaling then
      Memo1Scaling1.Caption := 'Memo1: No Scaling'
    else
      Memo1Scaling1.Caption := 'Memo1: Do Scaling';
    if Memo1.MaxFontSize = 13 then
      MaxTextSize1.Caption := 'Memo1: Text -- No Max'
    else
      MaxTextSize1.Caption := 'Memo1: Text Size 13';
    if Memo2.Enabled then
      EnableMemo21.Caption := 'Disable Memo 2'
    else
      EnableMemo21.Caption := 'Enable Memo 2'
  end;
end;

procedure TMainForm.ShowScroller1Click(Sender: TObject);
begin
  with fTestForm do begin
    Scroller.SetBounds(Width+Width div 3,Height+Height div 3,Scroller.Width,Scroller.Height);
    Scroller.Visible := not fTestForm.Scroller.Visible;
  end;
end;

procedure TMainForm.Memo1Scaling1Click(Sender: TObject);
begin
  fTestForm.Memo1.DoScaling := not fTestForm.Memo1.DoScaling;
end;

procedure TMainForm.MaxTextSize1Click(Sender: TObject);
begin
  with fTestForm do
    if Memo1.MaxFontSize < 255 then
      Memo1.MaxFontSize := 255
    else
      Memo1.MaxFontSize := 13;
end;

procedure TMainForm.MemoVisible1Click(Sender: TObject);
begin
  fTestForm.Memo1.Visible := not fTestForm.Memo1.Visible;
end;

procedure TMainForm.EnableMemo21Click(Sender: TObject);
begin
  fTestForm.ToggleMemo2Enabled;
end;

procedure TMainForm.DesignSize1Click(Sender: TObject);
begin
  with fTestForm do
    SetDims(fmCWidth,fmCHeight);
end;

procedure TMainForm.DefaultSize1Click(Sender: TObject);
begin
  fTestForm.SetDims(DefTestFormCWidth, DefTestFormCHeight);
end;

procedure TMainForm.Close1Click(Sender: TObject);
begin
  fTestForm.Close;
end;

procedure TMainForm.ShowFormStack1Click(Sender: TObject);
begin
  StackForm.Show;
end;

procedure TMainForm.ShowFormFolder1Click(Sender: TObject);
begin
  fFormFolder.Show;
end;


procedure TMainForm.DataSetCacheChange(Sender: Tobject; SetsCreated,
  SetsInUse: Integer);
begin
  Label3.Caption := Format('Datasets: %2d  In use: %2d',[SetsCreated,SetsInUse]);
end;

end.
