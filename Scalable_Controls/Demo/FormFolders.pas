unit FormFolders;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus;

type
  TFormFolder = class(TForm)
    PageControl: TPageControl;
    PopupMenu1: TPopupMenu;
    MultiLineTabs1: TMenuItem;
    procedure MultiLineTabs1Click(Sender: TObject);
    procedure PageControlGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses
  AppMainForm, TestForms;

{$R *.dfm}

procedure TFormFolder.MultiLineTabs1Click(Sender: TObject);
begin
  MultiLineTabs1.Checked := not MultiLineTabs1.Checked;
  PageControl.MultiLine := MultiLineTabs1.Checked;
end;

procedure TFormFolder.PageControlGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  //
end;

end.
