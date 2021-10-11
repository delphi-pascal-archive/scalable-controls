program Demo;

{$R 'DemoTxt.res' 'DemoTxt.rc'}

uses
  Forms,
  TestForms in 'TestForms.pas' {TestForm},
  AppMainForm in 'AppMainForm.pas' {MainForm},
  FormFolders in 'FormFolders.pas' {FormFolder},
  FormStacks in 'FormStacks.pas' {StackForm},
  WinStuff in '..\Controls\WinStuff.pas',
  DockLists in '..\Controls\DockLists.pas',
  ParentScaledControls in '..\Controls\ParentScaledControls.pas',
  ScaleableButton in '..\Controls\ScaleableButton.pas',
  ScaleableCheckBox in '..\Controls\ScaleableCheckBox.pas',
  ScaleableControl in '..\Controls\ScaleableControl.pas',
  ScaleableControlTemplate in '..\Controls\ScaleableControlTemplate.pas',
  ScaleableDBGrid in '..\Controls\ScaleableDBGrid.pas',
  ScaleableMemo in '..\Controls\ScaleableMemo.pas',
  ScaleableShape in '..\Controls\ScaleableShape.pas',
  ScaleableUpDown in '..\Controls\ScaleableUpDown.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
