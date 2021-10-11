program Frenetics;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ParentScaledControls in '..\Controls\ParentScaledControls.pas',
  ScaleableMemo in '..\Controls\ScaleableMemo.pas',
  ScaleableButton in '..\Controls\ScaleableButton.pas',
  ScaleableControl in '..\Controls\ScaleableControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
