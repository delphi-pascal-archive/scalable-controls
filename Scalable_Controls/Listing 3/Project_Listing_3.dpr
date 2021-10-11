program Project_Listing_3;

uses
  Forms,
  Listing_3Main in 'Listing_3Main.pas' {Form1},
  Unit2 in '..\..\..\Scaleable Controls Article\Listing 2_3\Unit2.pas',
  Listing_3 in 'Listing_3.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
