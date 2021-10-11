program Project_Listing_1;

uses
  Forms,
  Listing_1 in 'Listing_1.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
