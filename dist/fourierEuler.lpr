program fourierEuler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, aformula, aboutUs
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
    Application.CreateForm(TFormAboutUs, FormAboutUs);
  Application.Run;
end.

