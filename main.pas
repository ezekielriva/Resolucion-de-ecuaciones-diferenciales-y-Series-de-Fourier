unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonCalcular: TButton;
    EditCantCompoenentes: TEdit;
    EditEcuacion: TEdit;
    EditPeriodo: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MemoResultado: TMemo;
    procedure ButtonCalcularClick(Sender: TObject);
    procedure EditPeriodoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ButtonCalcularClick(Sender: TObject);
begin

end;

procedure TFormMain.EditPeriodoChange(Sender: TObject);
begin

end;

procedure TFormMain.FormCreate(Sender: TObject);
begin

end;

procedure TFormMain.Label1Click(Sender: TObject);
begin

end;

procedure TFormMain.Label2Click(Sender: TObject);
begin

end;

end.

