unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, formula, math, Grids;

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
    Matriz: TStringGrid;
    MemoResultado: TMemo;
    Formula: TArtFormula;
    procedure ButtonCalcularClick(Sender: TObject);
    procedure EditPeriodoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);

  private
    { private declarations }
    SerieF: String;
  public
    procedure CalcularAn(funcion:String; periodo:Double; subN: Integer);
    function func(x:Double):Double;
    { public declarations }

  end; 

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ButtonCalcularClick(Sender: TObject);
var
  T :Double;
begin
  if (EditPeriodo.Text = 'Pi') then
  begin
       T := 3.14;
  end
  else
  begin
       T:= StrToFloat(EditPeriodo.Text)
  end;
  CalcularAn(EditEcuacion.Text, T , 0);   //Calculo A0
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

// Calcula el coeficiente An.
// @Parametros: Funcion, Periodo, n?
procedure TFormMain.CalcularAn(funcion:String; periodo:Double; subN: Integer);
var
  fa, fb, T, R0, a, b, h, sum, a0: Double;
  n, k, m: Integer;
begin
  {             a=T/2
     An = 2/T Integral f(t).cos [(2nPi/T)t] dt
               b=-T/2
  }
  T :=  periodo;
  b := T/2; a := -T/2;
  fa := FormMain.func(a);
  fb := FormMain.func(b);
  Matriz.RowCount := 1;
  Matriz.ColCount:= 1;
  if (subN = 0) then
  begin
       //Approximate the definite integral of f from a to b by Romberg's method.
       //    eps is the desired accuracy."""
       Matriz.Cells[0,0] := FloatToStr( (0.5)*( b - a ) * (fa+fb) );  //R[0][0]

       n := 1;
       While True do
       begin
            h := (b - a) / (2**n); // h = hn
            MemoResultado.Append( FloatToStr(h));
            Matriz.RowCount := Matriz.RowCount + 1; //Agregamos una fila vacia
            sum := 0;
            for k := 1 to 2**(n-1) do
                sum := sum + func( a + (2*k-1)*h );  // Sumatoria
            Matriz.Cells[0,n] := FloatToStr( (0.5)*StrToFloat(Matriz.Cells[n-1,0])+(h*sum) );  //R[n][0]

            for m:=1 to n+1 do
            begin
                 Matriz.ColCount:= Matriz.ColCount +1;
                 if ( Matriz.Cells[m-1,n-1] = '') then
                    Matriz.Cells[m-1,n-1] := '0';
                 Matriz.Cells[m,n] := FloatToStr( StrToFloat(Matriz.Cells[m-1,n]) + ( StrToFloat(Matriz.Cells[m-1,n]) - StrToFloat(Matriz.Cells[m-1,n-1] ) )/  ((4**m)-1) );
            end;

            if abs( StrToFloat(Matriz.Cells[n-1,n]) - StrToFloat(Matriz.Cells[n,n]) ) < 0.00000001 Then
            begin
                 a0 := StrToFloat(Matriz.Cells[n,n])*2/T;
                 if ( a0 <> 0 ) then
                    SerieF := FloatToStr(a0);
                 MemoResultado.Append('A0 = '+FloatToStr(a0));
                 Break;
            end;
            n := n + 1;
       end;

  end;

end;

{ Funcion que calcula el valor de f(a) siendo a = cte }
function TFormMain.func(x:Double):Double;
var
  // Vector de variables. Contiene las variables.
  variables: array of string;
  //Vector de valores. Contiene los valores de las variables, como ser, x=10, y=2, etc.
  vecVariables: TCalcArray;
  //Entero que especifica la cantidad de variables que uso
  cantidadVariables: Integer;
  s: String;
begin
     FormMain.Formula.CaseSensitive:= True; //Nos indica si es sensible a Mayusculas y Minusculas
     FormMain.Formula.TestUsedVars:= True; //Comprueba las variables
     FormMain.Formula.NoLeadingZero:= True; //Considera o no el cero a la derecha
     FormMain.Formula.ZeroEmptyString:= True; //Considerar o no a la funcion vacia como cero
     cantidadVariables := 1;
     SetLength(vecVariables, cantidadVariables);
     SetLength(variables, cantidadVariables);
     variables[0] := 'x';
     setN(vecVariables[0],x);
     //Realizamos el calculo de la funcion y agregamos el resultado al Memo.
     s := FormMain.Formula.ComputeStr(FormMain.EditEcuacion.Text, cantidadVariables, @variables, @vecVariables);
     func := StrToFloat(s);
end;

end.

