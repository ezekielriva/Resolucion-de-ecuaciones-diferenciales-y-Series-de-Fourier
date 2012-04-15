unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, formula, math, Grids;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonLimpiar: TButton;
    ButtonCalcular: TButton;
    EditCantCompoenentes: TEdit;
    EditEcuacion: TEdit;
    EditPeriodoMin: TEdit;
    EditPeriodoMax: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    Matriz: TStringGrid;
    MemoResultado: TMemo;
    Formula: TArtFormula;
    procedure ButtonCalcularClick(Sender: TObject);
    procedure ButtonLimpiarClick(Sender: TObject);
    procedure EditPeriodoMinChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);

  private
    { private declarations }
    SerieF: String;
  public
    procedure CalcularAn(funcion:String; periodo:Double; subN: Integer);
    //procedure CalcularBn(funcion:String; periodo:Double; subN: Integer);
    procedure SerieFourierToString(funcion:String; periodo:Double; subN: Integer);
    function CalcularComponentes(funcion, s:String; periodo, ax:Double; subN: Integer):String;
    function func(ec:String;x:Double):Double;
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
  i :Integer;
  Tmax, Tmin :String;
begin
  SerieF:='';
  Tmax := EditPeriodoMax.Text;
  Tmin:= EditPeriodoMin.Text;
  { T = Periodo = Tmax - Tmin }
  if ( Pos('pi',EditPeriodoMax.Text) <> 0 ) then
     EditPeriodoMax.Text:=FloatToStr(func(StringReplace(EditPeriodoMax.Text, 'pi', 'x',[rfReplaceAll, rfIgnoreCase]),pi));
  if (  Pos('pi',EditPeriodoMin.Text) <> 0 ) then
     EditPeriodoMin.Text:=FloatToStr(func(StringReplace(EditPeriodoMin.Text, 'pi', 'x',[rfReplaceAll, rfIgnoreCase]),pi));

  T := StrToFloat(EditPeriodoMax.Text) - StrToFloat(EditPeriodoMin.Text);
  EditPeriodoMax.Text := Tmax;
  EditPeriodoMin.Text := Tmin;
  MemoResultado.Append('Periodo de la Función: '+FloatToStr(T));

  //Calculo los coeficientes
  CalcularAn(EditEcuacion.Text, T , 0);
  for i:=1 to StrToInt(EditCantCompoenentes.Text) do
  begin
       if (i <> 1) then
          SerieF:=SerieF+'+';
       SerieFourierToString(EditEcuacion.Text, T, i); //Calculo de Serie
  end;
  MemoResultado.Append('Serie de Fourier='+SerieF);
end;

procedure TFormMain.ButtonLimpiarClick(Sender: TObject);
begin
  MemoResultado.Text:='=== Resultados de la Serie de Fourier ===';
end;

procedure TFormMain.EditPeriodoMinChange(Sender: TObject);
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

//Armar Serie de Fourier Serie de Fourier
procedure TFormMain.SerieFourierToString(funcion:String; periodo:Double; subN: Integer);
var
  fan, fbn, parcialA, parcialB: String;
  ax: Double;
begin
  ax := func('2*x*'+FloatToStr(pi)+'/'+FloatToStr(periodo)+'*180/'+FloatToStr(pi),subN);
  fan := funcion+'* cos('+FloatToStr(ax)+'*x)';
  fbn := funcion+'* sin('+FloatToStr(ax)+'*x)';

  //Calcular An + Bn
  parcialA := CalcularComponentes(fan,'A',periodo,ax,subN); //Calculamos An
  parcialB := CalcularComponentes(fbn,'B',periodo,ax,subN); //Calculamos Bn ;
  if ( parcialA = '0') then
  begin
     SerieF := SerieF + parcialB;
  end
  else
  begin
     if ( parcialB = '0') then
     begin
        SerieF := SerieF + parcialA;
     end
     else
     begin
         SerieF := SerieF + parcialA + ' + ' + parcialB //Calculamos Bn
     end;
  end;
end;

// Calcula el coeficiente An.
// @Parametros: Funcion, Periodo, n
procedure TFormMain.CalcularAn(funcion:String; periodo:Double; subN: Integer);
var
  fa, fb, T, a, b, h, sum, a0: Double;
  n, k, m: Integer;
  j: String;
begin
  {             b=T/2
     A0 = 2/T Integral f(t) dt
               a=-T/2
  }
  T :=  periodo;
  b := T/2; a := -T/2;
  Matriz.RowCount := 1;
  Matriz.ColCount:= 1;
  if (subN = 0) then
  begin
       fa := FormMain.func(funcion,a);
       fb := FormMain.func(funcion,b);
       //Approximate the definite integral of f from a to b by Romberg's method.
       //    eps is the desired accuracy."""
       Matriz.Cells[0,0] := FloatToStr( (0.5)*( b - a ) * (fa+fb) );  //R[0][0]

       n := 1;
       While True do
       begin
            h := (b - a) / (2**n); // h = hn
            Matriz.RowCount := Matriz.RowCount + 1; //Agregamos una fila vacia
            sum := 0;
            for k := 1 to 2**(n-1) do
                sum := sum + func( funcion, a + (2*k-1)*h );  // Sumatoria
            Matriz.Cells[0,n] := FloatToStr( (0.5)*StrToFloat(Matriz.Cells[0,n-1])+(h*sum) );  //R[n][0]

            for m:=1 to n+1 do
            begin
                 Matriz.ColCount:= Matriz.ColCount +1;
                 if ( Matriz.Cells[m-1,n-1] = '') then
                    Matriz.Cells[m-1,n-1] := '0';
                 Matriz.Cells[m,n] := FloatToStr( StrToFloat(Matriz.Cells[m-1,n]) + ( StrToFloat(Matriz.Cells[m-1,n]) - StrToFloat(Matriz.Cells[m-1,n-1] ) )/  ((4**m)-1) );
            end;

            if abs( StrToFloat(Matriz.Cells[n-1,n]) - StrToFloat(Matriz.Cells[n,n]) ) < 0.00000000001 Then
            begin
                 a0 := StrToFloat(Matriz.Cells[n,n])*2/T;
                 if ( a0 <> 0 ) then
                    SerieF := FormatFloat('0.#####', a0);
                 MemoResultado.Append('Componente A0 = '+FloatToStr(a0));
                 Break;
            end;
            n := n + 1;
       end;
  end;
end;

{
  Calculamos Componente
  Return: An * cos ax*t o Bn * sin ax*t
}
function TFormMain.CalcularComponentes(funcion, s:String; periodo,ax:Double; subN: Integer): String;
var
  fa, fb, T, a, b, h, sum, a0, parcial: Double;
  n, k, m: Integer;
begin
  T :=  periodo;
  b := T/2; a := -T/2;
  Matriz.RowCount := 1;
  Matriz.ColCount:= 1;
  fa := func(funcion,a);
  fb := func(funcion,b);
  //Approximate the definite integral of f from a to b by Romberg's method.
  //    eps is the desired accuracy."""
  Matriz.Cells[0,0] := FloatToStr( (0.5)*( b - a ) * (fa+fb) );  //R[0][0]
  n := 1;
  While True do
  begin
       h := (b - a) / (2**n); // h = hn
       Matriz.RowCount := Matriz.RowCount + 1; //Agregamos una fila vacia
       sum := 0;
       for k := 1 to 2**(n-1) do
           sum := sum + func( funcion, a + (2*k-1)*h );  // Sumatoria
       Matriz.Cells[0,n] := FloatToStr( (0.5)*StrToFloat(Matriz.Cells[0,n-1])+(h*sum) );  //R[n][0]
       for m:=1 to n+1 do
       begin
            Matriz.ColCount:= Matriz.ColCount +1;
            if ( Matriz.Cells[m-1,n-1] = '') then
               Matriz.Cells[m-1,n-1] := '0';
            Matriz.Cells[m,n] := FloatToStr( StrToFloat(Matriz.Cells[m-1,n]) + ( StrToFloat(Matriz.Cells[m-1,n]) - StrToFloat(Matriz.Cells[m-1,n-1]) )/  ((4**m)-1) );
       end;
       if abs( StrToFloat(Matriz.Cells[n-1,n]) - StrToFloat(Matriz.Cells[n,n]) ) < 0.00000000001 Then
       begin
            a0 := StrToFloat(Matriz.Cells[n,n]) * 2 / T;
            if (s = 'A') then
            begin
                 MemoResultado.Append('Componente A'+IntToStr(subN)+' = '+FloatToStr(a0));
                 if (a0 <> 0) then
                    CalcularComponentes := FormatFloat('0.#####', a0) + '*cos ('+FormatFloat('0.###', ax)+'t)';
                 CalcularComponentes:='0';
            end
            else
            begin
                 MemoResultado.Append('Componente B'+IntToStr(subN)+' = '+FloatToStr(a0));
                 CalcularComponentes := FormatFloat('0.#####', a0) + '*sin ('+FormatFloat('0.###', ax)+'t)';
            end;
            Break;
       end;
       n := n + 1;
  end;
end;

// Calcula el coeficiente Bn.
// @Parametros: Funcion, Periodo, n
{procedure TFormMain.CalcularBn(funcion:String; periodo:Double; subN: Integer);
var
  fa, fb, T, a, b, h, sum, a0, parcial: Double;
  n, k, m: Integer;
  j: String;
begin
  {             b=T/2
     Bn = 2/T Integral f(t).sen [(2nPi/T)t] dt
               a=-T/2
  }
  T :=  periodo;
  b := T/2; a := -T/2;
  Matriz.RowCount := 1;
  Matriz.ColCount:= 1;
  {}
       j:=funcion+'* sin(2*'+IntToStr(subN)+'*'+FloatToStr(pi)+'*x/'+FloatToStr(T)+')';
       fa := FormMain.func(j,a);
       fb := FormMain.func(j,b);

end;      }

{ Funcion que calcula el valor de f(a) siendo a = cte }
function TFormMain.func(ec:String; x:Double):Double;
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
     s := FormMain.Formula.ComputeStr(ec, cantidadVariables, @variables, @vecVariables);
     func := StrToFloat(s);
end;

end.

