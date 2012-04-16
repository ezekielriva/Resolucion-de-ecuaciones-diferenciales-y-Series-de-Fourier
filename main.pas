{
 Programa que resuelve Series de Fourier y Ec. Diferenciales.
 Copyright (C) 2012
 Authors:  Gonzales Lamenza, Agustin
           Rivadeneira Lichardi, Ezequiel

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, formula, math, Grids, ExtCtrls, Buttons, aboutUs;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonLimpiar: TButton;
    ButtonCalcular: TButton;
    EditXInicial: TEdit;
    EditXFinal: TEdit;
    EditYInicial: TEdit;
    EditIntervalos: TEdit;
    EditCantCompoenentes: TEdit;
    EditEcuacion: TEdit;
    EditPeriodoMin: TEdit;
    EditPeriodoMax: TEdit;
    GroupBox1: TGroupBox;
    GroupBoxMetodo: TGroupBox;
    GroupBoxDatosEcDiferenciales: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MainMenu1: TMainMenu;
    Matriz: TStringGrid;
    MemoResultado: TMemo;
    Formula: TArtFormula;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemSFourier: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemEcDif: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItemNuevo: TMenuItem;
    MenuItemSalir: TMenuItem;
    RadioButtonEuler: TRadioButton;
    RadioButton2: TRadioButton;
    procedure ButtonCalcularClick(Sender: TObject);
    procedure ButtonLimpiarClick(Sender: TObject);
    procedure MemoResultadoChange(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItemEcDifClick(Sender: TObject);
    procedure MenuItemNuevoClick(Sender: TObject);
    procedure MenuItemSalirClick(Sender: TObject);
    procedure MenuItemSFourierClick(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButtonEulerChange(Sender: TObject);
  private
    { private declarations }
    SerieF: String;
    Metodo: String;
  public
    procedure CalcularAn(funcion:String; periodo:Double; subN: Integer);
    //procedure CalcularBn(funcion:String; periodo:Double; subN: Integer);
    procedure SerieFourierToString(funcion:String; periodo:Double; subN: Integer);
    procedure ResolucionEcDiferenciales();
    function funcion1(x, y: Extended) : Extended;
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
  if MenuItemSFourier.Checked then
  begin
      MemoResultado.Text:='=== Resultados de la Serie de Fourier ===';
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
  end
  else
  begin
      MemoResultado.Text:='=== Resultados de la Ec. Diferencial ===';
      ResolucionEcDiferenciales();
  end;
end;

procedure TFormMain.ButtonLimpiarClick(Sender: TObject);
begin
     MemoResultado.Clear;
end;

procedure TFormMain.MemoResultadoChange(Sender: TObject);
begin

end;

procedure TFormMain.MenuItem6Click(Sender: TObject);
begin
     aboutUs.FormAboutUs.Visible:=true;
end;

procedure TFormMain.MenuItemEcDifClick(Sender: TObject);
begin
  if not MenuItemEcDif.Checked then
  begin
     MenuItemEcDif.Checked:=True;
     Label2.Visible:=False;
     Label4.Visible:=False;
     EditPeriodoMin.Visible:=False;
     EditPeriodoMax.Visible:=False;
     GroupBox1.Visible:=False;
     Label1.Caption:='dy/dx=';
     GroupBoxDatosEcDiferenciales.Visible:=True;
     MemoResultado.Clear;
  end;
end;

procedure TFormMain.MenuItemNuevoClick(Sender: TObject);
begin
    EditEcuacion.Text:='';
    EditPeriodoMax.Text:='';
    EditPeriodoMin.Text:='';
    EditCantCompoenentes.Text:='';
    EditXFinal.Text:='';
    EditXInicial.Text:='';
    RadioButtonEuler.Checked:=True;
    EditYInicial.Text:='';
    EditIntervalos.Text:='';
end;

procedure TFormMain.MenuItemSalirClick(Sender: TObject);
begin
    Exit;
end;

procedure TFormMain.MenuItemSFourierClick(Sender: TObject);
begin
  if not MenuItemSFourier.Checked then
  begin
       MenuItemSFourier.Checked:=True;
       Label2.Visible:=True;
       Label4.Visible:=True;
       EditPeriodoMin.Visible:=True;
       EditPeriodoMax.Visible:=True;
       GroupBox1.Visible:=True;
       Label1.Caption:='F(x)=';
       GroupBoxDatosEcDiferenciales.Visible:=False;
       MemoResultado.Clear;
  end;
end;

procedure TFormMain.RadioButton2Change(Sender: TObject);
begin
    Metodo:='Runge';
end;

procedure TFormMain.RadioButtonEulerChange(Sender: TObject);
begin
    Metodo:='Euler';
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
  fa, fb, T, a, b, h, sum, a0: Double;
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

procedure TFormMain.ResolucionEcDiferenciales();
var
  x:Extended;
  y:Extended;
  i:integer;
  k1:Extended;
  k2:Extended;
  k3:Extended;
  k4:Extended;
  h :Extended;
begin
 if RadioButtonEuler.Checked then
   begin
        MemoResultado.Clear;
        MemoResultado.Append('=== Resultados de la Ec. Dif. por Método de Euler ===');
      { Label10.Caption := FloatToStr( (IntSupX - IntInfX) / Intervalos);  }
       h := ( StrToFloat(EditXFinal.Text) - StrToFloat(EditXInicial.Text) ) / StrToFloat(EditIntervalos.Text);
       MemoResultado.Append('h: '+FloatToStr(h));
       x := StrToFloat(EditXInicial.Text);   { X inicial }
       y := StrToFloat(EditYInicial.Text);   { y inicial }
       for i := 1 to StrToInt(EditIntervalos.Text) do   { intervalos }
         begin
           MemoResultado.Append(FloatToStr(funcion1(x,y)));
           y := y + h * funcion1(x,y);
           x := x + h;
           MemoResultado.Append('E['+FloatToStr(i)+']: ['+FloatToStr(x)+','+FloatToStr(y)+']' );
         end;
       MemoResultado.Append('Resultado: '+FloattoStr(y));      { resultado }
   end
 else if RadioButton2.Checked then
   begin
       MemoResultado.Clear;
        MemoResultado.Append('=== Resultados de la Ec. Dif. por Método de Runge Kutta ===');
       { Label10.Caption := FloatToStr( (IntSupX - IntInfX) / Intervalos);  }
       h := ( StrToFloat(EditXFinal.Text) - StrToFloat(EditXInicial.Text) ) / StrToFloat(EditIntervalos.Text);
       x := StrToFloat(EditXInicial.Text);
       y := StrToFloat(EditYInicial.Text);   { y inicial }
       for i := 1 to StrToInt(EditIntervalos.Text) do   { intervalos }
         begin
           k1 := funcion1(x,y);
           k2 := funcion1(x + (h/2), y + (h*k1)/2 );
           k3 := funcion1(x + (h/2), y + (h*k2)/2 );
           k4 := funcion1(x + h, y + (h*k3) );
           y := y + ( k1 + 2*k2 + 2*k3 + k4) * h / 6;
           x := x + h;
           MemoResultado.Append('RK['+FloatToStr(i)+']: ['+FloatToStr(x)+','+FloatToStr(y)+']' );
         end;
        MemoResultado.Append('Resultado: '+FloattoStr(y));      { resultado }
   end;
end;

function TFormMain.funcion1(x, y: Extended) : Extended;
var
  vars : array of string;
  vals : TCalcArray;
  num : integer;
  ec: String;
  s: String;
begin

 num := 2;//Cantidad de variables que uso x e y
FormMain.Formula.CaseSensitive:= True; //Nos indica si es sensible a Mayusculas y Minusculas
     FormMain.Formula.TestUsedVars:= True; //Comprueba las variables
     FormMain.Formula.NoLeadingZero:= True; //Considera o no el cero a la derecha
     FormMain.Formula.ZeroEmptyString:= True; //Considerar o no a la funcion vacia como cero

 setlength(vals,num);
 setlength(vars,num);
 //Asignamos que variables vamos a usar
 vars[0] := 'x';
 vars[1] := 'y';
 //Asignamos el VALOR de x e y

 setN(vals[0],x);
 setN(vals[1],y);
 ec := EditEcuacion.Text;
 // s := FormMain.Formula.ComputeStr(ec, cantidadVariables, @variables, @vecVariables);
 //    func := StrToFloat(s);
 s := FormMain.Formula.ComputeStr( ec,num, @vars, @vals ) ;
 funcion1 := StrToFloat(s);
end;

end.

