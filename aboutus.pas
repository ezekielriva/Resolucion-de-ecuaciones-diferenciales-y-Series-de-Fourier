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
unit aboutUs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormAboutUs }

  TFormAboutUs = class(TForm)
      StaticText1: TStaticText;
      StaticText2: TStaticText;
      procedure StaticText1Click(Sender: TObject);
      procedure StaticText2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAboutUs: TFormAboutUs;

implementation

{$R *.lfm}

{ TFormAboutUs }

procedure TFormAboutUs.StaticText1Click(Sender: TObject);
begin

end;

procedure TFormAboutUs.StaticText2Click(Sender: TObject);
begin

end;

end.

