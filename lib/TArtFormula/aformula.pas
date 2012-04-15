{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit aformula; 

interface

uses
  formula, formulaf, formulan, formulanf, AMask, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('formula', @formula.Register); 
  RegisterUnit('formulan', @formulan.Register); 
end; 

initialization
  RegisterPackage('aformula', @Register); 
end.
