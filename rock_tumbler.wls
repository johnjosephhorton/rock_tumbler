#!/usr/bin/env wolframscript

NumChips = ToExpression[$ScriptCommandLine[[2]]];
outfile = $ScriptCommandLine[[3]]

Chip[p_] := Module[{corner, total, plus1, minus1, newp},
  total = Length[p];
  corner = RandomSample[Range[Length[p]], 1] // First;
  plus1 = If[corner == total, 1, corner + 1]; 
  minus1 = If[corner == 1, total, corner - 1]; 
  theta1 = Random[UniformDistribution[{0, 1}]];
  new1 = theta1*p[[corner]] + (1 - theta1)*p[[plus1]];
  theta2 = Random[UniformDistribution[{0, 1}]];
  new2 = theta2*p[[corner]] + (1 - theta2)*p[[minus1]];
  newp = p;
  newp[[corner]] = new1;
  newp = Insert[newp, new2, corner];
  newp
  ]

p = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};

g = Table[Table[Graphics[Polygon[Nest[Chip, p, NumChips]]], {10}], {10}];
Export[outfile, GraphicsGrid[g, Frame -> True]]