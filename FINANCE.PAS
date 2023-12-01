{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2023
  @website(https://www.gladir.com/CODER/FINANCIALLIB-TP)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
}

Unit FINANCE;

INTERFACE

Type
 BlockArray=Array[0..255]of Real;

Function CTerm(Rate,FutureValue,PresentValue:Real):Real;
Function DDB(Cost,Salvage,Life,Period:Real):Real;
Function DoubleDecliningBalance(Cost,Salvage,Life,Period:Real):Real;
Function FV(Payment,Rate,NumPeriods:Real):Real;
Function FVal(Rate,NumPeriods,Payment,PresentValue,PType:Real):Real;
Function FutureValue(Rate,NumPeriods,Payment,PresentValue,PType:Real):Real;
Function IPAYMT(Rate,Per,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Function IRate(NumPeriods,Payment,PresentValue,FutureValue,PType:Real):Real;
Function IRR(Guess:Real;BlockCount:Word;Var Block:BlockArray):Real;
Function NetPresentValue(Rate:Real;BlockCount:Word;Var Block:BlockArray;NType:Real):Real;
Function NPer(Rate,Payment,PresentValue,FutureValue,PaymentTime:Real):Real;
Function NPV(Rate:Real;BlockCount:Word;Var Block:BlockArray;NType:Real):Real;
Function NumberOfPeriods(Rate,Payment,PresentValue,FutureValue,PaymentTime:Real):Real;
Function PAYMT(Rate,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Function PeriodPayment(Rate,Per,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Function PresentValue(Rate,NumPeriods,Payment,FutureValue,PType:Real):Real;
Function PMT(PresentValue,Rate,NumPeriods:Real):Real;
Function PPAYMT(Rate,Per,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Function PV(Payment,Rate,NumPeriods:Real):Real;
Function PVal(Rate,NumPeriods,Payment,FutureValue,PType:Real):Real;
Function Rate(FutureValue,PresentValue,NumPeriods:Real):Real;
Function SLN(Cost,Salvage,Life:Real):Real;
Function SYD(Cost,Salvage,Life,Period:Real):Real;
Function Term(Payment,Rate,FutureValue:Real):Real;

IMPLEMENTATION

Function CTerm(Rate,FutureValue,PresentValue:Real):Real;Begin
 CTerm:=Ln(FutureValue/PresentValue)/Ln(1+Rate)
End;

Function DDB(Cost,Salvage,Life,Period:Real):Real;
Var
 X:Real;
 N:Integer;
Begin
 X:=0;N:=0;
 While Period>N do Begin
  X:=2*Cost/Life;
  If(Cost-X)<Salvage Then X:=Cost-Salvage;
  If X<0 Then X:=0;
  Cost:=Cost-X;
  Inc(N);
 End;
 DDB:=X;
End;

Function DoubleDecliningBalance(Cost,Salvage,Life,Period:Real):Real;
Var
 X:Real;
 N:Integer;
Begin
 X:=0;N:=0;
 While Period>N do Begin
  X:=2*Cost/Life;
  If(Cost-X)<Salvage Then X:=Cost-Salvage;
  If X<0 Then X:=0;
  Cost:=Cost-X;
  Inc(N);
 End;
 DoubleDecliningBalance:=X;
End;

Function FV(Payment,Rate,NumPeriods:Real):Real;Begin
 If Abs(Rate)>1E-6 Then FV:=Payment*(Exp(NumPeriods*Ln(1+Rate))-1)/Rate
                   Else FV:=Payment*NumPeriods*(1+(NumPeriods-1)*Rate/2);
End;

Function FVal(Rate,NumPeriods,Payment,PresentValue,PType:Real):Real;
Var
 R:Real;
Begin
 R:=Exp(NumPeriods*Ln(1+Rate));
 If Abs(Rate)<1E-6 Then Begin
  FVal:=-Payment*NumPeriods*(1+(NumPeriods-1)*Rate/2)*(1+Rate*PType)-PresentValue*R;
 End
  Else
 Begin
  FVal:=Payment*(1-R)*(1/Rate+PType)-PresentValue*R;
 End;
End;

Function FutureValue(Rate,NumPeriods,Payment,PresentValue,PType:Real):Real;
Var
 R:Real;
Begin
 R:=Exp(NumPeriods*Ln(1+Rate));
 If Abs(Rate)<1E-6 Then Begin
  FutureValue:=-Payment*NumPeriods*(1+(NumPeriods-1)*Rate/2)*(1+Rate*PType)-PresentValue*R;
 End
  Else
 Begin
  FutureValue:=Payment*(1-R)*(1/Rate+PType)-PresentValue*R;
 End;
End;

Function IPAYMT(Rate,Per,NumPeriods,PresentValue,FutureValue,PType:Real):Real;Begin
 IPaymt:=Rate*FVal(Rate,Per-PType-1,
  Paymt(Rate,NumPeriods,PresentValue,FutureValue,PType),PresentValue,PType);
End;

Function IRate(NumPeriods,Payment,PresentValue,FutureValue,PType:Real):Real;
Var
 Rate,x0,x1,y0,y1:Real;

 Function Y:Real;
 Var
  F:Real;
 Begin
  If Abs(Rate)<1E-6 Then Begin
   Y:=PresentValue*(1+NumPeriods*Rate)+Payment*(1+Rate*PType)*NumPeriods+FutureValue;
  End
   Else
  Begin
   F:=Exp(NumPeriods*Ln(1+Rate));
   Y:=PresentValue*F+Payment*(1/Rate+PType)*(F-1)+FutureValue;
  End;
 End;

Begin
 Rate:=0;
 Y0:=PresentValue+Payment*NumPeriods+FutureValue;
 X0:=Rate;
 Rate:=Exp(1/NumPeriods)-1;
 Y1:=Y;
 X1:=Rate;
 While Abs(Y0-Y1)>1E-6 do Begin
  Rate:=(Y1*X0-Y0*X1)/(Y1-Y0);
  X0:=X1;
  X1:=Rate;
  Y0:=Y1;
  Y1:=Y;
 End;
 IRate:=Rate;
End;

Function IRR(Guess:Real;BlockCount:Word;Var Block:BlockArray):Real;
Var
 ORate,Rate:Real;

 Function DRate(Rate:Real):Real;
 Var
  NPV,NPVPrime,BlockValueI:Real;
  I:LongInt;
 Begin
  NPV:=0;
  NPVPrime:=0;
  Rate:=1/(1+Rate);
  For I:=BlockCount downto 1 do Begin
   BlockValueI:=Block[I];
   NPV:=NPV*Rate+BlockValueI;
   NPVPrime:=(NPVPrime+BlockValueI*I)*Rate;
  End;
  If Abs(NPVPrime)<1E-6 Then Drate:=NPV*1E-6
                        Else Drate:=NPV/NPVPrime;
 End;

Begin
 ORate:=Guess;
 Rate:=ORate+DRate(ORate);
 While Abs(Rate-ORate)>1E-6 do Begin
  ORate:=Rate;
  Rate:=Rate+Drate(Rate);
 End;
 IRR:=Rate;
End;

Function NumberOfPeriods(Rate,Payment,PresentValue,FutureValue,PaymentTime:Real):Real;
Var
 F:Real;
Begin
 F:=Payment*(1+Rate*PaymentTime);
 If Abs(Rate)>1E-6 Then Begin
  NumberOfPeriods:=Ln((F-Rate*FutureValue)/(PresentValue*Rate+F))/Ln(1+Rate);
 End
  Else
 Begin
  NumberOfPeriods:=-(FutureValue+PresentValue)/(PresentValue*Rate+F);
 End;
End;

Function NPer(Rate,Payment,PresentValue,FutureValue,PaymentTime:Real):Real;
Var
 F:Real;
Begin
 F:=Payment*(1+Rate*PaymentTime);
 If Abs(Rate)>1E-6 Then Begin
  NPer:=Ln((F-Rate*FutureValue)/(PresentValue*Rate+F))/Ln(1+Rate);
 End
  Else
 Begin
  NPer:=-(FutureValue+PresentValue)/(PresentValue*Rate+F);
 End;
End;

Function NetPresentValue(Rate:Real;BlockCount:Word;Var Block:BlockArray;NType:Real):Real;
Var
 X:Real;
 I:LongInt;
Begin
 X:=0;
 Rate:=1/(1+Rate);
 For I:=BlockCount downto 1 do X:=X*Rate+Block[I];
 NetPresentValue:=X*Exp((1-NType)*Ln(Rate));
End;

Function NPV(Rate:Real;BlockCount:Word;Var Block:BlockArray;NType:Real):Real;
Var
 X:Real;
 I:LongInt;
Begin
 X:=0;
 Rate:=1/(1+Rate);
 For I:=BlockCount downto 1 do X:=X*Rate+Block[I];
 NPV:=X*Exp((1-NType)*Ln(Rate));
End;

Function PAYMT(Rate,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Var
 R:Real;
begin
 R:=Exp(NumPeriods*Ln(1+Rate));
 Paymt:=(FutureValue+PresentValue*R)*Rate/((1+Rate*PType)*(1-R));
End;

Function PMT(PresentValue,Rate,NumPeriods:Real):Real;Begin
 Pmt:=PresentValue*Rate/(1-Exp(-NumPeriods*Ln(1+Rate)));
End;

Function PeriodPayment(Rate,Per,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Var
 R:Real;
Begin
 R:=Paymt(Rate,NumPeriods,PresentValue,FutureValue,PType);
 PeriodPayment:=R-Rate*FVal(Rate,Per-PType-1,R,PresentValue,PType);
End;

Function PPAYMT(Rate,Per,NumPeriods,PresentValue,FutureValue,PType:Real):Real;
Var
 R:Real;
Begin
 R:=Paymt(Rate,NumPeriods,PresentValue,FutureValue,PType);
 PPaymt:=R-Rate*FVal(Rate,Per-PType-1,R,PresentValue,PType);
End;

Function PV(Payment,Rate,NumPeriods:Real):Real;Begin
 If Abs(Rate)>1E-6 Then Begin
  PV:=Payment*(1-Exp(-NumPeriods*Ln(1+Rate)))/Rate;
 End
  Else
 Begin
  PV:=Payment*NumPeriods*(1+(NumPeriods-1)*Rate/2)/(1+NumPeriods*Rate);
 End;
End;

Function PresentValue(Rate,NumPeriods,Payment,FutureValue,PType:Real):Real;
Var
 R:Real;
Begin
 If Abs(Rate)>1E-6 Then Begin
  R:=Exp(NumPeriods*Ln(1+Rate));
  PresentValue:=(Payment*(1/Rate+PType)*(1-R)-FutureValue)/R;
 End
  else
 PresentValue:=-(Payment*(1+Rate*PType)*NumPeriods+FutureValue)/(1+NumPeriods*Rate);
End;

Function PVal(Rate,NumPeriods,Payment,FutureValue,PType:Real):Real;
Var
 R:Real;
Begin
 If Abs(Rate)>1E-6 Then Begin
  R:=Exp(NumPeriods*Ln(1+Rate));
  PVal:=(Payment*(1/Rate+PType)*(1-R)-FutureValue)/R;
 End
  else
 PVal:=-(Payment*(1+Rate*PType)*NumPeriods+FutureValue)/(1+NumPeriods*Rate);
End;

Function Rate(FutureValue,PresentValue,NumPeriods:Real):Real;Begin
 Rate:=Exp(Ln(FutureValue/PresentValue)/NumPeriods)-1;
End;

Function SLN(Cost,Salvage,Life:Real):Real;Begin
 SLN:=(Cost-Salvage)/Life;
End;

Function SYD(Cost,Salvage,Life,Period:Real):Real;Begin
 SYD:=2*(Cost-Salvage)*(Life-Period+1)/(Life*(Life+1));
End;

Function Term(Payment,Rate,FutureValue:Real):Real;Begin
 Term:=Ln(1+(FutureValue/Payment)*Rate)/Ln(1+Rate);
End;

END.
