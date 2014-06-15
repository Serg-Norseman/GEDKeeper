using System;
using System.Collections.Generic;

using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCalculatorPlugin
{
	[Serializable]
	public class CalculateException : Exception
	{
		public CalculateException() {}
		public CalculateException(string message) : base(message) {}
	}

	// FIXME: перестроить, вычистить весь код, оптимизировать
	public sealed class ExtCalculator : IDisposable
	{
		public class NamedVar
		{
			public string Name;
			public double Value;
		}

		#region Private members

		private enum CallbackType : byte
		{
			ctGetValue,
			ctSetValue,
			ctFunction
		}

		private enum TToken : byte
		{
			tkEOF,
			tkERROR,
			tkASSIGN,
			tkLBRACE,
			tkRBRACE,
			tkNUMBER,
			tkIDENT,
			tkSEMICOLON,
			tkPOW,
			tkINV,
			tkNOT,
			tkMUL,
			tkDIV,
			tkMOD,
			tkPER,
			tkADD,
			tkSUB,
			tkLT,
			tkLE,
			tkEQ,
			tkNE,
			tkGE,
			tkGT,
			tkOR,
			tkXOR,
			tkAND
		}

		private double fvalue;
		private string svalue;
		private string FExpression;
		private int FPtr;
		private TToken FToken;
		private readonly List<NamedVar> FVars;
		private bool fDisposed;

		#endregion

		#region Instance control

		public ExtCalculator()
		{
			this.FVars = new List<NamedVar>();
		}

		public void Dispose()
		{
			if (!this.fDisposed)
			{
				this.ClearVars();
				//this.FVars.Dispose();
				this.fDisposed = true;
			}
		}

		public void ClearVars()
		{
			this.FVars.Clear();
		}

		#endregion

		#region Static utilities

		private static void RaiseError(string msg)
		{
			throw new CalculateException(msg);
		}

		private static double bfloat(bool B)
		{
			return ((B) ? 1.0 : 0.0);
		}

		private static double fmod(double x, double y)
		{
			return (x - Int((x / y)) * y);
		}

		private static double Int(double value)
		{
			return ((value > (double)0f) ? Math.Floor(value) : Math.Ceiling(value));
		}

		private static double Frac(double value)
		{
			return (value - Int(value));
		}

		private static bool DefCalcFunc(string S, ref double V)
		{
			bool result = true;

			if (S == "round")
			{
				V = ((double)checked((long)Math.Round(V)));
			}
			else if (S == "trunc")
			{
				V = ((double)Math.Truncate(V));
			}
			else if (S == "int")
			{
				V = Int(V);
			}
			else if (S == "frac")
			{
				V = Frac(V);
			}
			else if (S == "sin")
			{
				V = Math.Sin(V);
			}
			else if (S == "cos")
			{
				V = Math.Cos(V);
			}
			else if (S == "tan")
			{
				V = Math.Tan(V);
			}
			else if (S == "atan")
			{
				V = Math.Atan(V);
			}
			else if (S == "ln")
			{
				V = Math.Log(V);
			}
			else if (S == "exp")
			{
				V = Math.Exp(V);
			}
			else if (S == "sign")
			{
				if (V > (double)0f)
				{
					V = 1.0;
				}
				else
				{
					if (V < (double)0f)
					{
						V = -1.0;
					}
				}
			}
			else if (S == "sgn")
			{
				if (V > (double)0f)
				{
					V = 1.0;
				}
				else
				{
					if (V < (double)0f)
					{
						V = 0.0;
					}
				}
			}
			else if (S == "xsgn") {
				if (V < (double)0f)
				{
					V = 0.0;
				}
			} else {
				result = false;
			}

			return result;
		}

		private static bool DefCalcProc(CallbackType ctype, string S, ref double V)
		{
			bool result = true;

			switch (ctype) {
				case CallbackType.ctGetValue:
					if (S == "pi") {
						V = 3.1415926535897931;
					} else if (S == "e") {
						V = 2.718281828;
					} else {
						result = false;
					}
					break;

				case CallbackType.ctSetValue:
					result = false;
					break;

				case CallbackType.ctFunction:
					result = DefCalcFunc(S, ref V);
					break;
			}

			return result;
		}

		#endregion

		#region Valid code

		private bool Callback(CallbackType ctype, string name, ref double res)
		{
			bool result = DefCalcProc(ctype, name, ref res);

			if (!result) {
				result = true;

				switch (ctype) {
					case CallbackType.ctGetValue:
						res = this.GetVar(name);
						break;
					case CallbackType.ctSetValue:
						this.SetVar(name, res);
						break;
					case CallbackType.ctFunction:
						result = false;
						break;
				}
			}

			return result;
		}

		private void term(ref double R)
		{
			switch (this.FToken)
			{
				case TToken.tkLBRACE:
					this.lex();
					this.expr6(ref R);
					if (this.FToken == TToken.tkRBRACE) {
						this.lex();
					} else {
						RaiseError("Syntax error");
					}
					break;

				case TToken.tkNUMBER:
					R = this.fvalue;
					this.lex();
					break;

				case TToken.tkIDENT:
					{
						string st = this.svalue.ToLower();
						this.lex();

						if (this.FToken == TToken.tkLBRACE) {
							this.lex();
							this.expr6(ref R);
							if (this.FToken == TToken.tkRBRACE) {
								this.lex();
							} else {
								RaiseError("Syntax error");
							}

                            if (!this.Callback(CallbackType.ctFunction, st, ref R)) {
								RaiseError("Unknown function or variable \"" + st + "\".");
							}
						} else {
							if (this.FToken == TToken.tkASSIGN) {
								this.lex();
								this.expr6(ref R);
								if (!this.Callback(CallbackType.ctSetValue, st, ref R)) {
									RaiseError("Unknown function or variable \"" + st + "\".");
								}
							} else {
								if (!this.Callback(CallbackType.ctGetValue, st, ref R)) {
									RaiseError("Unknown function or variable \"" + st + "\".");
								}
							}
						}
					}
					break;

				default:
					RaiseError("Syntax error.");
					break;
			}
		}

		private void expr1(ref double R)
		{
			this.term(ref R);

            if (this.FToken == TToken.tkPOW)
			{
				this.lex();
				double V = 0.0;
				this.term(ref V);
				R = Math.Pow(R, V);
			}
		}

		private void expr2(ref double R)
		{
			TToken fToken = this.FToken;
			if (fToken >= TToken.tkINV && (fToken < TToken.tkMUL || (fToken >= TToken.tkADD && fToken < TToken.tkLT)))
			{
				TToken oldt = this.FToken;
				this.lex();
				this.expr2(ref R);

				switch (oldt) {
					case TToken.tkINV:
						R = ((double)(~SysUtils.Trunc(R)));
						break;
					case TToken.tkNOT:
						R = ExtCalculator.bfloat(SysUtils.Trunc(R) <= 0);
						break;
					case TToken.tkSUB:
						R = (-R);
						break;
				}
			} else {
				this.expr1(ref R);
			}
		}

		private void expr3(ref double R)
		{
			this.expr2(ref R);
			while (true)
			{
				if (this.FToken < TToken.tkMUL || this.FToken >= TToken.tkADD) break;

				TToken oldt = this.FToken;
				this.lex();
				double V = 0.0;
				this.expr2(ref V);

				switch (oldt) {
					case TToken.tkMUL:
						R = (R * V);
						break;
					case TToken.tkDIV:
						R = (R / V);
						break;
					case TToken.tkMOD:
						R = ((double)(SysUtils.Trunc(R) % SysUtils.Trunc(V)));
						break;
					case TToken.tkPER:
						R = ((R / V) * 100.0);
						break;
				}
			}
		}

		private void expr4(ref double R)
		{
			this.expr3(ref R);
			while (true)
			{
				if (this.FToken < TToken.tkADD || this.FToken >= TToken.tkLT) break;

				TToken oldt = this.FToken;
				this.lex();
				double V = 0.0;
				this.expr3(ref V);

				switch (oldt) {
					case TToken.tkADD:
						R = (R + V);
						break;
					case TToken.tkSUB:
						R = (R - V);
						break;
				}
			}
		}

		private void expr5(ref double R)
		{
			this.expr4(ref R);
			while (true)
			{
				if (this.FToken < TToken.tkLT || this.FToken >= TToken.tkOR) break;

				TToken oldt = this.FToken;
				this.lex();
				double V = 0.0;
				this.expr4(ref V);

				switch (oldt) {
					case TToken.tkLT:
						R = ExtCalculator.bfloat(R < V);
						break;
					case TToken.tkLE:
						R = ExtCalculator.bfloat(R <= V);
						break;
					case TToken.tkEQ:
						R = ExtCalculator.bfloat(R == V);
						break;
					case TToken.tkNE:
						R = ExtCalculator.bfloat(R != V);
						break;
					case TToken.tkGE:
						R = ExtCalculator.bfloat(R >= V);
						break;
					case TToken.tkGT:
						R = ExtCalculator.bfloat(R > V);
						break;
				}
			}
		}

		private void expr6(ref double R)
		{
			this.expr5(ref R);
			while (true)
			{
				if (this.FToken < TToken.tkOR || this.FToken >= (TToken)26) break;

				TToken oldt = this.FToken;
				this.lex();
				double V = 0.0;
				this.expr5(ref V);

				switch (oldt) {
					case TToken.tkOR:
						R = ((double)(SysUtils.Trunc(R) | SysUtils.Trunc(V)));
						break;
					case TToken.tkXOR:
						R = ((double)(SysUtils.Trunc(R) ^ SysUtils.Trunc(V)));
						break;
					case TToken.tkAND:
						R = ((double)(SysUtils.Trunc(R) & SysUtils.Trunc(V)));
						break;
				}
			}
		}

		private void start(ref double R)
		{
			this.expr6(ref R);

			while (this.FToken == TToken.tkSEMICOLON)
			{
				this.lex();
				this.expr6(ref R);
			}

			if (this.FToken != TToken.tkEOF)
			{
				RaiseError("Syntax error");
			}
		}

		private bool ConvertNumber(int first, int last, ushort @base)
		{
			this.fvalue = 0.0;
			if (first < last)
			{
				do
				{
					byte c = (byte)((int)this.FExpression[first - 1] - 48);
					if (c > 9)
					{
						c -= 7;
						if (c > 15)
						{
							c -= 32;
						}
					}
					if ((ushort)c >= @base)
					{
						break;
					}
					this.fvalue = (this.fvalue * @base + c);
					first++;
				}
				while (first < last);
			}
			return first == last;
		}

		#endregion

		private void lex()
		{
			while (this.FExpression[this.FPtr - 1] != '\0' && this.FExpression[this.FPtr - 1] <= ' ')
			{
				this.FPtr++;
			}

			this.FToken = TToken.tkEOF;

			if (this.FExpression[this.FPtr - 1] != '\0')
			{
				int s_pos = this.FPtr;
				this.FToken = TToken.tkNUMBER;
				if (this.FExpression[this.FPtr - 1] == '$')
				{
					this.FPtr++;
					while (true)
					{
						char c2 = this.FExpression[this.FPtr - 1];
						if (c2 < '0' || (c2 >= ':' && (c2 < 'A' || (c2 >= 'I' && (c2 < 'a' || c2 >= 'i')))))
						{
							break;
						}
						this.FPtr++;
					}

					if (this.ConvertNumber(s_pos, this.FPtr, 16))
					{
						return;
					}
				}
				else
				{
					char c3 = this.FExpression[this.FPtr - 1];
					if (c3 >= '0' && c3 <= '9')
					{
						if (this.FExpression[this.FPtr - 1] == '0')
						{
							this.FPtr++;
							char c4 = this.FExpression[this.FPtr - 1];
							if (c4 == 'X' || c4 == 'x')
							{
								this.FPtr++;
								s_pos = this.FPtr;
								while (true)
								{
									char c5 = this.FExpression[this.FPtr - 1];
									if (c5 < '0' || (c5 >= ':' && (c5 < 'A' || (c5 >= 'I' && (c5 < 'a' || c5 >= 'i')))))
									{
										break;
									}
									this.FPtr++;
								}

								if (this.ConvertNumber(s_pos, this.FPtr, 16))
								{
									return;
								}
								goto Error;
							}
							else
							{
								char c6 = this.FExpression[this.FPtr - 1];
								if (c6 == 'B' || c6 == 'b')
								{
									this.FPtr++;
									s_pos = this.FPtr;
									while (true)
									{
										char c7 = this.FExpression[this.FPtr - 1];
										if (c7 < '0' || c7 >= '2')
										{
											break;
										}
										this.FPtr++;
									}

									if (this.ConvertNumber(s_pos, this.FPtr, 2))
									{
										return;
									}
									goto Error;
								}
							}
						}
						while (true)
						{
							char c8 = this.FExpression[this.FPtr - 1];
							if (c8 < '0' || (c8 >= ':' && (c8 < 'A' || (c8 >= 'G' && (c8 < 'a' || c8 >= 'g')))))
							{
								break;
							}
							this.FPtr++;
						}
						char c9 = this.FExpression[this.FPtr - 1];
						if (c9 == 'H' || c9 == 'h')
						{
							if (this.ConvertNumber(s_pos, this.FPtr, 16))
							{
								this.FPtr++;
								return;
							}
						}
						else
						{
							char c10 = this.FExpression[this.FPtr - 1];
							if (c10 == 'B' || c10 == 'b')
							{
								if (this.ConvertNumber(s_pos, this.FPtr, 2))
								{
									this.FPtr++;
									return;
								}
							}
							else
							{
								if (this.ConvertNumber(s_pos, this.FPtr, 10))
								{
									if (this.FExpression[this.FPtr - 1] == '`')
									{
										this.fvalue = (this.fvalue * 3.1415926535897931 / 180.0);
										this.FPtr++;
										double frac = 0.0;
										while (true)
										{
											char c11 = this.FExpression[this.FPtr - 1];
											if (c11 < '0' || c11 >= ':')
											{
												break;
											}
											frac = (frac * 10.0 + (double)((int)this.FExpression[this.FPtr - 1] - 48));
											this.FPtr++;
										}
										this.fvalue = (this.fvalue + frac * 3.1415926535897931 / 180.0 / 60.0);
										if (this.FExpression[this.FPtr - 1] == '`')
										{
											this.FPtr++;
											frac = 0.0;
											while (true)
											{
												char c12 = this.FExpression[this.FPtr - 1];
												if (c12 < '0' || c12 >= ':')
												{
													break;
												}
												frac = (frac * 10.0 + (double)((int)this.FExpression[this.FPtr - 1] - 48));
												this.FPtr++;
											}
											this.fvalue = (this.fvalue + frac * 3.1415926535897931 / 180.0 / 60.0 / 60.0);
										}
										this.fvalue = ExtCalculator.fmod(this.fvalue, 6.2831853071795862);
										return;
									}
									if (this.FExpression[this.FPtr - 1] == '.')
									{
										this.FPtr++;
										double frac = 1.0;
										while (true)
										{
											char c13 = this.FExpression[this.FPtr - 1];
											if (c13 < '0' || c13 >= ':')
											{
												break;
											}
											frac = (frac / 10.0);
											this.fvalue = (this.fvalue + frac * (double)((int)this.FExpression[this.FPtr - 1] - 48));
											this.FPtr++;
										}
									}
									char c14 = this.FExpression[this.FPtr - 1];
									if (c14 != 'E' && c14 != 'e')
									{
										return;
									}
									this.FPtr++;
									int exp = 0;
									char sign = this.FExpression[this.FPtr - 1];
									char c15 = this.FExpression[this.FPtr - 1];
									if (c15 == '+' || c15 == '-')
									{
										this.FPtr++;
									}
									char c16 = this.FExpression[this.FPtr - 1];
									if (c16 >= '0' && c16 < ':')
									{
										while (true)
										{
											char c17 = this.FExpression[this.FPtr - 1];
											if (c17 < '0' || c17 >= ':')
											{
												break;
											}
											exp = exp * 10 + (int)this.FExpression[this.FPtr - 1] - 48;
											this.FPtr++;
										}
										if (exp == 0)
										{
											this.fvalue = 1.0;
											return;
										}
										if (sign == '-')
										{
											if (exp > 0)
											{
												do
												{
													this.fvalue = (this.fvalue * 10.0);
													exp--;
												}
												while (exp > 0);
												return;
											}
											return;
										}
										else
										{
											if (exp > 0)
											{
												do
												{
													this.fvalue = (this.fvalue / 10.0);
													exp--;
												}
												while (exp > 0);
												return;
											}
											return;
										}
									}
								}
							}
						}
					}
					else
					{
						char c18 = this.FExpression[this.FPtr - 1];
						if (c18 >= 'A' && (c18 < '[' || c18 == '_' || (c18 >= 'a' && c18 < '{')))
						{
							this.svalue = new string(this.FExpression[this.FPtr - 1], 1);
							this.FPtr++;
							while (true)
							{
								char c19 = this.FExpression[this.FPtr - 1];
								if (c19 < '0' || (c19 >= ':' && (c19 < 'A' || (c19 >= '[' && c19 != '_' && (c19 < 'a' || c19 >= '{')))))
								{
									break;
								}
								string text = this.svalue;
								if (((text != null) ? text.Length : 0) >= 32)
								{
									break;
								}
								this.svalue += this.FExpression[this.FPtr - 1];
								this.FPtr++;
							}
							this.FToken = TToken.tkIDENT;
							return;
						}

						char c = this.FExpression[this.FPtr - 1];
						this.FPtr++;

						switch (c)
						{
							case '!': //chk
							{
								this.FToken = TToken.tkNOT;
								if (this.FExpression[this.FPtr - 1] == '=')
								{
									this.FPtr++;
									this.FToken = TToken.tkNE;
									return;
								}
								return;
							}
							case '%': //chk
							{
								this.FToken = TToken.tkMOD;
								if (this.FExpression[this.FPtr - 1] == '%')
								{
									this.FPtr++;
									this.FToken = TToken.tkPER;
									return;
								}
								return;
							}
							case '&': //chk
							{
								this.FToken = TToken.tkAND;
								return;
							}
							case '(': //chk
							{
								this.FToken = TToken.tkLBRACE;
								return;
							}
							case ')': //chk
							{
								this.FToken = TToken.tkRBRACE;
								return;
							}
							case '*': //chk
							{
								this.FToken = TToken.tkMUL;
								if (this.FExpression[this.FPtr - 1] == '*')
								{
									this.FPtr++;
									this.FToken = TToken.tkPOW;
									return;
								}
								return;
							}
							case '+': //chk
							{
								this.FToken = TToken.tkADD;
								return;
							}
							case '-': //chk
							{
								this.FToken = TToken.tkSUB;
								return;
							}
							case '/': //chk
							{
								this.FToken = TToken.tkDIV;
								return;
							}
							case ';': //chk
							{
								this.FToken = TToken.tkSEMICOLON;
								return;
							}
							case '<': //chk
							{
								this.FToken = TToken.tkLT;
								if (this.FExpression[this.FPtr - 1] == '=')
								{
									this.FPtr++;
									this.FToken = TToken.tkLE;
									return;
								}
								if (this.FExpression[this.FPtr - 1] == '>')
								{
									this.FPtr++;
									this.FToken = TToken.tkNE;
									return;
								}
								return;
							}
							case '=': //chk
							{
								this.FToken = TToken.tkASSIGN;
								if (this.FExpression[this.FPtr - 1] == '=')
								{
									this.FPtr++;
									this.FToken = TToken.tkEQ;
									return;
								}
								return;
							}
							case '>': //chk
							{
								this.FToken = TToken.tkGT;
								if (this.FExpression[this.FPtr - 1] == '=')
								{
									this.FPtr++;
									this.FToken = TToken.tkGE;
									return;
								}
								if (this.FExpression[this.FPtr - 1] == '<')
								{
									this.FPtr++;
									this.FToken = TToken.tkNE;
									return;
								}
								return;
							}
							case '^': //chk
							{
								this.FToken = TToken.tkXOR;
								return;
							}
							case '|': //chk
							{
								this.FToken = TToken.tkOR;
								return;
							}
							case '~': //chk
							{
								this.FToken = TToken.tkINV;
								return;
							}
							default: //chk
								{
									this.FToken = TToken.tkERROR;
									this.FPtr--;
									return;
								}
						}
					}
				}

				Error:
				this.FToken = TToken.tkERROR;
			}
		}

		#region Public

		public double GetVar(string name)
		{
			int num = this.FVars.Count - 1;
			for (int i = 0; i <= num; i++) {
				NamedVar V = this.FVars[i];

				if (string.Compare(V.Name, name, false) == 0) {
					return V.Value;
				}
			}

			throw new CalculateException("Unknown function or variable \"" + name + "\".");
		}

		public void SetVar(string name, double value)
		{
			NamedVar V = null;

			int num = this.FVars.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				NamedVar nv = this.FVars[i];
				if (string.Compare(nv.Name, name, false) == 0) {
					V = nv;
				}
			}

			if (V == null)
			{
				V = new NamedVar();
				V.Name = name;
				this.FVars.Add(V);
			}

			V.Value = value;
		}

		public double Calc(string expression)
		{
			this.FExpression = expression + "\0";
			this.FPtr = 1;
			this.lex();

			double result = 0.0;
			this.start(ref result);
			return result;
		}

		#endregion
	}
}
