using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMDateInterpreted : TGEDCOMDate
	{
		private string FDatePhrase;

		public string DatePhrase
		{
			get	{ return this.FDatePhrase; }
			set	{ this.SetDatePhrase(value); }
		}

		private void SetDatePhrase([In] string Value)
		{
			this.FDatePhrase = Value;
			string fDatePhrase = this.FDatePhrase;
			if (((fDatePhrase != null) ? fDatePhrase.Length : 0) > 0)
			{
				if (this.FDatePhrase[0] == '(')
				{
					this.FDatePhrase = this.FDatePhrase.Remove(0, 1);
				}
				string fDatePhrase2 = this.FDatePhrase;
				if (((fDatePhrase2 != null) ? fDatePhrase2.Length : 0) > 0)
				{
					string arg_74_0 = this.FDatePhrase;
					string fDatePhrase3 = this.FDatePhrase;
					if (arg_74_0[((fDatePhrase3 != null) ? fDatePhrase3.Length : 0) - 1] == ')')
					{
						string fDatePhrase4 = this.FDatePhrase;
						int num = (fDatePhrase4 != null) ? fDatePhrase4.Length : 0;
						this.FDatePhrase = this.FDatePhrase.Remove(num - 1, 1);
					}
				}
			}
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDatePhrase = "";
		}

		protected override string GetStringValue()
		{
			return string.Concat(new string[]
			{
				"INT ", 
				base.GetStringValue(), 
				" ", 
				"(", 
				this.FDatePhrase, 
				")"
			});
		}

		private string ExtractPhrase([In] string S)
		{
			string Result = S;
			string fDatePhrase = this.FDatePhrase;
			if (((fDatePhrase != null) ? fDatePhrase.Length : 0) >= 2 && this.FDatePhrase[0] == '(')
			{
				Result = Result.Remove(0, 1);
				int C = 0;

				int num = (Result != null) ? Result.Length : 0;
				int I = 1;
				if (num >= I)
				{
					num++;
					while (true)
					{
						if (Result[I - 1] == '(')
						{
							C++;
						}
						else
						{
							if (Result[I - 1] == ')' || I == ((Result != null) ? Result.Length : 0))
							{
								C--;
								if (C <= 0 || I == ((Result != null) ? Result.Length : 0))
								{
									break;
								}
							}
						}
						I++;
						if (I == num)
						{
							return Result;
						}
					}
					if (Result[I - 1] == ')')
					{
						this.FDatePhrase = SysUtils.WStrCopy(Result, 1, I - 1);
					}
					else
					{
						this.FDatePhrase = SysUtils.WStrCopy(Result, 1, I);
					}
					Result = Result.Remove(0, I);
				}
			}
			return Result;
		}

		public override string ParseString([In] string S)
		{
			string Result = S;
			if (!string.IsNullOrEmpty(Result))
			{
				Result = base.ExtractDelimiter(Result, 0);
				if (Result.Substring(0, 3).ToUpper() == "INT")
				{
					Result = Result.Remove(0, 3);
				}
				Result = base.ExtractDelimiter(Result, 0);
				Result = base.ParseString(Result);
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.ExtractPhrase(Result);
			}
			return Result;
		}

		public TGEDCOMDateInterpreted(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
