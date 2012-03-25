using System;
using System.Runtime.InteropServices;

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

			string phrase = this.FDatePhrase;
			if (!string.IsNullOrEmpty(phrase))
			{
				if (phrase[0] == '(')
				{
					phrase = phrase.Remove(0, 1);
				}

				if (phrase.Length > 0 && phrase[phrase.Length - 1] == ')')
				{
					this.FDatePhrase = phrase.Remove(phrase.Length - 1, 1);
				}
			}
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDatePhrase = "";
		}

		protected override string GetStringValue()
		{
			return ("INT " + base.GetStringValue() + " " + "(" + this.FDatePhrase + ")");
		}

		private string ExtractPhrase([In] string S)
		{
			string result = S;
			if (result.Length >= 2 && result[0] == '(')
			{
				result = result.Remove(0, 1);

				int C = 0;
				int num = result.Length;
				for (int I = 1; I <= num; I++)
				{
					if (result[I - 1] == '(')
					{
						C++;
					}
					else
					{
						if (result[I - 1] == ')' || I == result.Length)
						{
							C--;
							if (C <= 0 || I == result.Length)
							{
								if (result[I - 1] == ')')
								{
									this.FDatePhrase = result.Substring(0, I - 1);
								}
								else
								{
									this.FDatePhrase = result.Substring(0, I);
								}
								result = result.Remove(0, I);
								break;
							}
						}
					}
				}
			}
			return result;
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

		public TGEDCOMDateInterpreted(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
