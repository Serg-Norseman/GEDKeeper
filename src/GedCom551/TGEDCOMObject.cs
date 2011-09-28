using System;
using System.Runtime.InteropServices;
using System.Text;

using GKCore.Sys;

namespace GedCom551
{
	public class EGEDCOMException : Exception
	{
		public EGEDCOMException()
		{
		}

		public EGEDCOMException(string message) : base(message)
		{
		}

		public EGEDCOMException(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public class TGEDCOMObject
	{
		public const char GEDCOMDelimiter = ' ';
		public const char GEDCOMYearModifierSeparator = '/';
		public const string GEDCOMYearBC = "B.C."; // const restored
		public const char GEDCOMPointerDelimiter = '@';
		public const byte GEDCOMMaxPhoneNumbers = 3;
		public const byte GEDCOMMaxEmailAddresses = 3;
		public const byte GEDCOMMaxFaxNumbers = 3;
		public const byte GEDCOMMaxWebPages = 3;
		public const byte GEDCOMMaxLanguages = 3;
		//const string GEDCOMNewLine = "#13#10";

		private object FExtData;

		public object ExtData
		{
			get	{ return this.FExtData;	}
			set	{ this.FExtData = value; }
		}

		protected TGEDCOMTag CreateGEDCOMTag(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string ATag, [In] string AValue)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = new TGEDCOMDateValue(AOwner, AParent, "DATE", AValue);
			}
			else
			{
				if (ATag == "TIME")
				{
					Result = new TGEDCOMTime(AOwner, AParent, "TIME", AValue);
				}
				else
				{
					if (ATag == "ADDR")
					{
						Result = new TGEDCOMAddress(AOwner, AParent, "ADDR", AValue);
					}
					else
					{
						Result = new TGEDCOMTag(AOwner, AParent, ATag, AValue);
					}
				}
			}

			return Result;
		}

		protected string ExtractDelimiter([In] string S, int Max)
		{
			string Result = S;

			if (Result != null)
			{
				while (Result.Length > 0 && Result[0] == ' ')
				{
					Result = Result.Remove(0, 1);
					if (Max > 0)
					{
						Max--;
						if (Max == 0)
						{
							break;
						}
					}
				}
			}

			return Result;
		}

		protected string ExtractDotDelimiter([In] string S, int Max)
		{
			string Result = S;

			if (Result != null)
			{
				while (Result.Length > 0 && Result[0] == '.')
				{
					Result = Result.Remove(0, 1);
					if (Max > 0)
					{
						Max--;
						if (Max == 0)
						{
							break;
						}
					}
				}
			}

			return Result;
		}

		protected string ExtractString([In] string S, out string AString, [In] string ADefault)
		{
			string Result = S;
			int I = 0;

			if (Result != null)
			{
				while (I < Result.Length && Result[I] != ' ')
				{
					I++;
				}
			}

			if (I > 0)
			{
				AString = Result.Substring(0, I);
				Result = Result.Remove(0, I);
			}
			else
			{
				AString = ADefault;
			}

			return Result;
		}

		protected string ExtractXRef([In] string S, out string AXRef, bool NoException, [In] string ADefault)
		{
			string Result = S;

			if (((Result != null) ? Result.Length : 0) > 0 && Result[0] == '@')
			{
				int P = Result.IndexOf('@', 1);
				if (P > 0)
				{
					AXRef = Result.Substring(1, P - 1);
					Result = Result.Remove(0, P + 1);
				}
				else
				{
					if (!NoException)
					{
						throw new EGEDCOMException(string.Format("The string {0} contains an unterminated XRef pointer", new object[]
						{
							S
						}));
					}
					AXRef = ADefault;
				}
			}
			else
			{
				if (!NoException)
				{
					throw new EGEDCOMException(string.Format("The string {0} is expected to start with an XRef pointer", new object[]
					{
						S
					}));
				}
				AXRef = ADefault;
			}
			return Result;
		}

		public static string CleanXRef([In] string XRef)
		{
			string result = XRef;

			if (result != null && result != "")
			{
				if (result[0] == '@')
				{
					result = result.Remove(0, 1);
				}

				if (result.Length > 0 && result[result.Length - 1] == '@')
				{
					result = result.Remove(result.Length - 1, 1);
				}
			}

			return result;
		}

		public static string EncloseXRef(string XRef)
		{
			if (XRef != null && XRef != "")
			{
				if (XRef[0] != '@')
				{
					XRef = "@" + XRef;
				}

				if (XRef[XRef.Length - 1] != '@')
				{
					XRef += "@";
				}
			}
			return XRef;
		}

		public static string ExtractNumber([In] string S, out int N, bool NoException, int ADefault)
		{
			string Result = S;
			int I = 0;

			if (Result != null)
			{
				while (I < Result.Length && SysUtils.IsDigit(Result[I]))
				{
					I++;
				}
			}

			if (I > 0)
			{
				N = int.Parse(Result.Substring(0, I));
				Result = Result.Remove(0, I);
			}
			else
			{
				if (!NoException)
				{
					throw new EGEDCOMException(string.Format("The string {0} doesn't start with a valid number", new object[]
					{
						S
					}));
				}
				N = ADefault;
			}
			return Result;
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		public static Encoding GetEncodingByCharacterSet(TGEDCOMCharacterSet cs)
		{
			Encoding res;
			switch (cs) {
				case TGEDCOMCharacterSet.csASCII:
					res = Encoding.GetEncoding(1251);
					break;
				case TGEDCOMCharacterSet.csUNICODE:
					res = Encoding.Unicode;
					break;
				case TGEDCOMCharacterSet.csUTF8:
					res = Encoding.UTF8;
					break;
				default:
					res = Encoding.Default; // alert: ANSEL, etc
					break;
			}
			return res;
		}

		protected static string PriorityToStr([In] TResearchPriority val)
		{
			string S = "";
			switch (val)
			{
				case TResearchPriority.rpNone:
				{
					S = "";
					break;
				}
				case TResearchPriority.rpLow:
				{
					S = "low";
					break;
				}
				case TResearchPriority.rpNormal:
				{
					S = "normal";
					break;
				}
				case TResearchPriority.rpHigh:
				{
					S = "high";
					break;
				}
				case TResearchPriority.rpTop:
				{
					S = "top";
					break;
				}
			}
			return S;
		}

		protected static TResearchPriority StrToPriority([In] string S)
		{
			TResearchPriority result;

			if (S == "low")
			{
				result = TResearchPriority.rpLow;
			}
			else
			{
				if (S == "normal")
				{
					result = TResearchPriority.rpNormal;
				}
				else
				{
					if (S == "high")
					{
						result = TResearchPriority.rpHigh;
					}
					else
					{
						if (S == "top")
						{
							result = TResearchPriority.rpTop;
						}
						else
						{
							result = TResearchPriority.rpNone;
						}
					}
				}
			}
			
			return result;
		}

		protected TGEDCOMRestriction GetRestrictionVal(string S)
		{
			TGEDCOMRestriction res;
			if (S == "CONFIDENTIAL")
			{
				res = TGEDCOMRestriction.rnConfidential;
			}
			else if (S == "LOCKED")
			{
				res = TGEDCOMRestriction.rnLocked;
			}
			else if (S == "PRIVACY")
			{
				res = TGEDCOMRestriction.rnPrivacy;
			}
			else
			{
				res = TGEDCOMRestriction.rnNone;
			}
			return res;
		}

		protected string GetRestrictionStr([In] TGEDCOMRestriction val)
		{
			string S;
			switch (val) {
				case TGEDCOMRestriction.rnConfidential:
					S = "confidential";
					break;
				case TGEDCOMRestriction.rnLocked:
					S = "locked";
					break;
				case TGEDCOMRestriction.rnPrivacy:
					S = "privacy";
					break;
				default:
					S = "";
					break;
			}
			return S;
		}

	}
}
