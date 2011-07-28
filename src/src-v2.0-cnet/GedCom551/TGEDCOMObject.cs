using GKSys;
using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

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
		public enum TGEDCOMRestriction : byte
		{
			rnNone, rnConfidential, rnLocked, rnPrivacy
		}

		public enum TGEDCOMCharacterSet : byte
		{
			csASCII, csANSEL, csUNICODE, csUTF8
		}

		public enum TGEDCOMSex : byte
		{
			svNone, svMale, svFemale, svUndetermined
		}

		public enum TGEDCOMSubList : byte
		{
			stNotes, stSource, stMultimedia
		}

		private object FExtData;

		public object ExtData
		{
			get	{ return this.FExtData;	}
			set	{ this.FExtData = value; }
		}

		protected internal TGEDCOMTag CreateGEDCOMTag(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string ATag, [In] string AValue)
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

		protected internal string ExtractDelimiter([In] string S, int Max)
		{
			string Result = S;

			while (((Result != null) ? Result.Length : 0) > 0 && Result[0] == ' ')
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
			return Result;
		}

		protected internal string ExtractDotDelimiter([In] string S, int Max)
		{
			string Result = S;
			while (((Result != null) ? Result.Length : 0) > 0 && Result[0] == '.')
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
			return Result;
		}

		protected internal string ExtractString([In] string S, ref string AString, [In] string ADefault)
		{
			string Result = S;
			int I = 0;
			while (I < ((Result != null) ? Result.Length : 0) && Result[I + 1 - 1] != ' ')
			{
				I++;
			}
			if (I > 0)
			{
				AString = BDSSystem.WStrCopy(Result, 1, I);
				Result = Result.Remove(0, I);
			}
			else
			{
				AString = ADefault;
			}
			return Result;
		}

		protected internal string ExtractXRef([In] string S, ref string AXRef, bool NoException, [In] string ADefault)
		{
			string Result = S;
			if (((Result != null) ? Result.Length : 0) > 0 && Result[0] == '@')
			{
				int P = BDSSystem.Pos("@", BDSSystem.WStrCopy(Result, 2, 2147483647));
				if (P > 0)
				{
					AXRef = BDSSystem.WStrCopy(Result, 2, P - 1);
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
			string Result = XRef;
			if (XRef != "")
			{
				if (Result[0] == '@') Result = Result.Remove(0, 1);

				if (((Result != null) ? Result.Length : 0) > 0 && Result[((Result != null) ? Result.Length : 0) - 1] == '@')
				{
					int num = (Result != null) ? Result.Length : 0;
					Result = Result.Remove(num - 1, 1);
				}
			}
			return Result;
		}

		public static string EncloseXRef(string XRef)
		{
			if (XRef != "")
			{
				if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(XRef, 1, 1), "@") != 0)
				{
					XRef = "@" + XRef;
				}
				if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(XRef, (XRef != null) ? XRef.Length : 0, 1), "@") != 0)
				{
					XRef += "@";
				}
			}
			return XRef;
		}

		public static string ExtractNumber([In] string S, ref int N, bool NoException, int ADefault)
		{
			string Result = S;
			int I = 0;
			while (I < ((Result != null) ? Result.Length : 0) && TGKSys.IsDigit(Result[I + 1 - 1]))
			{
				I++;
			}
			if (I > 0)
			{
				N = int.Parse(BDSSystem.WStrCopy(Result, 1, I));
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

	}
}
