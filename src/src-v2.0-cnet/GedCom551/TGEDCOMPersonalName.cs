using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPersonalName : TGEDCOMTag
	{
		public enum TGEDCOMNameType : byte
		{
			ntNone,
			ntAka,
			ntBirth,
			ntImmigrant,
			ntMaiden,
			ntMarried
		}

		internal TGEDCOMPersonalNamePieces FPieces;

		[Browsable(false)]
		public string FullName
		{
			get
			{
				return this.GetFullName();
			}
		}
		[Browsable(false)]
		public string FirstPart
		{
			get
			{
				return this.GetFirstPart();
			}
		}
		[Browsable(false)]
		public string Surname
		{
			get
			{
				return this.GetSurname();
			}
			set
			{
				this.SetSurname(value);
			}
		}
		[Browsable(false)]
		public string LastPart
		{
			get
			{
				return this.GetLastPart();
			}
		}
		[Browsable(false)]
		public TGEDCOMPersonalNamePieces Pieces
		{
			get
			{
				return this.FPieces;
			}
		}
		[Browsable(false)]
		public TGEDCOMPersonalName.TGEDCOMNameType NameType
		{
			get
			{
				return this.GetNameType();
			}
			set
			{
				this.SetNameType(value);
			}
		}
		internal string GetFirstPart()
		{
			string Result = base.StringValue;
			if (BDSSystem.Pos("/", Result) > 0)
			{
				Result = VCLUtils.TrimRight(BDSSystem.WStrCopy(Result, 1, BDSSystem.Pos("/", Result) - 1));
			}
			return Result;
		}
		internal string GetFullName()
		{
			string Result = base.StringValue;
			while (BDSSystem.Pos("/", Result) > 0)
			{
				int num = BDSSystem.Pos("/", Result);
				Result = Result.Remove(num - 1, 1);
			}
			return Result;
		}
		internal string GetLastPart()
		{
			string Result;
			if (BDSSystem.Pos("/", base.StringValue) > 0)
			{
				Result = BDSSystem.WStrCopy(base.StringValue, BDSSystem.Pos("/", base.StringValue) + 1, 2147483647);
				if (BDSSystem.Pos("/", Result) > 0)
				{
					Result = VCLUtils.TrimLeft(BDSSystem.WStrCopy(Result, BDSSystem.Pos("/", Result) + 1, 2147483647));
				}
				else
				{
					Result = "";
				}
			}
			else
			{
				Result = "";
			}
			return Result;
		}
		internal string GetSurname()
		{
			string Result;
			if (BDSSystem.Pos("/", base.StringValue) > 0)
			{
				Result = BDSSystem.WStrCopy(base.StringValue, BDSSystem.Pos("/", base.StringValue) + 1, 2147483647);
				if (BDSSystem.Pos("/", Result) > 0)
				{
					Result = BDSSystem.WStrCopy(Result, 1, BDSSystem.Pos("/", Result) - 1);
				}
			}
			else
			{
				Result = "";
			}
			return Result;
		}
		internal void SetSurname([In] string Value)
		{
			base.StringValue = string.Concat(new string[]
			{
				VCLUtils.TrimLeft(this.FirstPart + " "), 
				"/", 
				Value, 
				"/", 
				VCLUtils.TrimRight(" " + this.LastPart)
			});
		}
		internal TGEDCOMPersonalName.TGEDCOMNameType GetNameType()
		{
			string S = base.GetTagStringValue("TYPE").Trim().ToUpper();
			TGEDCOMPersonalName.TGEDCOMNameType Result;
			if (BDSSystem.WStrCmp(S, "aka") == 0)
			{
				Result = TGEDCOMPersonalName.TGEDCOMNameType.ntAka;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "birth") == 0)
				{
					Result = TGEDCOMPersonalName.TGEDCOMNameType.ntBirth;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "immigrant") == 0)
					{
						Result = TGEDCOMPersonalName.TGEDCOMNameType.ntImmigrant;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "maiden") == 0)
						{
							Result = TGEDCOMPersonalName.TGEDCOMNameType.ntMaiden;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "married") == 0)
							{
								Result = TGEDCOMPersonalName.TGEDCOMNameType.ntMarried;
							}
							else
							{
								Result = TGEDCOMPersonalName.TGEDCOMNameType.ntNone;
							}
						}
					}
				}
			}
			return Result;
		}
		internal void SetNameType([In] TGEDCOMPersonalName.TGEDCOMNameType Value)
		{
			string S = "";
			switch (Value)
			{
				case TGEDCOMPersonalName.TGEDCOMNameType.ntNone:
				{
					S = "";
					break;
				}
				case TGEDCOMPersonalName.TGEDCOMNameType.ntAka:
				{
					S = "aka";
					break;
				}
				case TGEDCOMPersonalName.TGEDCOMNameType.ntBirth:
				{
					S = "birth";
					break;
				}
				case TGEDCOMPersonalName.TGEDCOMNameType.ntImmigrant:
				{
					S = "immigrant";
					break;
				}
				case TGEDCOMPersonalName.TGEDCOMNameType.ntMaiden:
				{
					S = "maiden";
					break;
				}
				case TGEDCOMPersonalName.TGEDCOMNameType.ntMarried:
				{
					S = "married";
					break;
				}
			}
			base.SetTagStringValue("TYPE", S);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "NAME";
			this.FPieces = new TGEDCOMPersonalNamePieces(AOwner, this, "", "");
			this.FPieces.SetLevel(base.Level);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				object fPieces = this.FPieces;
				VCLUtils.FreeAndNil(ref fPieces);
				this.FPieces = (fPieces as TGEDCOMPersonalNamePieces);
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "TYPE") == 0 || BDSSystem.WStrCmp(ATag, "FONE") == 0 || BDSSystem.WStrCmp(ATag, "ROMN") == 0)
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			else
			{
				Result = this.FPieces.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}
		public override void Assign(TGEDCOMCustomTag Source)
		{
			base.Assign(Source);
			if (Source is TGEDCOMPersonalName)
			{
				this.FPieces.Assign(((TGEDCOMPersonalName)Source).Pieces);
			}
		}
		public override void Clear()
		{
			base.Clear();
			if (this.FPieces != null)
			{
				this.FPieces.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FPieces.IsEmpty();
		}

		public override void Pack()
		{
			base.Pack();
			this.FPieces.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.FPieces.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			this.FPieces.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this.FPieces.SaveToStream(AStream);
		}

		public void SetNameParts([In] string FirstPart, [In] string Surname, [In] string LastPart)
		{
			base.StringValue = string.Concat(new string[]
			{
				VCLUtils.TrimLeft(FirstPart + " "), 
				"/", 
				Surname, 
				"/", 
				VCLUtils.TrimRight(" " + LastPart)
			});
		}

		public TGEDCOMPersonalName(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
