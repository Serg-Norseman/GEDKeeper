using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMPersonalName : TGEDCOMTag
	{
		private TGEDCOMPersonalNamePieces FPieces;

		public string FullName
		{
			get { return this.GetFullName(); }
		}

		public string FirstPart
		{
			get { return this.GetFirstPart(); }
		}

		public string Surname
		{
			get { return this.GetSurname(); }
			set { this.SetSurname(value); }
		}

		public string LastPart
		{
			get { return this.GetLastPart(); }
		}

		public TGEDCOMPersonalNamePieces Pieces
		{
			get { return this.FPieces; }
		}

		public TGEDCOMNameType NameType
		{
			get { return this.GetNameType(); }
			set { this.SetNameType(value); }
		}

		private string GetFirstPart()
		{
			string Result = base.StringValue;
			if (SysUtils.Pos("/", Result) > 0)
			{
				Result = SysUtils.TrimRight(SysUtils.WStrCopy(Result, 1, SysUtils.Pos("/", Result) - 1));
			}
			return Result;
		}

		private string GetFullName()
		{
			string Result = base.StringValue;
			while (SysUtils.Pos("/", Result) > 0)
			{
				int num = SysUtils.Pos("/", Result);
				Result = Result.Remove(num - 1, 1);
			}
			return Result;
		}

		private string GetLastPart()
		{
			string Result;
			if (SysUtils.Pos("/", base.StringValue) > 0)
			{
				Result = SysUtils.WStrCopy(base.StringValue, SysUtils.Pos("/", base.StringValue) + 1, 2147483647);
				if (SysUtils.Pos("/", Result) > 0)
				{
					Result = SysUtils.TrimLeft(SysUtils.WStrCopy(Result, SysUtils.Pos("/", Result) + 1, 2147483647));
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

		private string GetSurname()
		{
			string Result;
			if (SysUtils.Pos("/", base.StringValue) > 0)
			{
				Result = SysUtils.WStrCopy(base.StringValue, SysUtils.Pos("/", base.StringValue) + 1, 2147483647);
				if (SysUtils.Pos("/", Result) > 0)
				{
					Result = SysUtils.WStrCopy(Result, 1, SysUtils.Pos("/", Result) - 1);
				}
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		private void SetSurname([In] string Value)
		{
			base.StringValue = string.Concat(new string[]
			{
				SysUtils.TrimLeft(this.FirstPart + " "), 
				"/", 
				Value, 
				"/", 
				SysUtils.TrimRight(" " + this.LastPart)
			});
		}

		private TGEDCOMNameType GetNameType()
		{
			string S = base.GetTagStringValue("TYPE").Trim().ToUpper();
			TGEDCOMNameType Result;
			if (S == "aka")
			{
				Result = TGEDCOMNameType.ntAka;
			}
			else
			{
				if (S == "birth")
				{
					Result = TGEDCOMNameType.ntBirth;
				}
				else
				{
					if (S == "immigrant")
					{
						Result = TGEDCOMNameType.ntImmigrant;
					}
					else
					{
						if (S == "maiden")
						{
							Result = TGEDCOMNameType.ntMaiden;
						}
						else
						{
							if (S == "married")
							{
								Result = TGEDCOMNameType.ntMarried;
							}
							else
							{
								Result = TGEDCOMNameType.ntNone;
							}
						}
					}
				}
			}
			return Result;
		}

		private void SetNameType([In] TGEDCOMNameType Value)
		{
			string S = "";
			switch (Value)
			{
				case TGEDCOMNameType.ntNone:
				{
					S = "";
					break;
				}
				case TGEDCOMNameType.ntAka:
				{
					S = "aka";
					break;
				}
				case TGEDCOMNameType.ntBirth:
				{
					S = "birth";
					break;
				}
				case TGEDCOMNameType.ntImmigrant:
				{
					S = "immigrant";
					break;
				}
				case TGEDCOMNameType.ntMaiden:
				{
					S = "maiden";
					break;
				}
				case TGEDCOMNameType.ntMarried:
				{
					S = "married";
					break;
				}
			}
			base.SetTagStringValue("TYPE", S);
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
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
				this.FPieces.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "TYPE" || ATag == "FONE" || ATag == "ROMN")
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
				SysUtils.TrimLeft(FirstPart + " "), "/", Surname, "/", SysUtils.TrimRight(" " + LastPart)
			});
		}

		public TGEDCOMPersonalName(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
