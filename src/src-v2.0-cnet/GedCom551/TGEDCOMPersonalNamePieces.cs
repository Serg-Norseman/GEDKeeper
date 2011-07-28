using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPersonalNamePieces : TGEDCOMTagWithLists
	{
		public enum TGEDCOMNamePieceType : byte
		{
			nptPrefix,
			nptGiven,
			nptNickname,
			nptSurnamePrefix,
			nptSurname,
			nptSuffix
		}

		[Browsable(false)]
		public string Prefix
		{
			get
			{
				return this.GetStringTag(1);
			}
			set
			{
				this.SetStringTag(1, value);
			}
		}
		[Browsable(false)]
		public string Given
		{
			get
			{
				return this.GetStringTag(2);
			}
			set
			{
				this.SetStringTag(2, value);
			}
		}
		[Browsable(false)]
		public string Nickname
		{
			get
			{
				return this.GetStringTag(3);
			}
			set
			{
				this.SetStringTag(3, value);
			}
		}
		[Browsable(false)]
		public string SurnamePrefix
		{
			get
			{
				return this.GetStringTag(4);
			}
			set
			{
				this.SetStringTag(4, value);
			}
		}
		[Browsable(false)]
		public string Surname
		{
			get
			{
				return this.GetStringTag(5);
			}
			set
			{
				this.SetStringTag(5, value);
			}
		}
		[Browsable(false)]
		public string Suffix
		{
			get
			{
				return this.GetStringTag(6);
			}
			set
			{
				this.SetStringTag(6, value);
			}
		}
		[Browsable(false)]
		public string PatronymicName
		{
			get
			{
				return this.GetStringTag(7);
			}
			set
			{
				this.SetStringTag(7, value);
			}
		}
		[Browsable(false)]
		public string MarriedName
		{
			get
			{
				return this.GetStringTag(8);
			}
			set
			{
				this.SetStringTag(8, value);
			}
		}
		[Browsable(false)]
		public string ReligiousName
		{
			get
			{
				return this.GetStringTag(9);
			}
			set
			{
				this.SetStringTag(9, value);
			}
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			switch (Index)
			{
				case 1:
				{
					Result = base.GetTagStringValue("NPFX");
					break;
				}
				case 2:
				{
					Result = base.GetTagStringValue("GIVN");
					break;
				}
				case 3:
				{
					Result = base.GetTagStringValue("NICK");
					break;
				}
				case 4:
				{
					Result = base.GetTagStringValue("SPFX");
					break;
				}
				case 5:
				{
					Result = base.GetTagStringValue("SURN");
					break;
				}
				case 6:
				{
					Result = base.GetTagStringValue("NSFX");
					break;
				}
				case 7:
				{
					Result = base.GetTagStringValue("_PATN");
					break;
				}
				case 8:
				{
					Result = base.GetTagStringValue("_MARN");
					break;
				}
				case 9:
				{
					Result = base.GetTagStringValue("_RELN");
					break;
				}
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			switch (Index)
			{
				case 1:
				{
					base.SetTagStringValue("NPFX", Value);
					break;
				}
				case 2:
				{
					base.SetTagStringValue("GIVN", Value);
					break;
				}
				case 3:
				{
					base.SetTagStringValue("NICK", Value);
					break;
				}
				case 4:
				{
					base.SetTagStringValue("SPFX", Value);
					break;
				}
				case 5:
				{
					base.SetTagStringValue("SURN", Value);
					break;
				}
				case 6:
				{
					base.SetTagStringValue("NSFX", Value);
					break;
				}
				case 7:
				{
					base.SetTagStringValue("_PATN", Value);
					break;
				}
				case 8:
				{
					base.SetTagStringValue("_MARN", Value);
					break;
				}
				case 9:
				{
					base.SetTagStringValue("_RELN", Value);
					break;
				}
			}
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stSource
			}));
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			string[] aTagSorting = new string[0];
			this.SaveTagsToStream(AStream, aTagSorting);
			if (this.FNotes != null)
			{
				this.FNotes.SaveToStream(AStream);
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.SaveToStream(AStream);
			}
		}

		public TGEDCOMPersonalNamePieces(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
