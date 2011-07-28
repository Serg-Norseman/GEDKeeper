using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public abstract class TGEDCOMCustomDate : TGEDCOMTag
	{

		[Browsable(false)]
		public DateTime Date
		{
			get
			{
				return this.GetDateTime();
			}
			set
			{
				this.SetDateTime(value);
			}
		}

		protected internal abstract DateTime GetDateTime();
		protected internal abstract void SetDateTime(DateTime Value);
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "DATE";
		}

		public override string ParseString([In] string AString)
		{
			string temp = AString;

			if (temp != "" && (this.FOwner as TGEDCOMTree).Header.CharacterSet == TGEDCOMObject.TGEDCOMCharacterSet.csUTF8)
			{
				temp = BDSSystem.StrToUtf8(AString);
			}

			return base.ParseString(temp);
		}

		public TGEDCOMCustomDate(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
