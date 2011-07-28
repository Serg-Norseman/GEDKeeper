using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMMap : TGEDCOMTag
	{
		[Browsable(false)]
		public string Lati
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
		public string Long
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

		internal string GetStringTag([In] int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index == 2)
				{
					Result = base.GetTagStringValue("LONG");
				}
			}
			else
			{
				Result = base.GetTagStringValue("LATI");
			}
			return Result;
		}

		internal void SetStringTag([In] int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index == 2)
				{
					base.SetTagStringValue("LONG", Value);
				}
			}
			else
			{
				base.SetTagStringValue("LATI", Value);
			}
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "MAP";
		}

		public TGEDCOMMap(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
