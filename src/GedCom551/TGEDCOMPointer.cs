using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPointer : TGEDCOMTag
	{
		protected string FXRef = "";

		public TGEDCOMRecord Value
		{
			get	{ return this.GetValue(); }
			set	{ this.SetValue(value); }
		}

		public string XRef
		{
			get { return this.GetXRef(); }
			set { this.SetXRef(value); }
		}

		protected TGEDCOMRecord GetValue()
		{
			return base.FindRecord(this.GetXRef());
		}

		protected void SetValue(TGEDCOMRecord AValue)
		{
			this.FXRef = "";
			if (AValue != null)
			{
				string XRef = AValue.XRef;
				if (string.IsNullOrEmpty(XRef))
				{
					XRef = AValue.NewXRef();
				}
				this.SetXRef(XRef);
			}
		}

		private string GetXRef()
		{
			return TGEDCOMObject.CleanXRef(this.FXRef);
		}

		private void SetXRef([In] string Value)
		{
			this.FXRef = TGEDCOMObject.EncloseXRef(Value);
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FXRef = "";
		}

		protected override string GetStringValue()
		{
			return this.FXRef;
		}

		public override bool IsEmpty()
		{
			return (string.IsNullOrEmpty(this.FXRef));
		}

		public override string ParseString([In] string AString)
		{
			this.FXRef = "";
			string result = AString;
			result = base.ExtractDelimiter(result, 0);

			if (!string.IsNullOrEmpty(result) && result[0] == '@' && result[1] != '#')
			{
				int pos = result.IndexOf('@', 2);
				if (pos > 0)
				{
					pos++;
					this.FXRef = result.Substring(0, pos);
					result = result.Remove(0, pos);
				}
			}
			return result;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.SetXRef(aMap.FindNewXRef(this.XRef));
		}

		public void SetNamedValue([In] string aName, TGEDCOMRecord aValue)
		{
			base.Name = aName;
			this.SetValue(aValue);
		}

		public TGEDCOMPointer(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
