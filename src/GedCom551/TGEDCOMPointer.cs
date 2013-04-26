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
			get { return TGEDCOMObject.CleanXRef(this.FXRef); }
			set { this.FXRef = TGEDCOMObject.EncloseXRef(value); }
		}

		protected TGEDCOMRecord GetValue()
		{
			return base.FindRecord(this.XRef);
		}

		protected void SetValue(TGEDCOMRecord AValue)
		{
			this.FXRef = "";
			if (AValue != null)
			{
				string xrf = AValue.XRef;
				if (string.IsNullOrEmpty(xrf))
				{
					xrf = AValue.NewXRef();
				}
				this.XRef = xrf;
			}
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
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
			this.XRef = aMap.FindNewXRef(this.XRef);
		}

		public void SetNamedValue([In] string aName, TGEDCOMRecord aValue)
		{
			base.Name = aName;
			this.SetValue(aValue);
		}

		public TGEDCOMPointer(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMPointer(AOwner, AParent, AName, AValue);
		}
	}
}
