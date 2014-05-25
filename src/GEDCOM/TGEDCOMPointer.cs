using System;

namespace GedCom551
{
	public class TGEDCOMPointer : TGEDCOMTag
	{
		protected string FXRef = "";

		public TGEDCOMRecord Value
		{
			get	{
				return base.FindRecord(this.XRef);
			}
			set	{
				this.FXRef = "";
				if (value != null)
				{
					string xrf = value.XRef;
					if (string.IsNullOrEmpty(xrf))
					{
						xrf = value.NewXRef();
					}
					this.XRef = xrf;
				}
			}
		}

		public string XRef
		{
			get { return GEDCOMUtils.CleanXRef(this.FXRef); }
			set { this.FXRef = GEDCOMUtils.EncloseXRef(value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
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

		public override string ParseString(string strValue)
		{
			this.FXRef = "";
			string result = strValue;
			result = GEDCOMUtils.ExtractDelimiter(result, 0);

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

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.XRef = map.FindNewXRef(this.XRef);
		}

		public void SetNamedValue(string aName, TGEDCOMRecord aValue)
		{
			base.Name = aName;
			this.Value = aValue;
		}

		public TGEDCOMPointer(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMPointer(owner, parent, tagName, tagValue);
		}
	}
}
